import {visible,editor,status,Project_,Task_,Type_,Provider_} from './lynx.js';

export class OPProject extends Project_ {
  constructor(id, description) {
		super(id);
		this._Description = description;
	}

  get Description() {
   return this._Description;
  }
	
	get Tasks() {
		return this._Tasks;
	}
	
	async Load(ProjectChanged) {
		this._Tasks = await OpenProjectType.instance.Provider.Tasks(this);
		this._Dependencies = await OpenProjectType.instance.Provider.Dependencies(this._Tasks);
	}
}

export class OPTask extends Task_ {
  constructor(id, description) {
		super(id);
		this._Description = description;
	}

  get Description() {
		return this._Description;
  }
}

export class OpenProjectType extends Type_ {
	static instance = null;
	
	constructor() {	
		super();
		
		//this.Binder = new JSBinder();			// IContentBinder
		this.Provider = new OPProvider();	// IContentProvider
		this.PropertyDescriptor = {
			// Object descriptor
			OPProject: {
				EditorType: editor.Combo,
				// IFormatter
				Formatter: {
					Format: (ctx, item, format) => { 
						if(item != null)
							return item.Name;
					}
				},
				Marshaller: {
					Marshal: (ctx, item) => {
						let json = {
							ID: `${item.ID}`,
							Name: `${item.Name}`
						}
						return JSON.stringify(json);
					},
					Unmarshal: (ctx, item) => {
						console.log(item);
						if(typeof item === 'string') {
							var js = JSON.parse(item);
							return new OPProject(js.ID, js.Name)
						}
					}
				},
				Picklist: {
					Items: (filter) => { return this.Provider.Data(filter) }
				}
			},
			ID: {
				Visible: false
			},
			Name: {
				EditorType: editor.Edit
			}
		}
	} 
	
	static register() {
		OpenProjectType.instance = new OpenProjectType();
		globalThis.OpenProject = OpenProjectType;
		globalThis.OPProject = OPProject;
		app.Config.RegisterType(OPProject, OpenProjectType.instance);
		console.log('OpenProject registration succesfull');
	}

	static async fetchGET(resource) {
			return fetch('https://a-dato.openproject.com/api/v3' + resource,
			{
				method: 'GET',
				headers: {
					'Authorization': `Basic ${OpenProject.Token}`,
					'Content-Type': 'application/json'
				}		
			});		
	}

  static get Token() {
		return OpenProjectType.instance.AccessToken;
  }

  static set Token(value) {
		OpenProjectType.instance.AccessToken = value;
  }

	static Projects() {
		return OpenProjectType.instance.Provider.Data(null);
	}
}

class OPProvider extends Provider_ {
	constructor() {
		super();
	}
	
	async Fetch() {
		console.log('Items loading');
		
		try {
			const response = await OpenProjectType.fetchGET('/projects');
			
			if (!response.ok) {
				throw new Error(`HTTP error! Status: ${response.status}`);
			}

			const data = await response.json();	
			console.log(data);
			const result = new List();

			for(const item of data._embedded.elements) {
				result.Add(new OPProject(item.id, item.name));
			}	
			console.log(`Items loaded: ${result.Count}`);
			return result;

		} catch(error) {
			console.log('Error fetching projects:', error);
			throw(error);
		};	
	}
		
	async Data(filter) {
		return this.Fetch();
	}
	
	async Tasks(Project) {
		console.log('Tasks loading');
		
		try {
			const response = await OpenProjectType.fetchGET(`/projects/${Project.ID}/work_packages`);

			if (!response.ok) {
				throw new Error(`HTTP error! Status: ${response.status}`);
			}

			const data = await response.json();
			// console.log(JSON.stringify(data));

			const result = new List();
			for(const item of data._embedded.elements) {
				result.Add(new OPTask(item.id, item.subject));
			}	
			console.log(`Tasks loaded: ${result.Count}`);
			return result;

		} catch(error) {
			console.log('Error fetching tasks:', error);
			throw(error);
		};		
	}
	
	async Dependencies(Tasks) {
		console.log('Dependencies loading');
		
		try {
			this.Dependency(Tasks[0]);
		
		} catch(error) {
			console.log('Error fetching sheets:', error);
			throw(error);
		}
	};	

	async Dependency(Task) {
		const response = await OpenProjectType.fetchGET(`/work_packages/${Task.ID}/relations`);
		if (!response.ok) {
			throw new Error(`HTTP error! Status: ${response.status}`);
		}

		const data = await response.json();
		console.log(JSON.stringify(data));
		
		const result = new List();
		for(const item of data._embedded.elements) {
			result.Add(new OPTask(item.id, item.subject));
		}	
		console.log(`Tasks loaded: ${result.Count}`);
		return result;
	}		
}
