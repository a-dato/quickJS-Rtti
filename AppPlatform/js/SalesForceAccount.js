import {visible,editor,status} from './lynx_app.js';

export class SFAccount {
  constructor(id, name) {
		this._id = id;
		this._Name = name;
	}

  get ID() {
   return this._id;
  }

  get Name() {
   return this._Name;
  }
}

export class SFAccountType {
	static ct = null;
	
	static register() {
		SFAccountType.ct = new SFAccountType();
		globalThis.SFAccount = SFAccount;
		globalThis.SFAccountType = SFAccountType;
		app.Config.RegisterType(SFAccount, SFAccountType.ct);
	}

	constructor() {
		this.Binder = new JSBinder();			// IContentBinder
		this.Builder = new JSFrameBuilder();	// IContentBuilder
		this.Provider = new SFAccountProvider();	// IContentProvider
		this.PropertyDescriptor = {
			// Object descriptor
			SFAccount: {
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
							return new SFAccount(js.ID, js.Name)
							// return this.Provider.Lookup(js);
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
}

class SFAccountProvider {
	constructor() {
		this._Data = null;
	}
	
	async Fetch() {
		console.log('Items loading');
		
		const accessToken = '00DgK000003SlYd!AQEAQKxGBdMZ6OpK3c8XXCFD68zChSovn6VLLjNegywLOgJo3m_GR_yKBCjXguXwQ7Z0NrbuMIv9jy9f6uHc_U9zhXrWcTuM';
		try {
			const response = await fetch('https://orgfarm-768b4d1db3-dev-ed.develop.lightning.force.com/services/data/v60.0/query?q=SELECT+Id,+Name+FROM+Account',
			{
				method: 'GET',
				headers: {
					'Authorization': `Bearer ${accessToken}`,
					'Content-Type': 'application/json'
				}		
			});
			
			if (!response.ok) {
				throw new Error(`HTTP error! Status: ${response.status}`);
			}

			const data = await response.json();		
			const result = new List();
			for(const item of data.records) {
				result.Add(new SFAccount(item.Id, item.Name));
			}	
			console.log(`Items loaded: ${result.Count}`);
			return result;

		} catch(error) {
			console.log('Error fetching accounts:', error);
			throw(error);
		};	
	}
		
	async Data(filter) {
		if(this._Data == null)
			this._Data = await this.Fetch(); // Fills this._Data
		
		console.log(`Items: ${this._Data.Count}`);
		
		let result = null;
		if(typeof filter === 'string' && filter.trim() !== '') {
			result = new List();
			for(const item of this._Data)
				if(SFAccountType.ct.Format(item).includes(filter))
					result.Add(item);
		} else
			result = this._Data;
		
		return result;
	}
	
	Lookup(item) {
		let id = null;
		if(typeof item === 'number')
			id = item;
		else
			id = item.ID;
				
		if(id != null) {
			for(const c of this._Data)
				if(c.ID == id)
					return c;
		}		
	}
}
