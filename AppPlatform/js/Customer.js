import {visible,editor,status} from './lynx_app.js';

export class Customer {
  constructor(id, n) {
		this._id = id;
    this._Name = n;
		this._Address = null;
    this._Status = status.None;
  }

  get ID() {
   return this._id;
  }

  get Name() {
   return this._Name;
  }

  set Name(name) {
   this._Name = name;
  }

  get Address() {
   return this._Address;
  }

  set Address(address) {
   this._Address = address;
  }

  get Status() {
	return this._Status;
  }

	set Status(status) {
		this._Status = status;
	}
  
	toString() {
		return this._Name;
	}
}

export class CustomerType {
	static ct = null;
	
	static register() {
		CustomerType.ct = new CustomerType();
		globalThis.Customer = Customer;
		globalThis.CustomerType = CustomerType;
		app.Config.RegisterJSType(CustomerType.ct);
	}

	constructor() {
		this.Binder = new JSBinder();			// IContentBinder
		this.Builder = new JSFrameBuilder();	// IContentBuilder
		this.Provider = new CustomerProvider();	// IContentProvider
		this.PropertyDescriptor = {
			// Object descriptor
			CustomerType: {
				EditorType: editor.Combo,
				// IFormatter
				Formatter: {
					Format: (ctx, item, format) => { 
						if(item != null)
							return item.Name;
					},
					Url: (ctx, item) => {
							return `https://lynx.a-dato.com/customer/${item.ID}`;
					},
					Marshal: (ctx, item) => {
						let json = {
							ID: `${item.ID}`,
							Value: `${item.Name}`
						}
						return JSON.stringify(json);
					},
					Unmarshal: (ctx, item) => {
						console.log('Unmarshal');
						console.log(item);
						return this.Provider.Lookup(JSON.parse(item));
					}
				},
				Picklist: {
					Items: (filter) => { return this.Provider.Data(filter) }
				}
			},
			ID: {
				Visible: false
			},
			Address: {
				EditorType: editor.Text,
				Formatter: {
					Format: (ctx, item, format) => { 				
						if(typeof item === 'string')
							item = this.Provider.Lookup(JSON.parse(item));
												
						return `Project: ${ctx.Name} @ ${item.Address.Zip}`;
					}
				}
			},
			Name: {
				EditorType: editor.Edit
			},
			Status: {
				EditorType: editor.Combo,
				Format: '#.##',
				Picklist: {
					Items: (filter) => {return Object.values(status)},
					Format: (Item) => {return Item.Name;}			
				}
			}
		}
	}
  
	GetType() {
		return new Customer();
	}
}

class CustomerProvider {
	constructor() {
		this._Data = null;
	}
	
	Data(filter) {
		if(this._Data == null) {
			this._Data = new List();
			for(let i=0; i<100;i++) {
				var c = new Customer(i, 'Customer ' + i);
				c.Address = {
					Name: `Zuideindseweg ${i}`,
					Zip: `2645BG-${i}`
				};
				c.Age = i;
				this._Data.Add(c);
			}
		}
		
		let result = null;
		if(typeof filter === 'string' && filter.trim() !== '') {
			result = new List();
			for(const item of this._Data)
				if(CustomerType.ct.Format(item).includes(filter))
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
