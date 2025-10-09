import {TypeCode, ITypeDescriptor_} from './app.js';
import {visible,editor,status,LynxObject,LynxType,LynxProvider} from './lynx.js';

/*
 * Address type
*/
export class Address extends LynxObject {
  constructor(id, street, zip) {
		super(id);
    this._Street = street;
    this._Zip = zip;
  }
	
  get Street() {
   return this._Street;
  }

  set Street(value) {
   this._Street = value;
  }
	
  get Zip() {
   return this._Zip;
  }

  set Zip(value) {
   this._Zip = value;
  }
	
	ToString(format) {
		let frmt = AddressType.Instance?.PropertyDescriptor?.Address?.Formatter;
		if(frmt != undefined)
			return frmt.Format(null, this, format);
		else
			return this._Street;
	}
}

export class AddressType {
	static Instance = null;
	
	static register() {
		AddressType.Instance = new AddressType();
		globalThis.Address = Address;
		globalThis.AddressType = AddressType;
		app.Config.RegisterType(Address, AddressType.Instance);
	}

	constructor() {
		this.Binder = new JSBinder();			// IContentBinder
		this.Builder = new JSFrameBuilder();	// IContentBuilder
		this.Provider = new AddressProvider();	// IContentProvider
		this.PropertyDescriptor = {
			// object property descriptor
			Address: {
				EditorType: editor.Combo,
				// IFormatter
				Formatter: {
					Format: (ctx, item, format) => { 
						if(item != null)
							return item.Street;
					},
					Url: (ctx, item) => {
							return `https://lynx.a-dato.com/customer/${item.ID}`;
					},
				},
				Marshaller: {
					Marshal: (ctx, item) => {
						console.log('Address marshal');
						let json = {
							ID: `${item.ID}`,
							Street: `${item.Street}`
						}
						return JSON.stringify(json);
					},
					Unmarshal: (ctx, item) => {
						console.log(item);
						if(typeof item === 'string') {
							var js = JSON.parse(item);
							return new Address(js.ID, js.Street, js.Zip);
						}
						// return this.Provider.Lookup(JSON.parse(item));
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
}

class AddressProvider {
	constructor() {
		this._Data = null;
	}
	
	Data(filter) {
		if(this._Data == null) {
			this._Data = new List();
			for(let i=0; i<100;i++) {
				var c = new Address(i, `Parkway ${i}`, `2453-${i}`);
				this._Data.Add(c);
			}
		}
		
		let result = null;
		if(typeof filter === 'string' && filter.trim() !== '') {
			result = new List();
			for(const item of this._Data)
				if(AddressType.ct.Format(item).includes(filter))
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


/*
 * Customer
*/
export class Customer extends LynxObject {
  constructor(id, n) {
		super(id);
    this._Name = n;
		this._Address = null;
    this._Status = status.None;
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
  
	ToString(format) {
		let frmt = CustomerType.Instance?.PropertyDescriptor?.Customer?.Formatter;
		if(frmt != undefined)
			return frmt.Format(null, this, format);
		else
			return this._Street;
	}
}

export class CustomerType extends ITypeDescriptor_ {
	static Instance = null;
	
	static register() {
		CustomerType.Instance = new CustomerType();
		globalThis.Customer = Customer;
		globalThis.CustomerType = CustomerType;
		app.Config.RegisterType(Customer, CustomerType.Instance);
	}

	constructor() {
		super();
		this.Binder = new JSBinder();			// IContentBinder
		this.Builder = new JSFrameBuilder();	// IContentBuilder
		this.Provider = new CustomerProvider();	// IContentProvider
		this.PropertyDescriptor = {
			// IPropertyDescriptor
			Customer: {
				EditorType: editor.Combo,
				Label: 'Customer',
				// IFormatter
				Formatter: {
					Format: (ctx, item, format) => { 
						if(item != null)
							return item.Name;
					},
					Url: (ctx, item) => {
							return `https://lynx.a-dato.com/customer/${item.ID}`;
					},
				},
				Notify: {
					OnChanging: (ctx, item) => {
						return item;
					},
					OnChanged: (ctx, item) => {
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
							return new Customer(js.ID, js.Name)
							// return this.Provider.Lookup(js);
						}
					}
				},
				Picklist: {
					Items: (filter) => { 
						return app.Storage[CustomerType.StorageName()];
					}
				}
			},
			ID: {
				Type: TypeCode.Int64,
				Visible: false
			},
			Address: {
				Type: Address
			},
			Name: {
				Type: TypeCode.string,
				EditorType: editor.Edit
			},
			Status: {
				Type: TypeCode.Int32,
				EditorType: editor.Combo,
				// IFormatter
				Formatter: {
					Format: (ctx, item, format) => { 
						return item;
					}
				},
				Picklist: {
					Items: (filter) => {return Object.values(status)},
				}
			}
		}
	} 
	
	CreateInstance() {
		return new Customer();
	}
	
	static StorageName() {
		return 'Customers';
	}
}

class CustomerProvider extends LynxProvider {
	constructor() {
		super();
	}

	Data(filter) {
		let l = new List();
		for(let i=0; i<100;i++) {
			var c = new Customer(i, `Customer ${i}`);
			c.Address = new Address(i, `Parkway ${i}`, `2645BG-${i}`);
			c.Age = i;
			l.Add(c);
		}
		
		return l;
	}

/*		
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
*/
	
/*	
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
*/
}
