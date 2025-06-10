const visible = {
  hidden: 0,
  visible: 1
};

const editor = {
	Text: 0,
	Edit: 1,
	Date: 2,
	Time: 3,
	DateTime: 4, 
	Check: 5,
	Memo: 6,
	Number: 7, 
	Combo: 8, 
	ComboEdit: 9, 
	Tags: 10, 
	Color: 11, 
	CustomEditor: 12
}

const status = {
	None: 0,
	Started: 1,
	Released: 3,
	Closed: 4
}

export class Customer {
  constructor(id, n) {
	this._id = id;
    this._Name = n;
	this._Address = "";
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
		this.Binder = new JSBinder();
		this.Builder = new JSFrameBuilder();
		this.Provider = new CustomerProvider();
		this.Format = (Item) => {return Item.Name};
		this.Parse = (value) => {return this.Provider.Locate(value)};
		this.PropertyDescriptor = {
			// Object descriptor
			Customer: {
				EditorType: editor.Combo,
				Picklist: {
					Items: (filter) => {return this.Provider.Data(filter)},
					Format: this.Format,
					Parse: this.Parse
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
				c.Address = 'Zuideindseweg ' + i;
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
	
	Uri(customer) {
		return 'https:////lynx.a-dato.com//customer//' + customer.ID.toString();
	}

	Locate(ID) {
		return ID;
	}	
}
