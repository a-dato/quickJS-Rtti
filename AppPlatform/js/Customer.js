export class Customer {
  constructor(id, n) {
	this._id = id;
    this._Name = n;
	this._Address = "";
    this._Age = 0;
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

  get Age() {
	return this._Age;
  }

  set Age(age) {
	this._Age = age;
  }
  
}

export class CustomerType {
	static register() {
		globalThis.Customer = Customer;
		globalThis.CustomerType = CustomerType;
		const ct = new CustomerType();
		app.Config.RegisterJSType(ct);
	}

	constructor() {
		this.Binder = new JSBinder();
		this.Builder = new JSFrameBuilder();
		this.Provider = new CustomerProvider();
	}
  
	GetType() {
		return new Customer();
	}
}

class CustomerProvider {
	constructor() {
		this._Data = null;
	}
	
	get Data() {
		if(this._Data == null) {
			this._Data = new List();
			for(let i=0; i<10000;i++) {
				var c = new Customer(i, 'Customer ' + i);
				c.Address = 'Zuideindseweg ' + i;
				c.Age = i;
				this._Data.Add(c);
			}
		}

		return this._Data;
	}
	
	uri(customer) {
		return 'https:////lynx.a-dato.com//customer//' + customer.ID.toString();
	}

	locate(ID) {
		return ID;
	}	
}
