import {Object_, Provider_, TypeDescriptor_, registerType} from '../platform.js';

// The object: one getter/setter per property
export class Customer extends Object_ {
  get Name() { return this._Name; }
  set Name(value) { this._Name = value; }

  get Email() { return this._Email; }
  set Email(value) { this._Email = value; }

  get Active() { return this._Active; }
  set Active(value) { this._Active = value; }

  get Address() { return this._Address; }
  set Address(value) { this._Address = value; }

  // The display text: pickers show it, and other scripts refer to a customer by it
  ToString() { return this.Name; }
}

// Where the customers come from
class CustomerProvider extends Provider_ {
  Data(filter) {
    return [
      { Name: 'Acme Corp',        Email: 'info@acme.example',      Active: true,  Address: { Street: 'Main Street 1',   Zip: '1011 AB', City: 'Amsterdam' } },
      { Name: 'Globex',           Email: 'hello@globex.example',   Active: true,  Address: { Street: 'Harbour 12',      Zip: '3011 CD', City: 'Rotterdam' } },
      { Name: 'Initech',          Email: 'office@initech.example', Active: false, Address: { Street: 'Stationsplein 5', Zip: '3511 ED', City: 'Utrecht' } },
      { Name: 'Umbrella Trading', Email: 'sales@umbrella.example', Active: true,  Address: { Street: 'Kade 7',          Zip: '2511 EF', City: 'Den Haag' } },
    ];
  }

  // Customers are this script's own data, so new ones can be made ("+ Add object")
  Add(values) {
    return { Name: 'New customer', Active: true, ...values };
  }
}

// The type info: storage and property descriptors
class CustomerType extends TypeDescriptor_ {
  constructor() {
    super();
    this.Provider = new CustomerProvider();
    this.PropertyDescriptor = {
      Name:    { Type: 'string' },
      Email:   { Type: 'string', Label: 'E-mail' },
      Active:  { Type: 'boolean' },
      Address: { Type: 'Address' },
    };
  }

  get StorageName() { return 'Customers'; }
  CreateInstance() { return new Customer(); }
}

registerType(Customer, new CustomerType());
