import {Object_, TypeDescriptor_, registerType} from '../platform.js';

// A value type: no storage and no provider. Each customer has its own
// address, edited in place.
export class Address extends Object_ {
  get Street() { return this._Street; }
  set Street(value) { this._Street = value; }

  get Zip() { return this._Zip; }
  set Zip(value) { this._Zip = value; }

  get City() { return this._City; }
  set City(value) { this._City = value; }

  ToString() {
    return [this.Street, this.City].filter(Boolean).join(', ');
  }
}

class AddressType extends TypeDescriptor_ {
  constructor() {
    super();
    this.PropertyDescriptor = {
      Street: { Type: 'string' },
      Zip:    { Type: 'string', Label: 'Postal code' },
      City:   { Type: 'string' },
    };
  }

  CreateInstance() { return new Address(); }
}

registerType(Address, new AddressType());
