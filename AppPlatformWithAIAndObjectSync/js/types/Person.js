import { Object_, Provider_, TypeDescriptor_, registerType } from '../platform.js';

export class Person extends Object_ {
  ToString() {
    return this.Name;
  }
}

class PersonProvider extends Provider_ {
  Data(filter) {
    return [];
  }

  Add(values) {
    return { Name: 'New person', ...values };
  }
}

class PersonType extends TypeDescriptor_ {
  constructor() {
    super();
    this.Provider = new PersonProvider();
    this.PropertyDescriptor = {
      Name: { Type: 'string' }
    };
    this.Extends = {
      IProject: {
        Person: { Type: 'Person', Label: 'Person' }
      }
    };
  }

  get StorageName() {
    return 'People';
  }

  CreateInstance() {
    return new Person();
  }
}

registerType(Person, new PersonType());