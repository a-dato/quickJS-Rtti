import {Object_, Provider_, TypeDescriptor_, registerType} from '../platform.js';

export class Invoice extends Object_
{
  ToString()
  {
    return this.Name;
  }
}

class InvoiceProvider extends Provider_
{
  Data(filter)
  {
    return [{ Name: 'First Invoice' }];
  }
}

class InvoiceType extends TypeDescriptor_
{
  constructor()
  {
    super();
    this.Provider = new InvoiceProvider();
    this.PropertyDescriptor = {
      Name: { Type: 'string' },
      Project: { Type: 'IProject' }
    };
  }

  get StorageName()
  {
    return 'Invoices';
  }

  CreateInstance()
  {
    return new Invoice();
  }
}

registerType(Invoice, new InvoiceType());