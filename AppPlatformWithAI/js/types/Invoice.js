import {defineType} from '../platform.js';

defineType({
  name: 'Invoice',
  storage: 'Invoices',
  display: 'Name',
  properties: {
    Name: { type: 'string' },
  },
  data: [
    { Name: 'First Invoice' },
  ],
});
