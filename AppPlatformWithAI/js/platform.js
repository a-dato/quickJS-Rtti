// platform.js - what a type script builds on.
//
// A type script (js/types/<Name>.js) defines exactly one type, named like the
// file, out of the three parts of an AppPlatform type:
//
//   the class            the objects: one getter/setter per property
//   the provider         where the objects come from (Data), may be async
//   the type descriptor  the type info: storage, property descriptors (type,
//                        label, editor), properties it adds to Delphi types
//
// and hands them to registerType(). Example, js/types/Customer.js:
//
//   import {Object_, Provider_, TypeDescriptor_, registerType} from '../platform.js';
//
//   export class Customer extends Object_ {
//     get Name() { return this._Name; }
//     set Name(value) { this._Name = value; }
//     ToString() { return this.Name; }        // the object's display text
//   }
//
//   class CustomerProvider extends Provider_ {
//     Data(filter) {                          // plain objects or Customers
//       return [{ Name: 'Acme' }, { Name: 'Globex' }];
//     }
//   }
//
//   class CustomerType extends TypeDescriptor_ {
//     constructor() {
//       super();
//       this.Provider = new CustomerProvider();
//       this.PropertyDescriptor = {
//         Name:    { Type: 'string', Label: 'Name' },
//         Address: { Type: 'Address' },       // another type, by name
//         Project: { Type: 'IProject' },      // a Delphi type
//       };
//     }
//     get StorageName() { return 'Customers'; }   // omit for a value type
//     CreateInstance() { return new Customer(); }
//   }
//
//   registerType(Customer, new CustomerType());
//
// registerType completes the parts (accessors for declared properties the
// class leaves out, conversion of values set from editors, references by
// display text, the type's own descriptor: formatter, marshaller, picklist),
// registers the type with app.Config, creates the storage and fills it from
// the provider. Running the script again redefines the type: the host
// unregisters the old one first.
//
// The older one-call form, defineType({ name, storage, display, properties,
// data }), still works and builds the same three parts.

import {TypeCode, ITypeDescriptor_} from './app.js';
import {editor, Object_, Provider_} from './lynx.js';

export {TypeCode, editor, Object_, Provider_};

// The base of a type descriptor. A type with storage overrides StorageName.
export class TypeDescriptor_ extends ITypeDescriptor_ {
  constructor() {
    super();
    this.Provider = null;
    this.PropertyDescriptor = {};
  }

  get StorageName() { return ''; }
}

const SCALARS = {
  string:  { code: TypeCode.String,  editor: editor.Edit,   empty: '' },
  number:  { code: TypeCode.Double,  editor: editor.Number, empty: 0 },
  integer: { code: TypeCode.Int32,   editor: editor.Number, empty: 0 },
  boolean: { code: TypeCode.Boolean, editor: editor.Check,  empty: false },
  date:    { code: TypeCode.String,  editor: editor.Edit,   empty: '' },   // 'YYYY-MM-DD'
};

// TypeCode values a descriptor may use instead of a scalar name
const TYPECODE_SCALARS = {
  [TypeCode.String]: 'string', [TypeCode.Char]: 'string',
  [TypeCode.Double]: 'number', [TypeCode.Single]: 'number', [TypeCode.Decimal]: 'number',
  [TypeCode.Int16]: 'integer', [TypeCode.Int32]: 'integer', [TypeCode.Int64]: 'integer',
  [TypeCode.UInt16]: 'integer', [TypeCode.UInt32]: 'integer', [TypeCode.UInt64]: 'integer',
  [TypeCode.Byte]: 'integer', [TypeCode.SByte]: 'integer',
  [TypeCode.Boolean]: 'boolean',
  [TypeCode.DateTime]: 'date',
};

// The types defined by type scripts, by name
export const definedTypes = {};

// What the host reads about the types (it cannot see JavaScript property
// types): { <type>: { storage, display, properties: { <prop>: { type, label } },
// extends: [{ owner, name, type, label }] } }
const schemas = globalThis.__typeSchemas ??= {};

// The Delphi types, set by the host: { <type>: { storage } }
const delphiTypes = () => globalThis.__delphiTypes ?? {};

// Called by the host before a type is redefined or deleted
globalThis.__forgetType = name => {
  delete definedTypes[name];
  delete schemas[name];
  delete globalThis[name];
};

// Text typed in an editor becomes the declared scalar type
function coerce(type, value) {
  if (value == null) return value;
  switch (type) {
    case 'number':  { const n = Number(value); return Number.isNaN(n) ? value : n; }
    case 'integer': { const n = parseInt(value, 10); return Number.isNaN(n) ? value : n; }
    case 'boolean': return value === true || value === 'true' || value === 'True' || value === 1 || value === '1';
    case 'string':  return String(value);
    // Dates are 'YYYY-MM-DD' text: readable in every editor and grid
    case 'date':    return value instanceof Date ? value.toISOString().slice(0, 10) : String(value);
    default: return value;
  }
}

let nextId = 1;

const isIdent = s => typeof s === 'string' && /^[A-Za-z_][A-Za-z0-9_]*$/.test(s);
const isPromise = v => v != null && typeof v.then === 'function';
const isPlainObject = v => v != null && typeof v === 'object' && Object.getPrototypeOf(v) === Object.prototype;

// The items of an array or a bridged List (no for...of on those)
function itemsOf(value) {
  if (value == null) return [];
  if (Array.isArray(value)) return value;
  if (typeof value.Count === 'number') {
    const items = [];
    for (let i = 0; i < value.Count; i++) items.push(value[i]);
    return items;
  }
  return [value];
}

function toList(values) {
  const list = new List();
  for (const value of values)
    list.Add(value);
  return list;
}

function storageItems(storageName) {
  if (!storageName || !app.HasStorage(storageName)) return [];
  return itemsOf(app.Storage[storageName]);
}

function typeNames() {
  return [...Object.keys(SCALARS), ...Object.keys(definedTypes), ...Object.keys(delphiTypes())];
}

// A declared property type -> { kind: 'scalar' | 'script' | 'delphi', name }
function resolveType(fail, prop, type, selfName) {
  if (typeof type === 'number' && type in TYPECODE_SCALARS)
    return { kind: 'scalar', name: TYPECODE_SCALARS[type] };
  if (typeof type === 'function')
    type = type.name;
  if (typeof type !== 'string')
    fail(`property ${prop} needs a Type, e.g. { Type: 'string' }`);

  const scalar = Object.keys(SCALARS).find(s => s === type.toLowerCase());
  if (scalar) return { kind: 'scalar', name: scalar };
  if (type === selfName || type in definedTypes) return { kind: 'script', name: type };
  if (type in delphiTypes()) return { kind: 'delphi', name: type };
  fail(`property ${prop} has Type '${type}', which is not defined (yet). Use one of: ${typeNames().join(', ')}.`);
}

// An object of a Delphi type's storage: the object itself, its ID, its
// Description / Name / display text, or { ID }
function delphiReference(typeName, value, warn) {
  if (value == null || value === '') return null;
  if (typeof value === 'object' && !isPlainObject(value)) return value;   // a bridged object

  let data = value;
  if (typeof value === 'string' && value.trim().startsWith('{')) {
    try { data = JSON.parse(value); } catch { data = value; }
  }
  const storageName = delphiTypes()[typeName]?.storage;
  const read = (item, name) => { try { return item[name]; } catch { return undefined; } };
  const text = typeof data === 'object' ? null : String(data);
  const id = typeof data === 'object' ? String(data.ID) : text;
  for (const item of storageItems(storageName)) {
    if (String(read(item, 'ID')) === id) return item;
    if (text != null && [read(item, 'Description'), read(item, 'Name')].some(v => v != null && String(v) === text)) return item;
  }
  warn(`Warning: no ${typeName} in ${storageName ?? '(no storage)'} matches ${JSON.stringify(value)}; the reference is left empty. ` +
    `Refer to a ${typeName} by its ID or Description.`);
  return null;
}

// The accessor of a property on the class or its own base classes
function findAccessor(cls, prop) {
  for (let proto = cls.prototype; proto != null && proto !== Object_.prototype; proto = Object.getPrototypeOf(proto)) {
    const d = Object.getOwnPropertyDescriptor(proto, prop);
    if (d != null) return d;
  }
  return null;
}

function ownsMethod(cls, method) {
  const d = findAccessor(cls, method);
  return d != null && typeof d.value === 'function';
}

export function registerType(cls, descriptor) {
  const name = typeof cls === 'function' ? cls.name : null;
  const fail = message => { throw new Error(`registerType(${name ?? '?'}): ${message}`); };

  if (typeof cls !== 'function' || !isIdent(name))
    fail('pass the class first: registerType(MyType, new MyTypeType())');
  if (!(cls.prototype instanceof Object_))
    fail(`class ${name} must extend Object_ (import it from '../platform.js')`);
  if (descriptor == null || typeof descriptor !== 'object')
    fail(`pass the type descriptor second: registerType(${name}, new ${name}Type())`);
  if (descriptor instanceof Object_ || descriptor.constructor === cls)
    fail('the second argument is the type descriptor (a TypeDescriptor_), not an object of the type');

  const pd = descriptor.PropertyDescriptor;
  if (pd == null || typeof pd !== 'object')
    fail('the descriptor needs this.PropertyDescriptor = { <property>: { Type, Label }, ... }');

  // The declared properties: every PropertyDescriptor entry but the type's own
  // entry (named like the type) and ID
  const props = {};
  for (const [prop, def] of Object.entries(pd)) {
    if (prop === name || prop === 'ID') continue;
    if (!isIdent(prop)) fail(`property name '${prop}' must be an identifier`);
    if (def == null || typeof def !== 'object') fail(`PropertyDescriptor.${prop} must be an object like { Type: 'string' }`);
    props[prop] = { ...resolveType(fail, prop, def.Type, name), label: def.Label ?? prop };
  }
  if (Object.keys(props).length === 0)
    fail('PropertyDescriptor must declare at least one property, e.g. Name: { Type: \'string\' }');

  let storageName = null;
  try { storageName = descriptor.StorageName || null; } catch { storageName = null; }
  if (storageName != null && typeof storageName !== 'string')
    fail('StorageName must return a string');

  // Properties added to Delphi types: Extends = { IProject: { Prop: { Type, Label, List } } }
  // (List: true makes it a list of objects of Type, picked with checkboxes)
  const extensions = [];
  for (const [owner, added] of Object.entries(descriptor.Extends ?? {})) {
    if (owner in definedTypes || owner === name)
      fail(`Extends.${owner}: ${owner} is a script type; add the property to ${owner}.js instead`);
    if (!(owner in delphiTypes()))
      fail(`Extends.${owner}: there is no Delphi type ${owner}. Delphi types: ${Object.keys(delphiTypes()).join(', ') || '(none)'}`);
    for (const [prop, def] of Object.entries(added ?? {})) {
      if (!isIdent(prop)) fail(`Extends.${owner}: property name '${prop}' must be an identifier`);
      const type = resolveType(fail, `${owner}.${prop}`, def?.Type ?? name, name);
      if (def?.List === true && type.kind === 'scalar')
        fail(`Extends.${owner}.${prop}: a List holds objects of a type (e.g. Type: '${name}'), not ${type.name} values`);
      extensions.push({ owner, name: prop, type: type.name, label: def?.Label ?? prop, list: def?.List === true });
    }
  }

  const warn = message => console.log(message);

  // ---- the class: accessors and display text ----
  const convert = (prop, value, warnFn) => {
    const def = props[prop];
    if (def.kind === 'scalar') return coerce(def.name, value);
    if (def.kind === 'delphi') return delphiReference(def.name, value, warnFn);
    const ref = def.name === name ? self : definedTypes[def.name];
    return value != null && !(value instanceof ref.cls) ? ref.unmarshal(value, warnFn) : value;
  };

  for (const prop of Object.keys(props)) {
    const own = findAccessor(cls, prop);
    if (own != null && typeof own.value === 'function')
      fail(`${prop} is a method of class ${name}; a property needs a getter/setter (or none: registerType adds them)`);
    const get = own?.get;
    const set = own?.set;
    Object.defineProperty(cls.prototype, prop, {
      // AppPlatform also reads the getter on the prototype itself
      get: get != null
        ? function () { try { return get.call(this); } catch { return undefined; } }
        : function () { return this._values?.[prop]; },
      set(value) {
        if (this === cls.prototype) return;
        value = convert(prop, value, warn);
        if (set != null) set.call(this, value);
        else if (get == null) (this._values ??= {})[prop] = value;
      },
      enumerable: true,
      configurable: true,
    });
  }

  const display = Object.keys(props).find(p => props[p].name === 'string') ?? Object.keys(props)[0];
  const ownToString = ownsMethod(cls, 'ToString');
  if (!ownToString)
    cls.prototype.ToString = function (format) {
      const own = pd[name]?.Formatter?.Format;
      if (own != null) return String(own(null, this, format) ?? '');
      return String(this[display] ?? '');
    };
  if (!Object.prototype.hasOwnProperty.call(cls.prototype, 'toString'))
    cls.prototype.toString = function () { return this.ToString(); };

  // A new object from plain values (or an object of the class, which gets an ID)
  const create = (values, warnFn = warn) => {
    let obj;
    if (values instanceof cls) {
      obj = values;
      values = null;
    } else
      obj = new cls();
    if (obj._id == null) obj._id = values?.ID ?? nextId++;
    // IDs from saved state: later objects must not reuse them
    if (typeof obj._id === 'number' && obj._id >= nextId) nextId = obj._id + 1;
    // Scalars start at their empty value, unless the class set one
    for (const [prop, def] of Object.entries(props))
      if (def.kind === 'scalar' && obj[prop] === undefined)
        obj[prop] = SCALARS[def.name].empty;
    if (values == null) return obj;
    if (!isPlainObject(values)) {
      warnFn(`Warning: ${name}: ${JSON.stringify(values)} is not an object of property values; skipped.`);
      return null;
    }
    for (const [prop, value] of Object.entries(values)) {
      if (prop === 'ID') continue;
      if (!(prop in props)) {
        warnFn(`Warning: ${name} has no property ${prop}; ${JSON.stringify(value)} is ignored. Properties: ${Object.keys(props).join(', ')}.`);
        continue;
      }
      obj[prop] = convert(prop, value, warnFn);
    }
    return obj;
  };

  // ---- references to objects of this type ----
  const find = match => storageItems(storageName).find(match) ?? null;
  const marshal = item => {
    if (item == null) return null;
    if (storageName != null) return JSON.stringify({ ID: String(item.ID), Type: name });
    return JSON.stringify(Object.fromEntries([['ID', String(item.ID)], ...Object.keys(props).map(p => [p, item[p]])]));
  };
  // A reference as written in data or stored by the host: the object, its
  // display text, its ID, { ID } or the JSON of that. A value type takes
  // plain values.
  const unmarshal = (value, warnFn = warn) => {
    if (value == null || value === '') return null;
    if (value instanceof cls) return value;
    let data = value;
    if (typeof value === 'string' && value.trim().startsWith('{')) {
      try { data = JSON.parse(value); } catch { data = value; }
    }
    if (storageName != null) {
      const found = typeof data === 'object'
        ? find(item => String(item.ID) === String(data.ID))
        : find(item => item.ToString() === String(data)) ?? find(item => String(item.ID) === String(data));
      if (found == null)
        warnFn(`Warning: no ${name} matches ${JSON.stringify(value)}; the reference is left empty. ` +
          `Refer to a ${name} by its display text (ToString) or ID.`);
      return found;
    }
    if (typeof data !== 'object') {
      warnFn(`Warning: ${name} is a value type; give its properties as an object, not ${JSON.stringify(value)}.`);
      return null;
    }
    return create(data, warnFn);
  };

  // ---- the provider: Data always gives objects of the class, in a List ----
  const provider = descriptor.Provider;
  if (provider != null && typeof provider.Data !== 'function')
    fail('Provider needs a Data(filter) method');
  const toObjects = result => toList(itemsOf(result).map(v => create(v)).filter(v => v != null));
  if (provider != null) {
    const data = provider.Data.bind(provider);
    provider.Data = filter => {
      const result = data(filter);
      return isPromise(result) ? result.then(toObjects) : toObjects(result);
    };
  }

  // ---- the type descriptor: defaults for what the script leaves out ----
  const choices = () => storageName != null && app.HasStorage(storageName) ? app.Storage[storageName] : null;
  const ownEntry = pd[name] ?? {};
  pd[name] = {
    GetType: () => cls,
    Label: name,
    EditorType: editor.Combo,
    ...ownEntry,
    Formatter: { Format: (ctx, item, format) => item?.ToString?.() ?? '', Url: (ctx, item) => '', ...ownEntry.Formatter },
    Marshaller: { Marshal: (ctx, item) => marshal(item), Unmarshal: (ctx, item) => unmarshal(item), ...ownEntry.Marshaller },
    Picklist: { Items: filter => choices(), ...ownEntry.Picklist },
  };
  pd.ID ??= { Type: TypeCode.Int64, Visible: false };
  for (const [prop, def] of Object.entries(props)) {
    const scalar = SCALARS[def.name];
    const entry = pd[prop];
    entry.Type = def.kind === 'scalar' ? scalar.code : def.kind === 'script' ? (def.name === name ? cls : definedTypes[def.name].cls) : TypeCode.Interface;
    entry.Label ??= prop;
    entry.EditorType ??= scalar ? scalar.editor : editor.Combo;
    if (def.kind === 'delphi')
      entry.Picklist ??= { Items: filter => { const s = delphiTypes()[def.name]?.storage; return s && app.HasStorage(s) ? app.Storage[s] : null; } };
    else if (def.kind === 'script' && def.name !== name && definedTypes[def.name].storage != null)
      entry.Picklist ??= { Items: filter => definedTypes[def.name].choices() };
  }
  if (descriptor.ClassName !== name)
    Object.defineProperty(descriptor, 'ClassName', { get: () => name, configurable: true });
  const createInstance = typeof descriptor.CreateInstance === 'function' ? descriptor.CreateInstance.bind(descriptor) : null;
  descriptor.CreateInstance = () => create(createInstance?.() ?? undefined);

  const self = { cls, descriptor, storage: storageName, create, marshal, unmarshal, choices, props };
  definedTypes[name] = self;
  schemas[name] = {
    storage: storageName,
    display: ownToString ? 'ToString()' : display,
    properties: Object.fromEntries(Object.entries(props).map(([prop, def]) => [prop, { type: def.name, label: def.label }])),
    extends: extensions,
  };
  globalThis[name] = cls;

  app.Config.RegisterType(cls, descriptor);

  if (storageName == null)
    return;

  // Saved state (the host sets it at startup, from state\<Name>.json) takes
  // the place of the provider's data: the objects as they were saved, with
  // their IDs, so references to them still match.
  const storage = app.AddStorage(cls, storageName);
  const saved = globalThis.__savedState?.[name];
  if (saved != null) {
    delete globalThis.__savedState[name];
    const dropped = new Set();
    const quiet = message => {
      const m = /has no property (\w+)/.exec(message);
      if (m) dropped.add(m[1]); else console.log(message);
    };
    const objects = itemsOf(saved.objects).map(values => {
      const id = values?.ID;
      return create({ ...values, ID: id != null && /^\d+$/.test(String(id)) ? Number(id) : id }, quiet);
    }).filter(v => v != null);
    if (dropped.size)
      console.log(`Saved state of ${name}: ${[...dropped].join(', ')} no longer exist(s); those values are dropped.`);
    storage.Attach(toList(objects));
    return;
  }

  // The storage starts with the provider's data. An async provider (fetch) is
  // waited for by the host before the script counts as loaded.
  const attach = list => storage.Attach(list);
  const failed = err => {
    console.log(`Error: ${name}: Provider.Data failed: ${err?.message ?? err}. ${storageName} starts empty.`);
    attach(new List());
  };
  let data;
  try {
    data = provider != null ? provider.Data(null) : new List();
  } catch (err) {
    failed(err);
    return;
  }
  if (isPromise(data))
    return data.then(attach, failed);
  attach(data);
}

// ---- for the host (App.TypeScripts) ----

// Adds or updates objects of a type from JSON: [{ <prop>: value }, ...]. With
// a key property, an object whose key matches is updated instead of added.
globalThis.__importObjects = (name, items, key) => {
  const type = definedTypes[name];
  if (type == null) return JSON.stringify({ ok: false, error: `There is no script type ${name}.` });
  if (type.storage == null) return JSON.stringify({ ok: false, error: `${name} is a value type; it has no storage to import into.` });
  if (key && !(key in type.props)) return JSON.stringify({ ok: false, error: `${name} has no property ${key} to match on. Properties: ${Object.keys(type.props).join(', ')}.` });
  if (!Array.isArray(items)) return JSON.stringify({ ok: false, error: 'Pass an array of objects with property values.' });

  const warnings = [];
  const warn = message => { if (!warnings.includes(message)) warnings.push(message); };
  const storage = app.Storage[type.storage];
  let added = 0, updated = 0;
  for (const values of items) {
    const existing = key && values?.[key] != null
      ? storageItems(type.storage).find(item => String(item[key]) === String(values[key]))
      : null;
    if (existing != null) {
      for (const [prop, value] of Object.entries(values)) {
        if (prop === 'ID') continue;
        if (!(prop in type.props)) { warn(`Warning: ${name} has no property ${prop}; ignored.`); continue; }
        existing[prop] = value;
      }
      updated++;
    } else {
      const obj = type.create(values, warn);
      if (obj == null) continue;
      storage.Add(obj);
      added++;
    }
  }
  return JSON.stringify({ ok: true, added, updated, count: storage.Count, ...(warnings.length ? { warnings } : {}) });
};

// The state of a type's storage, for state\<Name>.json: every object with its
// ID and values. References are marshalled: { ID } of the object they point
// to (another storage or a Delphi object); a value type holds its values.
function stateOf(type, item) {
  const values = { ID: String(item.ID) };
  for (const [prop, def] of Object.entries(type.props)) {
    const value = item[prop];
    if (def.kind === 'scalar' || value == null) { values[prop] = value ?? null; continue; }
    if (def.kind === 'delphi') { values[prop] = { ID: String(value.ID) }; continue; }
    const ref = definedTypes[def.name];
    values[prop] = ref?.storage != null ? JSON.parse(ref.marshal(value)) : stateOf(ref, value);
  }
  return values;
}

globalThis.__exportState = (name) => {
  const type = definedTypes[name];
  if (type == null || type.storage == null) return '';
  return JSON.stringify({ type: name, storage: type.storage, objects: storageItems(type.storage).map(item => stateOf(type, item)) }, null, 2);
};

// The objects of a type as JSON: references as their display text
globalThis.__exportObjects = (name) => {
  const type = definedTypes[name];
  if (type == null) return JSON.stringify({ ok: false, error: `There is no script type ${name}.` });
  const text = value => {
    if (value == null || typeof value !== 'object') return value;
    try {
      if (value instanceof Object_) return value.ToString();
      // A Delphi object: its ToString is its class name
      return String(value.Description ?? value.Name ?? value.ID);
    } catch { return String(value); }
  };
  const objects = storageItems(type.storage).map(item =>
    Object.fromEntries([['ID', String(item.ID)], ...Object.keys(type.props).map(p => [p, text(item[p])])]));
  return JSON.stringify({ ok: true, storage: type.storage, objects });
};

// ---- the older one-call form ----

export function defineType(spec) {
  const fail = message => { throw new Error(`defineType(${spec?.name ?? '?'}): ${message}`); };
  if (spec == null || typeof spec !== 'object') fail('pass an object: { name, properties, ... }');
  if (!isIdent(spec.name)) fail('name must be an identifier');
  if (spec.properties == null || typeof spec.properties !== 'object') fail('properties must name at least one property');
  if (spec.data != null && !Array.isArray(spec.data)) fail('data must be an array of plain objects');
  const display = spec.display ?? Object.keys(spec.properties)[0];

  const cls = ({ [spec.name]: class extends Object_ {
    ToString(format) {
      return typeof display === 'function' ? String(display(this) ?? '') : String(this[display] ?? '');
    }
  }})[spec.name];

  class Provider extends Provider_ {
    Data(filter) { return spec.data ?? []; }
  }

  class Descriptor extends TypeDescriptor_ {
    constructor() {
      super();
      this.Provider = new Provider();
      this.PropertyDescriptor = Object.fromEntries(Object.entries(spec.properties).map(([prop, def]) =>
        [prop, { Type: def?.type, Label: def?.label ?? prop }]));
      if (spec.label) this.PropertyDescriptor[spec.name] = { Label: spec.label };
    }
    get StorageName() { return spec.storage ?? ''; }
  }

  registerType(cls, new Descriptor());
  return cls;
}
