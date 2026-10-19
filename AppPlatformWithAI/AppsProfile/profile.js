// profile.js — the AppPlatform host profile for the Lynx Apps builder (IntegrationProject).
//
// AppPlatformWithAI is the test host for building apps in an application other
// than Lynx. The builder pages are host-independent; this profile tells them
// what AppPlatform is: names, the app format, what host scripts can use and the
// prompt section that describes it to the AI. Every other prompt section is the
// builder's generic default.
//
// The connector (AppPlatform.AppsConnector.pas) serves the files of this folder
// to the pages through the ScriptGate method LoadAssistantProfile; they are read
// from disk on every page load, so edits here take effect without a rebuild.
// Modules may only import each other with relative './x.js' specifiers.

const DEFAULT_APP_HTML = `<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<style>
  * { box-sizing: border-box; }
  body { margin: 0; padding: 8px; font: 12px/1.35 "Segoe UI", Arial, sans-serif; color: #1f2a3d; background: #f4f6fa; }
  h1 { margin: 0 0 6px; font-size: 14px; font-weight: 650; }
  button { padding: 4px 8px; border: 0; border-radius: 4px; background: #2f6fed; color: #fff; font: inherit; cursor: pointer; }
  button:disabled { opacity: .55; cursor: not-allowed; }
  ul { margin: 6px 0 0; padding: 0; list-style: none; background: #fff; border: 1px solid #dfe4ee; border-radius: 5px; }
  li { padding: 4px 8px; border-bottom: 1px solid #f0f3f8; }
  #status { margin-top: 6px; min-height: 1.2em; color: #4b5d78; }
  #status.err { color: #c23737; }
</style>
</head>
<body>
  <h1>AppPlatform projects</h1>
  <button id="refresh">Refresh</button>
  <ul id="projects"></ul>
  <div id="status"></div>
<script>
const listEl = document.getElementById('projects');
const statusEl = document.getElementById('status');
const refreshButton = document.getElementById('refresh');

function setStatus(text, isError) {
  statusEl.textContent = text;
  statusEl.className = isError ? 'err' : '';
}

async function loadProjects() {
  refreshButton.disabled = true;
  setStatus('Loading projects...', false);
  try {
    const projects = await host.run('listProjects', null, { label: 'Loading projects' });
    listEl.replaceChildren(...projects.map(project => {
      const item = document.createElement('li');
      item.textContent = project.name + ' (ID ' + project.id + ', ' + project.taskCount + ' tasks)';
      return item;
    }));
    setStatus(projects.length + ' project(s).', false);
  } catch (err) {
    setStatus(err.name === 'AbortError' ? 'Stopped.' : err.message, err.name !== 'AbortError');
  } finally {
    refreshButton.disabled = false;
  }
}

refreshButton.addEventListener('click', loadProjects);
loadProjects();
</script>
<script_host>
if (action === 'listProjects') {
  return listStorage('Projects').map(project => ({
    id: project.id,
    name: project.name,
    taskCount: project.taskCount,
  }));
}

throw new Error('Unknown action: ' + action);
</script_host>
</body>
</html>`;

// QuickJS source shared by every host script (see appHost.js in the builder).
const PRELUDE = `// Iterates a bridged list (Count + index), an enumerable or a JavaScript array.
function items(list) {
  const result = [];
  if (list == null) return result;
  let count = typeof list.Count === 'function' ? list.Count() : list.Count;
  if (typeof count !== 'number') count = list.length;
  if (typeof count === 'number' && count >= 0) {
    for (let i = 0; i < count; i++) result.push(list[i]);
    return result;
  }
  if (typeof list.GetEnumerator === 'function') {
    const enumerator = list.GetEnumerator();
    while (enumerator.MoveNext()) result.push(enumerator.Current);
    return result;
  }
  if (typeof list[Symbol.iterator] === 'function') return [...list];
  throw new Error('items: the value is not a list.');
}

function requireStorage(name) {
  const hostApp = globalThis.app;
  if (!hostApp) throw new Error('The AppPlatform app object is not available.');
  const storage = hostApp.Storage[name];
  if (storage == null) throw new Error('AppPlatform has no storage named "' + name + '".');
  return storage;
}

// Plain DTOs of a storage: id, name and, for projects, taskCount.
function listStorage(name) {
  return items(requireStorage(name)).filter(item => item != null).map(item => {
    const dto = {
      id: String(item.ID ?? ''),
      name: String(item.Description ?? item.Name ?? ''),
    };
    if (item.Tasks != null) dto.taskCount = items(item.Tasks).length;
    return dto;
  });
}

function findProject(id) {
  const wanted = String(id ?? '');
  return items(requireStorage('Projects')).find(project => project != null && String(project.ID ?? '') === wanted) ?? null;
}`;

const HOST_API_PROMPT = `## AppPlatform host API
AppPlatform is a small Delphi test application with a QuickJS runtime. In \`script_host\`, in addition to the platform functions, these names are in scope:
- \`app\`: the AppPlatform application object. \`app.Storage[name]\` is a bridged list of the objects of that storage. The storage \`Projects\` always exists (objects of type IProject); other storages exist only when the user registered them.
- Bridged lists have \`Count\`, index access \`list[i]\` and \`Add(item)\`. Never iterate them with \`for...of\` or array methods directly; use \`items(list)\`, which returns a JavaScript array.
- \`IProject\`: \`ID\`, \`Description\` (the project name, read/write string), \`Tasks\` (bridged list of ITask), \`Holidays\`, \`Child\`.
- \`ITask\`: \`ID\`, \`Description\` (read/write string).
- \`items(list)\` → JavaScript array of the items of a bridged list.
- \`listStorage(name)\` → \`[{ id, name, taskCount? }]\` DTOs of a storage (taskCount for objects with Tasks).
- \`findProject(id)\` → the project whose \`ID\` equals \`String(id)\`, or \`null\`.

- \`types\`: the type scripts (see below). \`JSON.parse(types.List())\` lists every type with its storage and properties; \`types.Objects(name)\` and \`types.Import(name, json, key)\` read and add objects of a script type.

AppPlatform's object model is small and still evolving: before relying on any member that is not listed above, verify it with \`host_eval\` on the live host. Never invent a member. Changes made to objects are in memory only; AppPlatform has no save for data. Type scripts are saved to disk.`;

const TYPE_SCRIPTS_PROMPT = `## AppPlatform types and type scripts
Besides building apps you can define the data model itself: the types of AppPlatform. Every JavaScript type is one *type script*, a file js/types/<Name>.js on the host. The user sees the types, their objects and their scripts in AppPlatform Studio.

Use the type tools for this, not host_eval:
- \`list_types\` — every type (Delphi and script), its storage, object count and properties. Call it first.
- \`get_type\` — the source of one type script (or the properties of a Delphi type).
- \`get_objects\` — the objects in a script type's storage, references shown as display text.
- \`create_type\` — a new type script. \`update_type\` — replace a whole script. \`edit_type\` — exact text replacements in a script (like edit_app).
- \`delete_type\` — remove a type script (refused while another type uses it).
Saving runs the script on the live host right away and redefines the type; the storage starts over from the provider's data. If the script fails, nothing changes and the tool returns the error: fix it and save again.

A type script has the three parts of an AppPlatform type — the class, the provider and the type descriptor — and registers them:
\`\`\`js
import {Object_, Provider_, TypeDescriptor_, registerType} from '../platform.js';

// The class: the objects. A getter/setter per property (registerType adds
// plain ones for declared properties you leave out).
export class Invoice extends Object_ {
  get Number() { return this._Number; }
  set Number(value) { this._Number = value; }

  // The display text: pickers show it, and other scripts refer to an invoice by it
  ToString() { return this.Number; }
}

// The provider: where the objects come from. Return plain objects (property
// values) or Invoices; may be async, e.g. with fetch.
class InvoiceProvider extends Provider_ {
  Data(filter) {
    return [
      { Number: 'INV-1', Amount: 100, Paid: false, DueDate: '2026-11-01',
        Customer: 'Acme Corp',                                    // another type's object, by its display text
        Project: 'Project 1',                                     // a Delphi object, by its Description or ID
        Address: { Street: 'Main 1', Zip: '1000', City: 'Amsterdam' } },  // a value type, by its properties
    ];
  }
}

// The type descriptor: the type info — storage and property descriptors
class InvoiceType extends TypeDescriptor_ {
  constructor() {
    super();
    this.Provider = new InvoiceProvider();
    this.PropertyDescriptor = {
      Number:   { Type: 'string' },
      Amount:   { Type: 'number', Label: 'Amount (EUR)' },
      Paid:     { Type: 'boolean' },
      DueDate:  { Type: 'date', Label: 'Due date' },
      Customer: { Type: 'Customer' },   // another script type: picks one of its objects
      Project:  { Type: 'IProject' },   // a Delphi type: picks one of its objects
      Address:  { Type: 'Address' },    // a value type (no storage): edited in place
    };
    // Optional: properties this type adds to Delphi types (they cannot have scripts)
    this.Extends = {
      IProject: {
        MainInvoice: { Type: 'Invoice', Label: 'Main invoice' },           // picks one
        Invoices:    { Type: 'Invoice', Label: 'Invoices', List: true },   // picks several (checkboxes)
      },
    };
  }

  get StorageName() { return 'Invoices'; }   // omit for a value type
  CreateInstance() { return new Invoice(); }
}

registerType(Invoice, new InvoiceType());
\`\`\`
Rules:
- The class name, the file / tool name and the type name are the same. One type per script.
- Property types (\`Type\`) are 'string', 'number', 'integer', 'boolean', 'date' (text 'YYYY-MM-DD'), the name of another script type, or a Delphi type (e.g. 'IProject'). Labels are optional.
- Every object gets an \`ID\` automatically; do not declare it, and do not refer to script objects by ID: IDs change at every load. Refer to an object of another script type by its display text (its ToString; read it with get_objects). Refer to a Delphi object by its Description or ID.
- A type used by another type must exist first; create it before the type that uses it.
- Delphi types (e.g. IProject) have no script. To give one a property, declare it in the \`Extends\` of a script type's descriptor; it is added whenever that script loads. \`List: true\` makes it a list of objects the user checks ("which tasks belong to this project"). The values the user picks live in memory until AppPlatform closes.
- The provider runs inside AppPlatform when the type loads, and again when a token changes. It may \`await fetch(url)\` and read AppPlatform's tokens as \`tokens['Name']\` (the user keeps them on the Tokens tab of AppPlatform Studio, in tokens.json next to the exe; \`list_tokens\` gives the names). So data from an API that needs a key (e.g. Trello cards with \`tokens['TrelloKey']\` and \`tokens['TrelloToken']\`) comes from the provider itself and is there after every restart. Never write a token value into a script; if a token the provider needs is missing, ask the user to add it on the Tokens tab under the exact name, and make the provider return [] while \`tokens['Name']\` is undefined. Explore the API first with http_get using \`{{token:Name}}\`.
- \`types.Import('<Type>', JSON.stringify(items), '<key property>')\` adds objects from host code (host_eval or the app's script_host): plain objects of property values, an object whose key matches is updated. It returns JSON { ok, added, updated, warnings? }. Imported objects last until the type is saved again or AppPlatform restarts; prefer a provider for data that should stay.
- Saved state: the user can press Save state in AppPlatform Studio (or host code can call \`types.SaveState()\`). Every storage is then written to state/<Type>.json next to the exe, and at the next start a type with a state file gets those objects (IDs, values, links; IProject's added properties too) instead of its provider's data. Saving a type script (create/update/edit_type) still starts its storage over from the provider. Only call SaveState when the user asks to keep the current data.
- A save returns \`output\` with warnings (e.g. a reference that matches nothing, a property that does not exist). Read them and fix the script; a save with warnings is not finished.
- Scripts in the older form \`defineType({ name, storage, display, properties, data })\` still load; when you change one, rewrite it in the form above.
- Host code must not construct objects of script types or change their storage directly: those live in AppPlatform's main runtime. Use \`types.Import\` and \`types.Objects('<Type>')\` (JSON) instead.`;

// The type tools: they run on the host (RunIntegration) against the live
// 'types' object (App.TypeScripts.pas), so they need AppPlatform.
const TYPE_TOOLS = [
  {
    name: 'list_types',
    description: 'List every AppPlatform type: name, origin ("script" or "delphi"), storage, object count and properties with their types.',
    input_schema: { type: 'object', properties: {} },
  },
  {
    name: 'get_type',
    description: 'Get the source of the type script of a type (js/types/<name>.js), or the properties of a Delphi type.',
    input_schema: { type: 'object', properties: { name: { type: 'string', description: 'Type name, e.g. Customer' } }, required: ['name'] },
  },
  {
    name: 'list_tokens',
    description: 'List the names of the tokens AppPlatform has (its Tokens tab plus the app\'s vault). Values are never returned.',
    input_schema: { type: 'object', properties: {} },
  },
  {
    name: 'get_objects',
    description: 'Get the objects in the storage of a script type (property values; references as their display text).',
    input_schema: { type: 'object', properties: { name: { type: 'string', description: 'Type name, e.g. Customer' } }, required: ['name'] },
  },
  {
    name: 'create_type',
    description: 'Create a new type: writes js/types/<name>.js and runs it on the live host. Fails if the type exists (use update_type or edit_type).',
    input_schema: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'Type name (an identifier), equal to the class name in the script' },
        source: { type: 'string', description: 'The whole type script' },
      },
      required: ['name', 'source'],
    },
  },
  {
    name: 'update_type',
    description: 'Replace the whole type script of an existing script type and run it. The storage starts over from the provider\'s data.',
    input_schema: {
      type: 'object',
      properties: { name: { type: 'string' }, source: { type: 'string', description: 'The whole new type script' } },
      required: ['name', 'source'],
    },
  },
  {
    name: 'edit_type',
    description: 'Apply exact text replacements to a type script and run it. Each old_text must occur exactly once in the current script. All or nothing.',
    input_schema: {
      type: 'object',
      properties: {
        name: { type: 'string' },
        edits: {
          type: 'array',
          minItems: 1,
          maxItems: 20,
          items: {
            type: 'object',
            properties: { old_text: { type: 'string' }, new_text: { type: 'string' } },
            required: ['old_text', 'new_text'],
          },
        },
      },
      required: ['name', 'edits'],
    },
  },
  {
    name: 'delete_type',
    description: 'Delete a script type and its script. Refused while another type uses it as a property type.',
    input_schema: { type: 'object', properties: { name: { type: 'string' } }, required: ['name'] },
  },
];

// Host script bodies (QuickJS, with 'args' and the 'types' global)
const TYPE_TOOL_BODIES = {
  list_types: `return JSON.parse(types.List());`,
  get_type: `
    const source = types.Get(args.name);
    if (source) return { name: args.name, origin: 'script', file: 'js/types/' + args.name + '.js', source };
    const type = JSON.parse(types.List()).find(t => t.name === args.name);
    if (type) return { ...type, note: args.name + ' is defined in Delphi; it has no script. A script type can add properties to it with Extends in its type descriptor.' };
    return { error: 'TYPE_NOT_FOUND', message: 'There is no type named ' + args.name + '. Call list_types.' };`,
  get_objects: `return JSON.parse(types.Objects(args.name));`,
  list_tokens: `return { names: Object.keys(tokens || {}) };`,
  create_type: `
    if (JSON.parse(types.List()).some(t => t.name === args.name))
      return { ok: false, error: args.name + ' already exists. Use get_type, then update_type or edit_type.' };
    return JSON.parse(types.Save(args.name, args.source));`,
  update_type: `
    if (!types.Get(args.name))
      return { ok: false, error: 'There is no type script named ' + args.name + '. Use create_type.' };
    return JSON.parse(types.Save(args.name, args.source));`,
  edit_type: `
    let source = types.Get(args.name);
    if (!source) return { ok: false, error: 'There is no type script named ' + args.name + '.' };
    for (const [i, edit] of (args.edits || []).entries()) {
      const at = source.indexOf(edit.old_text);
      if (!edit.old_text || at < 0) return { ok: false, error: 'EDIT_MATCH_NOT_FOUND', edit: i, message: 'old_text of edit ' + i + ' is not in the script. Call get_type for the current source.' };
      if (source.indexOf(edit.old_text, at + 1) >= 0) return { ok: false, error: 'EDIT_MATCH_AMBIGUOUS', edit: i, message: 'old_text of edit ' + i + ' occurs more than once; include more context.' };
      source = source.slice(0, at) + edit.new_text + source.slice(at + edit.old_text.length);
    }
    return { ...JSON.parse(types.Save(args.name, source)), source };`,
  delete_type: `return JSON.parse(types.Delete(args.name));`,
};

// Set by init(): runs a body on the host (or on the mock without AppPlatform)
let runHostCall = null;
let hasHost = false;

async function executeTypeTool(name, input) {
  const body = TYPE_TOOL_BODIES[name];
  if (body === undefined)
    return undefined;   // not a type tool
  if (!hasHost || runHostCall == null)
    return { error: 'HOST_REQUIRED', message: 'The type tools need AppPlatform; this builder is not running inside it.' };
  try {
    return await runHostCall(body, input ?? {}, 60000);
  } catch (err) {
    return { error: 'HOST_CALL_FAILED', message: err?.message || String(err) };
  }
}

export default function createProfile() {
  return {
    id: 'appplatform',
    version: 1,
    names: {
      product: 'AppPlatform',
      host: 'AppPlatform',
      app: 'AppPlatform app',
      builder: 'AppPlatform App Builder',
    },
    app: {
      bridgeGlobal: 'host',
      hostScriptTag: 'script_host',
      defaultName: 'New AppPlatform App',
      defaultHtml: DEFAULT_APP_HTML,
      exportFormat: 'appplatform-app.html-app',
      exportFileSuffix: '.appplatform-app.json',
    },
    ui: {
      welcome: 'Describe the app you want. The AI writes one HTML page with a browser script for UI and a script_host block that runs inside AppPlatform.',
      suggestions: [
        { label: 'New Type', prompt: 'Create a type Invoice with a number, an amount, a paid flag and a customer, with three sample invoices' },
        { label: 'Project List', prompt: 'Build an app that lists all AppPlatform projects with their number of tasks' },
        { label: 'Rename Project', prompt: 'Build an app that lets me pick a project and change its description' },
        { label: 'GitHub Import', prompt: 'Build an app that imports the open issues of a GitHub repository as tasks of a selected project' },
      ],
      tokenNamePlaceholder: 'Name (e.g. GitHub)',
    },
    hostRuntime: {
      prelude: PRELUDE,
      scope: 'const app = globalThis.app;',
      nativeGlobals: ['IProject', 'List', 'StatusCode', 'DateTime', 'types'],
    },
    prompt: {
      knowledge: HOST_API_PROMPT + '\n\n' + TYPE_SCRIPTS_PROMPT,
    },
    // The AI's tools for the data model, next to the platform's app tools
    tools: TYPE_TOOLS,
    async init(context) {
      runHostCall = context?.runHostCall ?? null;
      hasHost = Boolean(context?.hasHost);
    },
    createSession() {
      return {
        executeTool: (name, input) => executeTypeTool(name, input),
        checkSave: () => null,
      };
    },
    // Preview without AppPlatform: three sample projects, and the prelude's
    // helpers evaluated against them (globalThis inside the prelude is shadowed).
    mock: {
      createContext() {
        const list = values => Object.assign(values, { Count: values.length, Add(item) { values.push(item); this.Count = values.length; } });
        const tasks = n => list([1, 2].map(i => ({ ID: n * 10 + i, Description: 'Task ' + i })));
        const app = { Storage: { Projects: list([0, 1, 2].map(n => ({ ID: n, Description: 'Project ' + n, Tasks: tasks(n) }))) } };
        const helpers = new Function('globalThis', `${PRELUDE}\nreturn { items, listStorage, findProject };`)({ app });
        return { globals: { app, ...helpers } };
      },
    },
  };
}
