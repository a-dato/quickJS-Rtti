// profile.js — the AppPlatform host profile for the Lynx Apps builder (IntegrationProject).
//
// AppPlatformWithAI is the test host for building apps in an application other
// than Lynx. The builder pages are host-independent; this profile tells them
// what AppPlatform is: names, the app format, what host scripts can use and the
// prompt section that describes it to the AI. Every other prompt section is the
// builder's generic default.
//
// Apps run in LIVE SESSIONS (app.liveSessions): an app page's <script_host>
// block runs once when the page opens and stays alive with it. It exposes host
// objects to the page as live objects (page.expose), kept in sync with every
// other window showing them, and registers the page's actions (actions.x = ...).
// See SESSION_PRELUDE below and AppsHost\AppPlatform.AppSession.pas.
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
  .row { display: flex; gap: 6px; align-items: end; flex-wrap: wrap; }
  label { display: block; font-size: 11px; color: #51627e; margin: 0 0 2px; }
  select, input { font: inherit; padding: 4px 6px; border: 1px solid #cfd8e6; border-radius: 4px; min-width: 180px; }
  input:read-only { background: #eef1f6; color: #6b7385; }
  #status { margin-top: 6px; min-height: 1.2em; color: #4b5d78; }
  #status.err { color: #c23737; }
</style>
</head>
<body>
  <h1>Projects</h1>
  <div class="row">
    <div>
      <label for="project">Project</label>
      <select id="project"></select>
    </div>
    <div>
      <label for="description">Description</label>
      <input id="description" type="text">
    </div>
  </div>
  <div id="status">Loading...</div>
<script>
const projectEl = document.getElementById('project');
const descriptionEl = document.getElementById('description');
const statusEl = document.getElementById('status');

// The live object the host block exposed as 'project'
let project = null;

function setStatus(text, isError) {
  statusEl.textContent = text;
  statusEl.className = isError ? 'err' : '';
}

// Shows the live values; runs again whenever they change anywhere
function showProject() {
  descriptionEl.value = project.Description.value ?? '';
  descriptionEl.readOnly = !project.Description.editable;
}

async function init() {
  try {
    const { projects, selectedId } = await host.run('listProjects', null, { label: 'Loading projects' });
    projectEl.replaceChildren(...projects.map(item => new Option(item.name, item.id)));
    projectEl.value = selectedId;
    project = await host.open('project');
    project.onChanged(showProject);
    showProject();
    setStatus('', false);
  } catch (err) {
    setStatus(err.message, true);
  }
}

// Another project: the host block exposes that one; the live object follows
async function onProjectChange() {
  try {
    await host.run('selectProject', { id: projectEl.value }, { label: 'Opening project' });
  } catch (err) {
    setStatus(err.message, true);
  }
}

// A live edit: AppPlatform checks it, and every window showing this project follows
async function onDescriptionChange() {
  try {
    await project.Description.edit(descriptionEl.value);
    setStatus('Saved.', false);
  } catch (err) {
    showProject();
    setStatus(err.message, true);
  }
}

projectEl.addEventListener('change', onProjectChange);
descriptionEl.addEventListener('change', onDescriptionChange);
init();
</script>
<script_host>
// Runs once when the page opens, and stays alive while it is open
const fields = { Description: 'edit' };

function projectById(id) {
  const project = findProject(id);
  if (!project) {
    throw new Error('There is no project with ID ' + id + '.');
  }
  return project;
}

// Opened from AppPlatform Studio with a project: start with that one
let selected = page.param('project') ?? items(app.Storage['Projects'])[0];
page.expose('project', fields, selected);

actions.listProjects = () => ({
  projects: listStorage('Projects'),
  selectedId: String(selected.ID),
});

actions.selectProject = ({ id }) => {
  selected = projectById(id);
  page.expose('project', fields, selected);
  return { id: String(selected.ID) };
};
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

// The host block's API in a live session (AppPlatform.AppSession.pas). The
// builder puts it before the block (appHost.js buildAppSessionScript); the
// block runs once, as module code, when the app page opens.
const SESSION_PRELUDE = `// AppPlatform hands the session over as __appSession right before this runs.
const __session = globalThis.__appSession;
delete globalThis.__appSession;

function __sessionCall(json) {
  const value = JSON.parse(json);
  if (value && value.error) throw new Error(value.error);
  return value;
}

// The constructor name of a script type's object (Supplier, ...); Delphi
// objects are recognised by AppPlatform itself
function __typeName(object) {
  try {
    return object && object.constructor && object.constructor.name ? String(object.constructor.name) : '';
  } catch (error) {
    return '';
  }
}

const page = {
  // [{ name, type, isObject, value? }]: what the app was opened with
  params: () => __sessionCall(__session.Params()),
  // The object (or value) the app was opened with under this name, or null
  param: name => __session.Param(String(name)) ?? null,
  // What the page gets of an object: { Property: 'read' | 'edit' }. Again: switch the object or the list.
  // An array (or a storage list) is a LIVE LIST: one live object per item.
  expose(name, properties, object) {
    if (Array.isArray(object) || (object != null && typeof object.Count === 'number')) {
      // A storage list itself (not an array): the page's list follows the
      // storage when objects are added to it (AppPlatform calls back below)
      if (Array.isArray(object)) delete __storageLists[String(name)];
      else __storageLists[String(name)] = { properties, storage: object };
      return __exposeList(String(name), properties, Array.isArray(object) ? object : items(object));
    }
    return __sessionCall(__session.Expose(String(name), JSON.stringify(properties || {}), object ?? null, __typeName(object)));
  },
  // Sets properties so that every window showing the object hears it
  update: (object, values) =>
    __sessionCall(__session.Update(object ?? null, __typeName(object), JSON.stringify(values || {}))),
  setTitle: title => __sessionCall(__session.SetTitle(String(title))),
};

// Live lists made from a storage list: exposed again when that storage
// changes (objects added in the Studio, ...)
const __storageLists = {};
(globalThis.__appSessionStorageChanged ??= {})[__session.SessionId()] = storageName => {
  for (const [name, list] of Object.entries(__storageLists)) {
    let listName = '';
    try {
      listName = String(list.storage.Name);
    } catch (error) {
      listName = '';
    }
    if (!listName || listName === storageName) __exposeList(name, list.properties, items(list.storage));
  }
  return '';
};

// Each item of a live list is exposed as '<list>/<key>' (its ID, else its
// position); the list itself is its keys
function __exposeList(name, properties, list) {
  const keys = [];
  const used = new Set();
  list.forEach((item, index) => {
    let key = '';
    try {
      key = item != null && item.ID != null ? String(item.ID) : '';
    } catch (error) {
      key = '';
    }
    if (!key || used.has(key)) key = '#' + index;
    used.add(key);
    keys.push(key);
    __sessionCall(__session.Expose(name + '/' + key, JSON.stringify(properties || {}), item, __typeName(item)));
  });
  return __sessionCall(__session.ExposeList(name, JSON.stringify(keys)));
}

// The page's host.run(action, args) runs actions[action](args)
const actions = {};

function __sessionLoadFailed(error) {
  __session.SetLoadError(String((error && error.message) || error));
}

// Kept by session id: Delphi calls it as globalThis.__appSessionHandlers[id](json)
(globalThis.__appSessionHandlers ??= {})[__session.SessionId()] = async requestJson => {
  try {
    const { action, args } = JSON.parse(requestJson);
    const fn = actions[action];
    if (typeof fn !== 'function') {
      throw new Error('Unknown action "' + action + '". The host block defines: ' + (Object.keys(actions).join(', ') || 'none'));
    }
    return JSON.stringify({ ok: true, result: toJsonSafe(await fn(args ?? null)) });
  } catch (error) {
    return JSON.stringify({ ok: false, error: String((error && error.message) || error) });
  }
};
__session.HandlerReady();`;

// The app format for the AI: live sessions. Replaces the builder's default
// description of the app format and its example; its other default sections
// (tools, token vault, rules, save checks) are kept.
function liveAppSection({ names }) {
  const H = names.host;
  return `An app is ONE self-contained HTML document (markup + CSS + JavaScript in a single text). ${H} shows it in a sandboxed iframe and injects a \`window.host\` bridge before the app's own code runs.

## What an app is
- A full HTML page with exactly two code blocks: one browser \`<script>\` for UI logic, and one \`<script_host>\` for host-side logic.
- Structure: \`<!DOCTYPE html><html><head>...styles...</head><body>...markup...<script>...browser logic...</script><script_host>...host block...</script_host></body></html>\`.
- All CSS and JS inline. No external stylesheets, fonts, CDNs, or script tags with src.
- The \`<script_host>\` block runs ONCE, when the page opens, inside ${H}, and stays alive for as long as the page is open (its variables keep their values between calls). It decides what the page may see and do: it EXPOSES host objects to the page as live objects, and it registers ACTIONS. The browser script never contains host code and never decides privileged behaviour on its own.
- Design a dense, usable desktop-tool UI (see "App design rules").

## The page: window.host
- \`const project = await host.open('project')\` → a LIVE OBJECT with one property per property the host block exposed under that name:
  - \`project.Description.value\` — the current value.
  - \`project.Description.editable\` — false when the host block exposed it as 'read' or ${H} does not allow editing it now. Show it: \`input.readOnly = !prop.editable\`.
  - \`await project.Description.edit(newValue)\` — changes it in ${H}. Rejects with a readable error when refused; the property then holds ${H}'s value again, so show it again in the catch.
  - \`project.Description.onChanged(prop => ...)\` and \`project.onChanged(prop => ...)\` (any property) — called when the value or editability changes ANYWHERE: AppPlatform Studio, another app window, another page, or the host block switching to another object. Render from the live object in these callbacks; never keep your own copy of a value.
  - open rejects when the host block did not expose that name.
- \`const rows = await host.open('projects')\` → a LIVE LIST when the host block exposed an array under that name: a JavaScript array of live objects (one per item, each like the object above: \`rows[i].Description.value\`, \`.editable\`, \`.edit(v)\`, \`.onChanged(fn)\`).
  - \`rows.onChanged((item, prop) => ...)\` — called when a property of any item changes anywhere (\`item\` and \`prop\` say which), and with \`(null, null)\` when the host block exposed other items (the array now holds them). Re-render the rows (or just that row) from the live objects in it.
  - \`rows.keys\` → the items' keys (their IDs), in order.
- \`await host.run(action, args?, options?)\` → runs \`actions[action](args)\` of the host block and resolves with what it returned. \`options.label\` is the text ${H} shows while the call runs; always pass one. \`options.timeoutMs\` defaults to 60000 of inactivity (each \`progress()\` resets it). Throws on failure: always try/catch and show the error. \`err.name === 'AbortError'\` means the user pressed Stop.
- Use live objects and live lists to SHOW and edit properties of host objects: they stay in sync with the rest of ${H}, so the page never needs a Refresh button for them. Use actions for everything else: searches, imports, changes that involve several objects or external APIs. A table or list of host objects (projects, customers, tasks, ...) is a live list, not a list returned by an action.
- A live property edit is the one exception to "${H} is changed only from the commit button": the user changing a field bound to a live property and leaving it (\`change\` event) is the commit of that field.

## Writing <script_host> (host-side code)
The block runs once as module code inside the ${H} QuickJS host; top-level \`await\` is allowed. In scope: \`page\`, \`actions\`, \`tokens\` (name → value), the platform functions below and the host objects of the "AppPlatform host API". Nothing else exists: no \`action\`/\`args\` variables (that was the old format), no DOM, no browser globals (URLSearchParams, DOMParser, btoa, atob, TextEncoder, setTimeout, fetch). Use \`console.log\`, not \`console.error\`.

### The page API
- \`page.expose(name, { Property: 'read' | 'edit', ... }, object)\` → the page gets \`object\` as live object \`name\` with only the listed properties. \`object\` is a host object: \`app.Storage['Projects'][0]\`, \`findProject(id)\`, an object of a script type from its storage (\`items(app.Storage['Suppliers'])[0]\`). Call it again (typically in an action) to switch to another object or change the list; open pages follow right away. Without \`object\` it keeps the object exposed under that name before (or uses the param of that name, below).
- \`page.expose(name, { Property: 'read' | 'edit' }, arrayOrStorageList)\` → a LIVE LIST: pass an array of host objects (\`items(app.Storage['Projects'])\`, a filtered array) or a storage list itself (\`app.Storage['Projects']\`). Each item is a live object with the listed properties. Call it again (e.g. in an action after the user changed a filter, or after the host block added or removed objects) to change which items the page has; open pages get the new items. A property whose value is another object (a link, e.g. IProject.TrelloTask) shows as that object's display text.
- Expose 'edit' only for properties the app is meant to change; everything else 'read' or not at all.
- \`page.param(name)\` → the object the app was opened with, or null. From AppPlatform Studio the user can open an app with the selected object ("Open in app"); the param is named after its type: \`project\` for IProject, \`supplier\` for a Supplier, and so on (the type name, without a leading I for Delphi interfaces, starting lowercase). An app about one object starts with it when present: \`let selected = page.param('project') ?? items(app.Storage['Projects'])[0];\`. \`page.params()\` lists them as \`[{ name, type, isObject }]\`.
- \`actions.name = async (args) => { ...; return dto; }\` → what \`host.run('name', args)\` runs. Validate args. Return small JSON-safe DTOs, never host objects. Unknown actions are rejected for you.
- \`page.update(object, { Property: value })\` → changes properties of a host object so that every window showing it follows (Studio, live objects in any page). Assigning \`object.Property = value\` directly changes the object but open windows do NOT hear about it; always use page.update for objects a window may show.
- \`page.setTitle(text)\` → the window title.

### Platform functions
- \`await httpGet(url, headers)\` → \`{ status, statusText, body }\`; throws for non-success statuses. GET only.
- \`basicAuthHeader(username, password)\` → \`{ Authorization: 'Basic …' }\`; for a token with an empty username: \`httpGet(url, basicAuthHeader('', tokens['Name']))\`.
- \`await getOAuthClientCredentialsToken(options)\` → token response; the only POST-like call allowed.
- \`log(message)\`, \`progress(current, total, message?)\` (inside every loop over more than a handful of items).

### Exact httpGet return contract
\`httpGet(url, headers)\` ALWAYS returns \`{ status, statusText, body }\`; the API payload is ALWAYS \`response.body\`. Validate its shape (\`Array.isArray(body)\`, an object, a string) and throw a descriptive error when it is not what you expect.

### Rules for host code
- Normally formatted code: one statement per line, indented, small named functions. Never minify.
- Return JSON-safe data only from actions (primitives, arrays, plain objects).
- Values read from \`httpGet\` bodies and host objects can be host-backed: normalise with \`String(value)\` / \`Number(value)\` before prototype calls.
- Never interpolate user input into code; take it through action \`args\`.
- Verify host-side calls with \`host_eval\` before saving. host_eval runs a separate, one-off probe (with \`args\`); it has \`app\`, \`types\` and the helpers, but not \`page\` or \`actions\`.`;
}

function liveExampleSection() {
  return `## Example app
A complete app in this format; start from it (adapt the layout, keep the scale and the patterns):
\`\`\`html
${DEFAULT_APP_HTML}
\`\`\`

## Example: a live list (a table that stays up to date)
The table shows every project with its description and linked Trello task; an edit anywhere in AppPlatform shows up right away, without a Refresh button.
\`\`\`html
<table><thead><tr><th>Project</th><th>Trello task</th></tr></thead><tbody id="rows"></tbody></table>
<div id="status"></div>
<script>
const rowsEl = document.getElementById('rows');
const statusEl = document.getElementById('status');
let projects = [];

function render() {
  rowsEl.replaceChildren(...projects.map(project => {
    const row = document.createElement('tr');
    const name = document.createElement('td');
    const task = document.createElement('td');
    name.textContent = project.Description.value ?? '';
    task.textContent = project.TrelloTask.value || 'No task linked';
    row.append(name, task);
    return row;
  }));
}

async function init() {
  try {
    projects = await host.open('projects');
    projects.onChanged(render);   // any project changed, or other projects
    render();
  } catch (err) {
    statusEl.textContent = err.message;
  }
}

init();
</script>
<script_host>
page.expose('projects', { Description: 'read', TrelloTask: 'read' }, app.Storage['Projects']);
</script_host>
\`\`\``;
}

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

  // Optional: invoices are this script's own data, so new ones can be made
  // (AppPlatform Studio's "+ Add object"). Return the new object's values.
  Add(values) {
    return { Number: 'INV-NEW', Amount: 0, Paid: false, ...values };
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
- New objects (AppPlatform Studio's "+ Add object") are made by the provider's \`Add(values)\`: it returns the new object's values (it may be async). Give a provider \`Add\` only when the type owns its objects (sample or local data). A provider that reads its objects from somewhere else (an API such as Trello, another system) has no \`Add\`: those objects come from the source, and adding one locally would not create it there. (An \`Add\` that first creates the item in the API and then returns its values is fine, when the API allows it and the user asked for it.)
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
      // One host session per open app page (AppPlatform.AppSession.pas)
      liveSessions: true,
    },
    ui: {
      welcome: 'Describe the app you want. The AI writes one HTML page with a browser script for UI and a script_host block that runs inside AppPlatform: it shows AppPlatform objects as live objects that stay in sync with the Studio and other windows.',
      suggestions: [
        { label: 'New Type', prompt: 'Create a type Invoice with a number, an amount, a paid flag and a customer, with three sample invoices' },
        { label: 'Project List', prompt: 'Build an app that lists all AppPlatform projects with their number of tasks' },
        { label: 'Rename Project', prompt: 'Build an app that lets me pick a project and change its description' },
        { label: 'Live Editor', prompt: 'Build an app that shows the description of the project it was opened with (or the first project) as a live field I can edit, with a checkbox that makes it read-only in this window' },
        { label: 'GitHub Import', prompt: 'Build an app that imports the open issues of a GitHub repository as tasks of a selected project' },
      ],
      tokenNamePlaceholder: 'Name (e.g. GitHub)',
    },
    hostRuntime: {
      prelude: PRELUDE,
      scope: 'const app = globalThis.app;',
      nativeGlobals: ['IProject', 'List', 'StatusCode', 'DateTime', 'types'],
      sessionPrelude: SESSION_PRELUDE,
    },
    prompt: {
      // The live-session app format replaces the builder's default format
      // description and example; its other parts stay
      app: ({ profile, sections }) => [
        sections.identity(),
        liveAppSection(profile),
        liveExampleSection(),
        sections.tools(),
        sections.tokenVault(),
        sections.rules(),
        sections.saveChecks(),
      ].join('\n\n'),
      knowledge: HOST_API_PROMPT + '\n\n' + TYPE_SCRIPTS_PROMPT,
    },
    toolDescriptions: {
      update_app: 'Save the AppPlatform app. Pass the COMPLETE HTML document in html — it fully replaces the previous version. The document must be self-contained and contain one browser <script> plus one <script_host> block. The page uses window.host.open(name) for live objects and window.host.run(action, args) for actions; the host block exposes objects with page.expose and defines actions with actions.name = ... .',
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
