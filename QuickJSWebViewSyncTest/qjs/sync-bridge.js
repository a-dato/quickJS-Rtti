// Runs inside QuickJS (not in the webview).
//
// Delphi provides two globals:
//   bindings - IWebBindingHost: objects, contexts and bindings
//   webview  - IWebViewChannel: channel to the page
//
// The page sends requests; this file answers them by combining bindings.* calls.

// bindings.* answer JSON strings; {"error": "..."} means it failed
function call(result) {
  const value = JSON.parse(result);
  if (value && value.error) throw new Error(value.error);
  return value;
}

// What inline scripts from the page can use
const app = {
  // [{key: '1', caption: 'Order 1', Name: 'Order 1', Quantity: 2, ..., Total: 80}, ...]
  orders: () => call(bindings.ListObjects('order')).map(o => ({ key: o.key, caption: o.caption, ...o.values })),
};

function bindAll(context, properties) {
  const fields = {};
  for (const property of properties) fields[property] = call(bindings.Bind(context, property));
  return { context, fields, stats: call(bindings.Stats()) };
}

const handlers = {
  // Run an inline script from the page; whatever it returns goes back to the page
  run: ({ code }) => new Function('app', code)(app),

  // One object: a context of the page's own, on that object
  subscribe({ type, key, properties }) {
    const { context } = call(bindings.OpenContext(type, String(key)));
    return bindAll(context, properties);
  },

  // Many objects in one round trip (e.g. every row of a list)
  subscribeMany({ type, keys, properties }) {
    const subs = keys.map(key => {
      const { context } = call(bindings.OpenContext(type, String(key)));
      return bindAll(context, properties);
    });
    return { subs, stats: call(bindings.Stats()) };
  },

  // The list's current item: bind into the list model's own context, the one
  // that follows the selected row (shared with the Delphi detail controls)
  subscribeList({ type, properties }) {
    const { context, key } = call(bindings.OpenListContext(type));
    return { ...bindAll(context, properties), current: key };
  },

  // Unbind: page-owned contexts are dropped; on the list context only the
  // page's own bindings are removed
  unsubscribe({ context, contexts }) {
    for (const c of contexts || [context]) call(bindings.CloseContext(c));
    return { stats: call(bindings.Stats()) };
  },

  // Move the list's current item, same as selecting a row in the DataControl
  setCurrent: ({ type, key }) => call(bindings.SetCurrent(type, String(key))),

  // The list model's own add/remove/edit-session operations (IAddToList,
  // IEditableModel). Each answers the list state; the actual add/remove
  // reaches the page as a 'list' push, like it reaches the DataControl.
  listState:     ({ type }) => call(bindings.ListState(type)),
  addNew:        ({ type }) => call(bindings.AddNew(type)),
  removeCurrent: ({ type }) => call(bindings.RemoveCurrent(type)),
  endEdit:       ({ type }) => call(bindings.EndEdit(type)),
  cancelEdit:    ({ type }) => call(bindings.CancelEdit(type)),

  edit: ({ id, value }) => call(bindings.Edit(id, String(value))),
};

webview.SetRequestHandler((requestJson) => {
  const request = JSON.parse(requestJson);
  try {
    const handler = handlers[request.method];
    if (!handler) throw new Error(`Unknown request "${request.method}"`);
    const result = handler(request.payload || {});
    return JSON.stringify({ ok: true, result: result === undefined ? null : result });
  } catch (e) {
    return JSON.stringify({ ok: false, error: String((e && e.message) || e) });
  }
});

// Delphi pushes: 'value' (a bound property changed) or 'current' (the list's current item moved)
bindings.SetPushHandler((json) => {
  webview.Post(JSON.parse(json).channel, json);
  return '';
});
