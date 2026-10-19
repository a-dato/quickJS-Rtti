// Page side of the transport (same idea as IntegrationProject/wwwroot/scriptGateIntegration.js)
//   syncHost.call(method, payload) -> Promise   page -> Delphi request
//   syncHost.on(channel, fn)                    Delphi -> page pushes
(function () {
  let counter = 0;
  const pending = new Map();
  const listeners = new Map();
  let traceFn = () => {};

  const toBase64 = (text) => btoa(String.fromCharCode(...new TextEncoder().encode(text)));
  const fromBase64 = (b64) => JSON.parse(new TextDecoder().decode(Uint8Array.from(atob(b64), c => c.charCodeAt(0))));

  function call(method, payload = {}) {
    const requestId = `${method}_${++counter}`;   // must not contain ',' or ')'
    return new Promise((resolve, reject) => {
      pending.set(requestId, { resolve, reject });
      traceFn('PAGE -> DELPHI', `syncHost.call('${method}', ${JSON.stringify(payload)})  [request ${requestId}]`);
      // ScriptGate intercepts this navigation and calls TSyncBrowserConnector.SyncRequest.
      // encodeURIComponent: ScriptGate URL-decodes, so '+' and '/' must survive.
      const data = encodeURIComponent(toBase64(JSON.stringify({ method, payload })));
      window.location.href = `qjs-sync:SyncRequest(${requestId},${data})`;
    });
  }

  // Delphi answers a request
  function onResponse(requestId, b64, isError) {
    const p = pending.get(requestId);
    if (!p) return;
    pending.delete(requestId);
    const response = fromBase64(b64);   // {ok, result} or {ok:false, error}
    const shown = response.result && response.result.values
      ? `{"ok":true,"result":<snapshot>}` : JSON.stringify(response);
    traceFn('DELPHI -> PAGE', `onResponse('${requestId}', ${shown})`);
    if (isError || !response.ok) p.reject(new Error(response.error));
    else p.resolve(response);
  }

  // Delphi pushes a message
  function receive(channel, b64) {
    const value = fromBase64(b64);
    const shown = value && value.changes ? `batch #${value.batch} (${value.changes.length} changes)` : JSON.stringify(value);
    traceFn('DELPHI -> PAGE', `receive('${channel}', ${shown})`);
    for (const fn of listeners.get(channel) || []) fn(value);
  }

  function on(channel, fn) {
    if (!listeners.has(channel)) listeners.set(channel, []);
    listeners.get(channel).push(fn);
  }

  // onTrace(fn(tag, text)) - lets the page show every call in and out
  window.syncHost = { call, on, onResponse, receive, onTrace: (fn) => { traceFn = fn; } };
})();
