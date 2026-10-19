# QuickJS WebView Sync Test

A list of orders is bound in Delphi the way BindSimpleTest does it, and the webview binds to the **same** list model.

**Delphi:**
- An `IList<IOrder>` goes into a `TObjectListModel<IOrder>`, shown by a `TDataControl`.
- The detail controls are bound to `ListModel.ObjectModelContext`. That context follows the selected row (like Edit2/Label2 in BindSimpleTest).

**Webview, two tabs:**

| Tab | What the page does | Bound to |
|---|---|---|
| **List** | <ul><li>Runs `return app.orders();` in QuickJS and builds a table, subscribing every row.</li><li>Clicking a name moves the list's current item (`ListModel.ObjectContext := …`); the DataControl follows.</li><li>The "Current item" panel is bound to the **list model's ObjectModelContext**, the same context as the Delphi detail controls.</li></ul> | <ul><li>A page-owned context per row.</li><li>The shared list context.</li></ul> |
| **Select one** | <ul><li>Runs a script for the keys, then builds a dropdown.</li><li>On every change it unbinds the previous order and binds the chosen one.</li></ul> | <ul><li>One page-owned context.</li></ul> |

**Unbinding:** switching tabs unbinds everything the old tab bound, and the header shows the live count (`page bindings: 36 in 6 context(s)` / `6 in 1`).
- **Page-owned contexts** are unbound completely (`ctx.Unbind`) and dropped.
- **The shared list context** only loses the page's own bindings (`ctx.Unbind(binding)`). The Delphi detail controls stay bound.

## Adding and removing (change tracking)

The list model is `TObjectListModelWithChangeTracking<IOrder>`, as in BindSimpleTest's "Bind+Tracking", with a creator function that makes "Order 6", "Order 7" and so on.

**Operations.** Both UIs use the list model's own operations, the same calls the DataControl makes:

| Action | Call | Effect |
|---|---|---|
| Add | `(ListModel as IAddToList).AddNew` | creates the item, inserts it and makes it the current item, state **New** |
| Remove | `(ListModel as IEditableModel).Remove` | removes the current item; a neighbour becomes current |
| End edit | `IEditableModel.EndEdit` | closes the edit session (an edit through the list context starts one: state **Edit**) |
| Cancel edit | `IEditableModel.CancelEdit` | on a **New** item, removes it again |

**How the webview hears about it.** It uses the same hook as the DataControl: the binding host registers an `IListItemChanged` listener in the model's `OnItemChanged`, and pushes `list {change: added | removed | beginEdit | endEdit | cancelEdit, key, index, state}` to the page.
- **List tab:** a row is subscribed for an added item, and a removed item's row is unsubscribed.
- **Select one tab:** the dropdown refreshes; if the chosen order is gone, the next one is bound.

**State display.**
- **Delphi:** shows `CanAdd`/`CanEdit`/`CanRemove` and `IsNew`/`IsEdit`/`IsChanged`, polled like BindSimpleTest's timer.
- **Page:** gets the state with every `list` push, and asks for it again after an edit.

**The page's own contexts are plain `TObjectModelContext`s on the shared model.** `ListModel.CreateObjectModelContext` would return an editable context that takes part in the list's edit session. As a result, edits made in the Select one tab are not tracked as list edits.

**Known framework behaviour.** After cancelling a new item the list has no current item. The page shows "(none)", but the Delphi detail controls keep showing the cancelled item's values: the editable context skips its binding refresh while it cancels.

## What can be edited (IPropertyAccessibility) and list limits

**The object decides what can be edited.** `TOrder` implements `IPropertyAccessibility` (`ADato.PropertyAccessibility.Intf`):

| Property | State |
|---|---|
| Quantity, UnitPrice | Editable |
| Discount | Editable when Subtotal > 100, otherwise ReadOnly. It is reset to 0 when Subtotal drops to 100 or below, and must stay within 0..Subtotal |
| Name, Subtotal, Total | ReadOnly |

**Delphi:** the framework's control bindings already use this (`TControlBinding.CheckControlEditability`). They enable or disable `spinDiscount` when the current row changes, and when a property it is **linked** to changes (Quantity, UnitPrice).

**Webview:** `TWebViewPropertyBinding` runs the same check. It sends `editable`/`visible` with every bind answer and push, and pushes when *only* the editability changes. It also refuses edits to read-only fields on the Delphi side.

**Page:** has no editable list. Every panel field is an input that is made read-only or editable from those flags.

**List size 2..10.** `TOrderListModel` (a subclass of the tracking model) re-implements `IEditableModel.CanAdd`/`CanRemove`, and guards `AddNew`/`Remove`. The DataControl, the Delphi buttons and the webview all ask the model, so all of them obey it.

**Not covered:**
- **The DataControl does not use `IPropertyAccessibility`.** An inline grid edit of Discount is still refused by the setter, but the grid does not show the cell as read-only.
- **Edits made through the page's own contexts don't re-check Delphi control editability.** For example, an edit in the "Select one" tab to the current row does not re-check `spinDiscount`. The framework only re-checks on the list context's own `OnPropertyChanged`.

## How the pieces fit

| Piece | Role |
|---|---|
| `web/index.html` | <ul><li>All page logic: `run` → build UI → `subscribe` / `subscribeList`.</li><li>Click a name → `setCurrent`; input change → `edit`.</li><li>Tab or dropdown change → `unsubscribe` first. These changes run one after the other.</li></ul> |
| `qjs/sync-bridge.js` | <ul><li>In QuickJS: `app.orders()` and the handlers `run`, `subscribe`, `subscribeList`, `unsubscribe`, `setCurrent` and `edit`, built from `bindings.*`.</li><li>Pushes go to the page per channel (`value`, `current`).</li></ul> |
| `SyncTest.WebBinding.pas` | <ul><li>`TWebBindingHost` (`bindings`): `ListObjects`, `OpenContext` (page-owned), `OpenListContext` (the list model's context, shared), `Bind`, `Edit`, `SetCurrent`, `CloseContext`, `Stats`.</li><li>`TWebViewPropertyBinding` is the `IPropertyBinding` for a page element.</li></ul> |
| `SyncTest.MainForm.*` | <ul><li>The list, the list model, the DataControl and the detail controls.</li><li>Registers the orders as source `'order'`.</li><li>Tells the DataControl to repaint a row when the page edits through a context of its own.</li></ul> |
| `SyncTest.OrderModel.pas` | <ul><li>`IOrder` / `TOrder` (Name, Quantity, UnitPrice, Subtotal, Discount, Total) and the rules.</li><li>`BindOrderProperty` = Bind + Link.</li></ul> |
| `SyncTest.Bridge.pas`, `web/syncHost.js` | <ul><li>ScriptGate transport.</li></ul> |
| `SyncTest.Trace.pas` | <ul><li>The call trace (Delphi panel and `<exe>.log`).</li></ul> |

## Why it stays consistent

- **Shared model:** all page contexts are created from the list model (`ListModel.CreateObjectModelContext`), so every context shares the model's property list.
- **Writes reach every view of that order:** a write notifies every binding whose context currently holds that order. That covers grid rows, the page rows, the current-item panel and the Select panel.
- **The DataControl needs a nudge:** it listens only to the list model's own context. An edit through another context (the Select tab) therefore needs the form to ask it to repaint that row (`DoDataItemChangedInternal`).

## Things to know

- **Inline scripts run inside Delphi:** `run` executes the page's script in QuickJS. That's fine for this local page; pages from elsewhere should only name known scripts.
- **Errors, not exceptions:** `bindings.*` never raise into QuickJS. Errors come back as `{"error": "..."}`.
- **Edit guards:** `Edit` refuses non-numbers, and refuses edits when the list has no current item. A framework `UpdateValue` on a nil context crashes.
- **IControlBinding:** `TWebViewPropertyBinding` implements it (with no control), because `TControlBinding.Equals` casts every binding to it.
- **WebView2 profiles:** each running copy uses its own WebView2 profile folder (`%TEMP%\QuickJSWebViewSyncTest\WebView2-<pid>`). These folders are not cleaned up.

## Performance

- **What a request costs.** Every request now writes a `[TIMING]` line to the trace with:
  - how long Delphi spent handling it,
  - how much of that went into writing the trace,
  - how long the page took to send it after the previous request finished.
- **The trace is buffered.** It is flushed every 100 ms, and the memo keeps the last 2000 lines. Writing it line by line used to take 20–50 ms *per line*, which was over 95% of the time.
- **Fewer round trips.** The real work per request is 0–15 ms; a round trip page → ScriptGate → Delphi → page costs about 30–100 ms. So the page batches:
  - all list rows go in one `subscribeMany`;
  - leaving a tab is one `unsubscribe` with all its contexts.

## Build / run

- **Target platform:** Windows 64-bit.
- **Command line:** `build.bat`. It now also needs the fmx-datacontrol `datacontrol`, `grid_vcl` and `datamodel\Package` folders; the IDE library path already has them.
- **Runtime DLLs:** loaded from `redist\`.
