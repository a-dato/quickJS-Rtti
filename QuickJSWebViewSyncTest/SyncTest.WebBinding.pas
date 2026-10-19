unit SyncTest.WebBinding;

// Binding webview elements with the same ObjectModel mechanism as Delphi controls.
// Everything is started by the page (through qjs\sync-bridge.js):
//
//   ListObjects('order')            the objects, with their values
//   OpenContext('order', key)       a NEW IObjectModelContext owned by the page, on one object
//   OpenListContext('order')        the list model's own ObjectModelContext (the one that
//                                   follows the current row, like Edit2/Label2 in BindSimpleTest).
//                                   Shared with Delphi; the page only adds bindings to it.
//   Bind(context, 'Quantity')       bind one property in a context; the answer has the value
//   Edit(binding, value)            the page changed a bound field
//   SetCurrent('order', key)        move the list's current item (ObjectContext)
//   CloseContext(context)           page-owned: unbind everything and drop the context
//                                   list context: unbind only the page's own bindings
//   Stats                           how many contexts and bindings the page has right now
//   ListState('order')              count, CanAdd/CanEdit/CanRemove, IsNew/IsEdit/IsChanged
//   AddNew / RemoveCurrent / EndEdit / CancelEdit('order')
//                                   the list model's own add/remove/edit operations
//                                   (IAddToList, IEditableModel), same as the DataControl uses
//
// Pushes to the page (through the push handler), as JSON with a "channel":
//   value   {id, property, value, editable, visible, reason}
//                                          a bound property changed, or only whether
//                                          it can be edited (IPropertyAccessibility)
//   current {type, key}                    the list's current item changed
//   list    {type, change, key, index, state}  an item was added/removed, or an edit
//                                          started/ended/was cancelled (IListItemChanged)

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON, FMX.Controls,
  System_, ADato.ObjectModel.intf, ADato.ObjectModel.List.intf, ADato.ObjectModel.Binders,
  ADato.ObjectModel.TrackInterfaces, ADato.Models.VirtualListItemDelegate, ADato.InsertPosition,
  ADato.PropertyAccessibility.Intf;

type
  TWebPushProc = reference to procedure(const Json: string);

  // Also implements IControlBinding (with no Delphi control): the framework
  // compares bindings with TControlBinding.Equals, which casts the other
  // binding to IControlBinding.
  TWebViewPropertyBinding = class(TPropertyBinding, IControlBinding)
  private
    FId: Integer;
    FContextId: Integer;
    FPageValue: CObject;   // last value the page sent (GetValue)
    FShown: CObject;       // last value the page has
    FHasShown: Boolean;
    FShownEditable, FShownVisible: Boolean;
    FPush: TWebPushProc;
    procedure Push(const Value: CObject; const Reason: string);
    // Same rule as TControlBinding.CheckControlEditability: ask the context's
    // object (IPropertyAccessibility); without it, writable properties are editable
    procedure GetState(out Editable, Visible: Boolean);
  protected
    function  GetValue: CObject; override;
    procedure SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject); override;
    procedure ExecuteOriginalOnChangeEvent; override;
    // IControlBinding
    function  get_Control: TControl;
    procedure UpdateControlEditability(IsEditable: Boolean);
    procedure OnFreeNotificationDestroy;
  public
    constructor Create(Id, ContextId: Integer; const PushProc: TWebPushProc);
    procedure PageEdited(const Value: CObject);
  end;

  // What Delphi offers to the page, per type name ('order')
  TObjectSource = record
    ListModel: IObjectListModel;      // its ObjectModel is used for page contexts too
    Items: TFunc<TArray<CObject>>;    // the objects, in list order
    KeyOf: TFunc<CObject, string>;
    CaptionOf: TFunc<CObject, string>;
    // ctx.Bind (+ ctx.Link for calculated properties), same as for Delphi controls
    BindProperty: TProc<IObjectModelContext, string, IPropertyBinding>;
    function Find(const Key: string): CObject;
  end;

  {$M+}
  TJsPushHandler = reference to function(const Json: string): string;

  // Exposed to QuickJS as 'bindings'. Never raises (called from QuickJS):
  // answers are JSON strings, errors are {"error":"..."}.
  IWebBindingHost = interface
    ['{3B9E5D71-2C4A-4F86-B1D3-7A0E6C9F2B48}']
    function  ListObjects(const TypeName: string): string;
    function  OpenContext(const TypeName, Key: string): string;
    function  OpenListContext(const TypeName: string): string;
    function  Bind(const ContextId: Integer; const PropName: string): string;
    function  Edit(const BindingId: Integer; const Value: string): string;
    function  SetCurrent(const TypeName, Key: string): string;
    function  CloseContext(const ContextId: Integer): string;
    function  Stats: string;
    function  ListState(const TypeName: string): string;
    function  AddNew(const TypeName: string): string;
    function  RemoveCurrent(const TypeName: string): string;
    function  EndEdit(const TypeName: string): string;
    function  CancelEdit(const TypeName: string): string;
    procedure SetPushHandler(const Handler: TJsPushHandler);
  end;
  {$M-}

  TWebBindingHost = class;

  // Listens to the list model like the DataControl does (OnItemChanged) and
  // tells the page about adds, removes and edit sessions
  TWebListListener = class(TVirtualListItemChanged)
  private
    FHost: TWebBindingHost;   // not ref counted, the host outlives the registration
    FTypeName: string;
    procedure Send(const Change: string; const Item: CObject; Index: Integer);
  protected
    procedure AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition); override;
    procedure Removed(const Value: CObject; const Index: Integer); override;
    procedure BeginEdit(const Item: CObject); override;
    procedure CancelEdit(const Item: CObject); override;
    procedure EndEdit(const Item: CObject); override;
  public
    constructor Create(const Host: TWebBindingHost; const TypeName: string);
  end;

  TWebContext = class
    TypeName: string;
    Context: IObjectModelContext;
    Owned: Boolean;          // False: the list model's context, shared with Delphi
    BindingIds: TList<Integer>;
    constructor Create;
    destructor Destroy; override;
  end;

  TWebBindingHost = class(TInterfacedObject, IWebBindingHost)
  private
    FSources: TDictionary<string, TObjectSource>;
    FContexts: TObjectDictionary<Integer, TWebContext>;
    FBindings: TDictionary<Integer, IPropertyBinding>;
    FWatchedLists: TList<string>;
    FListListeners: TDictionary<string, IListItemChanged>;
    FNextId: Integer;
    FPushHandler: TJsPushHandler;
    FOnContextCreated: TProc<IObjectModelContext>;
    function  Error(const Msg: string): string;
    procedure SendPush(const Json: string);
    procedure ListCurrentChanged(const Sender: IObjectModelContext; const Context: CObject);
    function  NewContext(const TypeName: string; const Ctx: IObjectModelContext; Owned: Boolean): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterSource(const TypeName: string; const Source: TObjectSource);

    function  ListObjects(const TypeName: string): string;
    function  OpenContext(const TypeName, Key: string): string;
    function  OpenListContext(const TypeName: string): string;
    function  Bind(const ContextId: Integer; const PropName: string): string;
    function  Edit(const BindingId: Integer; const Value: string): string;
    function  SetCurrent(const TypeName, Key: string): string;
    function  CloseContext(const ContextId: Integer): string;
    function  Stats: string;
    function  ListState(const TypeName: string): string;
    function  AddNew(const TypeName: string): string;
    function  RemoveCurrent(const TypeName: string): string;
    function  EndEdit(const TypeName: string): string;
    function  CancelEdit(const TypeName: string): string;
    procedure SetPushHandler(const Handler: TJsPushHandler);
    function  ListStateObject(const TypeName: string): TJSONObject;

    // Unbinds everything the page bound and releases the JS function
    procedure Detach;
    // Called for every context the page opens itself (the form hooks it)
    property OnContextCreated: TProc<IObjectModelContext> read FOnContextCreated write FOnContextCreated;
  end;

implementation

uses
  System.StrUtils, ADato.ObjectModel.impl, SyncTest.Trace;

function ValueToJson(const Value: CObject): TJSONValue;
begin
  var d: Double;
  if Value = nil then
    Result := TJSONNull.Create
  else if Value.TryGetValue<Double>(d) then
    Result := TJSONNumber.Create(d)
  else
    Result := TJSONString.Create(string(Value.ToString));
end;

function ValueText(const Value: CObject): string;
begin
  if Value = nil then Result := 'nil' else Result := Value.ToString;
end;

{ TObjectSource }

function TObjectSource.Find(const Key: string): CObject;
begin
  for var item in Items() do
    if KeyOf(item) = Key then
      Exit(item);
  Result := nil;
end;

{ TWebContext }

constructor TWebContext.Create;
begin
  inherited;
  BindingIds := TList<Integer>.Create;
end;

destructor TWebContext.Destroy;
begin
  BindingIds.Free;
  inherited;
end;

{ TWebViewPropertyBinding }

constructor TWebViewPropertyBinding.Create(Id, ContextId: Integer; const PushProc: TWebPushProc);
begin
  inherited Create;
  FId := Id;
  FContextId := ContextId;
  FPush := PushProc;
end;

function TWebViewPropertyBinding.GetValue: CObject;
begin
  Result := FPageValue;
end;

procedure TWebViewPropertyBinding.ExecuteOriginalOnChangeEvent;
begin
end;

function TWebViewPropertyBinding.get_Control: TControl;
begin
  Result := nil;   // the "control" lives in the page
end;

procedure TWebViewPropertyBinding.UpdateControlEditability(IsEditable: Boolean);
begin
end;

procedure TWebViewPropertyBinding.OnFreeNotificationDestroy;
begin
end;

// Called by the framework, exactly like it calls TEditControlBinding.SetValue:
//  - AProperty is our bound property -> its value changed (edit, or a new object)
//  - AProperty is another property   -> we are Linked to it, re-read our own
procedure TWebViewPropertyBinding.SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject);
begin
  if IsUpdating then Exit;   // our own edit coming back, the page already shows it

  if IsBoundProperty(AProperty) then
    Push(Value, 'bound property changed')
  else
    Push(__PropertyInfo.GetValue(Obj, []), 'linked: ' + string(AProperty.Name) + ' changed');
end;

procedure TWebViewPropertyBinding.GetState(out Editable, Visible: Boolean);
begin
  Visible := True;
  var obj: CObject := nil;
  if _ObjectModelContext <> nil then
    obj := _ObjectModelContext.Context;
  if obj = nil then
  begin
    Editable := False;
    Exit;
  end;
  var pa: IPropertyAccessibility;
  if obj.TryAsType<IPropertyAccessibility>(pa) then
  begin
    var st := pa.PropertyState(__PropertyInfo.Name);
    Editable := st.IsEditable;
    Visible := st.IsVisible;
  end
  else
    Editable := __PropertyInfo.CanWrite;
end;

procedure TWebViewPropertyBinding.Push(const Value: CObject; const Reason: string);
begin
  var propName: string := __PropertyInfo.Name;
  var tag := Format('WEB BINDING #%d (context %d)', [FId, FContextId]);
  var editable, visible: Boolean;
  GetState(editable, visible);

  var sameState := FHasShown and (editable = FShownEditable) and (visible = FShownVisible);
  if FHasShown and CObject.Equals(Value, FShown) and sameState then
  begin
    Trace.Line(tag, Format('%s: %s, value %s unchanged, page not told', [propName, Reason, ValueText(Value)]));
    Exit;
  end;

  // First value (during Bind): the page gets it in the bind answer, no push
  if not FHasShown then
  begin
    FShown := Value;
    FShownEditable := editable;
    FShownVisible := visible;
    FHasShown := True;
    Exit;
  end;

  var what := 'push ' + ValueText(Value);
  if not sameState then
    what := what + IfThen(editable, ', now editable', ', now read only');
  FShown := Value;
  FShownEditable := editable;
  FShownVisible := visible;
  var o := TJSONObject.Create;
  try
    o.AddPair('channel', 'value');
    o.AddPair('id', TJSONNumber.Create(FId));
    o.AddPair('property', propName);
    o.AddPair('value', ValueToJson(Value));
    o.AddPair('editable', TJSONBool.Create(editable));
    o.AddPair('visible', TJSONBool.Create(visible));
    o.AddPair('reason', Reason);
    Trace.Enter(tag, Format('%s: %s -> %s', [propName, Reason, what]));
    try
      if Assigned(FPush) then
        FPush(o.ToJSON);
    finally
      Trace.Leave;
    end;
  finally
    o.Free;
  end;
end;

procedure TWebViewPropertyBinding.PageEdited(const Value: CObject);
begin
  FPageValue := Value;
  FShown := Value;     // the page shows this already
  // Same path a TEdit/TSpinBox binding takes on its OnChange
  NotifyModel(nil);
end;

{ TWebBindingHost }

constructor TWebBindingHost.Create;
begin
  inherited Create;
  FSources := TDictionary<string, TObjectSource>.Create;
  FContexts := TObjectDictionary<Integer, TWebContext>.Create([doOwnsValues]);
  FBindings := TDictionary<Integer, IPropertyBinding>.Create;
  FWatchedLists := TList<string>.Create;
  FListListeners := TDictionary<string, IListItemChanged>.Create;
end;

destructor TWebBindingHost.Destroy;
begin
  FListListeners.Free;
  FWatchedLists.Free;
  FBindings.Free;
  FContexts.Free;
  FSources.Free;
  inherited;
end;

procedure TWebBindingHost.RegisterSource(const TypeName: string; const Source: TObjectSource);
begin
  FSources.AddOrSetValue(TypeName, Source);

  // Same hook the DataControl uses to hear about adds/removes/edits
  var ct: IOnItemChangedSupport;
  if Interfaces.Supports<IOnItemChangedSupport>(Source.ListModel, ct) then
  begin
    var listener: IListItemChanged := TWebListListener.Create(Self, TypeName);
    ct.OnItemChanged.Add(listener);
    FListListeners.Add(TypeName, listener);
  end;
end;

function TWebBindingHost.Error(const Msg: string): string;
begin
  var o := TJSONObject.Create;
  try
    o.AddPair('error', Msg);
    Result := o.ToJSON;
  finally
    o.Free;
  end;
end;

procedure TWebBindingHost.SendPush(const Json: string);
begin
  if not Assigned(FPushHandler) then Exit;
  Trace.Enter('DELPHI -> QJS', 'pushHandler(' + Trace.Summary(Json) + ')');
  try
    FPushHandler(Json);
  finally
    Trace.Leave;
  end;
end;

procedure TWebBindingHost.SetPushHandler(const Handler: TJsPushHandler);
begin
  FPushHandler := Handler;
  Trace.Line('QJS -> DELPHI', 'bindings.SetPushHandler(<js function>)');
end;

function TWebBindingHost.NewContext(const TypeName: string; const Ctx: IObjectModelContext; Owned: Boolean): Integer;
begin
  Inc(FNextId);
  var wc := TWebContext.Create;
  wc.TypeName := TypeName;
  wc.Context := Ctx;
  wc.Owned := Owned;
  FContexts.Add(FNextId, wc);
  Result := FNextId;
end;

function TWebBindingHost.ListObjects(const TypeName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.ListObjects(%s)', [Trace.Quote(TypeName)]));
  try
    var src: TObjectSource;
    if not FSources.TryGetValue(TypeName, src) then
      Exit(Error(Format('Unknown type "%s"', [TypeName])));
    var arr := TJSONArray.Create;
    try
      for var obj in src.Items() do
      begin
        var o := TJSONObject.Create;
        o.AddPair('key', src.KeyOf(obj));
        o.AddPair('caption', src.CaptionOf(obj));
        var values := TJSONObject.Create;
        for var prop in src.ListModel.ObjectModel.GetType.GetProperties do
          values.AddPair(string(prop.Name), ValueToJson(prop.GetValue(obj, [])));
        o.AddPair('values', values);
        arr.AddElement(o);
      end;
      Result := arr.ToJSON;
    finally
      arr.Free;
    end;
  finally
    Trace.Leave(Trace.Summary(Result));
  end;
end;

function TWebBindingHost.OpenContext(const TypeName, Key: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.OpenContext(%s, %s)', [Trace.Quote(TypeName), Trace.Quote(Key)]));
  try
    var src: TObjectSource;
    if not FSources.TryGetValue(TypeName, src) then
      Exit(Error(Format('Unknown type "%s"', [TypeName])));
    var obj := src.Find(Key);
    if obj = nil then
      Exit(Error(Format('No %s with key "%s"', [TypeName, Key])));

    // A context of the page's own, on the SAME model as the list, so all
    // contexts share property wrappers and see each other's changes. A plain
    // context: ListModel.CreateObjectModelContext would give an editable context
    // that takes part in the list's own edit session.
    var ctx: IObjectModelContext := TObjectModelContext.Create(src.ListModel.ObjectModel);
    ctx.Context := obj;
    var id := NewContext(TypeName, ctx, True);
    Trace.Line('DELPHI', Format('new IObjectModelContext #%d (page owned), Context := %s', [id, src.CaptionOf(obj)]));
    if Assigned(FOnContextCreated) then
      FOnContextCreated(ctx);
    Result := Format('{"context":%d,"key":"%s"}', [id, Key]);
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.OpenListContext(const TypeName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.OpenListContext(%s)', [Trace.Quote(TypeName)]));
  try
    var src: TObjectSource;
    if not FSources.TryGetValue(TypeName, src) then
      Exit(Error(Format('Unknown type "%s"', [TypeName])));

    // The list model's own context: it follows the current row (DataControl
    // selection). Not owned by the page.
    var ctx := src.ListModel.ObjectModelContext;
    var id := NewContext(TypeName, ctx, False);

    if not FWatchedLists.Contains(TypeName) then
    begin
      ctx.OnContextChanged.Add(ListCurrentChanged);
      FWatchedLists.Add(TypeName);
    end;

    var key := '';
    if ctx.Context <> nil then
      key := src.KeyOf(ctx.Context);
    Trace.Line('DELPHI', Format('context #%d = the list model''s ObjectModelContext (shared), current item: %s', [id, key]));
    Result := Format('{"context":%d,"key":"%s"}', [id, key]);
  finally
    Trace.Leave(Result);
  end;
end;

procedure TWebBindingHost.ListCurrentChanged(const Sender: IObjectModelContext; const Context: CObject);
begin
  for var pair in FSources do
    if pair.Value.ListModel.ObjectModelContext = Sender then
    begin
      var key := '';
      if Context <> nil then
        key := pair.Value.KeyOf(Context);
      Trace.Line('DELPHI', Format('list current item -> %s: tell the page', [key]));
      SendPush(Format('{"channel":"current","type":"%s","key":"%s"}', [pair.Key, key]));
    end;
end;

function TWebBindingHost.SetCurrent(const TypeName, Key: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.SetCurrent(%s, %s)', [Trace.Quote(TypeName), Trace.Quote(Key)]));
  try
    var src: TObjectSource;
    if not FSources.TryGetValue(TypeName, src) then
      Exit(Error(Format('Unknown type "%s"', [TypeName])));
    var obj := src.Find(Key);
    if obj = nil then
      Exit(Error(Format('No %s with key "%s"', [TypeName, Key])));
    // Same as selecting the row in the DataControl
    Trace.Line('DELPHI', Format('listModel.ObjectContext := %s', [src.CaptionOf(obj)]));
    src.ListModel.ObjectContext := obj;
    Result := '{}';
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.Bind(const ContextId: Integer; const PropName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.Bind(%d, %s)', [ContextId, Trace.Quote(PropName)]));
  try
    var wc: TWebContext;
    if not FContexts.TryGetValue(ContextId, wc) then
      Exit(Error(Format('Unknown context %d', [ContextId])));

    Inc(FNextId);
    var id := FNextId;
    var binding := TWebViewPropertyBinding.Create(id, ContextId, SendPush);
    var intf: IPropertyBinding := binding;
    try
      // Same call as for a Delphi control. Bind reads the current value right away.
      FSources[wc.TypeName].BindProperty(wc.Context, PropName, intf);
    except
      on E: Exception do
        Exit(Error(Format('Cannot bind "%s": %s', [PropName, E.Message])));
    end;
    FBindings.Add(id, intf);
    wc.BindingIds.Add(id);

    var o := TJSONObject.Create;
    try
      o.AddPair('id', TJSONNumber.Create(id));
      o.AddPair('value', ValueToJson(binding.FShown));
      o.AddPair('editable', TJSONBool.Create(binding.FShownEditable));
      o.AddPair('visible', TJSONBool.Create(binding.FShownVisible));
      Result := o.ToJSON;
    finally
      o.Free;
    end;
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.Edit(const BindingId: Integer; const Value: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.Edit(%d, %s)', [BindingId, Trace.Quote(Value)]));
  try
    var binding: IPropertyBinding;
    if not FBindings.TryGetValue(BindingId, binding) then
      Exit(Error(Format('Unknown binding %d', [BindingId])));
    var wb := binding as TWebViewPropertyBinding;
    if binding.ObjectModelContext.Context = nil then
      Exit(Error('There is no current item to edit'));
    var editable, visible: Boolean;
    wb.GetState(editable, visible);
    if not editable then
    begin
      Result := Error(Format('%s cannot be edited right now', [string(binding.PropertyInfo.Name)]));
      wb.FShown := nil;   // put the model value back on the page
      wb.Push(binding.PropertyInfo.GetValue(binding.ObjectModelContext.Context, []), 'edit refused, restore');
      Exit;
    end;
    var d: Double;
    if not TryStrToFloat(Value.Trim, d, TFormatSettings.Invariant) then
      Exit(Error(Format('"%s" is not a number', [Value])));
    try
      wb.PageEdited(d);
      Result := '{}';
    except
      // e.g. the model's setter rejected the value. Must not reach QuickJS.
      on E: Exception do
      begin
        Result := Error(E.Message);
        wb.FShown := nil;
        wb.Push(binding.PropertyInfo.GetValue(binding.ObjectModelContext.Context, []), 'edit rejected, restore');
      end;
    end;
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.CloseContext(const ContextId: Integer): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.CloseContext(%d)', [ContextId]));
  try
    var wc: TWebContext;
    if not FContexts.TryGetValue(ContextId, wc) then
      Exit(Error(Format('Unknown context %d', [ContextId])));

    if wc.Owned then
    begin
      // The page's own context: unbind everything in it
      Trace.Line('DELPHI', Format('context #%d: ctx.Unbind (all %d bindings)', [ContextId, wc.BindingIds.Count]));
      wc.Context.Unbind;
    end
    else
    begin
      // Shared list context: unbind only what the page bound; Delphi's controls stay
      Trace.Line('DELPHI', Format('context #%d (shared list context): ctx.Unbind(binding) for the page''s %d bindings',
        [ContextId, wc.BindingIds.Count]));
      for var id in wc.BindingIds do
        wc.Context.Unbind(FBindings[id]);
    end;

    for var id in wc.BindingIds do
      FBindings.Remove(id);
    FContexts.Remove(ContextId);
    Result := '{}';
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.Stats: string;
begin
  Result := Format('{"contexts":%d,"bindings":%d}', [FContexts.Count, FBindings.Count]);
end;

function TWebBindingHost.ListStateObject(const TypeName: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  var src: TObjectSource;
  if not FSources.TryGetValue(TypeName, src) then Exit;
  var lm := src.ListModel;
  var count := 0;
  if lm.Context <> nil then count := lm.Context.Count;
  Result.AddPair('count', TJSONNumber.Create(count));
  var key := '';
  if lm.ObjectContext <> nil then key := src.KeyOf(lm.ObjectContext);
  Result.AddPair('current', key);
  var em: IEditableModel;
  if Interfaces.Supports<IEditableModel>(lm, em) then
  begin
    Result.AddPair('canAdd', TJSONBool.Create(em.CanAdd));
    Result.AddPair('canEdit', TJSONBool.Create(em.CanEdit));
    Result.AddPair('canRemove', TJSONBool.Create(em.CanRemove));
  end;
  var es: IEditState;
  if Interfaces.Supports<IEditState>(lm, es) then
  begin
    Result.AddPair('isNew', TJSONBool.Create(es.IsNew));
    Result.AddPair('isEdit', TJSONBool.Create(es.IsEdit));
    Result.AddPair('isChanged', TJSONBool.Create(es.IsChanged));
  end;
end;

function TWebBindingHost.ListState(const TypeName: string): string;
begin
  var o := ListStateObject(TypeName);
  try
    Result := o.ToJSON;
  finally
    o.Free;
  end;
end;

function TWebBindingHost.AddNew(const TypeName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.AddNew(%s)', [Trace.Quote(TypeName)]));
  try
    var src: TObjectSource;
    var add: IAddToList;
    if not FSources.TryGetValue(TypeName, src) or not Interfaces.Supports<IAddToList>(src.ListModel, add) then
      Exit(Error(Format('"%s" cannot add items', [TypeName])));
    try
      // Same call as the DataControl's "add row": creates the item through the
      // creator function, inserts it and makes it the current (new) item
      Trace.Line('DELPHI', '(ListModel as IAddToList).AddNew');
      if not add.AddNew then
        Exit(Error('The list did not add an item'));
      Result := ListState(TypeName);
    except
      on E: Exception do Result := Error(E.Message);
    end;
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.RemoveCurrent(const TypeName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.RemoveCurrent(%s)', [Trace.Quote(TypeName)]));
  try
    var src: TObjectSource;
    var em: IEditableModel;
    if not FSources.TryGetValue(TypeName, src) or not Interfaces.Supports<IEditableModel>(src.ListModel, em) then
      Exit(Error(Format('"%s" cannot remove items', [TypeName])));
    if not em.CanRemove then
      Exit(Error('Nothing to remove'));
    try
      // Same call as the DataControl's "delete row": removes the current item
      Trace.Line('DELPHI', '(ListModel as IEditableModel).Remove');
      em.Remove;
      Result := ListState(TypeName);
    except
      on E: Exception do Result := Error(E.Message);
    end;
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.EndEdit(const TypeName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.EndEdit(%s)', [Trace.Quote(TypeName)]));
  try
    var src: TObjectSource;
    var em: IEditableModel;
    if not FSources.TryGetValue(TypeName, src) or not Interfaces.Supports<IEditableModel>(src.ListModel, em) then
      Exit(Error(Format('"%s" has no edit session', [TypeName])));
    try
      em.EndEdit;
      Result := ListState(TypeName);
    except
      on E: Exception do Result := Error(E.Message);
    end;
  finally
    Trace.Leave(Result);
  end;
end;

function TWebBindingHost.CancelEdit(const TypeName: string): string;
begin
  Trace.Enter('QJS -> DELPHI', Format('bindings.CancelEdit(%s)', [Trace.Quote(TypeName)]));
  try
    var src: TObjectSource;
    var em: IEditableModel;
    if not FSources.TryGetValue(TypeName, src) or not Interfaces.Supports<IEditableModel>(src.ListModel, em) then
      Exit(Error(Format('"%s" has no edit session', [TypeName])));
    try
      em.CancelEdit;
      Result := ListState(TypeName);
    except
      on E: Exception do Result := Error(E.Message);
    end;
  finally
    Trace.Leave(Result);
  end;
end;

{ TWebListListener }

constructor TWebListListener.Create(const Host: TWebBindingHost; const TypeName: string);
begin
  inherited Create;
  FHost := Host;
  FTypeName := TypeName;
end;

procedure TWebListListener.Send(const Change: string; const Item: CObject; Index: Integer);
begin
  var src: TObjectSource;
  if not FHost.FSources.TryGetValue(FTypeName, src) then Exit;
  var key := '';
  if Item <> nil then key := src.KeyOf(Item);
  Trace.Line('LIST', Format('%s: %s (index %d), tell the page', [Change, key, Index]));
  var o := TJSONObject.Create;
  try
    o.AddPair('channel', 'list');
    o.AddPair('type', FTypeName);
    o.AddPair('change', Change);
    o.AddPair('key', key);
    o.AddPair('index', TJSONNumber.Create(Index));
    o.AddPair('state', FHost.ListStateObject(FTypeName));
    FHost.SendPush(o.ToJSON);
  finally
    o.Free;
  end;
end;

procedure TWebListListener.AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition);
begin
  Send('added', Value, Index);
end;

procedure TWebListListener.Removed(const Value: CObject; const Index: Integer);
begin
  Send('removed', Value, Index);
end;

procedure TWebListListener.BeginEdit(const Item: CObject);
begin
  Send('beginEdit', Item, -1);
end;

procedure TWebListListener.EndEdit(const Item: CObject);
begin
  Send('endEdit', Item, -1);
end;

procedure TWebListListener.CancelEdit(const Item: CObject);
begin
  // Cancelling a NEW item takes it out of the list again
  var src: TObjectSource;
  if FHost.FSources.TryGetValue(FTypeName, src) and (src.ListModel.Context.IndexOf(Item) = -1) then
    Send('removed', Item, -1)
  else
    Send('cancelEdit', Item, -1);
end;

procedure TWebBindingHost.Detach;
begin
  for var id in FContexts.Keys.ToArray do
    CloseContext(id);
  for var pair in FSources do
  begin
    if FWatchedLists.Contains(pair.Key) then
      pair.Value.ListModel.ObjectModelContext.OnContextChanged.Remove(ListCurrentChanged);
    var listener: IListItemChanged;
    var ct: IOnItemChangedSupport;
    if FListListeners.TryGetValue(pair.Key, listener) and
       Interfaces.Supports<IOnItemChangedSupport>(pair.Value.ListModel, ct) then
      ct.OnItemChanged.Remove(listener);
  end;
  FListListeners.Clear;
  FWatchedLists.Clear;
  FPushHandler := nil;
end;

end.
