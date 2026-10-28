unit AppPlatform.AppSession;

// TAppSession: one per open app page (an app window, or the builder's preview),
// ported from QuickJSWebViewSyncTestSimplified's TWebAppHost.
//
// The app's <script_host> block runs ONCE, when the page opens, on AppPlatform's
// main QuickJS runtime (so it sees app, types and the script types). It decides
// what the page gets, through 'page' (AppsProfile/profile.js, sessionPrelude):
//
//   page.expose('project', { Description: 'edit' }, app.Storage['Projects'][0])
//   page.expose('project', { Description: 'read' })   // again: same object, new list
//   actions.rename = ({ text }) => ...                 // the page's host.run('rename', ...)
//   page.update(project, { Description: 'x' })        // a change every window hears
//   page.expose('projects', { Description: 'read' }, items(app.Storage['Projects']))
//                                                      // a LIVE LIST: each item is
//                                                      // exposed as 'projects/<key>'
//
// Properties not listed never reach the page. The page (through the builder's
// bridge, not through QuickJS) then asks:
//   open('project')                       -> {object, type, properties: {Description: {value, editable}}}
//   edit('project', 'Description', value) -> {value, editable}
// and is pushed (through the connector):
//   'property'  {object, property, value, editable}   a value or editability changed
//   'object'    the open snapshot                       expose changed the object or the list
//   'title'     {title}                                 page.setTitle
//   'list'      {object, keys}                          a live list got other items
//
// A live list (an array passed to page.expose) is one exposed object per item,
// named '<list>/<key>', plus the list's keys (ExposeList). Opening the list's
// name answers {object, list: true, keys}; the page then opens the items.
//
// The page's live objects are bound like Delphi controls: a context on the
// shared model of the type (ModelFor) with one TPagePropertyBinding per exposed
// property. So an edit in the Studio, in another app window or in this page
// reaches all of them.
//
// Nothing here raises into QuickJS or the page: errors are {"error": "..."}.

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON,
  System_, ADato.ObjectModel.intf,
  AppPlatform.AppSync;

type
  {$M+}
  // The session as the host block sees it (the prelude wraps it as 'page')
  IAppSessionHost = interface(IBaseInterface)
    ['{8F2D6B13-4C7A-4E91-A35B-0D9E1C6F7A24}']
    function  Params: string;
    function  Param(const Name: string): CObject;
    function  Expose(const Name: string; const PolicyJson: string; const Value: CObject; const TypeName: string): string;
    function  Update(const Value: CObject; const TypeName: string; const ValuesJson: string): string;
    // The keys of a live list (its items are exposed as '<Name>/<key>')
    function  ExposeList(const Name: string; const KeysJson: string): string;
    function  SetTitle(const Title: string): string;
    function  SetLoadError(const Msg: string): string;
    // The prelude keeps its request handler in globalThis.__appSessionHandlers
    // under this id (a JS function cannot be handed to Delphi on this runtime),
    // and reports it with HandlerReady
    function  SessionId: string;
    function  HandlerReady: string;
  end;
  {$M-}

  // A named value the session was opened with (an object or a plain value)
  TSessionParam = record
    Name: string;
    Value: CObject;
    ValueType: &Type;
    function IsObject: Boolean;
  end;

  // An object the host block exposed, and (once the page opened it) its bindings
  TExposedObject = class
  public
    Name: string;
    Value: CObject;
    ValueType: &Type;
    Policy: TDictionary<string, string>;              // property -> 'read' | 'edit'
    Context: IObjectModelContext;                     // nil until the page opens it
    Bindings: TDictionary<string, TPagePropertyBinding>;
    BindingRefs: TList<IPropertyBinding>;             // keeps the bindings alive
    constructor Create;
    destructor Destroy; override;
    procedure Unbind;
  end;

  TSessionPushProc = reference to procedure(const Channel, Json: string);

  TAppSession = class(TBaseInterfacedObject, IAppSessionHost)
  private
    FId: string;
    FParams: TArray<TSessionParam>;
    FExposed: TObjectDictionary<string, TExposedObject>;
    FLists: TDictionary<string, string>;          // list name -> keys (JSON array)
    FOpenLists: TDictionary<string, Boolean>;     // lists the page opened
    FLoadError: string;
    FHandlerReady: Boolean;
    FPush: TSessionPushProc;
    FRedact: TFunc<string, string>;
    // Every open session (StorageChanged). Declared last: every field after a
    // 'class var' line is a class field too
    class var _sessions: TList<TAppSession>;
  private

    function  Error(const Msg: string): string;
    function  FindParam(const Name: string; out Param: TSessionParam): Boolean;
    function  TypeOf(const Value: CObject; const TypeName: string; out AType: &Type; out Obj: CObject): string;
    function  ParsePolicy(const AType: &Type; const PolicyJson: string; Policy: TDictionary<string, string>): string;
    function  CanEdit(const ObjectName, PropName: string): Boolean;
    function  MakeCanEdit(const ObjectName, PropName: string): TFunc<Boolean>;
    procedure DoPush(const Channel, Json: string);
    procedure BindAll(Obj: TExposedObject);
    function  Snapshot(Obj: TExposedObject): string;
    function  DoExpose(const Name, PolicyJson: string; const Value: CObject; const TypeName: string): string;
    function  DoOpen(const Name: string): string;
    function  DoEdit(const ObjectName, PropName: string; const Value: TJSONValue): string;
    function  DoUpdate(const Value: CObject; const TypeName, ValuesJson: string): string;
  public
    constructor Create(const Id: string; const Params: TArray<TSessionParam>; const Push: TSessionPushProc;
      const Redact: TFunc<string, string>);
    destructor Destroy; override;

    // IAppSessionHost (QuickJS)
    function  Params: string;
    function  Param(const Name: string): CObject;
    function  Expose(const Name: string; const PolicyJson: string; const Value: CObject; const TypeName: string): string;
    function  Update(const Value: CObject; const TypeName: string; const ValuesJson: string): string;
    function  ExposeList(const Name: string; const KeysJson: string): string;
    function  SetTitle(const Title: string): string;
    function  SetLoadError(const Msg: string): string;
    function  SessionId: string;
    function  HandlerReady: string;

    // Objects were added to or removed from a storage: every live session
    // exposes again the live lists it made from that storage
    class procedure StorageChanged(const StorageName: string); static;

    // A request of the page: {op: 'open' | 'edit' | 'run', ...} -> {ok, result} or {ok: false, error}
    function  HandleRequest(const RequestJson: string): string;
    // The host block ran and registered its actions, without an error
    function  Started: Boolean;
    property  LoadError: string read FLoadError;
    property  Id: string read FId;

    // Unbinds everything the page opened; the session answers nothing after this
    procedure Detach;

    // 'IProject:0' or 'Supplier:12' -> the object of that type with that ID
    class function ResolveObject(const Spec: string; out Param: TSessionParam) : string; static;
  end;

  // Runs the session's script on AppPlatform's main runtime, with the session
  // registered as globalThis.__appSession; returns what the script logged. Set
  // by TPlatform.
  TAppSessionRunner = reference to function(const Session: IAppSessionHost; const Code, CodeName: string): string;
  // Evaluates an expression on the main runtime (awaited); its value as text
  TAppSessionQuery = reference to function(const Expression: string): string;

var
  AppSessionRunner: TAppSessionRunner;
  AppSessionQuery: TAppSessionQuery;

implementation

uses
  System.Classes, System.StrUtils, System.Generics.Defaults,
  ADato.ObjectModel.impl,
  App.intf, App.TypeScripts, App.Storage.intf;

function JsString(const Value: string): string; forward;

{ TSessionParam }

function TSessionParam.IsObject: Boolean;
begin
  Result := ValueType.IsInterfaceType or ValueType.IsObjectType or
    ((TypeScriptStore <> nil) and TypeScriptStore.IsScripted(ValueType.Name));
end;

{ TExposedObject }

constructor TExposedObject.Create;
begin
  inherited Create;
  Policy := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  Bindings := TDictionary<string, TPagePropertyBinding>.Create(TIStringComparer.Ordinal);
  BindingRefs := TList<IPropertyBinding>.Create;
end;

destructor TExposedObject.Destroy;
begin
  Unbind;
  BindingRefs.Free;
  Bindings.Free;
  Policy.Free;
  inherited;
end;

procedure TExposedObject.Unbind;
begin
  for var binding in Bindings.Values do
    binding.Detach;
  if Context <> nil then
    Context.Unbind;
  Context := nil;
  Bindings.Clear;
  BindingRefs.Clear;
end;

{ TAppSession }

constructor TAppSession.Create(const Id: string; const Params: TArray<TSessionParam>; const Push: TSessionPushProc;
  const Redact: TFunc<string, string>);
begin
  inherited Create;
  FId := Id;
  FParams := Params;
  FPush := Push;
  FRedact := Redact;
  FExposed := TObjectDictionary<string, TExposedObject>.Create([doOwnsValues], TIStringComparer.Ordinal);
  FLists := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  FOpenLists := TDictionary<string, Boolean>.Create(TIStringComparer.Ordinal);
  if _sessions = nil then
    _sessions := TList<TAppSession>.Create;
  _sessions.Add(Self);
end;

destructor TAppSession.Destroy;
begin
  if _sessions <> nil then
    _sessions.Remove(Self);
  FOpenLists.Free;
  FLists.Free;
  FExposed.Free;
  inherited;
end;

function TAppSession.Error(const Msg: string): string;
begin
  var o := TJSONObject.Create;
  try
    o.AddPair('error', Msg);
    Result := o.ToJSON;
  finally
    o.Free;
  end;
end;

function TAppSession.FindParam(const Name: string; out Param: TSessionParam): Boolean;
begin
  for var p in FParams do
    if SameText(p.Name, Name) then
    begin
      Param := p;
      Exit(True);
    end;
  Result := False;
end;

function FindRegisteredType(const Name: string; out AType: &Type) : Boolean;
begin
  for var t in _app.Config.Types do
    if SameText(t.Name, Name) then
    begin
      AType := t;
      Exit(True);
    end;
  Result := False;
end;

// The registered type of an object from JavaScript: a script type by the name
// the prelude sends (its constructor), a Delphi type by the interfaces it supports
function TAppSession.TypeOf(const Value: CObject; const TypeName: string; out AType: &Type; out Obj: CObject): string;
begin
  Result := '';
  Obj := Value;
  if (TypeName <> '') and (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(TypeName) and FindRegisteredType(TypeName, AType) then
    Exit;

  for var t in _app.Config.Types do
  begin
    if (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(t.Name) then
      Continue;
    var cast: CObject;
    if Value.TryCast(t, cast) then
    begin
      AType := t;
      Obj := cast;
      Exit;
    end;
  end;

  if (TypeName <> '') and FindRegisteredType(TypeName, AType) then
    Exit;

  Result := 'it is not an object of a registered type (' + string(TypeName) + ')';
end;

// A JSON value from the page or the host block as a value for Prop
function JsonToValue(const Prop: _PropertyInfo; const Current: CObject; const Value: TJSONValue) : CObject;
begin
  var propType: &Type := Prop.GetType;
  if (Value = nil) or (Value is TJSONNull) then
    Result := nil
  else if Value is TJSONBool then
    Result := TJSONBool(Value).AsBoolean
  else if Value is TJSONNumber then
  begin
    if propType = &Type.From<Integer> then
      Result := Integer(Trunc(TJSONNumber(Value).AsDouble))
    else if propType = &Type.From<Int64> then
      Result := Trunc(TJSONNumber(Value).AsDouble)
    else
      Result := TJSONNumber(Value).AsDouble;
  end
  else
  begin
    var text := Value.Value;
    if (Current <> nil) and Current.IsBoolean then
      Result := SameText(text, 'true')
    else if (Current <> nil) and Current.IsNumber then
      Result := StrToFloat(text, TFormatSettings.Invariant)
    else
      Result := text;
  end;
end;

function TAppSession.CanEdit(const ObjectName, PropName: string): Boolean;
begin
  var obj: TExposedObject;
  var access: string;
  Result := FExposed.TryGetValue(ObjectName, obj) and obj.Policy.TryGetValue(PropName, access) and (access = 'edit');
end;

// (a method, so each closure captures its own names)
function TAppSession.MakeCanEdit(const ObjectName, PropName: string): TFunc<Boolean>;
begin
  Result := function: Boolean begin Result := CanEdit(ObjectName, PropName); end;
end;

procedure TAppSession.DoPush(const Channel, Json: string);
begin
  if Assigned(FPush) then
    FPush(Channel, FRedact(Json));
end;

procedure TAppSession.BindAll(Obj: TExposedObject);
begin
  var props := _app.Config.GetProperties(Obj.ValueType);
  for var prop in Obj.Policy.Keys do
  begin
    var binding := TPagePropertyBinding.Create(Obj.Name, MakeCanEdit(Obj.Name, prop), DoPush);
    var ref: IPropertyBinding := binding;
    // The same Bind the Delphi controls use. Then Link to every other property
    // of the type: the page does not know which property's editability depends
    // on which, so it re-checks on any change (the object decides).
    Obj.Context.Bind(prop, ref);
    for var other in props do
      if not SameText(other.Name, prop) then
      try
        Obj.Context.Link(string(other.Name), ref);
      except
        // a property the model cannot link (e.g. a list): not needed for editability
      end;
    Obj.Bindings.Add(prop, binding);
    Obj.BindingRefs.Add(ref);
  end;
end;

function TAppSession.Snapshot(Obj: TExposedObject): string;
begin
  var o := TJSONObject.Create;
  try
    o.AddPair('object', Obj.Name);
    o.AddPair('type', Obj.ValueType.Name);
    var props := TJSONObject.Create;
    for var pair in Obj.Bindings do
      props.AddPair(pair.Key, pair.Value.Snapshot);
    o.AddPair('properties', props);
    Result := o.ToJSON;
  finally
    o.Free;
  end;
end;

function TAppSession.Params: string;
begin
  try
    var arr := TJSONArray.Create;
    try
      for var p in FParams do
      begin
        var o := TJSONObject.Create;
        o.AddPair('name', p.Name);
        o.AddPair('type', p.ValueType.Name);
        o.AddPair('isObject', TJSONBool.Create(p.IsObject));
        if not p.IsObject then
          o.AddPair('value', ValueToJson(p.Value));
        arr.AddElement(o);
      end;
      Result := arr.ToJSON;
    finally
      arr.Free;
    end;
  except
    on E: Exception do Result := Error(E.ClassName + ': ' + E.Message);
  end;
end;

function TAppSession.Param(const Name: string): CObject;
begin
  Result := nil;
  try
    var p: TSessionParam;
    if FindParam(Name, p) then
      Result := p.Value;
  except
    Result := nil;
  end;
end;

// {"Description": "edit"} -> Policy; returns an error message or ''
function TAppSession.ParsePolicy(const AType: &Type; const PolicyJson: string; Policy: TDictionary<string, string>): string;
begin
  Result := '';
  var props := _app.Config.GetProperties(AType);
  var v := TJSONObject.ParseJSONValue(PolicyJson);
  try
    if not (v is TJSONObject) then
      Exit('the property list must be an object like { Description: ''edit'' }');
    for var pair in TJSONObject(v) do
    begin
      var propName := '';
      for var prop in props do
        if SameText(prop.Name, pair.JsonString.Value) then
          propName := prop.Name;
      if propName = '' then
      begin
        var names := '';
        for var prop in props do
          names := names + IfThen(names <> '', ', ', '') + string(prop.Name);
        Exit(Format('%s has no property "%s". Properties: %s', [AType.Name, pair.JsonString.Value, names]));
      end;
      var access := LowerCase(pair.JsonValue.Value);
      if (access <> 'read') and (access <> 'edit') then
        Exit(Format('"%s" must be ''read'' or ''edit''', [propName]));
      Policy.AddOrSetValue(propName, access);
    end;
  finally
    v.Free;
  end;
end;

function TAppSession.Expose(const Name, PolicyJson: string; const Value: CObject; const TypeName: string): string;
begin
  try
    Result := DoExpose(Name, PolicyJson, Value, TypeName);
  except
    on E: Exception do Result := Error(Format('expose(%s): %s: %s', [Name, E.ClassName, E.Message]));
  end;
end;

function TAppSession.DoExpose(const Name, PolicyJson: string; const Value: CObject; const TypeName: string): string;
begin
  if Name.Trim = '' then
    Exit(Error('expose: give the object a name, e.g. page.expose(''project'', ...)'));

  // The object: given, the one exposed under this name before, or a param
  var obj: CObject;
  var objType: &Type;
  var existing: TExposedObject := nil;
  FExposed.TryGetValue(Name, existing);
  if not Value.IsNull then
  begin
    var err := TypeOf(Value, TypeName, objType, obj);
    if err <> '' then
      Exit(Error(Format('expose(''%s''): %s', [Name, err])));
  end
  else if existing <> nil then
  begin
    obj := existing.Value;
    objType := existing.ValueType;
  end
  else
  begin
    var p: TSessionParam;
    if not FindParam(Name, p) then
      Exit(Error(Format('expose(''%s''): pass the object as the third argument (this app was not opened with a param "%s")', [Name, Name])));
    if not p.IsObject then
      Exit(Error(Format('expose(''%s''): param "%s" is a plain value, not an object', [Name, Name])));
    obj := p.Value;
    objType := p.ValueType;
  end;

  var policy := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
  try
    var err := ParsePolicy(objType, PolicyJson, policy);
    if err <> '' then
      Exit(Error(Format('expose(''%s''): %s', [Name, err])));

    if existing = nil then
    begin
      existing := TExposedObject.Create;
      existing.Name := Name;
      FExposed.Add(Name, existing);
    end;
    existing.Value := obj;
    existing.ValueType := objType;
    existing.Policy.Clear;
    for var pair in policy do
      existing.Policy.Add(pair.Key, pair.Value);
  finally
    policy.Free;
  end;

  // Already open in the page: rebind to the new object / list and send the new shape
  if existing.Context <> nil then
  begin
    existing.Unbind;
    existing.Context := TObjectModelContext.Create(ModelFor(existing.ValueType));
    existing.Context.Context := existing.Value;
    BindAll(existing);
    DoPush('object', Snapshot(existing));
  end;
  Result := '{}';
end;

function TAppSession.DoOpen(const Name: string): string;
begin
  if FLoadError <> '' then
    Exit(Error('The app failed to start: ' + FLoadError));

  // A live list: its keys; the page opens '<Name>/<key>' for each
  var keys: string;
  if FLists.TryGetValue(Name, keys) then
  begin
    FOpenLists.AddOrSetValue(Name, True);
    Exit(Format('{"object":%s,"list":true,"keys":%s}', [JsString(Name), keys]));
  end;

  var obj: TExposedObject;
  if not FExposed.TryGetValue(Name, obj) then
    Exit(Error(Format('"%s" is not exposed to the page; the host block exposes it with page.expose(''%s'', ...)', [Name, Name])));

  if obj.Context = nil then
  begin
    // A context of this page, on the shared model for the type
    obj.Context := TObjectModelContext.Create(ModelFor(obj.ValueType));
    obj.Context.Context := obj.Value;
    BindAll(obj);
  end;
  Result := Snapshot(obj);
end;

function TAppSession.DoEdit(const ObjectName, PropName: string; const Value: TJSONValue): string;
begin
  var obj: TExposedObject;
  if not FExposed.TryGetValue(ObjectName, obj) or (obj.Context = nil) then
    Exit(Error(Format('"%s" is not open', [ObjectName])));
  var binding: TPagePropertyBinding;
  if not obj.Bindings.TryGetValue(PropName, binding) then
    Exit(Error(Format('"%s.%s" is not exposed to the page', [ObjectName, PropName])));
  if not binding.IsEditable then
    Exit(Error(Format('%s cannot be edited', [string(binding.Prop.Name)])));

  var v := JsonToValue(binding.Prop, binding.Prop.GetValue(obj.Context.Context, []), Value);

  var refused := '';
  try
    binding.PageEdited(v);
  except
    on E: Exception do refused := E.Message;   // e.g. the setter refused
  end;
  ObjectsEdited;

  // The value as the model has it now (a setter may have changed or refused it)
  var s := binding.Snapshot;
  try
    if refused = '' then
      Exit(s.ToJSON);
    // the page shows what it typed: send the model value back
    s.AddPair('object', ObjectName);
    s.AddPair('property', string(binding.Prop.Name));
    DoPush('property', s.ToJSON);
    Result := Error(refused);
  finally
    s.Free;
  end;
end;

function TAppSession.Update(const Value: CObject; const TypeName: string; const ValuesJson: string): string;
begin
  try
    Result := DoUpdate(Value, TypeName, ValuesJson);
  except
    on E: Exception do Result := Error(Format('update: %s: %s', [E.ClassName, E.Message]));
  end;
end;

// Sets properties through the shared model, as an edit in a window does, so
// every window showing the object hears it
function TAppSession.DoUpdate(const Value: CObject; const TypeName, ValuesJson: string): string;
begin
  if Value.IsNull then
    Exit(Error('update: pass the object first: page.update(object, { Property: value })'));
  var objType: &Type;
  var obj: CObject;
  var err := TypeOf(Value, TypeName, objType, obj);
  if err <> '' then
    Exit(Error('update: ' + err));

  var values := TJSONObject.ParseJSONValue(ValuesJson);
  try
    if not (values is TJSONObject) then
      Exit(Error('update: the values must be an object like { Description: ''x'' }'));

    var props := _app.Config.GetProperties(objType);
    var context: IObjectModelContext := TObjectModelContext.Create(ModelFor(objType));
    context.Context := obj;
    try
      for var pair in TJSONObject(values) do
      begin
        var prop: _PropertyInfo := nil;
        for var p in props do
          if SameText(p.Name, pair.JsonString.Value) then
            prop := p;
        if prop = nil then
          Exit(Error(Format('update: %s has no property "%s"', [objType.Name, pair.JsonString.Value])));
        if not prop.CanWrite then
          Exit(Error(Format('update: %s.%s cannot be written', [objType.Name, string(prop.Name)])));

        var binding := TCallbackBinding.Create(nil);
        var ref: IPropertyBinding := binding;
        context.Bind(string(prop.Name), ref);
        binding.Edit(JsonToValue(prop, prop.GetValue(obj, []), pair.JsonValue));
      end;
    finally
      context.Unbind;
    end;
  finally
    values.Free;
  end;
  ObjectsEdited;
  Result := '{}';
end;

function TAppSession.ExposeList(const Name: string; const KeysJson: string): string;
begin
  try
    var keys := TJSONObject.ParseJSONValue(KeysJson);
    try
      if not (keys is TJSONArray) then
        Exit(Error('exposeList: the keys must be an array'));
      FLists.AddOrSetValue(Name, keys.ToJSON);
      // Open in the page: it gets the new items
      if FOpenLists.ContainsKey(Name) then
        DoPush('list', Format('{"object":%s,"keys":%s}', [JsString(Name), keys.ToJSON]));
    finally
      keys.Free;
    end;
    Result := '{}';
  except
    on E: Exception do Result := Error('exposeList: ' + E.Message);
  end;
end;

function TAppSession.SetTitle(const Title: string): string;
begin
  try
    var o := TJSONObject.Create;
    try
      o.AddPair('title', Title);
      DoPush('title', o.ToJSON);
    finally
      o.Free;
    end;
    Result := '{}';
  except
    on E: Exception do Result := Error(E.Message);
  end;
end;

function TAppSession.SetLoadError(const Msg: string): string;
begin
  FLoadError := Msg;
  Result := '{}';
end;

function TAppSession.SessionId: string;
begin
  Result := FId;
end;

function TAppSession.HandlerReady: string;
begin
  FHandlerReady := True;
  Result := '{}';
end;

function TAppSession.Started: Boolean;
begin
  Result := FHandlerReady and (FLoadError = '');
end;

function JsString(const Value: string): string;
begin
  var s := TJSONString.Create(Value);
  try
    Result := s.ToJSON;
  finally
    s.Free;
  end;
end;

function TAppSession.HandleRequest(const RequestJson: string): string;

  function Answer(const Json: string): string;
  begin
    var v := TJSONObject.ParseJSONValue(Json);
    try
      if (v is TJSONObject) and (TJSONObject(v).GetValue('error') <> nil) then
      begin
        var o := TJSONObject.Create;
        try
          o.AddPair('ok', TJSONBool.Create(False));
          o.AddPair('error', TJSONObject(v).GetValue('error').Value);
          Result := o.ToJSON;
        finally
          o.Free;
        end;
      end
      else
        Result := '{"ok":true,"result":' + Json + '}';
    finally
      v.Free;
    end;
  end;

begin
  try
    var req := TJSONObject.ParseJSONValue(RequestJson) as TJSONObject;
    try
      if req = nil then
        Exit(Answer(Error('Invalid request')));
      var op := req.GetValue<string>('op', 'run');
      if op = 'open' then
        Result := Answer(DoOpen(req.GetValue<string>('object', '')))
      else if op = 'edit' then
        Result := Answer(DoEdit(req.GetValue<string>('object', ''), req.GetValue<string>('property', ''), req.GetValue('value')))
      else if op = 'run' then
      begin
        if FLoadError <> '' then
          Exit(Answer(Error('The app failed to start: ' + FLoadError)));
        if not FHandlerReady or not Assigned(AppSessionQuery) then
          Exit(Answer(Error('The app''s host block did not start')));
        // The prelude's handler answers {ok, result} or {ok: false, error}
        var o := TJSONObject.Create;
        try
          o.AddPair('action', req.GetValue<string>('action', ''));
          var args := req.GetValue('args');
          if args <> nil then
            o.AddPair('args', args.Clone as TJSONValue)
          else
            o.AddPair('args', TJSONNull.Create);
          Result := AppSessionQuery(Format('await globalThis.__appSessionHandlers[%s](%s)', [JsString(FId), JsString(o.ToJSON)]));
          if not Result.StartsWith('{') then
            Result := Answer(Error('The action did not answer: ' + Result));
        finally
          o.Free;
        end;
      end
      else
        Result := Answer(Error('Unknown request: ' + op));
    finally
      req.Free;
    end;
  except
    on E: Exception do
      Result := Answer(Error(E.ClassName + ': ' + E.Message));
  end;
  Result := FRedact(Result);
end;

class procedure TAppSession.StorageChanged(const StorageName: string);
begin
  if (_sessions = nil) or not Assigned(AppSessionQuery) then
    Exit;
  for var session in _sessions.ToArray do
    if session.FHandlerReady then
    try
      // The prelude keeps which lists came from which storage
      AppSessionQuery(Format('globalThis.__appSessionStorageChanged?.[%s]?.(%s) ?? ""', [JsString(session.FId), JsString(StorageName)]));
    except
      // one session's failure is not the others'
    end;
end;

procedure TAppSession.Detach;
begin
  if _sessions <> nil then
    _sessions.Remove(Self);
  for var obj in FExposed.Values do
    obj.Unbind;
  FExposed.Clear;
  // The page is gone: its handler (and what it holds on to) can be collected
  if FHandlerReady and Assigned(AppSessionQuery) then
  try
    AppSessionQuery(Format('delete globalThis.__appSessionHandlers[%0:s], delete globalThis.__appSessionStorageChanged?.[%0:s]', [JsString(FId)]));
  except
  end;
  FHandlerReady := False;
  FPush := nil;
end;

class function TAppSession.ResolveObject(const Spec: string; out Param: TSessionParam) : string;
begin
  Result := '';
  // <Type>:<ID>
  var at := Spec.IndexOf(':');
  if at <= 0 then
    Exit(Format('"%s" is not <Type>:<ID>', [Spec]));
  var typeName := Spec.Substring(0, at);
  var id := Spec.Substring(at + 1);

  var t: &Type;
  if not FindRegisteredType(typeName, t) then
    Exit(Format('there is no type %s', [typeName]));

  var storage: IStorage := nil;
  var descriptor := _app.Config.TypeDescriptor(t);
  if descriptor <> nil then
  try
    var storageName: string := descriptor.StorageName;
    if storageName <> '' then
      _app.TryGetStorage(storageName, storage);
  except
    storage := nil;
  end;
  if storage = nil then
    Exit(Format('%s has no storage', [typeName]));

  var idProp: _PropertyInfo := nil;
  for var prop in _app.Config.GetProperties(t) do
    if SameText(prop.Name, 'ID') then
      idProp := prop;
  if idProp = nil then
    Exit(Format('%s has no ID', [typeName]));

  for var i := 0 to storage.Count - 1 do
    if idProp.GetValue(storage[i], []).ToString = id then
    begin
      Param.Value := storage[i];
      Param.ValueType := t;
      Exit;
    end;
  Result := Format('there is no %s with ID %s', [typeName, id]);
end;

end.
