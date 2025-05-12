unit QuickJS.Register.dn4d.impl;

interface

uses
  System_,
  System.TypInfo,
  System.Rtti,
  quickjs,
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  QuickJS.Register.dn4d.intf,
  System.Generics.Collections, System.Collections;

type
  TJSRegisterTypedObjects = class(TJSRegister)
  const
    tkJSType = TTypeKind(Ord(tkMRecord) + 1);

  protected
    class var FJSObjectsToClassIDMapping: TDictionary<JSValueConst, JSClassID>;

  protected
    class procedure InternalRegisterType(const Reg: IRegisteredObject; ctx: JSContext; ClassName: string); override;
    function CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor) : IRegisteredObject; override;

  public
    class procedure Initialize(const Context: IJSContext);
  end;

  TJSTypedConverter = class(JSConverter)
    function JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue; override;
    function TValueToJSValue(ctx: JSContext; const Value: TValue) : JSValue; override;
  end;

  TJSObjectReference = class(TBaseInterfacedObject, IJSObjectReference)
  protected
    _ctx: JSContext;
    _object: JSValueConst;

    function IntervalInvoke(const Func: AnsiString; argc:Integer; argv: PJSValueConstArr) : JSValue;
    function Invoke(const Func: AnsiString) : CObject; overload;
    function Invoke(const Func: AnsiString; const Param: CObject) : CObject; overload;

  public
    constructor Create(ctx: JSContext; Value: JSValueConst);
    destructor Destroy; override;

    function  GetType: &Type; override;
  end;

  TRegisteredTypedObject = class(TRegisteredObject)
  protected
    function  GetArrayIndexer: IPropertyDescriptor; override;
    function  GetIterator: IPropertyDescriptor; override;
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IPropertyDescriptor; override;
    function  GetMemberNames(MemberTypes: TMemberTypes) : TArray<string>; override;

    function  get_ObjectSupportsIndexing: Boolean; override;
  end;

  // Class describing objects defined in JavaScript
  TRegisteredJSObject = class(TRegisteredObject)
  public
    destructor Destroy; override;
  end;

  TTypedStandardPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FProp: _PropertyInfo;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const AProp: _PropertyInfo);
  end;

  TTypedArrayIndexerDescriptor = class(TPropertyDescriptor)
  protected
    _innerType: PTypeInfo;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo); override;
  end;

  TTypedIteratorDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
  end;

  TJSIEnumerableIterator = class(TJSIterator)
  protected
    _enumerator: System.Collections.IEnumerator;

  public
    class function CreateIterator(const E: IEnumerable) : TJSIEnumerableIterator;

    function MoveNext: Boolean; override;
    function Current: TValue; override;
  end;

  TTypedForEachDescriptor = class(TPropertyDescriptor, IMethodsPropertyDescriptor)
  protected
    FIsInterface: Boolean;

    function  get_MemberType: TMemberType; override;
    function  Methods: TArray<TRttiMethod>;
    function  Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;

  public
    constructor Create(AInfo: PTypeInfo; IsInterface: Boolean);
  end;

implementation

uses
  System.SysUtils;

{ TJSObjectReference }

constructor TJSObjectReference.Create(ctx: JSContext; Value: JSValueConst);
begin
  _ctx := ctx;
  _object := JS_DupValue(ctx, Value);
end;

destructor TJSObjectReference.Destroy;
begin
  JS_FreeValue(_ctx, _object);
  inherited;
end;

function TJSObjectReference.GetType: &Type;
type
  JSPropertyEnumArr  = array[0..(MaxInt div SizeOf(JSPropertyEnum))-1] of JSPropertyEnum;
  PJSPropertyEnumArr = ^JSPropertyEnumArr;

begin
  var classID: JSClassID;
  if not TJSRegisterTypedObjects.FJSObjectsToClassIDMapping.TryGetValue(_object, classID) then
  begin
    var tp: PTypeInfo := New(PTypeInfo);
    tp.Kind := TJSRegisterTypedObjects.tkJSType;
    tp.Name := 'Customer'; // v.ToString; // proto.constructor.name

    TJSRegister.RegisterObject(_ctx, 'Customer', tp, nil);

    TJSRegisterTypedObjects.FJSObjectsToClassIDMapping.Add(_object, classID);
  end;


//  var p_enum: PJSPropertyEnum;
//  var p_len: UInt32;
//  var r := JS_GetOwnPropertyNames(_ctx, @p_enum, @p_len, _object, JS_PROP_C_W_E);
//
//  var class_id := TJSRegister.GetClassID(_object);
//
//  for var i := 0 to p_len -1 do
//    var atom_str := JS_AtomToCString(_ctx, PJSPropertyEnumArr(p_enum)[i].atom);
//
//  js_free(_ctx, p_enum);

//  var classID := TJSRegister.GetClassID(_object);
//  var reg: IRegisteredObject;
//  if not TJSRegister.TryGetRegisteredObjectFromClassID(ClassID, reg) then
//  begin
//    var v := JSConverter.Instance.JSValueToTValue(_ctx, IntervalInvoke('GetType', 0, nil), TypeInfo(string));
//
//    var tp: PTypeInfo := New(PTypeInfo);
//    tp.Kind := TJSRegisterTypedObjects.tkJSType;
//    tp.Name := v.ToString;
//
//    reg := TJSRegister.RegisterObject(_ctx, v.ToString, tp);
//    reg.ClassID := classID;
//    TJSRegister.AddRegisteredObjectWithClassID(classID, reg);
//  end;
end;

function TJSObjectReference.IntervalInvoke(const Func: AnsiString; argc:Integer; argv: PJSValueConstArr) : JSValue;
begin
  var atom := JS_NewAtom(_ctx, PAnsiChar(Func));
  try
    var prop := JS_GetProperty(_ctx, _object, atom);
    if not TJSRuntime.Check(_ctx, prop) then Exit;

    if JS_IsFunction(_ctx, prop) then
    begin
      Result := JS_Call(_ctx, prop, _object, 0 {argc}, nil {PJSValueConstArr(argv)});
      if not TJSRuntime.Check(_ctx, prop) then Exit;
    end;
  finally
    JS_FreeAtom(_ctx, atom);
  end;
end;

function TJSObjectReference.Invoke(const Func: AnsiString; const Param: CObject): CObject;
begin

end;

function TJSObjectReference.Invoke(const Func: AnsiString) : CObject;
begin
  var js_type := IntervalInvoke(Func, 0, nil);
end;

{ TJSRegisterTypedObjects }

class procedure TJSRegisterTypedObjects.Initialize(const Context: IJSContext);
begin
  FJSObjectsToClassIDMapping := TDictionary<JSValueConst, JSClassID>.Create;

  JSConverter.Instance := TJSTypedConverter.Create;
  TJSRegister.Instance := TJSRegisterTypedObjects.Create;

  TJSRegister.RegisterObject(Context.ctx, 'JSIEnumerableIterator', TypeInfo(TJSIEnumerableIterator));
end;

class procedure TJSRegisterTypedObjects.InternalRegisterType(const Reg: IRegisteredObject; ctx: JSContext; ClassName: string);
begin
  // Registration for JS types skipped
  if Reg.GetTypeInfo.Kind <> TJSRegisterTypedObjects.tkJSType then
    inherited;
end;

function TJSRegisterTypedObjects.CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject;
begin
  // Type defined in JS engine?
  if ATypeInfo.Kind = TJSRegisterTypedObjects.tkJSType then
  begin
    Result := TRegisteredJSObject.Create(ATypeInfo, AConstructor);
    Result.ClassID := 10000;
  end else
    Result := TRegisteredTypedObject.Create(ATypeInfo, AConstructor);
end;

{ JSIEnumerableIterator }

class function TJSIEnumerableIterator.CreateIterator(const E: IEnumerable): TJSIEnumerableIterator;
begin
  Result := nil;

  var enum := E.GetEnumerator;
  if enum <> nil then
  begin
    Result := TJSIEnumerableIterator.Create;
    Result._enumerator := enum;
  end;
end;

function TJSIEnumerableIterator.Current: TValue;
begin
  Result := _enumerator.Current.AsType<TValue>;
end;

function TJSIEnumerableIterator.MoveNext: Boolean;
begin
  Result := _enumerator.MoveNext;
end;

{ TRegisteredTypedObject }

function TRegisteredTypedObject.GetArrayIndexer: IPropertyDescriptor;
begin
  if FTypeInfo.Kind = tkInterface then // Like IList or List
    Result := TTypedArrayIndexerDescriptor.Create(FTypeInfo) else
    Result := inherited;
end;

function TRegisteredTypedObject.GetIterator: IPropertyDescriptor;
begin
  Result := TTypedIteratorDescriptor.Create(FTypeInfo);
end;

function TRegisteredTypedObject.GetMemberByName(const AName: string; MemberTypes: TMemberTypes): IPropertyDescriptor;
begin
  var handled := False;
  Result := DoOnGetMemberByName(AName, MemberTypes, handled);
  if handled then Exit;

  if AName = 'Symbol.iterator' then
  begin
    Result := inherited;
    Exit;
  end;

  if (AName = 'next') and get_IsIterator then
  begin
    Result := inherited;
    Exit;
  end;

  if AName = 'forEach' then
  begin
    if get_ObjectSupportsEnumeration then
    begin
      Result := TTypedForEachDescriptor.Create(FTypeInfo, True);
      Exit;
    end;
  end;

  var tp := &Type.Create(FTypeInfo);
  if TMemberType.Property in MemberTypes then
  begin
    var prop := tp.PropertyByName(AName);
    if prop <> nil then
    begin
      Result := TTypedStandardPropertyDescriptor.Create(FTypeInfo, prop);
      Exit;
    end;
  end;

  Result := inherited GetMemberByName(AName, [TMemberType.Methods]);
end;

function TRegisteredTypedObject.GetMemberNames(MemberTypes: TMemberTypes): TArray<string>;
begin
  var handled := False;
  Result := DoOnGetMemberNames(MemberTypes, handled);
  if handled then Exit;

  var tp := &Type.Create(FTypeInfo);
  var methods := tp.GetMethods;
  var properties := tp.GetProperties;
  var i := 0;

  SetLength(Result, Length(methods) + Length(properties));

  var m: TRttiMethod;
  for m in methods do
  begin
    Result[i] := m.Name;
    inc(i);
  end;

  var p: _PropertyInfo;
  for p in properties do
  begin
    Result[i] := p.Name;
    inc(i);
  end;
end;

function TRegisteredTypedObject.get_ObjectSupportsIndexing: Boolean;
begin
  Result := inherited;
end;

{ TTypedStandardPropertyDescriptor }

constructor TTypedStandardPropertyDescriptor.Create(AInfo: PTypeInfo; const AProp: _PropertyInfo);
begin
  FTypeInfo := AInfo;
  FProp := AProp;
end;

function TTypedStandardPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  var vt: TValue;

  if FTypeInfo.Kind = tkInterface then
    vt := TValue.From<IInterface>(IInterface(Ptr)) else
    vt := TValue.From<TObject>(TObject(Ptr));

  Result := FProp.GetValue(CObject.From<TValue>(vt), []).GetValue<TValue>;
end;

function TTypedStandardPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

function TTypedStandardPropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := FProp.PropInfo.PropType;
end;

procedure TTypedStandardPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  var vt: TValue;

  if FTypeInfo.Kind = tkInterface then
    vt := TValue.From<IInterface>(IInterface(Ptr)) else
    vt := TValue.From<TObject>(TObject(Ptr));

  FProp.SetValue(CObject.From<TValue>(vt), Value, []);
end;

{ TTypedIteratorDescriptor }

function TTypedIteratorDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  if FTypeInfo.Kind = tkInterface then
  begin
    var e: IEnumerable;
    if Interfaces.Supports<IEnumerable>(IInterface(ptr), e) then
      Result := TJSIEnumerableIterator.CreateIterator(e);
  end else
    Result := inherited;
end;

function TTypedIteratorDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Iterator;
end;

{ TJSTypedConverter }

function TJSTypedConverter.JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue;
begin
  if Target.Kind = tkInterface then
  begin
    if JS_IsObject(Value) then
    begin
      var ptr := TJSRegister.GetObjectFromJSValue(Value, False {Is NOT object type?});
      if ptr = nil then
      begin
        var obj_ref: IJSObjectReference := TJSObjectReference.Create(ctx, Value);
        Exit(TValue.From<IJSObjectReference>(obj_ref));
      end;
    end;

    Result := inherited;
  end
  else if (Target.Kind = tkRecord) and (Target = TypeInfo(CObject)) then
  begin
    if JS_IsBool(Value) then
      Result := JS_ToBool(ctx, Value) <> 0

    else if JS_IsString(Value) then
    begin
      var str: PAnsiChar := JS_ToCString(ctx, Value);
      Result := string(str);
      JS_FreeCString(ctx, str);
    end

    else if JS_IsNumber(Value) then
    begin
      var v: Integer;
      JS_ToInt32(ctx, @v, Value);
      Result := v;
    end

    else if JS_IsObject(Value) then
    begin
      var cid := TJSRegister.GetClassID(Value);
      var reg: IRegisteredObject;
      var isObjectType := True;
      if TJSRegister.TryGetRegisteredObjectFromClassID(cid, reg) then
        isObjectType := not reg.IsInterface;
      var ptr := TJSRegister.GetObjectFromJSValue(Value, isObjectType);
      if ptr <> nil then
      begin
        if isObjectType then
          Result := TValue.From<TObject>(ptr) else
          Result := TValue.From<IInterface>(IInterface(ptr));
      end;
    end
  end else
    Result := inherited;
end;

function TJSTypedConverter.TValueToJSValue(ctx: JSContext; const Value: TValue): JSValue;
begin
  Result := JS_NULL;

  if Value.Kind = tkRecord then
  begin
    Assert(Value.TypeInfo <> TypeInfo(CObject));

    if Value.TypeInfo = TypeInfo(CString) then
    begin
      var s := AnsiString(CString(Value.GetReferenceToRawData^).ToString);
      Result := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));
    end
    else if Value.TypeInfo = TypeInfo(CDateTime) then
    begin
      var dt := CDateTime(Value.GetReferenceToRawData^).ToUniversalTime;
      var epoch := CDateTime.Create(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
      var ts := (dt - epoch).TotalMilliSeconds;
      Result := TJSRegister.JS_NewDate(ctx, ts);
    end;

  end else
    Result := inherited;
end;

{ TTypedArrayIndexerDescriptor }

constructor TTypedArrayIndexerDescriptor.Create(AInfo: PTypeInfo);
begin
  inherited;
end;

function TTypedArrayIndexerDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  var l: IList;

  if (FTypeInfo.Kind = tkInterface) and Interfaces.Supports<IList>(IInterface(Ptr), l) then
    Result := l[Index[0].AsInteger].AsType<TValue> else
    Result := inherited;
end;

procedure TTypedArrayIndexerDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  var l: IList;

  if (FTypeInfo.Kind = tkInterface) and Interfaces.Supports<IList>(IInterface(Ptr), l) then
    l[Index[0].AsInteger] := Value else
    inherited;
end;

function TTypedArrayIndexerDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.ArrayIndexer;
end;

function TTypedArrayIndexerDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := TypeInfo(CObject);
end;

{ TTypedForEachDescriptor }

function TTypedForEachDescriptor.Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
begin
  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');

  var e: IEnumerable;
  if Interfaces.Supports<IEnumerable>(IInterface(Ptr), e) and JS_IsFunction(ctx, PJSValueConstArr(argv)[0]) then
  begin
    var func := PJSValueConstArr(argv)[0];
    var enum := e.GetEnumerator;
    while enum.MoveNext do
    begin
      var target: JSValue := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
      var call_argv: array of JSValueConst;
      SetLength(call_argv, 1);
      call_argv[0] := target;
      Result := JS_Call(ctx, func, JS_Null, argc, PJSValueConstArr(call_argv));
      JS_FreeValue(ctx, call_argv[0]);
    end;
  end;
end;

constructor TTypedForEachDescriptor.Create(AInfo: PTypeInfo; IsInterface: Boolean);
begin
  FTypeInfo := AInfo;
  FIsInterface := IsInterface;
end;

function TTypedForEachDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Methods;
end;

function TTypedForEachDescriptor.Methods: TArray<TRttiMethod>;
begin
  Result := nil;
end;

{ TRegisteredJSObject }

destructor TRegisteredJSObject.Destroy;
begin
  Dispose(FTypeInfo);
  inherited;
end;

end.
