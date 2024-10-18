unit QuickJS.Register.dn4d.impl;

interface

uses
  System.TypInfo,
  quickjs,
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  System.Collections,
  System.Collections.Generic, System.Rtti, System_;

type
  TJSRegisterTypedObjects = class(TJSRegister)
  protected
    function CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor) : IRegisteredObject; override;

  public
    class procedure Initialize(const Context: IJSContext);
  end;

  TJSTypedConverter = class(JSConverter)
    function JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue; override;
    function TValueToJSValue(ctx: JSContext; const Value: TValue) : JSValue; override;
  end;

  TRegisteredTypedObject = class(TRegisteredObject)
  protected
    function  GetArrayIndexer: IPropertyDescriptor; override;
    function  GetIterator: IPropertyDescriptor; override;
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IPropertyDescriptor; override;
    function  GetMemberNames(MemberTypes: TMemberTypes) : TArray<string>; override;

    function  get_ObjectSupportsIndexing: Boolean; override;
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

{ TJSRegisterTypedObjects }


{ TJSRegisterTypedObjects }

class procedure TJSRegisterTypedObjects.Initialize(const Context: IJSContext);
begin
  JSConverter.Instance := TJSTypedConverter.Create;
  TJSRegister.Instance := TJSRegisterTypedObjects.Create;

  TJSRegister.RegisterObject(Context.ctx, 'JSIEnumerableIterator', TypeInfo(TJSIEnumerableIterator));
end;

function TJSRegisterTypedObjects.CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject;
begin
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
  if (Target.Kind = tkRecord) and (Target = TypeInfo(CObject)) then
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
  if Interfaces.Supports<IEnumerable>(IInterface(Ptr), e) and JS_IsFunction(ctx, PJSValueConstArray(argv)[0]) then
  begin
    var func := PJSValueConstArray(argv)[0];
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

end.
