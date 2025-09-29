unit QuickJS.Register.dn4d.impl;

interface

uses
  System_,
  System.TypInfo,
  System.Rtti,
  quickjs_ng,
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  QuickJS.Register.dn4d.intf,
  System.Generics.Collections, System.Collections, System.Collections.Generic,
  QuickJS.Register.PropertyDescriptors.intf,
  QuickJS.Register.PropertyDescriptors.impl;

type
  TJSRegisterTypedObjects = class(TJSRegister)
  const
    tkJSType = TTypeKind(Ord(tkMRecord) + 1);

  protected
    function CreateRegisteredJSObject(ctx: JSContext; JSConstructor: JSValueConst; ATypeInfo: PTypeInfo) : IRegisteredObject; override;
    function CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject; override;

  public
    class procedure Initialize(const Context: IJSContext);
  end;

  TJSTypedConverter = class(JSConverter)
    class function GetTypeFromJSObject(ctx: JSContext; Value: JSValueConst): &Type;

    function JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue; override;
    function TValueToJSValue(ctx: JSContext; const Value: TValue) : JSValue; override;
    function TestParamsAreCompatible(ctx: JSContext; const Param: TRttiParameter; Value: JSValue; out ParamIsGenericValue: Boolean) : Boolean; override;
  end;

  TJSPropertyInfo = class(TBaseInterfacedObject, _PropertyInfo)
  protected
    _ctx: JSContext;
    _ownerType: &Type;
    _name: AnsiString;

    function  get_CanRead: Boolean;
    function  get_CanWrite: Boolean;
    function  get_Name: CString;
    function  get_OwnerType: &Type;
    function  get_PropInfo: IPropInfo;

    function  GetType: &Type; override;
    function  IsIndexedProperty: Boolean;
    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);
  public
    constructor Create(Ctx: JSContext; const AOwnerType: &Type; Name: AnsiString);

    function ToString: CString; override;
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
  TRegisteredJSObject = class(TRegisteredObject, IJSRegisteredObject)
  protected
    _ctx: JSContext;
    _JSConstructor: JSValueConst;

    function GetProperties : PropertyInfoArray;

  public
    constructor Create(Ctx: JSContext; JSConstructor: JSValueConst; ATypeInfo: PTypeInfo);
    destructor Destroy; override;

    function GetType : &Type;
  end;

  TTypedStandardPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FProp: _PropertyInfo;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const AProp: _PropertyInfo); reintroduce;
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
    destructor Destroy; override;
    class function CreateIterator(const E: IEnumerable) : TJSIEnumerableIterator;

    function MoveNext: Boolean; override;
    function Current: TValue; override;
  end;

  

implementation

uses
  System.SysUtils;

{ TJSRegisterTypedObjects }

class procedure TJSRegisterTypedObjects.Initialize(const Context: IJSContext);
begin
  JSConverter.Instance := TJSTypedConverter.Create;
  TJSRegister.Instance := TJSRegisterTypedObjects.Create;

  TJSRegister.RegisterObject(Context, 'JSIEnumerableIterator', TypeInfo(TJSIEnumerableIterator));
end;

function TJSRegisterTypedObjects.CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject;
begin
  Result := TRegisteredTypedObject.Create(ATypeInfo, AConstructor);
end;

function TJSRegisterTypedObjects.CreateRegisteredJSObject(ctx: JSContext; JSConstructor: JSValueConst; ATypeInfo: PTypeInfo) : IRegisteredObject;
begin
  Result := TRegisteredJSObject.Create(ctx, JSConstructor, ATypeInfo);
end;

{ JSIEnumerableIterator }
destructor TJSIEnumerableIterator.Destroy;
begin
  inherited;
end;

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

  if (FTypeInfo.Kind in [tkClass, tkInterface]) and (TMemberType.Property in MemberTypes) then
  begin
    var tp := &Type.Create(FTypeInfo);
    var prop := tp.PropertyByName(AName);
    if prop <> nil then
    begin
      Result := TTypedStandardPropertyDescriptor.Create(FTypeInfo, prop);
      Exit;
    end;
  end;

  Result := inherited GetMemberByName(AName, MemberTypes);
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

  var prop_index: array of CObject;
  if Length(Index) > 0 then
  begin
    SetLength(prop_index, Length(Index));
    for var n := 0 to High(Index) do
      prop_index[n] := CObject.From<TValue>(Index[n]);
  end;

  Result := FProp.GetValue(CObject.From<TValue>(vt), prop_index).GetValue<TValue>;
end;

function TTypedStandardPropertyDescriptor.get_MemberType: TMemberType;
begin
  if FProp.IsIndexedProperty then
    Result := TMemberType.IndexedProperty else
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
class function TJSTypedConverter.GetTypeFromJSObject(ctx: JSContext; Value: JSValueConst): &Type;
begin
  var reg: IRegisteredObject;

  // Did we get a constructor?
  if JS_IsConstructor(ctx, Value) then
  begin
    // Registered Delphi object?
    if TJSRegister.TryGetRegisteredObjectFromConstructor(Value, reg) then
      Exit(&Type.Create(reg.GetTypeInfo));

    // Registered JS Object?
    if not TJSRegister.TryGetRegisteredJSObject(Value, reg) then
    begin
      var n := JS_GetPropertyStr(ctx, Value, 'name');
      var name := JS_ToCString(ctx, n);

      var tp: PTypeInfo := New(PTypeInfo);
      tp.Kind := TJSRegisterTypedObjects.tkJSType;
      tp.Name := name;

      reg := TJSRegister.RegisterJSObject(TJSRuntime.Context[ctx], Value, tp);

      JS_FreeCString(ctx, name);
      JS_FreeValue(ctx, n);
    end;

    var js_reg: IJSRegisteredObject;
    if Interfaces.Supports<IJSRegisteredObject>(reg, js_reg) then
      Result := js_reg.GetType;
  end else
    Result := &Type.Unknown;
end;

function TJSTypedConverter.JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue;
begin
  if Target = nil then
  begin
    if JS_IsUndefined(Value) then
      Result := TValue.Empty

    else if JS_IsNull(Value) then
      Result := TValue.From<IInterface>(nil)

    else if JS_IsBool(Value) then
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

    else if JS_IsBigInt(Value) then
    begin
      var v: Int64;
      JS_ToBigInt64(ctx, @v, Value);
      Result := v;
    end

//    else if JS_IsBigFloat(Value) then
//    begin
//      var v: Double;
//      JS_ToFloat64(ctx, @v, Value);
//      Result := v;
//    end

    else if JS_IsObject(Value) then
    begin
      var ptr := TJSRegister.GetObjectFromJSValue(Value, False {Is NOT object type?});

      if ptr <> nil {Object points to a Delphi object} then
        TValue.Make(@ptr, Target, Result) else
        // Result := TValue.From<JSObjectReference>(JSObjectReference.Create(ctx, Value));
        Result := TValue.From<IJSObject>(TJSObject.Create(ctx, Value));
    end;

    Exit;
  end;

  if Target.Kind = tkInterface then
  begin
    if JS_IsNull(Value) then
      // Exit(TValue.From<JSObjectReference>(JSObjectReference.Empty));
      Exit(TValue.From<IJSObject>(nil));

    if JS_IsFunction(ctx, Value) then
      Exit(inherited);

    if JS_IsObject(Value) then
    begin
      var obj: TRegisteredObjectWithPtr;
      if TJSRegister.TryGetRegisteredObjectFromJSValue(Value, {out}obj) then
      begin
        if (Target <> obj.Reg.GetTypeInfo) and not Interfaces.Supports(IInterface(obj.Ptr), Target.TypeData.GUID, obj.Ptr) then
          raise ArgumentException.Create('Interface not supported: ' + Target.Name);

        TValue.Make(@obj.Ptr, Target, Result);

        {$IFDEF DEBUG}
        if Target = TypeInfo(IList) then
        begin
          var l := Result.AsType<IList>;
          var c := l.Count;
          if c = 0 then;
        end;
        {$ENDIF}
      end
      else
      begin
        var obj_ref: IJSObject := TJSObject.Create(ctx, Value);

        if Target = TypeInfo(IJSObject) then
          Result := TValue.From<IJSObject>(obj_ref) else
          Result := WrapIJSObjectInVirtualInterface(Target, obj_ref);
      end;

      Exit;
    end;

    Result := inherited;
  end
  else if Target.Kind = tkRecord then
  begin
    if Target = TypeInfo(CString) then
    begin
      if JS_IsNull(Value) then
        Result := TValue.From<CString>(nil)

      else if JS_IsString(Value) then
      begin
        var str: PAnsiChar := JS_ToCString(ctx, Value);
        Result := TValue.From<CString>(string(str));
        JS_FreeCString(ctx, str);
      end

      else if JS_IsBool(Value) then
      begin
        if JS_ToBool(ctx, Value) <> 0 then
          Result := TValue.From<CString>('1') else
          Result := TValue.From<CString>('0');
      end

      else if JS_IsNumber(Value) then
      begin
        var v: Integer;
        JS_ToInt32(ctx, @v, Value);
        Result := TValue.From<CString>(v.ToString);
      end
    end

    else if Target = TypeInfo(CObject) then
    begin
      if JS_IsNull(Value) then
        Result := TValue.From<CObject>(CObject.Create(nil))

      else if JS_IsBool(Value) then
        Result := TValue.From<CObject>(JS_ToBool(ctx, Value) <> 0)

      else if JS_IsString(Value) then
      begin
        var str: PAnsiChar := JS_ToCString(ctx, Value);
        Result := TValue.From<CObject>(string(str));
        JS_FreeCString(ctx, str);
      end

      else if JS_IsNumber(Value) then
      begin
        var v: Integer;
        JS_ToInt32(ctx, @v, Value);
        Result := TValue.From<CObject>(v);
      end

      else if JS_IsBigInt(Value) then
      begin
        var v: Int64;
        JS_ToBigInt64(ctx, @v, Value);
        Result := TValue.From<CObject>(v);
      end

      else if JS_IsObject(Value) then
      begin
        var obj: TRegisteredObjectWithPtr;
        if TJSRegister.TryGetRegisteredObjectFromJSValue(Value, {out}obj) then
        begin
          var v: TValue;
          if obj.Reg.Kind in [tkInterface, tkClass] then
            TValue.Make(@obj.Ptr, obj.Reg.GetTypeInfo, v) else
            // Value types (aka Records)
            TValue.Make(obj.Ptr, obj.Reg.GetTypeInfo, v);
          Result := TValue.From<CObject>(CObject.From<TValue>(v));
        end else
          Result := TValue.From<CObject>(CObject.From<IJSObject>(TJSObject.Create(ctx, Value)));
          // Result := TValue.From<CObject>(CObject.From<JSObjectReference>(JSObjectReference.Create(ctx, Value)));
      end else
        Result := TValue.From<CObject>(CObject.Create(nil))
    end
    else if Target = TypeInfo(CDateTime) then
    begin
      if JS_IsObject(Value) then
      begin
        // Check if it's a Date object by checking constructor name
        var constructor_val := JS_GetPropertyStr(ctx, Value, 'constructor');
        var name_val := JS_GetPropertyStr(ctx, constructor_val, 'name');
        var name_str := JS_ToCString(ctx, name_val);
        var is_date := string(name_str) = 'Date';
        JS_FreeCString(ctx, name_str);
        JS_FreeValue(ctx, name_val);
        JS_FreeValue(ctx, constructor_val);
        
        if is_date then
        begin
          // Call getTime() method to get milliseconds since epoch
          var getTime := JS_GetPropertyStr(ctx, Value, 'getTime');
          var time_val := JS_Call(ctx, getTime, Value, 0, nil);
          var u_milis: Double;
          JS_ToFloat64(ctx, @u_milis, time_val);
          var ticks := DateTimeOffset.FromUnixTimeMiliSeconds(Trunc(u_milis));
          Result := TValue.From<CDateTime>(CDateTime.Create(ticks));
          JS_FreeValue(ctx, time_val);
          JS_FreeValue(ctx, getTime);
        end
        else
          Result := TValue.From<CDateTime>(CDateTime.Create(0));
      end
      else
        Result := TValue.From<CDateTime>(CDateTime.Create(0));
    end
    else if Target = TypeInfo(&Type) then
      Result := TValue.From<&Type>(GetTypeFromJSObject(ctx, Value))
    else
    begin
      // Generic record unwrap: if JS value is a registered record wrapper, unwrap to record pointer
      if JS_IsObject(Value) then
      begin
        var recPtr := TJSRegister.GetObjectFromJSValue(Value, True {PointerIsAnObject to unwrap TRecordReference});
        if recPtr <> nil then
        begin
          TValue.Make(recPtr, Target, Result);
          Exit;
        end;
      end;
      Result := inherited;
    end;
  end else
    Result := inherited;
end;

function TJSTypedConverter.TValueToJSValue(ctx: JSContext; const Value: TValue): JSValue;
begin
//  if Value.Kind = tkInterface then
//  begin
//    var obj_ref: JSObjectReference;
//    if Value.TryAsType<JSObjectReference>(obj_ref) then
//      Exit(JS_DupValue(ctx, obj_ref.Value));
//  end else
  if Value.Kind = tkRecord then
  begin
//    if Value.TypeInfo = TypeInfo(JSObjectReference) then
//      Exit(JS_DupValue(ctx, JSObjectReference(Value.GetReferenceToRawData^).Value));

    if Value.TypeInfo = TypeInfo(CObject) then
      Exit(TValueToJSValue(ctx, CObject(Value.GetReferenceToRawData^).AsType<TValue>));

    if Value.TypeInfo = TypeInfo(CString) then
    begin
      var cs := CString(Value.GetReferenceToRawData^);
      if CString.IsNullOrEmpty(cs) then
        Exit(JS_NULL);

      var s := AnsiString(cs.ToString);
      Exit(JS_NewStringLen(ctx, PAnsiChar(s), Length(s)));
    end;

    if Value.TypeInfo = TypeInfo(CDateTime) then
    begin
      var u_milis := DateTimeOffset.ToUnixTimeMiliSeconds(CDateTime(Value.GetReferenceToRawData^).Ticks);
      Exit(TJSRegister.JS_NewDate(ctx, u_milis));
    end;

    if Value.TypeInfo = TypeInfo(&Type) then
    begin
      var reg: IRegisteredObject;
      if TJSRegister.TryGetRegisteredObjectFromTypeInfo(Value.AsType<&Type>.GetTypeInfo, reg) and not JS_IsUndefined(reg.JSConstructor) then
        Exit(JS_DupValue(ctx, reg.JSConstructor));
    end;
  end
  else if Value.Kind = tkInterface then
  begin
    var ref: IJSObject;
    if Interfaces.Supports<IJSObject>(Value.AsInterface, ref) then
      Exit(JS_DupValue(ref.Ctx, ref.Value));

//    var ref: IJSObjectReference;
//    if Interfaces.Supports<IJSObjectReference>(Value.AsInterface, ref) then
//      Exit(JS_DupValue(ref.GetReference.Ctx, ref.GetReference.Value));
  end;

  Result := inherited;
end;

function TJSTypedConverter.TestParamsAreCompatible(ctx: JSContext; const Param: TRttiParameter; Value: JSValue; out ParamIsGenericValue: Boolean) : Boolean;
begin
  if (Param.ParamType.TypeKind = tkRecord) and (Param.Handle = TypeInfo(CObject)) then
  begin
    ParamIsGenericValue := True;
    Result := True;
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


{ TRegisteredJSObject }

constructor TRegisteredJSObject.Create(Ctx: JSContext; JSConstructor: JSValueConst; ATypeInfo: PTypeInfo);
begin
  inherited Create(ATypeInfo, nil);
  _ctx := Ctx;
  _JSConstructor := JS_DupValue(Ctx, JSConstructor);
end;

destructor TRegisteredJSObject.Destroy;
begin
  Dispose(FTypeInfo);
  JS_FreeValue(_ctx, _JSConstructor);
  inherited;
end;

function TRegisteredJSObject.GetProperties: PropertyInfoArray;
type
  JSPropertyEnumArr  = array[0..(MaxInt div SizeOf(JSPropertyEnum))-1] of JSPropertyEnum;
  PJSPropertyEnumArr = ^JSPropertyEnumArr;

begin
  {$IFDEF DEBUG}
  var s := TJSRegister.Describe(_ctx, _JSConstructor);
  {$ENDIF}

  var proto := JS_GetPropertyStr(_ctx, _JSConstructor, 'prototype');

  var p_enum: PJSPropertyEnum := nil;
  var p_len: UInt32;
  JS_GetOwnPropertyNames(_ctx, @p_enum, @p_len, proto, JS_PROP_C_W_E);

  if p_len > 0 then
  begin
    var ownerType := &Type.Create(FTypeInfo);

    for var i := 0 to p_len -1 do
    begin
      var jv := JS_AtomToString(_ctx, PJSPropertyEnumArr(p_enum)[i].atom);
      var ansistr := JS_ToCString(_ctx, jv);
      var jsPropType := JS_GetPropertyStr(_ctx, proto, ansistr);

      // Filter out 'real' properties ==> prototype.PropertyName returns 'undefined'
      if JS_IsUndefined(jsPropType) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TJSPropertyInfo.Create(_ctx, ownerType, ansistr);
      end;

      JS_FreeValue(_ctx, jsPropType);
      JS_FreeCString(_ctx, ansistr);
      JS_FreeValue(_ctx, jv);
    end;
  end;

  js_free(_ctx, p_enum);
  JS_FreeValue(_ctx, proto);
end;

function TRegisteredJSObject.GetType: &Type;
begin
  Result := &Type.Create(FTypeInfo);
  Result.GetPropertiesExternal := GetProperties;
end;

{ TJSPropertyInfo }

constructor TJSPropertyInfo.Create(Ctx: JSContext; const AOwnerType: &Type; Name: AnsiString);
begin
  _ctx := Ctx;
  _ownerType := AOwnerType;
  _name := Name;
end;

function TJSPropertyInfo.GetAttributes: TArray<TCustomAttribute>;
begin
  Result := nil;
end;

function TJSPropertyInfo.GetType: &Type;
begin
  Result := &Type.Unknown;
end;

function TJSPropertyInfo.GetValue(const obj: CObject; const index: array of CObject): CObject;
begin
  var js_obj: IJSObject;
  if obj.TryAsType<IJSObject>(js_obj) then
  begin
    var val := JS_GetPropertyStr(_ctx, js_obj.Value, PAnsiChar(_name));
    if not TJSRuntime.Check(_ctx) then Exit;
    Result := JSConverter.Instance.JSValueToTValue(_ctx, val, nil);
  end;

//  var js_obj: JSObjectReference;
//  if obj.TryAsType<JSObjectReference>(js_obj) then
//  begin
//    var val := JS_GetPropertyStr(_ctx, js_obj.Value, PAnsiChar(_name));
//    if not TJSRuntime.Check(_ctx) then Exit;
//    Result := JSConverter.Instance.JSValueToTValue(_ctx, val, nil);
//  end;
end;

function TJSPropertyInfo.get_CanRead: Boolean;
begin
  Result := True;
end;

function TJSPropertyInfo.get_CanWrite: Boolean;
begin
  Result := True;
end;

function TJSPropertyInfo.get_Name: CString;
begin
  Result := _name;
end;

function TJSPropertyInfo.get_OwnerType: &Type;
begin
  Result := _ownerType;
end;

function TJSPropertyInfo.get_PropInfo: IPropInfo;
begin
  Result := nil;
end;

function TJSPropertyInfo.IsIndexedProperty: Boolean;
begin
  Result := False;
end;

procedure TJSPropertyInfo.SetValue(const obj, value: CObject; const index: array of CObject; ExecuteTriggers: Boolean);
begin
  var js_obj: IJSObject;
  if obj.TryAsType<IJSObject>(js_obj) then
  begin
    JS_SetPropertyStr(_ctx, js_obj.Value, PAnsiChar(_name), JSConverter.Instance.TValueToJSValue(_ctx, value.AsType<TValue>));
    TJSRuntime.Check(_ctx);
  end;

//  var js_obj: JSObjectReference;
//  if obj.TryAsType<JSObjectReference>(js_obj) then
//  begin
//    JS_SetPropertyStr(_ctx, js_obj.Value, PAnsiChar(_name), JSConverter.Instance.TValueToJSValue(_ctx, value.AsType<TValue>));
//    TJSRuntime.Check(_ctx);
//  end;
end;

function TJSPropertyInfo.ToString: CString;
begin
  Result := string(_name);
end;

end.

