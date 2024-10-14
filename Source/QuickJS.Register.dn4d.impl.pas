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
    function TValueToJSValue(ctx: JSContext; const Value: TValue) : JSValue; override;
  end;

  TRegisteredTypedObject = class(TRegisteredObject)
  protected
    function  GetIterator: IPropertyDescriptor; override;
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IPropertyDescriptor; override;
    function  GetMemberNames(MemberTypes: TMemberTypes) : TArray<string>; override;
  end;

  TTypedStandardPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FProp: _PropertyInfo;

    function  get_MemberType: TMemberType; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const AProp: _PropertyInfo);
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

function TRegisteredTypedObject.GetIterator: IPropertyDescriptor;
begin
  Result := TTypedIteratorDescriptor.Create(FTypeInfo);
end;

function TRegisteredTypedObject.GetMemberByName(const AName: string; MemberTypes: TMemberTypes): IPropertyDescriptor;
begin
  var handled := False;
  Result := DoOnGetMemberByName(AName, MemberTypes, handled);
  if handled then
    Exit;

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

  var tp := &Type.Create(FTypeInfo);
  if TMemberType.Properties in MemberTypes then
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
  Result := TMemberType.Properties;
end;

procedure TTypedStandardPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  inherited;

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
      var s := AnsiString(CString(Value.GetReferenceToRawData^).ToString);
      Result := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));
    end;

  end else
    Result := inherited;
end;

end.
