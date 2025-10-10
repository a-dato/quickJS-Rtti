unit QuickJS.Register.PropertyDescriptors.impl;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  quickjs_ng,
  QuickJS.Register.PropertyDescriptors.intf, QuickJS.Register.intf;

type
  TJSIterator = class abstract
  public
    function MoveNext: Boolean; virtual; abstract;
    function Current: TValue; virtual; abstract;
  end;

  TJSObjectIterator = class(TJSIterator)
  protected
    _instance: TObject;
    _move_next: PObjectMember;
    _get_current: PObjectMember;

  public
    destructor Destroy; override;
    class function CreateIterator(const Obj: TObject) : TJSIterator;

    function MoveNext: Boolean; override;
    function Current: TValue; override;
  end;

  // TPropertyDescriptor base class now lives in QuickJS.Register.PropertyDescriptors.intf

  TRttiStandardPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FProp: TRttiMember;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const RttiProp: TRttiMember); reintroduce;
  end;

  TRttiMethodPropertyDescriptor = class(TPropertyDescriptor, IMethodsPropertyDescriptor)
  protected
    FIsInterface: Boolean;
    FMethods: TArray<TRttiMethod>; // Multiple methods can have the same name

    function  get_MemberType: TMemberType; override;
    function  Methods: TArray<TRttiMethod>;
    function  Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue; virtual;

    function  SelectMethodMatchArguments(ctx: JSContext; argc: Integer; argv: PJSValueConst): TRttiMethod;
  public
    constructor Create(AInfo: PTypeInfo; const RttiMethods: TArray<TRttiMethod>; IsInterface: Boolean); reintroduce;
  end;

  TRttiAccessorPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FGetter: TRttiMethod;
    FSetter: TRttiMethod;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const RttiGetter: TRttiMethod; const RttiSetter: TRttiMethod); reintroduce;
  end;

  TRttiIndexedAccessorPropertyDescriptor = class(TRttiAccessorPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
  end;

  TRttiArrayIndexDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo); reintroduce;
  end;

  TRttiIteratorDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
  end;

  TRttiIteratorNextDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
  end;

  TRttiExtensionPropertyDescriptor = class(TPropertyDescriptor)
  protected
    _propertyName: string;

    function  get_MemberType: TMemberType; override;

  public
    constructor Create(const PropertyName: string); reintroduce;
  end;

implementation

{ TPropertyDescriptor } // implemented in .intf unit

{ TRttiStandardPropertyDescriptor }

constructor TRttiStandardPropertyDescriptor.Create(AInfo: PTypeInfo; const RttiProp: TRttiMember);
begin
  inherited Create(AInfo);
  FProp := RttiProp;
end;

function TRttiStandardPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  if FProp is TRttiIndexedProperty then
    Result := (FProp as TRttiIndexedProperty).GetValue(Ptr, Index)
  else if FProp is TRttiProperty then
    Result := (FProp as TRttiProperty).GetValue(Ptr)
  else if FProp is TRttiField then
    Result := (FProp as TRttiField).GetValue(Ptr);
end;

function TRttiStandardPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

function TRttiStandardPropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := FProp.Handle;
end;

procedure TRttiStandardPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  if FProp is TRttiIndexedProperty then
    (FProp as TRttiIndexedProperty).SetValue(Ptr, Index, Value)
  else if FProp is TRttiProperty then
    (FProp as TRttiProperty).SetValue(Ptr, Value)
  else if FProp is TRttiField then
    (FProp as TRttiField).SetValue(Ptr, Value);
end;

{ TRttiMethodPropertyDescriptor }

function TRttiMethodPropertyDescriptor.Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
begin
  var method := SelectMethodMatchArguments(ctx, argc, argv);

  try
    var params := Method.GetParameters;
    var arr: array of TValue;

    if Length(params) > 0 then
    begin
      SetLength(arr, Length(params));
      for var i := 0 to High(params) do
      begin
        if i < argc then
          arr[i] := JSConverterFuncs.JSValueToTValue(ctx, PJSValueConstArr(argv)[i], params[i].ParamType.Handle) else
          arr[i] := JSConverterFuncs.GetDefaultValue(params[i]);
      end;
    end;

    var v: TValue;
    var vt: TValue;
    // Reference type?
    if FTypeInfo.Kind in [tkInterface, tkClass] then
      TValue.Make(@Ptr, FTypeInfo, vt) else
      // Value types (aka Records)
      TValue.Make(Ptr, FTypeInfo, vt);

    v := Method.Invoke(vt, arr);
    Result := JSConverterFuncs.TValueToJSValue(ctx, v);
  except
    on E: Exception do
    begin
      Result := JS_EXCEPTION;
    end;
  end;
end;

function TRttiMethodPropertyDescriptor.SelectMethodMatchArguments(ctx: JSContext; argc: Integer; argv: PJSValueConst): TRttiMethod;
begin
  if Length(Methods) = 0 then Exit(nil);
  if Length(Methods) = 1 then Exit(FMethods[0]);

  for var m in FMethods do
  begin
    var params := m.GetParameters;
    if argc = Length(params) then
      Exit(m);
  end;

  Exit(nil);
end;

constructor TRttiMethodPropertyDescriptor.Create(AInfo: PTypeInfo; const RttiMethods: TArray<TRttiMethod>; IsInterface: Boolean);
begin
  inherited Create(AInfo);
  FMethods := RttiMethods;
  FIsInterface := IsInterface;
end;

function TRttiMethodPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Methods;
end;

function TRttiMethodPropertyDescriptor.Methods: TArray<TRttiMethod>;
begin
  Result := FMethods;
end;

{ TRttiAccessorPropertyDescriptor }

constructor TRttiAccessorPropertyDescriptor.Create(AInfo: PTypeInfo; const RttiGetter: TRttiMethod; const RttiSetter: TRttiMethod);
begin
  inherited Create(AInfo);
  FGetter := RttiGetter;
  FSetter := RttiSetter;
end;

function TRttiAccessorPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  if FGetter <> nil then
  begin
    var vt: TValue;
    
    case FTypeInfo.Kind of
      tkInterface:
        vt := TValue.From<IInterface>(IInterface(ptr));
      tkRecord:
        TValue.Make(Ptr, FTypeInfo, vt);
      tkClass:
        TValue.Make(@Ptr, FTypeInfo, vt);
    else
      raise Exception.Create('Unsupported type kind for accessor property');
    end;
    
    Result := FGetter.Invoke(vt, Index);
  end else
    raise Exception.Create('Property cannot be read');
end;

function TRttiAccessorPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

function TRttiAccessorPropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := FGetter.ReturnType.Handle;
end;

procedure TRttiAccessorPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  if FSetter <> nil then
  begin
    var vt: TValue;
    
    case FTypeInfo.Kind of
      tkInterface:
        vt := TValue.From<IInterface>(IInterface(ptr));
      tkRecord:
        TValue.Make(Ptr, FTypeInfo, vt);
      tkClass:
        TValue.Make(@Ptr, FTypeInfo, vt);
    else
      raise Exception.Create('Unsupported type kind for accessor property');
    end;

    var args: array of TValue;
    SetLength(args, 1);
    args[0] := Value;

    FSetter.Invoke(vt, args);
  end else
    raise Exception.Create('Property cannot be set');
end;

{ TRttiIndexedAccessorPropertyDescriptor }

function TRttiIndexedAccessorPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.IndexedProperty;
end;

{ TRttiArrayIndexDescriptor }

constructor TRttiArrayIndexDescriptor.Create(AInfo: PTypeInfo);
begin
  inherited Create(AInfo);
end;

function TRttiArrayIndexDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  Result := TValue.Empty;
end;

function TRttiArrayIndexDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.ArrayIndexer;
end;

function TRttiArrayIndexDescriptor.get_PropertyType: PTypeInfo;
begin

end;

procedure TRttiArrayIndexDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  inherited;
end;

{ TRttiIteratorDescriptor }
function TRttiIteratorDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  var obj: TObject;

  if FTypeInfo.Kind = tkInterface then
    obj := TObject(IInterface(ptr)) else
    obj := TObject(ptr);

  Result := TJSObjectIterator.CreateIterator(obj);
end;

function TRttiIteratorDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Iterator;
end;

{ TRttiIteratorNextDescriptor }

function TRttiIteratorNextDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.IteratorNext;
end;

{ TRttiExtensionPropertyDescriptor }
constructor TRttiExtensionPropertyDescriptor.Create(const PropertyName: string);
begin
  _propertyName := PropertyName;
end;

function TRttiExtensionPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.ExtensionProperty;
end;

{ TJSObjectIterator }
class function TJSObjectIterator.CreateIterator(const Obj: TObject) : TJSIterator;
begin
  Result := nil;

  var tp := _RttiContext.GetType(Obj.ClassType);
  var getEnumerator := tp.GetMethod('GetEnumerator');
  if getEnumerator = nil then Exit;
  var enumarator := getEnumerator.Invoke(Obj, []).AsObject;
  if enumarator = nil then Exit;
  var tp_enum := _RttiContext.GetType(enumarator.ClassType);
  var move_next := tp_enum.GetMethod('MoveNext');
  if move_next = nil then Exit;
  var get_current := tp_enum.GetProperty('Current');
  if get_current = nil then Exit;

  var iter := TJSObjectIterator.Create;
  iter._instance := enumarator;
  iter._move_next := move_next;
  iter._get_current := get_current;

  Result := iter;
end;

function TJSObjectIterator.Current: TValue;
begin
  Result := TRttiProperty(_get_current).GetValue(_instance);
end;

destructor TJSObjectIterator.Destroy;
begin
  inherited;
end;

function TJSObjectIterator.MoveNext: Boolean;
begin
  Result := TRttiMethod(_move_next).Invoke(_instance, []).AsBoolean;
end;

end.
