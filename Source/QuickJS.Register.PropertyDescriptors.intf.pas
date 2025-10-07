unit QuickJS.Register.PropertyDescriptors.intf;

interface

uses
  System.Variants,
  System.Rtti,
  System.TypInfo,
  quickjs_ng,
  System.SysUtils,
  QuickJS.Register.intf;

type
  TObjectConstuctor = reference to function: Pointer;

  DefaultValueAttribute = class(TCustomAttribute)
  protected
    FValue: Variant;
  public
    constructor Create(Value: Boolean); overload;
    constructor Create(Value: string); overload;
    property Value: Variant read FValue;
  end;


  PObjectMember = Pointer;  // Generic pointer to TRttiMember/TRttiMethod/TrriProperty/_PropertyInfo
  PRttiMember = ^TRttiMember;

  TInterfaceRef = record
    IID: TGuid;
    ii: IInterface;
  end;


  IMethodsPropertyDescriptor = interface
    ['{3D51ABCB-4C43-482A-8AE4-0749F56CD1CA}']
    function Methods: TArray<TRttiMethod>;
    function Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
  end;

  // Base descriptor so dependents can inherit without using the .impl unit
  TPropertyDescriptor = class(TInterfacedObject, IPropertyDescriptor)
  protected
    FTypeInfo: PTypeInfo;

    function  get_MemberType: TMemberType; virtual;
    function  get_TypeInfo: PTypeInfo; virtual;
    function  get_PropertyType: PTypeInfo; virtual;
    function  IsInterface: Boolean; virtual;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; virtual;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); virtual;
  public
    constructor Create(AInfo: PTypeInfo); virtual;
  end;


implementation

{ DefaultValueAttribute }

constructor DefaultValueAttribute.Create(Value: Boolean);
begin
  FValue := Value;
end;

constructor DefaultValueAttribute.Create(Value: string);
begin
  FValue := Value;
end;

{ TPropertyDescriptor }

constructor TPropertyDescriptor.Create(AInfo: PTypeInfo);
begin
  FTypeInfo := AInfo;
end;

function TPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  raise ENotImplemented.Create('GetValue not implemented');
end;

function TPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.None;
end;

function TPropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  raise ENotImplemented.Create('PropertyType not implemented');
end;

function TPropertyDescriptor.get_TypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TPropertyDescriptor.IsInterface: Boolean;
begin
  Result := (FTypeInfo <> nil) and (FTypeInfo.Kind = tkInterface);
end;

procedure TPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  raise ENotImplemented.Create('SetValue not implemented');
end;

end.
