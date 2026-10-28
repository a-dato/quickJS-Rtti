unit App.TypeMetadata;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Collections.Generic,
  System_;

type
  {$M+}
  IMethodMetadataInfo = interface(IBaseInterface)
    ['{4C821D20-844B-4AE1-B493-824155D658F9}']
    function get_Name: CString;
    function get_Description: CString;
    function get_UsagePattern: CString;

    property Name: CString read get_Name;
    property Description: CString read get_Description;
    property UsagePattern: CString read get_UsagePattern;
  end;

  ISupportedInterfaceMetadata = interface(IBaseInterface)
    ['{D7C42440-7695-49D5-98F9-C39C645D6BD3}']
    function get_Name: CString;
    function get_InterfaceType: &Type;
    function get_BehaviorName: CString;
    function get_BehaviorDescription: CString;
    function get_BehaviorUseWhen: CString;
    function get_Methods: List<IMethodMetadataInfo>;

    property Name: CString read get_Name;
    property InterfaceType: &Type read get_InterfaceType;
    property BehaviorName: CString read get_BehaviorName;
    property BehaviorDescription: CString read get_BehaviorDescription;
    property BehaviorUseWhen: CString read get_BehaviorUseWhen;
    property Methods: List<IMethodMetadataInfo> read get_Methods;
  end;
  {$M-}

  TBehaviorAttribute = class(TCustomAttribute)
  protected
    _Name: CString;
    _Description: CString;
    _UseWhen: CString;

  public
    constructor Create(const Name: string; const Description: string; const UseWhen: string);

    property Name: CString read _Name;
    property Description: CString read _Description;
    property UseWhen: CString read _UseWhen;
  end;

  TMethodMetadataAttribute = class(TCustomAttribute)
  protected
    _Description: CString;
    _UsagePattern: CString; 

  public
    constructor Create(const Description: string; const UsagePattern: string = '');

    property Description: CString read _Description;
    property UsagePattern: CString read _UsagePattern;
  end;

  TMethodMetadataInfo = class(TBaseInterfacedObject, IMethodMetadataInfo)
  protected
    _Name: CString;
    _Description: CString;
    _UsagePattern: CString;

    function get_Name: CString;
    function get_Description: CString;
    function get_UsagePattern: CString;
  public
    constructor Create(const Name: CString; const Attribute: TMethodMetadataAttribute);
  end;

  TSupportedInterfaceMetadata = class(TBaseInterfacedObject, ISupportedInterfaceMetadata)
  protected
    _Name: CString;
    _InterfaceType: &Type;
    _BehaviorName: CString;
    _BehaviorDescription: CString;
    _BehaviorUseWhen: CString;
    _Methods: List<IMethodMetadataInfo>;

    function get_Name: CString;
    function get_InterfaceType: &Type;
    function get_BehaviorName: CString;
    function get_BehaviorDescription: CString;
    function get_BehaviorUseWhen: CString;
    function get_Methods: List<IMethodMetadataInfo>;
  public
    constructor Create(const InterfaceType: &Type);
  end;

  TTypeMetadata = class
  public
    class function Behavior(const TypeInfo: PTypeInfo): TBehaviorAttribute; static;
    class function MethodMetadata(const Method: TRttiMethod): TMethodMetadataAttribute; overload; static;
    class function MethodMetadata(const TypeInfo: PTypeInfo; const MethodName: CString): TMethodMetadataAttribute; overload; static;
    class function SupportedInterface(const InterfaceType: &Type): ISupportedInterfaceMetadata; static;
  end;

implementation

{ TBehaviorAttribute }

constructor TBehaviorAttribute.Create(const Name, Description, UseWhen: string);
begin
  inherited Create;

  _Name := Name;
  _Description := Description;
  _UseWhen := UseWhen;
end;

{ TMethodMetadataAttribute }

constructor TMethodMetadataAttribute.Create(const Description, UsagePattern: string);
begin
  inherited Create;

  _Description := Description;
  _UsagePattern := UsagePattern;
end;

{ TMethodMetadataInfo }

constructor TMethodMetadataInfo.Create(const Name: CString; const Attribute: TMethodMetadataAttribute);
begin
  inherited Create;

  _Name := Name;
  if Attribute <> nil then
  begin
    _Description := Attribute.Description;
    _UsagePattern := Attribute.UsagePattern;
  end;
end;

function TMethodMetadataInfo.get_Description: CString;
begin
  Result := _Description;
end;

function TMethodMetadataInfo.get_Name: CString;
begin
  Result := _Name;
end;

function TMethodMetadataInfo.get_UsagePattern: CString;
begin
  Result := _UsagePattern;
end;

{ TSupportedInterfaceMetadata }

constructor TSupportedInterfaceMetadata.Create(const InterfaceType: &Type);
begin
  inherited Create;

  _InterfaceType := InterfaceType;
  _Methods := CList<IMethodMetadataInfo>.Create;

  var TypeInfo := InterfaceType.GetTypeInfo;
  if TypeInfo = nil then
    Exit;

  var Context := TRttiContext.Create;
  try
    var RttiType := Context.GetType(TypeInfo);
    if RttiType = nil then
      Exit;

    _Name := RttiType.Name;

    var BehaviorAttribute := TTypeMetadata.Behavior(TypeInfo);
    if BehaviorAttribute <> nil then
    begin
      _BehaviorName := BehaviorAttribute.Name;
      _BehaviorDescription := BehaviorAttribute.Description;
      _BehaviorUseWhen := BehaviorAttribute.UseWhen;
    end;

    for var Method in RttiType.GetMethods do
    begin
      var MethodAttribute := TTypeMetadata.MethodMetadata(Method);
      if MethodAttribute <> nil then
        _Methods.Add(TMethodMetadataInfo.Create(Method.Name, MethodAttribute));
    end;
  finally
    Context.Free;
  end;
end;

function TSupportedInterfaceMetadata.get_BehaviorDescription: CString;
begin
  Result := _BehaviorDescription;
end;

function TSupportedInterfaceMetadata.get_BehaviorName: CString;
begin
  Result := _BehaviorName;
end;

function TSupportedInterfaceMetadata.get_BehaviorUseWhen: CString;
begin
  Result := _BehaviorUseWhen;
end;

function TSupportedInterfaceMetadata.get_InterfaceType: &Type;
begin
  Result := _InterfaceType;
end;

function TSupportedInterfaceMetadata.get_Methods: List<IMethodMetadataInfo>;
begin
  Result := _Methods;
end;

function TSupportedInterfaceMetadata.get_Name: CString;
begin
  Result := _Name;
end;

{ TTypeMetadata }

class function TTypeMetadata.Behavior(const TypeInfo: PTypeInfo): TBehaviorAttribute;
begin
  Result := nil;

  if TypeInfo = nil then
    Exit;

  var Context := TRttiContext.Create;
  try
    var RttiType := Context.GetType(TypeInfo);
    if RttiType = nil then
      Exit;

    for var Attribute in RttiType.GetAttributes do
      if Attribute is TBehaviorAttribute then
        Exit(Attribute as TBehaviorAttribute);
  finally
    Context.Free;
  end;
end;

class function TTypeMetadata.MethodMetadata(const TypeInfo: PTypeInfo; const MethodName: CString): TMethodMetadataAttribute;
begin
  Result := nil;

  if TypeInfo = nil then
    Exit;

  var Context := TRttiContext.Create;
  try
    var RttiType := Context.GetType(TypeInfo);
    if RttiType = nil then
      Exit;

    for var Method in RttiType.GetMethods do
      if CString.Equals(Method.Name, MethodName) then
      begin
        Result := MethodMetadata(Method);
        if Result <> nil then
          Exit;
      end;
  finally
    Context.Free;
  end;
end;

class function TTypeMetadata.MethodMetadata(const Method: TRttiMethod): TMethodMetadataAttribute;
begin
  Result := nil;

  if Method = nil then
    Exit;

  for var Attribute in Method.GetAttributes do
    if Attribute is TMethodMetadataAttribute then
      Exit(Attribute as TMethodMetadataAttribute);
end;

class function TTypeMetadata.SupportedInterface(const InterfaceType: &Type): ISupportedInterfaceMetadata;
begin
  Result := TSupportedInterfaceMetadata.Create(InterfaceType);
end;

end.
