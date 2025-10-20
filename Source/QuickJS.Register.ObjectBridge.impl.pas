unit QuickJS.Register.ObjectBridge.impl;

interface

uses
  QuickJS.Register.ObjectBridgeTypes.intf,
  QuickJS.Register.ObjectBridge.intf,
  QuickJS.Register.intf, System.TypInfo,
  QuickJS.Register.ObjectBridgeTypes.impl,
  System.Generics.Collections;

type

  TObjectBridgeResolver = class(TInterfacedObject, IObjectBridgeResolver)
  private
    class var _propertyDescriptors: TDictionary<string, TList<IObjectBridgePropertyDescriptor>>;
    class var _methodDescriptors: TDictionary<string, TList<IObjectBridgeMethodDescriptor>>;
    class var _interfaceMappings: TDictionary<PTypeInfo, TList<PTypeInfo>>; // Source -> List of potential targets (ordered by priority)
    class var _isResolving: Boolean; // Recursion guard
    
    class procedure InitializeResolvers;
    class function TryGetPropertyDescriptor(const AObject: IRegisteredObject; const PropertyName: string): IPropertyDescriptor;
    class function TryGetMethodDescriptor(const AObject: IRegisteredObject; const MethodName: string): IPropertyDescriptor;
  public
    class constructor Create;
    class destructor Destroy;
    
    // IObjectBridgeResolver implementation
    function OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean): IPropertyDescriptor;
    procedure AddPropertyDescriptor(const Descriptor: IObjectBridgePropertyDescriptor);
    procedure AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
    procedure AddInterfaceMapping(const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo);
    function ResolveInterfaceMapping(const SourceInterface: PTypeInfo; const InterfacePtr: Pointer): PTypeInfo;
  end;

implementation

uses
  System.SysUtils,
  QuickJS.Register.impl,
//  System.Collections,
  System.Rtti,
  quickjs_ng;

{ TObjectBridgeResolver }

class constructor TObjectBridgeResolver.Create;
begin
  _isResolving := False;
  InitializeResolvers;
end;

class destructor TObjectBridgeResolver.Destroy;
begin
  _propertyDescriptors.Free;
  _methodDescriptors.Free;
  
  // Free interface mapping lists
  if Assigned(_interfaceMappings) then
  begin
    for var mappingList in _interfaceMappings.Values do
      mappingList.Free;
    _interfaceMappings.Free;
  end;
end;

class procedure TObjectBridgeResolver.InitializeResolvers;
begin
  _propertyDescriptors := TDictionary<string, TList<IObjectBridgePropertyDescriptor>>.Create;
  _methodDescriptors := TDictionary<string, TList<IObjectBridgeMethodDescriptor>>.Create;
  _interfaceMappings := TDictionary<PTypeInfo, TList<PTypeInfo>>.Create;
end;

class function TObjectBridgeResolver.TryGetPropertyDescriptor(const AObject: IRegisteredObject; const PropertyName: string): IPropertyDescriptor;
begin
  Result := nil;
  var lowerPropertyName: string := LowerCase(PropertyName);
  
  // Look up descriptors by name in dictionary
  var descriptorList: TList<IObjectBridgePropertyDescriptor>;
  if not _propertyDescriptors.TryGetValue(lowerPropertyName, descriptorList) then
    Exit;
  
  // Only check CanHandle on descriptors with matching name
  for var descriptor in descriptorList do
  begin
    if descriptor.CanHandle(AObject) then
    begin
      Result := descriptor as IPropertyDescriptor;
      Exit;
    end;
  end;
end;

class function TObjectBridgeResolver.TryGetMethodDescriptor(const AObject: IRegisteredObject; const MethodName: string): IPropertyDescriptor;
begin
  Result := nil;
  var lowerMethodName: string := LowerCase(MethodName);

  // Look up descriptors by name in dictionary
  var descriptorList: TList<IObjectBridgeMethodDescriptor>;
  if not _methodDescriptors.TryGetValue(lowerMethodName, descriptorList) then
    Exit;
  
  // Only check CanHandle on descriptors with matching name
  for var descriptor in descriptorList do
  begin
    if descriptor.CanHandle(AObject) then
    begin
      Result := descriptor as IPropertyDescriptor;
      Exit;
    end;
  end;
end;

procedure TObjectBridgeResolver.AddPropertyDescriptor(const Descriptor: IObjectBridgePropertyDescriptor);
begin
  var lowerPropertyName: string := LowerCase(Descriptor.PropertyName);
  
  // Get or create the list for this property name
  var descriptorList: TList<IObjectBridgePropertyDescriptor>;
  if not _propertyDescriptors.TryGetValue(lowerPropertyName, descriptorList) then
  begin
    descriptorList := TList<IObjectBridgePropertyDescriptor>.Create;
    _propertyDescriptors.Add(lowerPropertyName, descriptorList);
  end;
  
  // Insert at the beginning so later registrations take priority
  descriptorList.Insert(0, Descriptor);
end;

procedure TObjectBridgeResolver.AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
begin
  var lowerMethodName: string := LowerCase(Descriptor.MethodName);
  
  // Get or create the list for this method name
  var descriptorList: TList<IObjectBridgeMethodDescriptor>;
  if not _methodDescriptors.TryGetValue(lowerMethodName, descriptorList) then
  begin
    descriptorList := TList<IObjectBridgeMethodDescriptor>.Create;
    _methodDescriptors.Add(lowerMethodName, descriptorList);
  end;
  
  // Insert at the beginning so later registrations take priority
  descriptorList.Insert(0, Descriptor);
end;

function TObjectBridgeResolver.OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean): IPropertyDescriptor;
begin
  Result := nil;
  Handled := False;
  
  // Prevent infinite recursion
  if _isResolving then
    Exit;
  
  _isResolving := True;
  try
    // Try properties if requested
    if TMemberType.Property in MemberTypes then
    begin
      Result := TryGetPropertyDescriptor(AObject, AName);
      if Result <> nil then
      begin
        Handled := True;
        Exit;
      end;
    end;

    // Try methods if requested
    if TMemberType.Methods in MemberTypes then
    begin
      Result := TryGetMethodDescriptor(AObject, AName);
      if Result <> nil then
      begin
        Handled := True;
        Exit;
      end;
    end;
  finally
    _isResolving := False;
  end;
end;

procedure TObjectBridgeResolver.AddInterfaceMapping(const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo);
begin
  if (SourceInterface = nil) or (TargetInterface = nil) then
    Exit;
    
  if SourceInterface.Kind <> tkInterface then
    raise Exception.Create('Source must be an interface type');
    
  if TargetInterface.Kind <> tkInterface then
    raise Exception.Create('Target must be an interface type');
  
  // Get or create the list for this source interface
  var mappingList: TList<PTypeInfo>;
  if not _interfaceMappings.TryGetValue(SourceInterface, mappingList) then
  begin
    mappingList := TList<PTypeInfo>.Create;
    _interfaceMappings.Add(SourceInterface, mappingList);
  end;
  
  // Add to the beginning so later registrations take priority
  mappingList.Insert(0, TargetInterface);
end;

function TObjectBridgeResolver.ResolveInterfaceMapping(const SourceInterface: PTypeInfo; const InterfacePtr: Pointer): PTypeInfo;
begin
  Result := SourceInterface; // Default: return the source if no mapping found
  
  if (SourceInterface = nil) or (InterfacePtr = nil) then
    Exit;
  
  // Look up mappings for this source interface
  var mappingList: TList<PTypeInfo>;
  if not _interfaceMappings.TryGetValue(SourceInterface, mappingList) then
    Exit;
  
  // Try each potential target interface in priority order
  for var targetInterface in mappingList do
  begin
    // Check if the object supports the target interface
    var targetIntf: IInterface;
    var sourceIntf: IInterface := IInterface(InterfacePtr);
    
    if Supports(sourceIntf, targetInterface.TypeData.Guid, targetIntf) then
    begin
      // Object supports this target interface, use it
      Result := targetInterface;
      Exit;
    end;
  end;
end;


end.
