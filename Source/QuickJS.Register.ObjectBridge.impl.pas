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
    // Instance fields - no longer class vars
    FPropertyDescriptors: TDictionary<string, TList<IObjectBridgePropertyDescriptor>>;
    FMethodDescriptors: TDictionary<string, TList<IObjectBridgeMethodDescriptor>>;
    FInterfaceMappings: TDictionary<PTypeInfo, TList<PTypeInfo>>; // Source -> List of potential targets (ordered by priority)
    FIsResolving: Boolean; // Recursion guard
    
    procedure InitializeResolvers;
    function TryGetPropertyDescriptor(const AObject: IRegisteredObject; const PropertyName: string): IPropertyDescriptor;
    function TryGetMethodDescriptor(const AObject: IRegisteredObject; const MethodName: string): IPropertyDescriptor;
  public
    constructor Create;
    destructor Destroy; override;
    
    // IObjectBridgeResolver implementation
    function OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean): IPropertyDescriptor;
    procedure AddPropertyDescriptor(const Descriptor: IObjectBridgePropertyDescriptor);
    procedure AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
    procedure AddInterfaceMapping(const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo);
    function ResolveInterfaceMapping(const SourceInterface: PTypeInfo; const SourceInterfaceInstance: IInterface): PTypeInfo;
  end;

implementation

uses
  System.SysUtils,
  QuickJS.Register.impl,
//  System.Collections,
  System.Rtti;

{ TObjectBridgeResolver }

constructor TObjectBridgeResolver.Create;
begin
  inherited Create;
  FIsResolving := False;
  InitializeResolvers;
end;

destructor TObjectBridgeResolver.Destroy;
begin
  // Free property descriptor lists
  if Assigned(FPropertyDescriptors) then
  begin
    for var descriptorList in FPropertyDescriptors.Values do
      descriptorList.Free;
    FPropertyDescriptors.Free;
  end;
  
  // Free method descriptor lists
  if Assigned(FMethodDescriptors) then
  begin
    for var descriptorList in FMethodDescriptors.Values do
      descriptorList.Free;
    FMethodDescriptors.Free;
  end;
  
  // Free interface mapping lists
  if Assigned(FInterfaceMappings) then
  begin
    for var mappingList in FInterfaceMappings.Values do
      mappingList.Free;
    FInterfaceMappings.Free;
  end;
  
  inherited Destroy;
end;

procedure TObjectBridgeResolver.InitializeResolvers;
begin
  FPropertyDescriptors := TDictionary<string, TList<IObjectBridgePropertyDescriptor>>.Create;
  FMethodDescriptors := TDictionary<string, TList<IObjectBridgeMethodDescriptor>>.Create;
  FInterfaceMappings := TDictionary<PTypeInfo, TList<PTypeInfo>>.Create;
end;

function TObjectBridgeResolver.TryGetPropertyDescriptor(const AObject: IRegisteredObject; const PropertyName: string): IPropertyDescriptor;
begin
  Result := nil;
  var lowerPropertyName: string := LowerCase(PropertyName);
  
  // Look up descriptors by name in dictionary
  var descriptorList: TList<IObjectBridgePropertyDescriptor>;
  if not FPropertyDescriptors.TryGetValue(lowerPropertyName, descriptorList) then
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

function TObjectBridgeResolver.TryGetMethodDescriptor(const AObject: IRegisteredObject; const MethodName: string): IPropertyDescriptor;
begin
  Result := nil;
  var lowerMethodName: string := LowerCase(MethodName);

  // Look up descriptors by name in dictionary
  var descriptorList: TList<IObjectBridgeMethodDescriptor>;
  if not FMethodDescriptors.TryGetValue(lowerMethodName, descriptorList) then
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
  if not FPropertyDescriptors.TryGetValue(lowerPropertyName, descriptorList) then
  begin
    descriptorList := TList<IObjectBridgePropertyDescriptor>.Create;
    FPropertyDescriptors.Add(lowerPropertyName, descriptorList);
  end;
  
  // Insert at the beginning so later registrations take priority
  descriptorList.Insert(0, Descriptor);
end;

procedure TObjectBridgeResolver.AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
begin
  var lowerMethodName: string := LowerCase(Descriptor.MethodName);
  
  // Get or create the list for this method name
  var descriptorList: TList<IObjectBridgeMethodDescriptor>;
  if not FMethodDescriptors.TryGetValue(lowerMethodName, descriptorList) then
  begin
    descriptorList := TList<IObjectBridgeMethodDescriptor>.Create;
    FMethodDescriptors.Add(lowerMethodName, descriptorList);
  end;
  
  // Insert at the beginning so later registrations take priority
  descriptorList.Insert(0, Descriptor);
end;

function TObjectBridgeResolver.OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean): IPropertyDescriptor;
begin
  Result := nil;
  Handled := False;
  
  // Prevent infinite recursion
  if FIsResolving then
    Exit;
  
  FIsResolving := True;
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
    FIsResolving := False;
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
  if not FInterfaceMappings.TryGetValue(SourceInterface, mappingList) then
  begin
    mappingList := TList<PTypeInfo>.Create;
    FInterfaceMappings.Add(SourceInterface, mappingList);
  end;
  
  // Add to the beginning so later registrations take priority
  mappingList.Insert(0, TargetInterface);
end;

function TObjectBridgeResolver.ResolveInterfaceMapping(const SourceInterface: PTypeInfo; const SourceInterfaceInstance: IInterface): PTypeInfo;
begin
  Result := SourceInterface; // Default: return the source if no mapping found
  
  if (SourceInterface = nil) or (SourceInterfaceInstance = nil) then
    Exit;
  
  // Look up mappings for this source interface
  var mappingList: TList<PTypeInfo>;
  if not FInterfaceMappings.TryGetValue(SourceInterface, mappingList) then
    Exit;
  
  // Try each potential target interface in priority order
  for var targetInterfaceTypeInfo in mappingList do
  begin
    // Check if the object supports the target interface (without keeping a reference)
    if Supports(SourceInterfaceInstance, targetInterfaceTypeInfo.TypeData.Guid) then
    begin
      // Object supports this target interface, use it
      // tempIntf will be released automatically when it goes out of scope
      Result := targetInterfaceTypeInfo;
      Exit;
    end;
  end;
end;


end.
