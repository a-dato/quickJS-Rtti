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
end;

class procedure TObjectBridgeResolver.InitializeResolvers;
begin
  _propertyDescriptors := TDictionary<string, TList<IObjectBridgePropertyDescriptor>>.Create;
  _methodDescriptors := TDictionary<string, TList<IObjectBridgeMethodDescriptor>>.Create;
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


end.
