unit QuickJS.Register.ObjectBridge.impl;

interface

uses
  System_,
  QuickJS.Register.ObjectBridgeTypes.intf,
  QuickJS.Register.ObjectBridge.intf,
  App.PropertyDescriptor.intf,
  QuickJS.Register.intf, System.TypInfo,
  QuickJS.Register.ObjectBridgeTypes.impl,
  System.Collections.Generic;

type

  TObjectBridgeResolver = class(TBaseInterfacedObject, IObjectBridgeResolver)
  private
    class var _propertyDescriptors: IDictionary<CString, IList<IObjectBridgePropertyDescriptor>>;
    class var _methodDescriptors: IDictionary<CString, IList<IObjectBridgeMethodDescriptor>>;
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
  System.Collections,
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
  _propertyDescriptors := nil;
  _methodDescriptors := nil;
end;

class procedure TObjectBridgeResolver.InitializeResolvers;
begin
  _propertyDescriptors := CDictionary<CString, IList<IObjectBridgePropertyDescriptor>>.Create;
  _methodDescriptors := CDictionary<CString, IList<IObjectBridgeMethodDescriptor>>.Create;
end;

class function TObjectBridgeResolver.TryGetPropertyDescriptor(const AObject: IRegisteredObject; const PropertyName: string): IPropertyDescriptor;
begin
  Result := nil;
  var lowerPropertyName: CString := CString(LowerCase(PropertyName));
  
  // Look up descriptors by name in dictionary
  var descriptorList: IList<IObjectBridgePropertyDescriptor>;
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
  var lowerMethodName: CString := CString(LowerCase(MethodName));

  // Look up descriptors by name in dictionary
  var descriptorList: IList<IObjectBridgeMethodDescriptor>;
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
  var lowerPropertyName: CString := CString(LowerCase(Descriptor.PropertyName));
  
  // Get or create the list for this property name
  var descriptorList: IList<IObjectBridgePropertyDescriptor>;
  if not _propertyDescriptors.TryGetValue(lowerPropertyName, descriptorList) then
  begin
    descriptorList := CList<IObjectBridgePropertyDescriptor>.Create;
    _propertyDescriptors.Add(lowerPropertyName, descriptorList);
  end;
  
  descriptorList.Add(Descriptor);
end;

procedure TObjectBridgeResolver.AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
begin
  var lowerMethodName: CString := CString(LowerCase(Descriptor.MethodName));
  
  // Get or create the list for this method name
  var descriptorList: IList<IObjectBridgeMethodDescriptor>;
  if not _methodDescriptors.TryGetValue(lowerMethodName, descriptorList) then
  begin
    descriptorList := CList<IObjectBridgeMethodDescriptor>.Create;
    _methodDescriptors.Add(lowerMethodName, descriptorList);
  end;
  
  descriptorList.Add(Descriptor);
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
