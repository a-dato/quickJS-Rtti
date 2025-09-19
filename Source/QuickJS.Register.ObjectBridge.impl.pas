unit QuickJS.Register.ObjectBridge.impl;

interface

uses
  System_,
  QuickJS.Register.ObjectBridgeTypes.intf,
  QuickJS.Register.ObjectBridge.intf,
  App.PropertyDescriptor.intf,
  QuickJS.Register.intf, System.TypInfo,
  QuickJS.Register.ObjectBridgeTypes.impl;

type

  TObjectBridgeResolver = class(TBaseInterfacedObject, IObjectBridgeResolver)
  private
    class var _propertyDescriptors: TDescriptorList;
    class var _methodDescriptors: TDescriptorList;
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
  System.Collections.Generic, quickjs_ng;

{ TObjectBridgeResolver }

class constructor TObjectBridgeResolver.Create;
begin
  _isResolving := False;
  InitializeResolvers;
end;

class destructor TObjectBridgeResolver.Destroy;
begin
  SetLength(_propertyDescriptors, 0);
  SetLength(_methodDescriptors, 0);
end;

class procedure TObjectBridgeResolver.InitializeResolvers;
begin
  SetLength(_propertyDescriptors, 0);
  SetLength(_methodDescriptors, 0);
end;

class function TObjectBridgeResolver.TryGetPropertyDescriptor(const AObject: IRegisteredObject; const PropertyName: string): IPropertyDescriptor;
var
  i: Integer;
  lowerPropertyName: string;
  descriptor: IObjectBridgePropertyDescriptor;
begin
  Result := nil;
  lowerPropertyName := LowerCase(PropertyName);
  
  // Search through all registered property descriptors
  for i := 0 to Length(_propertyDescriptors) - 1 do
  begin
    if Supports(_propertyDescriptors[i], IObjectBridgePropertyDescriptor, descriptor) then
    begin
      if (LowerCase(descriptor.PropertyName) = lowerPropertyName) and descriptor.CanHandle(AObject) then
      begin
        Result := _propertyDescriptors[i];
        Exit;
      end;
    end;
  end;
end;

class function TObjectBridgeResolver.TryGetMethodDescriptor(const AObject: IRegisteredObject; const MethodName: string): IPropertyDescriptor;
var
  i: Integer;
  lowerMethodName: string;
  descriptor: IObjectBridgeMethodDescriptor;
begin
  Result := nil;
  lowerMethodName := LowerCase(MethodName);

  // Search through all registered method descriptors
  for i := 0 to Length(_methodDescriptors) - 1 do
  begin
    if Supports(_methodDescriptors[i], IObjectBridgeMethodDescriptor, descriptor) then
    begin
      if (LowerCase(descriptor.MethodName) = lowerMethodName) and descriptor.CanHandle(AObject) then
      begin
        Result := _methodDescriptors[i];
        Exit;
      end;
    end;
  end;
end;

procedure TObjectBridgeResolver.AddPropertyDescriptor(const Descriptor: IObjectBridgePropertyDescriptor);
begin
  SetLength(_propertyDescriptors, Length(_propertyDescriptors) + 1);
  _propertyDescriptors[Length(_propertyDescriptors) - 1] := Descriptor;
end;

procedure TObjectBridgeResolver.AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
begin
  SetLength(_methodDescriptors, Length(_methodDescriptors) + 1);
  _methodDescriptors[Length(_methodDescriptors) - 1] := Descriptor;
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
