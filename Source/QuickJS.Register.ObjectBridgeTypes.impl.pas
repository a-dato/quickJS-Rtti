unit QuickJS.Register.ObjectBridgeTypes.impl;

interface

uses
  System_,
  System.Collections.Generic,
  QuickJS.Register.intf,
  QuickJS.Register.PropertyDescriptors.intf,
  QuickJS.Register.ObjectBridgeTypes.intf,
  System.TypInfo, System.Rtti, quickjs_ng;

type
  // Descriptor implementation classes
  TObjectBridgePropertyDescriptor = class(TPropertyDescriptor, IObjectBridgePropertyDescriptor)
  private
    FPropertyName: string;
    FObjectChecker: TObjectChecker;
    FPropertyGetter: TPropertyGetter;
    FPropertySetter: TPropertySetter;
    FPropertyTypeInfo: PTypeInfo;
  protected
    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;
    function  get_PropertyName: string;
    function  GetValue(const Ptr: Pointer; const Index: array of TValue): TValue; override;
    procedure SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue); override;
  public
    constructor Create(const PropertyName: string; const ObjectChecker: TObjectChecker;
                      const PropertyGetter: TPropertyGetter; const PropertySetter: TPropertySetter = nil;
                      const PropertyTypeInfo: PTypeInfo = nil);

    // Static creation methods for common patterns
    class function CreateInterfaceProperty(const PropertyName: string; const TargetInterface: PTypeInfo; const DelphiPropertyName: string): IObjectBridgePropertyDescriptor; static;
    class function CreateCrossInterfaceProperty(const PropertyName: string; const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo; const DelphiPropertyName: string): IObjectBridgePropertyDescriptor; static;
    class function CreateExtensionProperty(const PropertyName: string; const TargetInterface: PTypeInfo; const Getter: TExtensionGetter): IObjectBridgePropertyDescriptor; static;
    class function CreatePatternProperty(const PropertyName: string; const PatternMatcher: TPatternChecker; const DelphiPropertyName: string): IObjectBridgePropertyDescriptor; static;

    function CanHandle(const AObject: IRegisteredObject): Boolean;
    property PropertyName: string read get_PropertyName;
  end;

  TObjectBridgeMethodDescriptor = class(TPropertyDescriptor, IMethodsPropertyDescriptor, IObjectBridgeMethodDescriptor)
  private
    FMethodName: string;
    FObjectChecker: TObjectChecker;
    FMethodCaller: TMethodCaller;
  protected
    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;
    function  get_MethodName: string;
    function  GetValue(const Ptr: Pointer; const Index: array of TValue): TValue; override;
    procedure SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue); override;
    function  Methods: TArray<TRttiMethod>;
    function  Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
  public
    constructor Create(const MethodName: string; const ObjectChecker: TObjectChecker; const MethodCaller: TMethodCaller);

    // Static creation methods for common patterns
    class function CreateInterfaceMethod(const MethodName: string; const TargetInterface: PTypeInfo; const DelphiMethodName: string): IObjectBridgeMethodDescriptor; static;
    class function CreateCrossInterfaceMethod(const MethodName: string; const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo; const DelphiMethodName: string): IObjectBridgeMethodDescriptor; static;

    function CanHandle(const AObject: IRegisteredObject): Boolean;
    property MethodName: string read get_MethodName;
  end;

implementation

uses
  System.SysUtils;

{ TObjectBridgePropertyDescriptor }

constructor TObjectBridgePropertyDescriptor.Create(const PropertyName: string; const ObjectChecker: TObjectChecker;
  const PropertyGetter: TPropertyGetter; const PropertySetter: TPropertySetter = nil;
  const PropertyTypeInfo: PTypeInfo = nil);
begin
  // Host type info is used by the bridge to determine whether Ptr is an interface or object.
  // Default to interface to match common usage (e.g., IEnumerable/IList). Property type is kept separately.
  inherited Create(TypeInfo(IInterface));
  FPropertyName := PropertyName;
  FObjectChecker := ObjectChecker;
  FPropertyGetter := PropertyGetter;
  FPropertySetter := PropertySetter;
  FPropertyTypeInfo := PropertyTypeInfo;
end;

function TObjectBridgePropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

function TObjectBridgePropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := FPropertyTypeInfo;
end;

function TObjectBridgePropertyDescriptor.get_PropertyName: string;
begin
  Result := FPropertyName;
end;

function TObjectBridgePropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  if Assigned(FPropertyGetter) then
    Result := FPropertyGetter(Ptr)
  else
    Result := TValue.Empty;
end;

procedure TObjectBridgePropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  if Assigned(FPropertySetter) then
    FPropertySetter(Ptr, Value);
  // If no setter, property is read-only
end;

function TObjectBridgePropertyDescriptor.CanHandle(const AObject: IRegisteredObject): Boolean;
begin
  Result := Assigned(FObjectChecker) and FObjectChecker(AObject);
end;

class function TObjectBridgePropertyDescriptor.CreateInterfaceProperty(const PropertyName: string; const TargetInterface: PTypeInfo; const DelphiPropertyName: string): IObjectBridgePropertyDescriptor;
begin
  Result := TObjectBridgePropertyDescriptor.Create(
    PropertyName,
    // Object checker - check if object type matches target interface
    function(const AObject: IRegisteredObject): Boolean
    begin
      Result := AObject.GetTypeInfo = TargetInterface;
    end,
    // Property getter - simplified version without complex reflection
    function(const Ptr: Pointer): TValue
    begin
      Result := TValue.Empty;
      // Simplified implementation - actual property access would need to be implemented
      // based on your specific requirements
    end
  );
end;

class function TObjectBridgePropertyDescriptor.CreateCrossInterfaceProperty(const PropertyName: string; const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo; const DelphiPropertyName: string): IObjectBridgePropertyDescriptor;
begin
  Result := TObjectBridgePropertyDescriptor.Create(
    PropertyName,
    // Object checker - check if object is source interface type
    function(const AObject: IRegisteredObject): Boolean
    begin
      var objectTypeName := LowerCase(string(AObject.GetTypeInfo.Name));
      var sourceInterfaceName := LowerCase(string(SourceInterface.Name));
      Result := objectTypeName = sourceInterfaceName;
    end,
    // Property getter - simplified version
    function(const Ptr: Pointer): TValue
    begin
      Result := TValue.Empty;
      // Simplified implementation - actual cross-interface property access would need
      // to be implemented based on your specific requirements
    end
  );
end;

class function TObjectBridgePropertyDescriptor.CreateExtensionProperty(const PropertyName: string; const TargetInterface: PTypeInfo; const Getter: TExtensionGetter): IObjectBridgePropertyDescriptor;
begin
  Result := TObjectBridgePropertyDescriptor.Create(
    PropertyName,
    // Object checker - check if object type matches target interface
    function(const AObject: IRegisteredObject): Boolean
    begin
      Result := AObject.GetTypeInfo = TargetInterface;
    end,
    // Property getter - use provided getter function
    function(const Ptr: Pointer): TValue
    begin
      Result := TValue.Empty;
      if (Ptr = nil) or not Assigned(Getter) then Exit;
      
      // Cast pointer to interface and call getter
      var targetInterface: IInterface := IInterface(Ptr);
      Result := Getter(targetInterface);
    end
  );
end;

class function TObjectBridgePropertyDescriptor.CreatePatternProperty(const PropertyName: string; const PatternMatcher: TPatternChecker; const DelphiPropertyName: string): IObjectBridgePropertyDescriptor;
begin
  Result := TObjectBridgePropertyDescriptor.Create(
    PropertyName,
    // Object checker - convert pattern matcher to object checker
    TObjectChecker(PatternMatcher),
    // Property getter - simplified version
    function(const Ptr: Pointer): TValue
    begin
      Result := TValue.Empty;
      // Simplified implementation - actual pattern-based property access would need
      // to be implemented based on your specific requirements
    end
  );
end;

{ TObjectBridgeMethodDescriptor }

constructor TObjectBridgeMethodDescriptor.Create(const MethodName: string; const ObjectChecker: TObjectChecker; const MethodCaller: TMethodCaller);
begin
  // Host type info must be non-nil so callers can inspect Kind. Default to interface.
  inherited Create(TypeInfo(IInterface));
  FMethodName := MethodName;
  FObjectChecker := ObjectChecker;
  FMethodCaller := MethodCaller;
end;

function TObjectBridgeMethodDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Methods;
end;

function TObjectBridgeMethodDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := nil; // Methods don't have a property type
end;

function TObjectBridgeMethodDescriptor.get_MethodName: string;
begin
  Result := FMethodName;
end;

function TObjectBridgeMethodDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  Result := TValue.Empty; // Methods don't return values via GetValue
end;

procedure TObjectBridgeMethodDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  // Methods don't support SetValue
end;

function TObjectBridgeMethodDescriptor.Methods: TArray<TRttiMethod>;
begin
  // Return empty array - actual method resolution is handled by the lambda caller
  SetLength(Result, 0);
end;

function TObjectBridgeMethodDescriptor.Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
begin
  if Assigned(FMethodCaller) then
    Result := FMethodCaller(ctx, Ptr, argc, argv)
  else
    Result := JS_UNDEFINED;
end;

function TObjectBridgeMethodDescriptor.CanHandle(const AObject: IRegisteredObject): Boolean;
begin
  Result := Assigned(FObjectChecker) and FObjectChecker(AObject);
end;

class function TObjectBridgeMethodDescriptor.CreateInterfaceMethod(const MethodName: string; const TargetInterface: PTypeInfo; const DelphiMethodName: string): IObjectBridgeMethodDescriptor;
begin
  Result := TObjectBridgeMethodDescriptor.Create(
    MethodName,
    // Object checker - check if object type matches target interface
    function(const AObject: IRegisteredObject): Boolean
    begin
      Result := AObject.GetTypeInfo = TargetInterface;
    end,
    // Method caller - simplified version
    function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
    begin
      Result := JS_UNDEFINED;
      // Simplified implementation - actual method calling would need to be implemented
      // based on your specific requirements
    end
  );
end;

class function TObjectBridgeMethodDescriptor.CreateCrossInterfaceMethod(const MethodName: string; const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo; const DelphiMethodName: string): IObjectBridgeMethodDescriptor;
begin
  Result := TObjectBridgeMethodDescriptor.Create(
    MethodName,
    // Object checker - check if object is source interface type
    function(const AObject: IRegisteredObject): Boolean
    begin
      var objectTypeName := LowerCase(string(AObject.GetTypeInfo.Name));
      var sourceInterfaceName := LowerCase(string(SourceInterface.Name));
      Result := objectTypeName = sourceInterfaceName;
    end,
    // Method caller - simplified version
    function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
    begin
      Result := JS_UNDEFINED;
      // Simplified implementation - actual cross-interface method calling would need
      // to be implemented based on your specific requirements
    end
  );
end;

end.
