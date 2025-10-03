unit QuickJS.Register.ObjectBridgeTypes.intf;

interface

uses
  QuickJS.Register.intf,
  System.TypInfo, System.Rtti, quickjs_ng;

type
  // Lambda types for descriptor functionality
  TObjectChecker = reference to function(const AObject: IRegisteredObject): Boolean;
  TPropertyGetter = reference to function(const Ptr: Pointer): TValue;
  TPropertySetter = reference to procedure(const Ptr: Pointer; const Value: TValue);
  TMethodCaller = reference to function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
  TExtensionGetter = reference to function(const Target: IInterface): TValue;
  TPatternChecker = reference to function(const AObject: IRegisteredObject): Boolean;
  
  // Simplified lambda types for typed helpers (already casted and nil-checked)
  TTypedPropertyGetter = reference to function(const Instance: IInterface): TValue;
  TTypedPropertySetter = reference to procedure(const Instance: IInterface; const Value: TValue);
  TTypedMethodCaller = reference to function(ctx: JSContext; const Instance: IInterface; argc: Integer; argv: PJSValueConst): JSValue;
  
  // Generic lambda types - the Instance parameter is the actual interface type T
  TTypedPropertyGetter<T: IInterface> = reference to function(const Instance: T): TValue;
  TTypedPropertySetter<T: IInterface> = reference to procedure(const Instance: T; const Value: TValue);
  TTypedMethodCaller<T: IInterface> = reference to function(ctx: JSContext; const Instance: T; argc: Integer; argv: PJSValueConst): JSValue;

  // Descriptor interfaces
  IObjectBridgePropertyDescriptor = interface(IPropertyDescriptor)
    ['{B1E2F3A4-5C6D-7E8F-9A0B-1C2D3E4F5A6B}']
    function get_PropertyName: string;
    function CanHandle(const AObject: IRegisteredObject): Boolean;
    property PropertyName: string read get_PropertyName;
  end;

  IObjectBridgeMethodDescriptor = interface(IPropertyDescriptor)
    ['{C2F3A4B5-6D7E-8F9A-0B1C-2D3E4F5A6B7C}']
    function get_MethodName: string;
    function CanHandle(const AObject: IRegisteredObject): Boolean;
    property MethodName: string read get_MethodName;
  end;

  // Collection types for descriptors
  TDescriptorList = array of IPropertyDescriptor;

implementation

end.
