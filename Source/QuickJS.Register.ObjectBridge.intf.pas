unit QuickJS.Register.ObjectBridge.intf;

interface

uses
  QuickJS.Register.ObjectBridgeTypes.intf,
  QuickJS.Register.intf, System.TypInfo;

type
  IObjectBridgeResolver = interface
    ['{A8E4B2F1-9C5D-4E6A-B3F8-1A2B3C4D5E6F}']
    function OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean): IPropertyDescriptor;

    // Registration methods - now only accept interface descriptors
    procedure AddPropertyDescriptor(const Descriptor: IObjectBridgePropertyDescriptor);
    procedure AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
    
    // Interface mapping - allows registering a more general interface to be treated as a more specific one
    // Example: Map IBaseInterface -> IUser (if the object supports IUser, use that instead)
    procedure AddInterfaceMapping(const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo);
    
    // Resolves the best interface to use for a given interface instance
    // Returns the target interface TypeInfo if a mapping exists and the object supports it, otherwise returns the source
    function ResolveInterfaceMapping(const SourceInterface: PTypeInfo; const SourceInterfaceInstance: IInterface): PTypeInfo;
  end;

implementation

end.
