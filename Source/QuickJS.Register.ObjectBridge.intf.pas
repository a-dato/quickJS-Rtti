unit QuickJS.Register.ObjectBridge.intf;

interface

uses
  System_,
  QuickJS.Register.ObjectBridgeTypes.intf,
  App.PropertyDescriptor.intf,
  QuickJS.Register.intf, System.TypInfo;

type
  IObjectBridgeResolver = interface(IBaseInterface)
    ['{A8E4B2F1-9C5D-4E6A-B3F8-1A2B3C4D5E6F}']
    function OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean): IPropertyDescriptor;

    // Registration methods - now only accept interface descriptors
    procedure AddPropertyDescriptor(const Descriptor: IObjectBridgePropertyDescriptor);
    procedure AddMethodDescriptor(const Descriptor: IObjectBridgeMethodDescriptor);
  end;

implementation

end.
