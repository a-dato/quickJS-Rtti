unit App.Config.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Windows.intf,
  App.TypeDescriptor.intf,
  App.PropertyDescriptor.intf;

type
  TTypeRegisteredEvent = procedure(const AType: &Type; const TypeDescriptor: ITypeDescriptor) of object;

  {$M+}
  IAppConfig = interface(IBaseInterface)
    ['{A3009BCA-0053-40AD-9C50-45EDB0793C48}']
    function get_Types: List<&Type>;

    function AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type; const Descriptor: IPropertyDescriptor) : _PropertyInfo;
    function WrapProperty(const AProperty: _PropertyInfo) : _PropertyInfo;

    procedure RegisterType(const AType: &Type; const ObjectType: ITypeDescriptor);
    procedure RegisterWindow(const Name: string; const CreateFunc: TFrameCreateFunc);

    function  FindType(const Name: string; out AType: &Type) : Boolean;
    function  FullName(const AType: &Type) : string;
    function  TypeIsRegistered(const AType: &Type) : Boolean;
    function  TypeByName(const Name: string) : &Type;
    function  TypeDescriptorByName(const Name: string) : ITypeDescriptor;
    function  TypeDescriptor(const AType: &Type): ITypeDescriptor;

    function  TryGetWindowType(const Name: string; out WindowType : IWindowType) : Boolean;

    procedure AddOnTypeRegistered(const Handler: TTypeRegisteredEvent);
    procedure RemoveOnTypeRegistered(const Handler: TTypeRegisteredEvent);

    property  Types: List<&Type> read get_Types;
  end;

implementation

end.

