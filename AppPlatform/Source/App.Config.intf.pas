unit App.Config.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Windows.intf,
  App.TypeDescriptor.intf,
  QuickJS.Register.dn4d.intf,
  App.PropertyDescriptor.intf;

type
  {$M+}
  IAppConfig = interface(IBaseInterface)
    ['{A3009BCA-0053-40AD-9C50-45EDB0793C48}']
    function get_Types: List<&Type>;

    function AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type; const Descriptor: IPropertyDescriptor) : _PropertyInfo;

    procedure RegisterType(const AType: &Type; const ObjectType: ITypeDescriptor);
    procedure RegisterWindow(const AType: &Type; const Name: string; const CreateFunc: WindowCreateFunc);

    function  TypeByName(const Name: string) : &Type;
    function  TypeDescriptorByName(const Name: string) : ITypeDescriptor;
    function  TypeDescriptor(const AType: &Type): ITypeDescriptor;

    property  Types: List<&Type> read get_Types;
  end;

implementation

end.

