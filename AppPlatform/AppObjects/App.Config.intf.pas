unit App.Config.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Windows.intf,
  App.Objects.intf,
  QuickJS.Register.dn4d.intf,
  App.PropertyDescriptor.intf;

type
  {$M+}
  IAppConfig = interface(IBaseInterface)
    ['{A3009BCA-0053-40AD-9C50-45EDB0793C48}']
    function get_Types: List<&Type>;
    function get_ObjectType(const AType: &Type): IObjectType;

    function AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type; const Descriptor: IPropertyDescriptor) : _PropertyInfo;

    procedure RegisterType(const AType: &Type; const ObjectType: IObjectType);

    function  TypeByName(const Name: string) : &Type;

    property Types: List<&Type> read get_Types;
    property ObjectType[const AType: &Type]: IObjectType read get_ObjectType;
  end;

implementation

end.
