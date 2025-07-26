unit App.TypeDescriptor.intf;

interface

uses
  System_,
  System.SysUtils,
  System.Collections.Generic,
  App.PropertyDescriptor.intf,
  App.Content.intf;

type
//  {$M+}
//  IPropertyDescriptors = interface(IBaseInterface)
//    ['{979D6ECA-0D0D-4CFA-98B4-33BB432908C0}']
//    procedure AddPropertyDescriptor(const Name: CString; const AProperty : IPropertyDescriptor);
//    function get_Property(const Name: CString) : IPropertyDescriptor;
//
//    property &Property[const Name: CString]: IPropertyDescriptor read get_Property; default;
//  end;

  {$M+}
  ITypeDescriptor = interface(IBaseInterface)
    ['{87B5C48E-38C7-4E6E-874D-0F22D1D5DC00}']
    function  get_Name: CString;
    function  get_Binder: IContentBinder;
    procedure set_Binder(const Value: IContentBinder);
    function  get_Builder: IContentBuilder;
    procedure set_Builder(const Value: IContentBuilder);
    function  get_Provider: IContentProvider;
    procedure set_Provider(const Value: IContentProvider);
    function  get_PropertyDescriptor(const Name: string) : IPropertyDescriptor;

    function  CreateInstance: CObject;

    function  AddPropertyDescriptor(const Name: string; const Value: IPropertyDescriptor) : Boolean;

    property Name: CString read get_Name;
    property Binder: IContentBinder read get_Binder write set_Binder;
    property Builder: IContentBuilder read get_Builder write set_Builder;
    property Provider: IContentProvider read get_Provider write set_Provider;
    property PropertyDescriptor[const Name: string]: IPropertyDescriptor read get_PropertyDescriptor;
  end;

implementation

end.

