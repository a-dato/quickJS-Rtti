unit App.TypeDescriptor.intf;

interface

uses
  System_,
  System.SysUtils,
  System.Collections.Generic,
  App.PropertyDescriptor.intf,
  App.Content.intf,
  App.TypeMetadata;

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
    function  get_ClassName: CString;
    function  get_FullName: CString;
    function  get_StorageName: CString;
    function  get_Binder: IContentBinder;
    procedure set_Binder(const Value: IContentBinder);
    function  get_Provider: IContentProvider;
    procedure set_Provider(const Value: IContentProvider);
    function  get_PropertyDescriptor(const Name: string) : IPropertyDescriptor;
    {$IFDEF APP_PLATFORM_MD}
    function  get_TypeInterface: ISupportedInterfaceMetadata;
    function  get_SupportedInterfaces: List<ISupportedInterfaceMetadata>;
    {$ENDIF}

    {$IFDEF APP_PLATFORM_MD}
    function  GetType: &Type;
    procedure AddSupportedInterface(const AType: &Type);
    {$ENDIF}

    function  AddPropertyDescriptor(const Name: string; const Value: IPropertyDescriptor) : Boolean;

    property ClassName: CString read get_ClassName;
    property FullName: CString read get_FullName;
    property StorageName: CString read get_StorageName;
    property Binder: IContentBinder read get_Binder write set_Binder;
    property Provider: IContentProvider read get_Provider write set_Provider;
    property PropertyDescriptor[const Name: string]: IPropertyDescriptor read get_PropertyDescriptor;
    {$IFDEF APP_PLATFORM_MD}
    property TypeInterface: ISupportedInterfaceMetadata read get_TypeInterface;
    property SupportedInterfaces: List<ISupportedInterfaceMetadata> read get_SupportedInterfaces;
    {$ENDIF}
  end;

implementation

end.

