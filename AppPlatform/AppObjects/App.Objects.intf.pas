unit App.Objects.intf;

interface

uses
  System_,
  System.SysUtils,
  System.Collections.Generic,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.Content.intf;

type
  EditorType = (Text, Edit, Date, Time, DateTime, Check, Memo, Number, Combo, ComboEdit, Tags, Color, CustomEditor);

  // Needed for Lynx-x
  TEditorType = EditorType;

  {$M+}
  IPicklist = interface(IBaseInterface)
    ['{4089066A-409D-44D7-8771-42088ADA36A1}']
    function Items(const Filter: CString) : CObject; overload; // List, array or else...
    function Items(const Pred: Predicate<CObject>) : CObject; overload; // List, array or else...
    function Format(const Item: CObject) : CString;
  end;

  {$M+}
  IPropertyDescriptor = interface(IBaseInterface)
    ['{95C029CF-190D-4844-A601-1B4806AF16A1}']
    function  get_Picklist: IPicklist;
    procedure set_Picklist(const Value: IPicklist);
    function  get_EditorType: EditorType;
    procedure set_EditorType(const Value: EditorType);
    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);

    property EditorType: EditorType read get_EditorType write set_EditorType;
    property Picklist: IPicklist read get_Picklist write set_Picklist;
    property Visible: Boolean read get_Visible write set_Visible;
  end;

  {$M+}
  IPropertyDescriptors = interface(IBaseInterface)
    ['{979D6ECA-0D0D-4CFA-98B4-33BB432908C0}']
    procedure AddPropertyDescriptor(const Name: CString; const AProperty : IPropertyDescriptor);
    function get_Property(const Name: CString) : IPropertyDescriptor;

    property &Property[const Name: CString]: IPropertyDescriptor read get_Property; default;
  end;

  {$M+}
  IObjectType = interface(IBaseInterface)
    ['{87B5C48E-38C7-4E6E-874D-0F22D1D5DC00}']
    function  get_Name: CString;
    function  get_Binder: IContentBinder;
    procedure set_Binder(const Value: IContentBinder);
    function  get_Builder: IContentBuilder;
    procedure set_Builder(const Value: IContentBuilder);
    function  get_Provider: IContentProvider;
    procedure set_Provider(const Value: IContentProvider);
    function  get_PropertyDescriptor: IPropertyDescriptors;
    procedure set_PropertyDescriptor(const Value: IPropertyDescriptors);

    property Name: CString read get_Name;
    property Binder: IContentBinder read get_Binder write set_Binder;
    property Builder: IContentBuilder read get_Builder write set_Builder;
    property Provider: IContentProvider read get_Provider write set_Provider;
    property PropertyDescriptor: IPropertyDescriptors read get_PropertyDescriptor write set_PropertyDescriptor;
  end;

implementation

end.
