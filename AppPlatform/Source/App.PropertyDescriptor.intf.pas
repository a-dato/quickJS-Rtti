unit App.PropertyDescriptor.intf;

interface

uses
  System_,
  System.SysUtils,
  System.Collections.Generic;

type
  EditorType = (Text, Edit, Date, Time, DateTime, Check, Memo, Number, Combo, ComboEdit, Tags, Color, CustomEditor);

  {$M+}
  IPicklist = interface(IBaseInterface)
    ['{4089066A-409D-44D7-8771-42088ADA36A1}']
    function Items(const Filter: CString) : CObject; overload; // List, array or else...
    function Items(const Pred: Predicate<CObject>) : CObject; overload; // List, array or else...
  end;

  {$M+}
  IFormatter = interface
    function Format(const Context: CObject; const Item: CObject; const Format: CString) : CString;
    function Url(const Context: CObject; const Value: CObject) : CString;
  end;

  {$M+}
  IMarshaller = interface
    function Marshal(const Context: CObject; const Value: CObject) : CObject;
    function Unmarshal(const Context: CObject; const Value: CObject) : CObject;
  end;

  {$M+}
  INotify = interface
    function  OnChanging(const Context: CObject; const Value: CObject) : CObject;
    procedure OnChanged(const Context: CObject; const Value: CObject);
  end;

  {$M+}
  INotifyPropertyChanged = interface
    procedure Add(const Value: INotify);
    procedure Remove(const Value: INotify);
  end;

  {$M+}
  IPropertyDescriptor = interface(IBaseInterface)
    ['{95C029CF-190D-4844-A601-1B4806AF16A1}']
    function  get_EqualityComparer: IEqualityComparer;
    // function  get_Flags: DescriptorFlags;
    function  get_Formatter: IFormatter;
    procedure set_Formatter(const Value: IFormatter);
    function  get_IsCollectionProperty: Boolean;
    function  get_Marshaller: IMarshaller;
    procedure set_Marshaller(const Value: IMarshaller);
    function  get_Notify: INotify;
    procedure set_Notify(const Value: INotify);
    function  get_Picklist: IPicklist;
    procedure set_Picklist(const Value: IPicklist);
    function  get_EditorType: EditorType;
    procedure set_EditorType(const Value: EditorType);
    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);

    property EditorType: EditorType read get_EditorType write set_EditorType;
    property EqualityComparer: IEqualityComparer read get_EqualityComparer;
    property IsCollectionProperty: Boolean read get_IsCollectionProperty;
    property Formatter: IFormatter read get_Formatter write set_Formatter;
    property Marshaller: IMarshaller read get_Marshaller write set_Marshaller;
    property Notify: INotify read get_Notify write set_Notify;
    property Picklist: IPicklist read get_Picklist write set_Picklist;
    property Visible: Boolean read get_Visible write set_Visible;
  end;

  ICollectionPropertyDescriptor = interface(IBaseInterface)

  end;

implementation

end.

