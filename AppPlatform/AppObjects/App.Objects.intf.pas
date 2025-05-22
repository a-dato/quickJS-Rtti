unit App.Objects.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.Content.intf;

type
  {$M+}
  IObjectType = interface(IBaseInterface)

    function  get_Name: CString;
    function  get_Binder: IContentBinder;
    procedure set_Binder(const Value: IContentBinder);
    function  get_Builder: IContentBuilder;
    procedure set_Builder(const Value: IContentBuilder);
    function  get_Provider: IContentProvider;
    procedure set_Provider(const Value: IContentProvider);

    property Name: CString read get_Name;
    property Binder: IContentBinder read get_Binder write set_Binder;
    property Builder: IContentBuilder read get_Builder write set_Builder;
    property Provider: IContentProvider read get_Provider write set_Provider;
  end;

implementation

end.
