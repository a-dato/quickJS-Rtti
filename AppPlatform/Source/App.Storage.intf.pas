unit App.Storage.intf;

interface

uses
  System_,
  ADato.ObjectModel.List.intf;

type
  {$M+}
  IAppStorage = interface(IBaseInterface)
    ['{29FCE97C-31A6-4F95-A404-B0747E9DB96F}']
    function get_DataType: &Type;
    function get_Model: IObjectListModel;
    function get_Name: string;

    function Attach(const Value: CObject) : Boolean;
    function Replace(const Value: CObject) : Boolean;

    property Model: IObjectListModel read get_Model;
    property Name: string read get_Name;
    property DataType: &Type read get_DataType;
  end;

implementation

end.

