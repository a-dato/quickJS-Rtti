unit App.Storage.intf;

interface

uses
  System_;

type
  {$M+}
  IAppStorage = interface(IBaseInterface)
    ['{29FCE97C-31A6-4F95-A404-B0747E9DB96F}']
    function get_DataType: &Type;
    function get_Name: string;
    function get_Data: CObject;

    function Attach(const Value: CObject) : Boolean;

    property Name: string read get_Name;
    property DataType: &Type read get_DataType;
    property Data: CObject read get_Data;
  end;

implementation

end.

