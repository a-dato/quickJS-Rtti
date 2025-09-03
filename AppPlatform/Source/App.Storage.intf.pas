unit App.Storage.intf;

interface

uses
  System_, System.Collections;

type
  {$M+}
  IAppStorage = interface(IBaseInterface)
    ['{29FCE97C-31A6-4F95-A404-B0747E9DB96F}']
    function get_DataType: &Type;
    function get_Name: string;
    function get_Data: IList;

    function  Attach(const Value: IList) : Boolean;

    function  Add(const Item: CObject) : Integer;
    function  Remove(const Item: CObject) : Boolean;
    procedure RemoveAt(const Index: Integer);
    function  Replace(const Item: CObject; const NewItem: CObject) : Boolean;
    procedure ReplaceAt(const Index: Integer; const NewItem: CObject);

    property Name: string read get_Name;
    property DataType: &Type read get_DataType;
    property Data: IList read get_Data;
  end;

implementation

end.

