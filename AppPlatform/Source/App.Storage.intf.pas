unit App.Storage.intf;

interface

uses
  System_, System.Collections;

type
  {$M+}
  IStorage = interface(IList)
    ['{29FCE97C-31A6-4F95-A404-B0747E9DB96F}']
    function get_DataType: &Type;
    function get_Name: string;
    function get_Data: IList;

    function  Attach(const Value: IList) : Boolean;
//
//    function  Add(const Item: CObject) : Integer;
//    function  Remove(const Item: CObject) : Boolean;
//    procedure RemoveAt(const Index: Integer);
//    function  Replace(const Item: CObject; const NewItem: CObject) : Boolean;
//    procedure ReplaceAt(const Index: Integer; const NewItem: CObject);

    property Name: string read get_Name;
    property DataType: &Type read get_DataType;
    property Data: IList read get_Data;
  end;

  TMarshalFunc = reference to function(const Context: CObject; const Item: CObject) : string;
  TUnMarshalFunc = reference to function(const Context: CObject; const Item: string) : CObject;

  IMarshalledStorage = interface(IBaseInterface)
    ['{0C27D84D-0258-4BCB-88D8-161F2EE9082E}']
    function get_MarshalFunc: TMarshalFunc;
    function get_UnMarshalFunc: TUnMarshalFunc;

    property MarshalFunc: TMarshalFunc read get_MarshalFunc;
    property UnMarshalFunc: TUnMarshalFunc read get_UnMarshalFunc;
  end;

  IStorageSupport = interface(IBaseInterface)
    ['{508B8F8E-31ED-44E1-A88F-84C1B93FF0B6}']
    function get_Storage(const Name: string): IStorage;

    procedure AddStorage(const Storage : IStorage); overload;
    function  AddStorage(const DataType: &Type; const Name: string) : IStorage; overload;
    function  HasStorage(const Name: string): Boolean;
    function  TryGetStorage(const Name: string; out Value: IStorage) : Boolean;
    function  RemoveStorage(const Name: string) : Boolean;

    property Storage[const Value: string]: IStorage read get_Storage;
  end;

implementation

end.

