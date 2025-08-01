unit App.Storage.impl;

interface

uses
  System_,
  App.Storage.intf;

type
  TAppStorage = class(TBaseInterfacedObject, IAppStorage)
  protected
    _data: CObject;
    _dataType: &Type;
    _name: string;

    function get_Data: CObject;
    function get_DataType: &Type;
    function get_Name: string;

    function Attach(const Value: CObject) : Boolean;

  public
    constructor Create(const DataType: &Type; const Name: string);
  end;

implementation

{ TAppStorage }

constructor TAppStorage.Create(const DataType: &Type; const Name: string);
begin
  _dataType := DataType;
  _name := Name;
end;

function TAppStorage.Attach(const Value: CObject) : Boolean;
begin
  _data := Value;
end;

function TAppStorage.get_DataType: &Type;
begin
  Result := _dataType;
end;

function TAppStorage.get_Data: CObject;
begin
  Result := _data;
end;

function TAppStorage.get_Name: string;
begin
  Result := _name;
end;

end.

