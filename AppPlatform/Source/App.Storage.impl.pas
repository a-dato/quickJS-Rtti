unit App.Storage.impl;

interface

uses
  System_,
  App.Storage.intf,
  ADato.ObjectModel.List.intf;

type
  TAppStorage = class(TBaseInterfacedObject, IAppStorage)
  protected
    _dataType: &Type;
    _model: IObjectListModel;
    _name: string;

    function get_DataType: &Type;
    function get_Model: IObjectListModel;
    function get_Name: string;

    function Attach(const Value: CObject) : Boolean;
    function Replace(const Value: CObject) : Boolean;

  public
    constructor Create(const DataType: &Type; const Name: string);
  end;

implementation

uses
  System.Collections,
  ADato.ObjectModel.List.Tracking.impl;

{ TAppStorage }

constructor TAppStorage.Create(const DataType: &Type; const Name: string);
begin
  _dataType := DataType;
  _name := Name;
end;

function TAppStorage.Attach(const Value: CObject) : Boolean;
begin
  if _model = nil  then
  begin
    var list: IList;
    if not Value.TryAsType<IList>(list) then
      raise ArgumentException.Create('Data must implement IList interface');

    _model := TObjectListModelWithChangeTracking<IUnknown>.Create(_dataType);
    _model.Context := list;

    Result := True;
  end else
    Result := False
end;

function TAppStorage.Replace(const Value: CObject) : Boolean;
begin

end;

function TAppStorage.get_DataType: &Type;
begin
  Result := _dataType;
end;

function TAppStorage.get_Model: IObjectListModel;
begin
  Result := _model;
end;

function TAppStorage.get_Name: string;
begin
  Result := _name;
end;

end.

