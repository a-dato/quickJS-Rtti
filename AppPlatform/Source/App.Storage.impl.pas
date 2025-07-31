unit App.Storage.impl;

interface

uses
  System_,
  App.Storage.intf,
  ADato.ObjectModel.List.intf, System.Collections;

type
  TAppStorage = class(TBaseInterfacedObject, IAppStorage)
  protected
    _dataType: &Type;
    _model: IObjectListModel;
    _name: string;

    function get_DataType: &Type;
    function get_Model: IObjectListModel;
    function get_Name: string;

    function Attach(const Value: IList) : Boolean;
    function Replace(const Value: IList) : Boolean;

  public
    constructor Create(const DataType: &Type; const Name: string);
  end;

implementation

uses
  ADato.ObjectModel.List.Tracking.impl;

{ TAppStorage }

constructor TAppStorage.Create(const DataType: &Type; const Name: string);
begin
  _dataType := DataType;
  _name := Name;
end;

function TAppStorage.Attach(const Value: IList) : Boolean;
begin
  if _model = nil  then
  begin
    _model := TObjectListModelWithChangeTracking<IUnknown>.Create(_dataType);
    _model.Context := Value;

    Result := True;
  end else
    Result := False
end;

function TAppStorage.Replace(const Value: IList) : Boolean;
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

