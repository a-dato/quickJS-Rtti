unit App.Storage.impl;

interface

uses
  System_,
  System.Collections,
  App.Storage.intf;

type
  TAppStorage = class(TBaseInterfacedObject, IAppStorage)
  protected
    _data: IList;
    _dataType: &Type;
    _name: string;

    function get_Data: IList; virtual;
    function get_DataType: &Type; virtual;
    function get_Name: string; virtual;

    function  Add(const Item: CObject) : Integer; virtual;
    function  Remove(const Item: CObject) : Boolean; virtual;
    procedure RemoveAt(const Index: Integer); virtual;
    function  Replace(const Item: CObject; const NewItem: CObject) : Boolean; virtual;
    procedure ReplaceAt(const Index: Integer; const NewItem: CObject); virtual;

    function Attach(const Value: IList) : Boolean; virtual;

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

function TAppStorage.Add(const Item: CObject): Integer;
begin
  if _data <> nil then
    Result := _data.Add(Item) else
    Result := -1;
end;

function TAppStorage.Attach(const Value: IList) : Boolean;
begin
  _data := Value;
end;

function TAppStorage.get_DataType: &Type;
begin
  Result := _dataType;
end;

function TAppStorage.get_Data: IList;
begin
  Result := _data;
end;

function TAppStorage.get_Name: string;
begin
  Result := _name;
end;

function TAppStorage.Remove(const Item: CObject): Boolean;
begin
  if _data <> nil then
    Result := _data.Remove(Item) else
    Result := False;
end;

procedure TAppStorage.RemoveAt(const Index: Integer);
begin
  if _data <> nil then
    _data.RemoveAt(Index);
end;

function TAppStorage.Replace(const Item, NewItem: CObject): Boolean;
begin
  Result := False;

  if _data <> nil then
  begin
    var i := _data.IndexOf(Item);
    if i <> -1 then
    begin
      _data[i] := NewItem;
      Result := True;
    end;
  end;
end;

procedure TAppStorage.ReplaceAt(const Index: Integer; const NewItem: CObject);
begin
  if _data <> nil then
    _data[Index] := NewItem;
end;

end.

