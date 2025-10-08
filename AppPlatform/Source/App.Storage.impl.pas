unit App.Storage.impl;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic,
  System.Collections.ListInterface.impl,
  App.Storage.intf;

type
  TStorage = class(TVirtualListBase, IStorage)
  protected
    _data: IList;
    _dataType: &Type;
    _name: string;

    function Attach(const Value: IList) : Boolean; virtual;

    function get_Data: IList; virtual;
    function get_DataType: &Type; virtual;
    function get_Name: string; virtual;

    function  get_InnerType: &Type; override;
    function  get_Count: Integer; override;

    function  get_Item_object(Index: Integer): CObject; override;
    procedure set_Item_object(Index: Integer; const Value: CObject); override;

    function  Add(const Value: CObject): Integer; override;
    procedure Clear; override;
    function  Contains(const Value: CObject): Boolean; override;

  public
    constructor Create(const DataType: &Type; const Name: string);
  end;

  TMarshalledStorage = class(TStorage, IMarshalledStorage)
  protected
    _MarshalFunc: TMarshalFunc;
    _UnMarshalFunc: TUnMarshalFunc;

    function get_MarshalFunc: TMarshalFunc;
    function get_UnMarshalFunc: TUnMarshalFunc;
  end;

  TStorageSupport = class(TBaseInterfacedObject, IStorageSupport)
  protected
    _storage: Dictionary<string, IStorage>;

    function  CreateStorage(const DataType: &Type; const Name: string): IStorage; virtual;
    function  get_Storage(const Name: string): IStorage;

    procedure AddStorage(const Storage : IStorage); overload;
    function  AddStorage(const DataType: &Type; const Name: string) : IStorage; overload;
    function  HasStorage(const Name: string): Boolean;
    function  TryGetStorage(const Name: string; out Value: IStorage) : Boolean;
    function  RemoveStorage(const Name: string) : Boolean;

  public
    constructor Create;
  end;

implementation

procedure TStorage.Clear;
begin
  inherited;

end;

function TStorage.Contains(const Value: CObject): Boolean;
begin

end;

{ TAppStorage }

constructor TStorage.Create(const DataType: &Type; const Name: string);
begin
  _dataType := DataType;
  _name := Name;
end;

function TStorage.Add(const Value: CObject): Integer;
begin
  if _data <> nil then
    Result := _data.Add(Value) else
    Result := -1;
end;

function TStorage.Attach(const Value: IList) : Boolean;
begin
  _data := Value;
end;

function TStorage.get_DataType: &Type;
begin
  Result := _dataType;
end;

function TStorage.get_Count: Integer;
begin
  if _data <> nil then
    Result := _data.Count else
    Result := -1;
end;

function TStorage.get_InnerType: &Type;
begin
  Result := _dataType;
end;

function TStorage.get_Item_object(Index: Integer): CObject;
begin
  if _data <> nil then
    Result := _data[Index];
end;

function TStorage.get_Data: IList;
begin
  Result := _data;
end;

function TStorage.get_Name: string;
begin
  Result := _name;
end;

procedure TStorage.set_Item_object(Index: Integer; const Value: CObject);
begin
  if _data <> nil then
    _data[Index] := Value;
end;

constructor TStorageSupport.Create;
begin
  _storage := CDictionary<string, IStorage>.Create;
end;

function TStorageSupport.CreateStorage(const DataType: &Type; const Name: string): IStorage;
begin
  Result := TStorage.Create(DataType, Name);
end;

procedure TStorageSupport.AddStorage(const Storage: IStorage);
begin
  _storage[Storage.Name] := Storage;
end;

function TStorageSupport.AddStorage(const DataType: &Type; const Name: string) : IStorage;
begin
  var storage: IStorage := CreateStorage(DataType, Name);
  _storage[Name] := storage;
  Result := storage;
end;

function TStorageSupport.get_Storage(const Name: string): IStorage;
begin
  if not _storage.TryGetValue(Name, Result) then
    raise ArgumentException.Create(CString.Format('Storage ''{0}'' does not exist', Name));
end;

function TStorageSupport.HasStorage(const Name: string): Boolean;
begin
  Result := _storage.ContainsKey(Name);
end;

function TStorageSupport.RemoveStorage(const Name: string): Boolean;
begin
  Result := _storage.Remove(Name);
end;

function TStorageSupport.TryGetStorage(const Name: string; out Value: IStorage): Boolean;
begin
  Result := _storage.TryGetValue(Name, Value);
end;

{ TMarshalledStorage }

function TMarshalledStorage.get_MarshalFunc: TMarshalFunc;
begin
  Result := _MarshalFunc;
end;

function TMarshalledStorage.get_UnMarshalFunc: TUnMarshalFunc;
begin
  Result := _UnMarshalFunc;
end;

end.

