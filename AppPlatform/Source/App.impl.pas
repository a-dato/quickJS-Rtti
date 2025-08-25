unit App.impl;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic,
  quickjs_ng,
  QuickJS.Register.intf,
  App.intf,
  App.Config.intf,
  App.Storage.intf,
  App.Windows.intf,
  App.Environment.intf,
  ADato.ObjectModel.List.intf,
  App.PropertyDescriptor.intf,
  ADato.AI.SpaceAccessor.intf;
  App.Factory.intf;

type
  TAppObject = class(TBaseInterfacedObject, IAppObject, IJSExtendableObject)
  protected
    _Config: IAppConfig;
    _Environment: IEnvironment;
    _Factory: IAppFactory;
    _Windows: IWindows;
    _storage: Dictionary<string, IAppStorage>;
    _SpaceAccessor: ISpaceAccessor;
    _extendabePropertyValues: Dictionary<string, JSValue>;

    // IAppObject
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Factory: IAppFactory;
    function get_Storage(const Name: string): IAppStorage;
    function get_Windows: IWindows;

    function  AddStorage(const DataType: &Type; const Name: string) : IAppStorage;
    function  HasStorage(const Name: string): Boolean;
    function  TryGetStorage(const Name: string; out Value: IAppStorage) : Boolean;
    function  RemoveStorage(const Name: string) : Boolean;

    // IJSExtendableObject
    function  define_own_property(Ctx: JSContext; const Name: string) : Boolean;
    function  GetValue(Ctx: JSContext; const Name: string): JSValue;
    procedure SetValue(Ctx: JSContext; const Name: string; Value: JSValue);

    function get_SpaceAccessor: ISpaceAccessor;
  public
    constructor Create(const Environment: IEnvironment);
  end;

var
  DataList: IList;

implementation

uses
  App.TypeDescriptor.intf,
  App.Config.impl,
  App.Windows.impl, App.Storage.impl,
  ADato.AI.SpaceAccessor.impl;
  App.Factory.impl;
{ TAppObject }

constructor TAppObject.Create(const Environment: IEnvironment);
begin
  _Environment := Environment;
  _Factory := TAppFactory.Create;
  _Config := TAppConfig.Create;
  _Windows := Windows.Create;
  _storage := CDictionary<string, IAppStorage>.Create;
  _extendabePropertyValues := CDictionary<string, JSValue>.Create;
end;

function TAppObject.AddStorage(const DataType: &Type; const Name: string) : IAppStorage;
begin
  var storage: IAppStorage := TAppStorage.Create(DataType, Name);
  _storage[Name] := storage;
  Result := storage;
end;

function TAppObject.define_own_property(Ctx: JSContext; const Name: string): Boolean;
begin
  Result := True;
end;

function TAppObject.GetValue(Ctx: JSContext; const Name: string): JSValue;
begin
  var val: JSValue;
  if _extendabePropertyValues.TryGetValue(Name, val) then
    Result := JS_DupValue(Ctx, val) else
    Result := JS_UNDEFINED;
end;

function TAppObject.get_Config: IAppConfig;
begin
  Result := _Config;
end;

function TAppObject.get_Environment: IEnvironment;
begin
  Result := _Environment;
end;

function TAppObject.get_Factory: IAppFactory;
begin

end;

function TAppObject.get_Storage(const Name: string): IAppStorage;
begin
  if not _storage.TryGetValue(Name, Result) then
    raise ArgumentException.Create(CString.Format('Storage ''{0}'' does not exist', Name));
end;

function TAppObject.get_Windows: IWindows;
begin
  Result := _Windows;
end;

function TAppObject.HasStorage(const Name: string): Boolean;
begin
  Result := _storage.ContainsKey(Name);
end;

function TAppObject.RemoveStorage(const Name: string): Boolean;
begin
  Result := _storage.Remove(Name);
end;

function TAppObject.get_SpaceAccessor: ISpaceAccessor;
begin
  if (_SpaceAccessor = nil) then
  begin
    _SpaceAccessor := TSpaceAccessor.Create();
  end;

  Result := _SpaceAccessor;
end;

procedure TAppObject.SetValue(Ctx: JSContext; const Name: string; Value: JSValue);
begin
  var val: JSValue;
  if _extendabePropertyValues.TryGetValue(Name, val) then
    JS_FreeValue(Ctx, val);

  if JS_IsUndefined(Value) then
    _extendabePropertyValues.Remove(Name) else
    _extendabePropertyValues[Name] := JS_DupValue(Ctx, Value);
end;

function TAppObject.TryGetStorage(const Name: string; out Value: IAppStorage): Boolean;
begin
  Result := _storage.TryGetValue(Name, Value);
end;

end.

