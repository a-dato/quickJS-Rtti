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
  App.Windows.intf,
  App.Environment.intf,
  ADato.ObjectModel.List.intf,
  App.PropertyDescriptor.intf;

type
  TAppObject = class(TBaseInterfacedObject, IAppObject, IJSExtendableObject)
  protected
    _Config: IAppConfig;
    _Environment: IEnvironment;
    _Windows: IWindows;
    _extendabePropertyValues: Dictionary<string, JSValue>;

    // IJSExtendableObject
    function  define_own_property(Ctx: JSContext; const Name: string) : Boolean;
    function  GetValue(Ctx: JSContext; const Name: string): JSValue;
    procedure SetValue(Ctx: JSContext; const Name: string; Value: JSValue);

    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Windows: IWindows;
  public
    constructor Create(const Environment: IEnvironment);
  end;

var
  DataList: IList;

implementation

uses
  App.TypeDescriptor.intf,
  App.Config.impl,
  App.Windows.impl;

{ TAppObject }

constructor TAppObject.Create(const Environment: IEnvironment);
begin
  _Environment := Environment;
  _Config := TAppConfig.Create;
  _Windows := Windows.Create;
  _extendabePropertyValues := CDictionary<string, JSValue>.Create;
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

function TAppObject.get_Windows: IWindows;
begin
  Result := _Windows;
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

end.

