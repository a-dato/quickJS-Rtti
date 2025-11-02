unit App.impl;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic,
  
  QuickJS.Register.intf,
  App.Base.intf,
  App.intf,
  App.Config.intf,
  App.Storage.impl,
  App.Windows.intf,
  App.Environment.intf,
  ADato.ObjectModel.List.intf,
  App.PropertyDescriptor.intf,
  App.Factory.intf, quickjs_ng;

type
  TAppObject = class(TStorageSupport, IAppObject, IConverterSupport, IJSExtendableObject)
  protected
    _Config: IAppConfig;
    _Environment: IEnvironment;
    _Windows: IWindows;
    _typeConverter: ITypeConverter;
    _extendabePropertyValues: Dictionary<string, JSValue>;

    function AsType(const AType: &Type) : CObject; override;

    // IAppObject
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Factory: IAppFactory;
    function get_Windows: IWindows;

    // IConverterSupport
    function  get_Converter: ITypeConverter;
    procedure set_Converter(const Value: ITypeConverter);

    // IJSExtendableObject
    function  define_own_property(Ctx: JSContext; const Name: string) : Boolean;
    function  GetValue(Ctx: JSContext; const Name: string): JSValue;
    procedure SetValue(Ctx: JSContext; const Name: string; Value: JSValue);
  public
    constructor Create(const Environment: IEnvironment);
  end;

var
  DataList: IList;

implementation

uses
  App.TypeDescriptor.intf,
  App.Config.impl,
  App.Windows.impl,
  App.Factory.impl;
{ TAppObject }

constructor TAppObject.Create(const Environment: IEnvironment);
begin
  inherited Create;

  TAppFactory.Instance := TAppFactory.Create;

  _Environment := Environment;
  _Config := TAppConfig.Create;
  _Windows := Windows.Create;
  _extendabePropertyValues := CDictionary<string, JSValue>.Create;
end;

function TAppObject.AsType(const AType: &Type) : CObject;
begin
  if (_typeConverter = nil) or not _typeConverter.TryAsType(AType, Result) then
    Result := inherited;
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

function TAppObject.get_Converter: ITypeConverter;
begin
  Result := _typeConverter;
end;

function TAppObject.get_Environment: IEnvironment;
begin
  Result := _Environment;
end;

function TAppObject.get_Factory: IAppFactory;
begin
  Result := TAppFactory.Instance;
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

procedure TAppObject.set_Converter(const Value: ITypeConverter);
begin
  _typeConverter := Value;
end;

end.

