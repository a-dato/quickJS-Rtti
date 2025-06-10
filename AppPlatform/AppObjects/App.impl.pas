unit App.impl;

interface

uses
  System_,
  App.intf,
  App.Config.intf,
  App.Windows.intf,
  App.Environment.intf,
  QuickJS.Register.dn4d.intf, ADato.ObjectModel.List.intf, System.Collections;

type
  TAppObject = class(TBaseInterfacedObject, IAppObject)
  protected
    _Config: IAppConfig;
    _Environment: IEnvironment;
    _Windows: IWindows;

    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Windows: IWindows;

    function  Test(const Value: JSObjectReference) : Boolean;

  public
    constructor Create(const Environment: IEnvironment);
  end;

  TDataObject = class(TBaseINterfacedObject, IDataObject)
  protected
    _Name: string;

    function  get_Name: string;
    procedure set_Name(const Value: string);

  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  DataList: IList;

implementation

uses
  App.Objects.intf,
  App.Config.impl,
  App.Windows.impl;

{ TAppObject }

constructor TAppObject.Create(const Environment: IEnvironment);
begin
  _Environment := Environment;
  _Config := TAppConfig.Create;
  _Windows := Windows.Create(Self);
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

function TAppObject.Test(const Value: JSObjectReference) : Boolean;
begin
  var descr := Value.Invoke<IPropertyDescriptors>('PropertyDescriptors');
  if descr <> nil then
  begin
    var prop := descr['ID'];
    var v := prop.Visible;
    if v then;

  end;

  // var n := descr.Invoke('ID', TypeInfo(IJSObjectReference));

end;

{ TDataObject }

constructor TDataObject.Create;
begin

end;

destructor TDataObject.Destroy;
begin
  inherited;
end;

function TDataObject.get_Name: string;
begin
  Result := _Name;
end;

procedure TDataObject.set_Name(const Value: string);
begin
  _Name := Value;
end;

end.
