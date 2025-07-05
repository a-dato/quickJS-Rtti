unit App.impl;

interface

uses
  System_,
  App.intf,
  App.Config.intf,
  App.Windows.intf,
  App.Environment.intf,
  QuickJS.Register.dn4d.intf, ADato.ObjectModel.List.intf, System.Collections,
  App.PropertyDescriptor.intf;

type
  TAppObject = class(TBaseInterfacedObject, IAppObject)
  protected
    _Config: IAppConfig;
    _Environment: IEnvironment;
    _Windows: IWindows;

    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Windows: IWindows;

    function  Test(const Descriptor: IPropertyDescriptor) : Boolean;
    function  Test1(const AObject: CObject) : Boolean;
    function  Test2(const AObject: JSObjectReference) : Boolean;
    function  Test3(const AObject: &Type) : Boolean;
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
  App.Windows.impl,
  quickjs_ng;

{ TAppObject }

constructor TAppObject.Create(const Environment: IEnvironment);
begin
  _Environment := Environment;
  _Config := TAppConfig.Create;
  _Windows := Windows.Create;
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

function TAppObject.Test(const Descriptor: IPropertyDescriptor) : Boolean;
begin
  var fmt := Descriptor.Formatter;
  if fmt <> nil then
    fmt.Format(nil, nil, nil);

  var mrs := Descriptor.Marshaller;
  if mrs <> nil then
    mrs.Marshal(nil, nil);
end;

function TAppObject.Test1(const AObject: CObject) : Boolean;
begin
  var sfAccounts := _app.Config.TypeByName('SFAccount');
  var ot := _app.Config.ObjectType[sfAccounts];

  var d := ot.Provider.Data(nil);
  var l: IList;

  if Interfaces.Supports<IList>(d, l) then
  begin
    var c := l.Count;
  end;
end;

function TAppObject.Test2(const AObject: JSObjectReference) : Boolean;
begin
  if AObject.Ctx = nil then;
end;

function TAppObject.Test3(const AObject: &Type) : Boolean;
begin
  if AObject <> nil then
  begin
    var props := AObject.GetProperties;
    for var p in props do
    begin
      var s: string := p.Name;
    end;

  end;
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
