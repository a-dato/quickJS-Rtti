unit App.impl;

interface

uses
  System_,
  App.intf,
  App.Config.intf,
  App.Models.intf,
  App.Windows.intf,
  App.Environment.intf,
  QuickJS.Register.dn4d.intf, ADato.ObjectModel.List.intf, System.Collections;

type
  TAppObject = class(TBaseInterfacedObject, IAppObject)
  protected
    _Config: IAppConfig;
    _Models: IAppModels;
    _Environment: IEnvironment;
    _Windows: IWindows;

    function get_AppModels: IAppModels;
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Windows: IWindows;

    function  Test(const Data: CObject; const Index: Integer) : CObject;
    procedure Register(const Value: IJSObjectReference);
    procedure RegisterType(const AType: &Type);
    procedure Show(const Value: IObjectListModel);
    function  GetData: IDataObject;

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

implementation

uses
  App.Config.impl,
  App.Models.impl,
  App.Windows.impl;

{ TAppObject }

constructor TAppObject.Create(const Environment: IEnvironment);
begin
  _Environment := Environment;
  _Config := TAppConfig.Create;
  _Models := AppModels.Create(Self);
  _Windows := Windows.Create(Self);
end;

function TAppObject.GetData: IDataObject;
begin
  Result := TDataObject.Create;
  Result.Name := CDateTime.Now.ToString;
end;

function TAppObject.get_AppModels: IAppModels;
begin
  Result := _Models;
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

procedure TAppObject.Register(const Value: IJSObjectReference);
begin
  var t := Value.GetType();
end;

procedure TAppObject.RegisterType(const AType: &Type);
begin
  var s := AType.GetTypeInfo.Name;
end;

procedure TAppObject.Show(const Value: IObjectListModel);
begin
  var tp := Value.ObjectModel.GetType;

  _Windows.CreateWindow(Self, tp).
    Build.
      Bind(Value).
        Show;
end;

function TAppObject.Test(const Data: CObject; const Index: Integer) : CObject;
begin
  var l: IList;
  if Data.TryAsType<IList>(l) then
  begin
    // DataList := l;
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
