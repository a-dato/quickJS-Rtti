unit App.Windows.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Intf,
  App.Windows.intf,
  ADato.ObjectModel.List.intf, App.Objects.intf;

type
  Window = class(TBaseInterfacedObject, IWindow)
  protected
    [weak] _app: IAppObject;
    _Frame: IWindowFrame;
    _ObjectType: IObjectType;

    function get_Frame: IWindowFrame;
    function get_Name: CString;

    function  Build: IWindow;
    function  Bind(const Data: CObject) : IWindow;
    function  Show : IWindow;

  public
    constructor Create(const App: IAppObject; const Frame: IWindowFrame; const ObjectType: IObjectType);
  end;

  Windows = class(CList<IWindow>, IWindows)
    [weak] _app: IAppObject;
    function  CreateWindow(const AOwner: CObject; const AType: &Type) : IWindow;

  public
    constructor Create(const App: IAppObject);
  end;

implementation

{ Windows }

constructor Windows.Create(const App: IAppObject);
begin
  _app := App;
end;

function Windows.CreateWindow(const AOwner: CObject; const AType: &Type): IWindow;
begin
  var ot := _app.Config.ObjectType[AType];
  if ot = nil then
    raise CException.Create('Unknown type');

  var frame := _app.Environment.CreateWindowFrame(AOwner, ot);
  Result := Window.Create(_app, frame, ot);
end;

{ Window }

function Window.Bind(const Data: CObject): IWindow;
begin
  var c := _frame.Content;
  _ObjectType.Binder.Bind(c, _ObjectType.GetType, Data);
  Result := Self;
end;

function Window.Build: IWindow;
begin
  _frame.Content := _ObjectType.Builder.Build(_frame.Owner);
  Result := Self;
end;

constructor Window.Create(const App: IAppObject; const Frame: IWindowFrame; const ObjectType: IObjectType);
begin
  _app := App;
  _frame := Frame;
  _ObjectType := ObjectType;
end;

function Window.get_Frame: IWindowFrame;
begin

end;

function Window.get_Name: CString;
begin

end;

function Window.Show: IWindow;
begin
  _Frame.Show;
  Result := Self;
end;

end.
