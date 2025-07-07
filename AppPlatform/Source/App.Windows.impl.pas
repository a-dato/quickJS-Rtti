unit App.Windows.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Intf,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.TypeDescriptor.intf;

type
  Window = class(TBaseInterfacedObject, IWindow)
  protected
    [weak] _app: IAppObject;
    _Frame: IWindowFrame;
    _Type: &Type;

    function get_Frame: IWindowFrame;
    function get_Name: CString;

    function  Build: IWindow;
    function  Bind(const Data: CObject) : IWindow;
    function  Show : IWindow;

  public
    constructor Create(const App: IAppObject; const Frame: IWindowFrame; const AType: &Type);
  end;

  Windows = class(CList<IWindow>, IWindows)
    function  CreateWindow(const AOwner: CObject; const AType: &Type) : IWindow;
  end;

implementation

uses
  FMX.Controls, FMX.Types;

{ Windows }

function Windows.CreateWindow(const AOwner: CObject; const AType: &Type): IWindow;
begin
  var ot := _app.Config.ObjectType(AType);
  if ot = nil then
    raise CException.Create('Unknown type');

  var frame := _app.Environment.CreateWindowFrame(AOwner, AType);
  Result := Window.Create(_app, frame, AType);
end;

{ Window }

function Window.Bind(const Data: CObject): IWindow;
begin
  var c := _frame.Content;
  _app.Config.ObjectType(_Type).Binder.Bind(c, _Type, Data);
  Result := Self;
end;

function Window.Build: IWindow;
begin
  _frame.Content := _app.Config.ObjectType(_Type).Builder.Build(_frame.Owner);
  _frame.Content.AsType<TControl>.Align := TAlignLayout.Client;
  Result := Self;
end;

constructor Window.Create(const App: IAppObject; const Frame: IWindowFrame; const AType: &Type);
begin
  _app := App;
  _frame := Frame;
  _Type := AType;
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

