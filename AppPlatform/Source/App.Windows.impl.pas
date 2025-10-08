unit App.Windows.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Intf,
  App.Component.impl,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.TypeDescriptor.intf,
  App.Storage.intf,
  App.Content.intf;

type
  Window = class(TComponent, IWindow)
  protected
    [weak] _app: IAppObject;
    _Frame: IWindowFrame;
    _Type: &Type;

    function get_Frame: IWindowFrame;

    function  Build: IWindow; overload;
    function  Build(const Builder: IContentBuilder): IWindow; overload;
    function  Bind(const Storage: IStorage) : IWindow;
    function  Show(OnClose: TWindowClose) : IWindow;

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
  var ot := _app.Config.TypeDescriptor(AType);
  if ot = nil then
    raise CException.Create('Unknown type');

  var frame := _app.Environment.CreateWindowFrame(AOwner, AType);
  Result := Window.Create(_app, frame, AType);
end;

{ Window }

function Window.Bind(const Storage: IStorage): IWindow;
begin
  var c := _frame.Content;
  _app.Config.TypeDescriptor(_Type).Binder.Bind(c, _Type, Storage);
  Result := Self;
end;

function Window.Build: IWindow;
begin
  Result := Build(_app.Config.TypeDescriptor(_Type).Builder);
end;

function Window.Build(const Builder: IContentBuilder): IWindow;
begin
  _frame.Content := _app.Config.TypeDescriptor(_Type).Builder.Build(_frame.Owner);
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
  Result := _frame;
end;

function Window.Show(OnClose: TWindowClose): IWindow;
begin
  _Frame.Show(OnClose);
  Result := Self;
end;

end.

