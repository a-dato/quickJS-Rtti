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
    function  Show(OnClose: WindowClose) : IWindow;

  public
    constructor Create(const App: IAppObject; const Frame: IWindowFrame; const AType: &Type);
  end;

  WindowType = class(TBaseInterfacedObject, IWindowType)
  protected
    _name: string;
    _objectType: &Type;
    _creatorFunc: WindowCreateFunc;

    function get_Name: CString;
    function get_ObjectType: &Type;

    function CreateInstance(const AOwner: CObject) : IWindow;
  public
    constructor Create(const ObjectType: &Type; const Name: string; const CreatorFunc: WindowCreateFunc);
  end;

  Windows = class(CList<IWindow>, IWindows)
  protected
    function  CreateWindow(const AType: &Type; const AOwner: CObject) : IWindow; overload;
    function  CreateWindow(const AType: &Type; const AOwner: CObject; const FrameClassName: string) : IWindow; overload;
  end;

implementation

uses
  FMX.Controls, FMX.Types;

{ Windows }

function Windows.CreateWindow(const AType: &Type; const AOwner: CObject): IWindow;
begin
  var ot := _app.Config.TypeDescriptor(AType);
  if ot = nil then
    raise CException.Create('Unknown type');

  var frame := _app.Environment.CreateWindowFrame(AOwner, AType);
  Result := Window.Create(_app, frame, AType);
end;

function Windows.CreateWindow(const AType: &Type; const AOwner: CObject; const FrameClassName: string) : IWindow;
begin

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

function Window.Show(OnClose: WindowClose): IWindow;
begin
  _Frame.Show(OnClose);
  Result := Self;
end;

{ WindowType }

constructor WindowType.Create(const ObjectType: &Type; const Name: string; const CreatorFunc: WindowCreateFunc);
begin
  _objectType := ObjectType;
  _name := Name;
  _creatorFunc := CreatorFunc;
end;

function WindowType.CreateInstance(const AOwner: CObject): IWindow;
begin

end;

function WindowType.get_Name: CString;
begin
  Result := _Name;
end;

function WindowType.get_ObjectType: &Type;
begin
  Result := _objectType;
end;

end.

