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
  TWindow = class(TComponent, IWindow)
  protected
    [weak] _app: IAppObject;
    _form: TObject;  // TForm or other container
    _Frame: IWindowFrame;
    _Type: &Type;

    function  get_Frame: IWindowFrame;
    procedure set_Frame(const Value: IWindowFrame);

    function  Build: IWindow; overload;
    function  Build(const Builder: IContentBuilder): IWindow; overload;
    function  Bind(const Storage: IStorage) : IWindow;
    function  Show(OnClose: WindowClose) : IWindow;

  public
    constructor Create(const App: IAppObject; const AType: &Type; Form: TObject);
  end;

  TWindowFrame = class(TComponent, IWindowFrame)
  protected
    _control: TObject;

    function  get_Control: TObject;
    procedure set_Control(const Value: TObject);

  public
    constructor Create(const AOwner: CObject; Control: TObject);
  end;

  TWindowType = class(TBaseInterfacedObject, IWindowType)
  protected
    _name: string;
    _objectType: &Type;
    _creatorFunc: WindowFrameCreateFunc;

    function get_Name: CString;
    function get_ObjectType: &Type;

    function CreateFrame(const AOwner: CObject) : IWindowFrame;
  public
    constructor Create(const ObjectType: &Type; const Name: string; const CreatorFunc: WindowFrameCreateFunc);
  end;

  TWindows = class(CList<IWindow>, IWindows)
  protected
    function  CreateWindow(const AType: &Type; const AOwner: CObject) : IWindow; overload;
    function  CreateWindow(const AType: &Type; const AOwner: CObject; const Name: string) : IWindow; overload;
  end;

implementation

uses
  FMX.Controls, FMX.Types, FMX.Forms;

{ Windows }

function TWindows.CreateWindow(const AType: &Type; const AOwner: CObject): IWindow;
begin
  Result := CreateWindow(AType, AOwner, AType.Name);
//  var ot := _app.Config.TypeDescriptor(AType);
//  if ot = nil then
//    raise CException.Create('Unknown type');
//
//  var frame := _app.Environment.CreateWindowFrame(AOwner, AType);
//  Result := TWindow.Create(_app, frame, AType);
end;

function TWindows.CreateWindow(const AType: &Type; const AOwner: CObject; const Name: string) : IWindow;
begin
  var wt := _app.Config.WindowType(AType, Name);
  if wt = nil then
    raise CException.Create('Window type not registered for: ' + AType.Name);

  var window := _app.Environment.CreateWindow(AType, AOwner);

  var frame := wt.CreateFrame(AOwner);
  window.Frame := frame;

  Result := window;
end;
{ Window }

function TWindow.Bind(const Storage: IStorage): IWindow;
begin
  var c := _frame.Control;
  _app.Config.TypeDescriptor(_Type).Binder.Bind(_Type, c, Storage);
  Result := Self;
end;

function TWindow.Build: IWindow;
begin
//  Result := Build(_app.Config.TypeDescriptor(_Type).Builder);
end;

function TWindow.Build(const Builder: IContentBuilder): IWindow;
begin
//  _frame.Content := _app.Config.TypeDescriptor(_Type).Builder.Build(_frame.Owner);
//  _frame.Content.AsType<TControl>.Align := TAlignLayout.Client;
//  Result := Self;
end;

constructor TWindow.Create(const App: IAppObject; const AType: &Type; Form: TObject);
begin
  _app := App;
  _type := AType;
  _form := Form;
end;

function TWindow.get_Frame: IWindowFrame;
begin
  Result := _frame;
end;

procedure TWindow.set_Frame(const Value: IWindowFrame);
begin
  _frame := Value;
  if (_form is TFmxObject) and (_frame.Control is TControl) then
  begin
    var c: TControl := _frame.Control as TControl;
    (_form as TFmxObject).AddObject(c);
    c.Align := TAlignLayout.Client;
  end;
end;

function TWindow.Show(OnClose: WindowClose): IWindow;
begin
  if _form is TForm then
    (_form as TForm).Show;

  Result := Self;
end;

{ WindowType }

constructor TWindowType.Create(const ObjectType: &Type; const Name: string; const CreatorFunc: WindowFrameCreateFunc);
begin
  _objectType := ObjectType;
  _name := Name;
  _creatorFunc := CreatorFunc;
end;

function TWindowType.CreateFrame(const AOwner: CObject): IWindowFrame;
begin
  Result := _creatorFunc(AOwner);
end;

function TWindowType.get_Name: CString;
begin
  Result := _Name;
end;

function TWindowType.get_ObjectType: &Type;
begin
  Result := _objectType;
end;

{ TWindowFrame }

constructor TWindowFrame.Create(const AOwner: CObject; Control: TObject);
begin
  inherited Create(nil);
  _control := Control;
end;

function TWindowFrame.get_Control: TObject;
begin
  Result := _control;
end;

procedure TWindowFrame.set_Control(const Value: TObject);
begin
  _control := Value;
end;

end.

