unit App.Windows.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Intf,
  App.Component.intf,
  App.Component.impl,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.TypeDescriptor.intf,
  App.Storage.intf,
  App.Content.intf;

type
  TWindow = class(TComponent, IWindow)
  protected
    _control: TObject;  // TForm or other container
    _Frame: IWindowFrame;
    _Type: &Type;

    function  get_Control: TObject;

    function  get_Frame: IWindowFrame;
    procedure set_Frame(const Value: IWindowFrame);

    function  CreateFrame(const Name: string) : IWindow;
    function  Bind(const Storage: IStorage) : IWindow;
    function  Show(OnClose: TWindowClose) : IWindow;

  public
    constructor Create(const AType: &Type; Form: TObject);
  end;

  TWindowFrame = class(TComponent, IWindowFrame)
  protected
    _control: TObject;

    function  get_Control: TObject;
    procedure set_Control(const Value: TObject);

  public
    constructor Create(const AOwner: IWindow; Control: TObject);
  end;

  TWindowType = class(TBaseInterfacedObject, IWindowType)
  protected
    _name: string;
    _creatorFunc: TFrameCreateFunc;

    function get_Name: CString;

    function CreateFrame(const AOwner: IWindow) : IWindowFrame;
  public
    constructor Create(const Name: string; const CreatorFunc: TFrameCreateFunc);
  end;

implementation

uses
  {$IFDEF FRAMEWORK_VCL}
  Vcl.Controls, Vcl.Forms
  {$ELSE}
  FMX.Controls, FMX.Types, FMX.Forms
  {$ENDIF}
  ;


{ Window }

function TWindow.Bind(const Storage: IStorage): IWindow;
begin
  var c := _frame.Control;
  _app.Config.TypeDescriptor(_Type).Binder.Bind(_Type, c, Storage);
  Result := Self;
end;

constructor TWindow.Create(const AType: &Type; Form: TObject);
begin
  _type := AType;
  _control := Form;
end;

function TWindow.CreateFrame(const Name: string) : IWindow;
begin
  var wt: IWindowType;
  if not _app.Config.TryGetWindowType(Name, wt) then
    raise CException.Create(CString.Format('Frame not registered: {0}', Name));

  var frame := wt.CreateFrame(Self);
  set_Frame(frame);

  Result := Self;
end;

function TWindow.get_Control: TObject;
begin
  Result := _control;
end;

function TWindow.get_Frame: IWindowFrame;
begin
  Result := _frame;
end;

procedure TWindow.set_Frame(const Value: IWindowFrame);
begin
  _frame := Value;

  {$IFDEF FRAMEWORK_VCL}
  if (_control is TWinControl) and (_frame.Control is TWinControl) then
  begin
    var c := _frame.Control as TControl;
    c.Parent := _control as TWinControl;
    c.Align := TAlign.alClient;
  end;
  {$ELSE}
  if (_control is TFmxObject) and (_frame.Control is TControl) then
  begin
    var c: TControl := _frame.Control as TControl;
    (_control as TFmxObject).AddObject(c);
    c.Align := TAlignLayout.Client;
  end;
  {$ENDIF}
end;

function TWindow.Show(OnClose: TWindowClose): IWindow;
begin
  // Works with VCL and FMX
  if _control is TForm then
    (_control as TForm).Show;

  Result := Self;
end;

{ WindowType }

constructor TWindowType.Create(const Name: string; const CreatorFunc: TFrameCreateFunc);
begin
  _name := Name;
  _creatorFunc := CreatorFunc;
end;

function TWindowType.CreateFrame(const AOwner: IWindow): IWindowFrame;
begin
  Result := _creatorFunc(AOwner);
end;

function TWindowType.get_Name: CString;
begin
  Result := _Name;
end;

{ TWindowFrame }

constructor TWindowFrame.Create(const AOwner: IWindow; Control: TObject);
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

