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
  App.Content.intf,
  System.UITypes
  {$IFDEF FRAMEWORK_VCL}
  //Vcl.Controls, Vcl.Forms
  {$ELSE}

  {$ENDIF}
  ;

type
  TWindow = class(TComponent, IWindow)
  protected
    _control: TObject;  // TForm or other container
    _Frame: IWindowFrame;
    _Type: &Type;
    _onClose: TWindowClose;

    function  get_Control: TObject;

    function  get_Frame: IWindowFrame;
    procedure set_Frame(const Value: IWindowFrame);

  protected
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    function  CreateFrame(const Name: string) : IWindow;
    function  Bind(const Data: CObject) : IWindow;
    function  Show(OnClose: TWindowClose) : IWindow;

  public
    constructor Create(const AType: &Type; Form: TObject);
    destructor Destroy; override;
  end;

  TWindowFrame = class(TComponent, IWindowFrame)
  protected
    _content: TObject;

    function  get_Content: TObject;
    procedure set_Content(const Value: TObject);

  public
    constructor Create(const AOwner: IWindow; Content: TObject);
    destructor Destroy; override;
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
  , App.Environment.impl;


{ Window }

function TWindow.Bind(const Data: CObject): IWindow;
begin
  var c := _frame.Content;
  var binder := _app.Config.TypeDescriptor(_Type).Binder;
  if binder = nil then
    binder := TFrameBinder.Create;
  binder.Bind(_Type, c, Data);
  Result := Self;
end;

constructor TWindow.Create(const AType: &Type; Form: TObject);
begin
  _type := AType;
  _control := Form;
end;

destructor TWindow.Destroy;
begin
  inherited;
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
  if (_control is TWinControl) and (_frame.Content is TWinControl) then
  begin
    var c := _frame.Content as TControl;
    c.Parent := _control as TWinControl;
    c.Align := TAlign.alClient;
  end;
  {$ELSE}
  if (_control is TFmxObject) and (_frame.Content is TControl) then
  begin
    var child: TControl := _frame.Content as TControl;
    var host := _control as TForm;
    host.Width := Trunc(child.Width);
    host.Height := Trunc(child.Height);
    host.AddObject(child);
    child.Align := TAlignLayout.Client;
  end;
  {$ENDIF}
end;

procedure TWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(_onClose) then
    _onClose(Self, False);

  Action := TCloseAction.caFree;
end;

procedure TWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(_onClose) then
    CanClose := _onClose(Self, True) else
    CanClose := True;
end;

function TWindow.Show(OnClose: TWindowClose): IWindow;
begin
  // Works with VCL and FMX
  if _control is TForm then
  begin
    _onClose := OnClose;
    var frm := _control as TForm;
    frm.OnCloseQuery := FormCloseQuery;
    frm.OnClose := FormClose;
    (_control as TForm).Show;
  end;

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

constructor TWindowFrame.Create(const AOwner: IWindow; Content: TObject);
begin
  inherited Create(nil);
  _content := Content;
end;

destructor TWindowFrame.Destroy;
begin
  inherited;
end;

function TWindowFrame.get_Content: TObject;
begin
  Result := _content;
end;

procedure TWindowFrame.set_Content(const Value: TObject);
begin
  _content := Value;
end;

end.


