unit Dn4dMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System_,
  ADato.Extensions.intf,
  ADato.Extensions.impl,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, QuickJS.Register.intf,
  QuickJS.Register.impl, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts;

type
  TForm1 = class(TForm)
    btnExecute: TButton;
    mmLog: TMemo;
    mmCode: TMemo;
    Layout1: TLayout;
    btnAddProperty: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnAddPropertyClick(Sender: TObject);
  private
    { Private declarations }

  protected
    procedure Initialize;
    procedure LogCallBack(S: string);

  public
    { Public declarations }
    _context: IJSContext;
  end;

  {$M+}
  IProject = interface(IBaseInterface)
    ['{95432947-A441-4022-8BDF-7D4F484755DF}']
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function  get_Name: string;
    procedure set_Name(const Value: string);

    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

  TProject = class(TBaseInterfacedObject, IProject)
  protected
    _ID: CObject;
    _Name: string;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function  get_Name: string;
    procedure set_Name(const Value: string);

  published
    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

  ITask = interface(IBaseInterface)
    ['{95432947-A441-4022-8BDF-7D4F484755DF}']
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function  get_Name: string;
    procedure set_Name(const Value: string);

    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

  TTask = class(TBaseInterfacedObject, ITask)
  protected
    _ID: CObject;
    _Name: string;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function  get_Name: string;
    procedure set_Name(const Value: string);

  published
    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

var
  Form1: TForm1;

implementation

uses
  quickjs, QuickJS.Register.dn4d.impl;

{$R *.fmx}

procedure TForm1.Initialize;
begin
  if _context = nil then
  begin
    QuickJS.Register.impl.OutputLogString := LogCallBack;

    _context := TJSContext.Create(TJSRuntime.Create);
    TJSRegisterTypedObjects.Initialize;

    TJSRegisterTypedObjects.RegisterObject<IProject>(_context.ctx, 'Project', function : IProject begin Result := TProject.Create; end);
    TJSRegisterTypedObjects.RegisterObject<ITask>(_context.ctx, 'Task', function : ITask begin Result := TTask.Create; end);

//    TJSRegister.RegisterObject(_context.ctx, 'XMLHttpRequest', TypeInfo(IXMLHttpRequest), function : TObject begin Result := TXMLHttpRequest.Create; end);
//    TJSRegister.RegisterObject<IResource>(_context.ctx, 'Resource', function : IResource begin Result := TResource.Create(-1, ''); end);
//    TJSRegister.RegisterObject(_context.ctx, 'User', TypeInfo(TUser));
//
//    var u := TUser.Create;
//    TJSRegister.RegisterLiveObject<TUser>(_context.ctx, 'usr', u, True);
  end;
end;

procedure TForm1.btnExecuteClick(Sender: TObject);
begin
  Initialize;

  var b: AnsiString := AnsiString(mmCode.Lines.Text);
  _context.eval_buf(PAnsiChar(b), Length(b), 'application', JS_EVAL_TYPE_GLOBAL);
end;

procedure TForm1.btnAddPropertyClick(Sender: TObject);
begin
  var tp := &Type.Create(TypeInfo(Integer));

  var prop: _PropertyInfo := CustomProperty.Create(System_.&Type.Unknown, 'AValue', 'AValue', tp);
  ExtensionManager.AddProperty(&Type.Create(TypeInfo(IProject)), prop);
end;

procedure TForm1.LogCallBack(S: string);
begin
  mmLog.Lines.Add(s);
end;

{ TProject }

function TProject.get_ID: CObject;
begin
  Result := _ID;
end;

function TProject.get_Name: string;
begin
  Result := _Name;
end;

procedure TProject.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TProject.set_Name(const Value: string);
begin
  _Name := Value;
end;

{ TTask }

function TTask.get_ID: CObject;
begin
  Result := _ID;
end;

function TTask.get_Name: string;
begin
  Result := _Name;
end;

procedure TTask.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TTask.set_Name(const Value: string);
begin
  _Name := Value;
end;

end.
