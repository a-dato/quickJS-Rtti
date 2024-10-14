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
  QuickJS.Register.impl, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  System.Collections.Generic, System.Generics.Collections;

type
  {$M+}
  ITask = interface;

  TForm1 = class(TForm)
    btnExecute: TButton;
    mmLog: TMemo;
    mmCode: TMemo;
    Layout1: TLayout;
    btnAddProperty: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnAddPropertyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    function OnGetMemberByName(const AObject: IRegisteredObject;
      const AName: string; MemberTypes: TMemberTypes;
      var Handled: Boolean): IPropertyDescriptor;

    function OnGetMemberNames(const AObject: IRegisteredObject; MemberTypes: TMemberTypes;
      var Handled: Boolean): TArray<string>;

    { Private declarations }

  protected
    procedure Initialize;
    procedure LogCallBack(S: string);

  public
    { Public declarations }
    _context: IJSContext;
  end;

  TEnumerableObj = class(TList<string>)
  public
    constructor Create;

  end;

  IProject = interface(IBaseInterface)
    ['{95432947-A441-4022-8BDF-7D4F484755DF}']
    function  get_Created: CDateTime;
    procedure set_Created(const Value: CDateTime);
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function  get_Description: CString;
    procedure set_Description(const Value: CString);

    function  get_Name: string;
    procedure set_Name(const Value: string);
    function  get_Tasks: List<ITask>;

    property Created: CDateTime read get_Created write set_Created;
    property ID: CObject read get_ID write set_ID;
    property Description: CString read get_Description write set_Description;
    property Name: string read get_Name write set_Name;
    property Tasks: List<ITask> read get_Tasks;
  end;

  TProject = class(TBaseInterfacedObject, IProject)
  protected
    _Created: CDateTime;
    _ID: CObject;
    _Name: string;
    _Description: CString;

    function  get_Created: CDateTime;
    procedure set_Created(const Value: CDateTime);

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function  get_Description: CString;
    procedure set_Description(const Value: CString);

    function  get_Name: string;
    procedure set_Name(const Value: string);
    function  get_Tasks: List<ITask>;

  public
    constructor Create;
    destructor Destroy; override;

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
  quickjs, QuickJS.Register.dn4d.impl, System.Rtti;

{$R *.fmx}

procedure TForm1.Initialize;
begin
  if _context = nil then
  begin
    QuickJS.Register.impl.OutputLogString := LogCallBack;

    _context := TJSContext.Create(TJSRuntime.Create);

    TJSRegisterTypedObjects.Initialize(_context);
    TRegisteredTypedObject.OnGetMemberByName := OnGetMemberByName;

    //TJSRegister.RegisterObject<TEnumerableObj>(_context.ctx, 'EObj', function : TEnumerableObj begin Result := TEnumerableObj.Create; end);
    TJSRegister.RegisterObject(_context.ctx, 'Project', TypeInfo(IProject), function : Pointer begin Result := TProject.Create; end);
    TJSRegister.RegisterObject(_context.ctx, 'Task', TypeInfo(ITask), function : Pointer begin Result := TTask.Create; end);

//      TJSRegister.RegisterObject<IProject>(_context.ctx, 'Project', function : IProject begin Result := TProject.Create; end);
//      TJSRegister.RegisterObject<ITask>(_context.ctx, 'Task', function : ITask begin Result := TTask.Create; end);

//    TJSRegister.RegisterObject(_context.ctx, 'XMLHttpRequest', TypeInfo(IXMLHttpRequest), function : TObject begin Result := TXMLHttpRequest.Create; end);
//    TJSRegister.RegisterObject<IResource>(_context.ctx, 'Resource', function : IResource begin Result := TResource.Create(-1, ''); end);
//    TJSRegister.RegisterObject(_context.ctx, 'User', TypeInfo(TUser));
//
//    var u := TUser.Create;
//    TJSRegister.RegisterLiveObject<TUser>(_context.ctx, 'usr', u, True);
  end;
end;

function TForm1.OnGetMemberByName(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean) : IPropertyDescriptor;
begin
  Result := nil;
  Handled := False;
end;

function TForm1.OnGetMemberNames(const AObject: IRegisteredObject; MemberTypes: TMemberTypes; var Handled: Boolean): TArray<string>;
begin
  Result := nil;
  Handled := False;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  var rtti := TRttiContext.Create;

  var ms := rtti.GetType(TypeInfo(ITask)).GetMethods;

  for var m in ms do
    mmLog.Lines.Add(m.Name);


//  var tp := &Type.Create(TypeInfo(ITask));
//
//  var p := tp.PropertyByName('Name');
//  if p = nil then;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
//  var si := JS_NewString(_context.ctx, 'Symbol.iterator');
//  var atom := JS_ValueToAtom(_context.ctx, si);

//    var jv := JS_AtomToString(_context.ctx, i);
//210:Symbol.iterator
//333:iterator
  for var i := 1 to 500 do
  begin
    var jv := JS_AtomToValue(_context.ctx, i);
    if JS_IsString(jv) then
    begin
      var s := JS_ToCString(_context.ctx, jv);
      if Pos('iter', s) > 0 then
        mmLog.Lines.Add(i.ToString + ':' + s);
    end;
  end;
end;

procedure TForm1.LogCallBack(S: string);
begin
  mmLog.Lines.Add(s);
end;

{ TProject }

constructor TProject.Create;
begin
  _ID := Int64(1);
  _Created := CDateTime.Now;

  _Name := 'Project 1';
  _Description := 'Project description 1';
end;

destructor TProject.Destroy;
begin
  inherited;
end;

function TProject.get_Created: CDateTime;
begin
  Result := _Created;
end;

function TProject.get_Description: CString;
begin
  Result := _Description;
end;

function TProject.get_ID: CObject;
begin
  Result := _ID;
end;

function TProject.get_Name: string;
begin
  Result := _Name;
end;

function TProject.get_Tasks: List<ITask>;
begin
  var l: List<ITask> := CList<ITask>.Create;

  for var i := 0 to 9 do
  begin
    var t: ITask := TTask.Create;
    t.ID := i;
    t.Name := 'Task ' + i.ToString;
    l.Add(t);
  end;

  Result := l;
end;

procedure TProject.set_Created(const Value: CDateTime);
begin
  _Created := Value;
end;

procedure TProject.set_Description(const Value: CString);
begin
  _Description := Value;
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


{ TEnumerableObj }

constructor TEnumerableObj.Create;
begin
  inherited;

  for var i := 0 to 9 do
    Add(i.ToString);
end;

end.
