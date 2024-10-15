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
  System.Collections.Generic, System.Generics.Collections, System.Rtti,
  System.TypInfo;

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
    _task: ITask;
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
    function  get_Names: List<string>;

    function  get_Tasks: List<ITask>;

    function Calc : Int64;

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

    function  get_Names: List<string>;

    function  get_Tasks: List<ITask>;

    function Calc : Int64;

  public
    constructor Create;
    destructor Destroy; override;

  published
    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
    property Names: List<string> read get_Names;
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

  public
    constructor Create;
    destructor Destroy; override;

  published
    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

  TCustomPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FName: string;

    function  get_MemberType: TMemberType; virtual;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;
  public
    constructor Create(const AName: string);
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

    TJSRegisterTypedObjects.Initialize(_context);
    TRegisteredTypedObject.OnGetMemberByName := OnGetMemberByName;

    //TJSRegister.RegisterObject<TEnumerableObj>(_context.ctx, 'EObj', function : TEnumerableObj begin Result := TEnumerableObj.Create; end);
    TJSRegister.RegisterObject(_context.ctx, 'Project', TypeInfo(IProject), function : Pointer begin Result := TProject.Create; end);
    TJSRegister.RegisterObject(_context.ctx, 'Task', TypeInfo(ITask), function : Pointer begin Result := TTask.Create; end);

    var t: ITask := TTask.Create;
    t.Name := 'Live task';

    TJSRegister.RegisterLiveObject(_context.ctx, 'tk', TypeInfo(ITask), t);
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

  if (AName = 'Custom') and (AObject.GetTypeInfo = TypeInfo(IProject)) then
  begin
    Result := TCustomPropertyDescriptor.Create('Custom');
    Handled := True;
  end else
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

function TProject.Calc: Int64;
begin
  Result := 310;
end;

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

function TProject.get_Names: List<string>;
begin
  var l: List<string> := CList<string>.Create;

  for var i := 0 to 9 do
    l.Add('Item ' + i.ToString);

  Result := l;
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

constructor TTask.Create;
begin
  _ID := 10;
  _Name := 'Task 10';
end;

destructor TTask.Destroy;
begin
  inherited;
end;

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

{ TCustomPropertyDescriptor }

constructor TCustomPropertyDescriptor.Create(const AName: string);
begin
  FName := AName;

end;

function TCustomPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  Result := 'Value for property ' + FName;
end;

function TCustomPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

procedure TCustomPropertyDescriptor.SetValue(const Ptr: Pointer;
  const Index: array of TValue; const Value: TValue);
begin

end;

end.
