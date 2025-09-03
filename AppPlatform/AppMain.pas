unit AppMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  QuickJS.Register.intf, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  App.intf, System.Actions, FMX.ActnList, FMX.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, System_,
  Project.intf, System.Collections, System.Collections.Generic, System.Rtti;

type
  {$M+}
  ITestObject = interface;
  TTestFunc = reference to function(const Param: string) : string;
  TGenFunc<T> = reference to function : T;

  TForm1 = class(TForm)
    Layout1: TLayout;
    mmCode: TMemo;
    mmLog: TMemo;
    ActionList1: TActionList;
    acExecute: TAction;
    Button1: TButton;
    btnCustomer: TButton;
    Button2: TButton;
    Button3: TButton;
    NetHTTPClient1: TNetHTTPClient;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    lblPosition: TLabel;
    Timer1: TTimer;
    Button4: TButton;
    mmInitialize: TMemo;
    Button5: TButton;
    btnExecResult: TButton;
    TestFunc: TButton;
    TestFunc2: TButton;
    procedure acExecuteExecute(Sender: TObject);
    procedure btnCustomerClick(Sender: TObject);
    procedure btnExecResultClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestFunc2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TestFuncClick(Sender: TObject);
  private
    procedure InitializeAppEnvironment;
    procedure Initialize;
    procedure LogCallBack(S: string);

    function CreateTestFunc : TTestFunc;
    { Private declarations }

  public
    _context: IJSContext;
    _test: ITestObject;
    _testFunc: TTestFunc;
    { Public declarations }
  end;

  {$M+}
  TScheduledInterval = record
    TaskID: Int64;
  end;

  ITestObject = interface(IBaseInterface)
    ['{4A8C6FB1-5CFB-49C5-A0EB-2BEC6E554F01}']
    function get_Data: IList;
    function get_Names(const Value: string): string;
    function Test(const Value: CObject) : CObject;
    function Test2(const Value: IProject) : IInterface;
    function Test3(const Value: &Type) : CObject;
    function Test4(const Value: TFunc<CObject>) : CObject;
    function Test5(const Value: TTestFunc) : CObject;

    function NextID: Int64;

    procedure Attach(const Data: IList);
    property Names[const Value: string]: string read get_Names;

    property Data: IList read get_Data;
  end;

  TTestObject = class(TBaseInterfacedObject, ITestObject)
  protected
    _id: Int64;
    _data: List<ITestObject>;

    procedure Attach(const Data: IList);
    function get_Data: IList;

    function get_Names(const Value: string): string;

    function NextID: Int64;

    function Test(const Value: CObject) : CObject;
    function Test2(const Value: IProject) : IInterface;
    function Test3(const Value: &Type) : CObject;
    function Test4(const Value: TFunc<CObject>) : CObject;
    function Test5(const Value: TTestFunc) : CObject;
  public
    constructor Create;

    property Names[const Value: string]: string read get_Names;
  end;

var
  Form1: TForm1;

implementation

uses
  quickjs_ng,
  QuickJS.Register.impl,
  QuickJS.Register.dn4d.impl,
  App.impl,
  App.Environment.impl,
  Project.impl,
  ObjectWindow,
  Customer.frame,
  QuickJS.Register.dn4d.intf,
  ADato.ObjectModel.List.Tracking.intf,
  ADato.ObjectModel.List.Tracking.impl,
  App.Content.intf,
  JSGeneral.frame, Winapi.Windows, App.Content.impl, System.Diagnostics,
  XMLHttpRequest.impl, XMLHttpRequest.intf,
  ADato.Extensions.intf,
  ADato.Extensions.impl,
  ObjectDesigner,
  Project.frame,
  App.TypeDescriptor.intf,
  App.PropertyDescriptor.intf, System.JSON,
  ADato.ObjectModel.impl,
  System.ComponentModel,
  ADato.ObjectModel.TrackInterfaces,
  App.Factory.impl,
  System.TypInfo,
  QuickJS.VirtualMethod.impl;

{$R *.fmx}

procedure TForm1.acExecuteExecute(Sender: TObject);
begin
  mmLog.Lines.Clear;
  var st := TStopWatch.StartNew;

  var b: AnsiString := AnsiString(mmCode.Lines.Text);
  _context.eval(mmCode.Lines.Text, '<eval>');

  mmLog.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

procedure TForm1.btnExecResultClick(Sender: TObject);
begin
  mmLog.Lines.Clear;
  var st := TStopWatch.StartNew;

  var b: AnsiString := AnsiString(mmCode.Lines.Text);
  var r := _context.eval_with_result(mmCode.Lines.Text, '<eval>');

  mmLog.Lines.Add('result: ' + r.ToString);
  mmLog.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

procedure TForm1.btnCustomerClick(Sender: TObject);
begin
  _app.Windows.CreateWindow(Self, TProject.Type).
    Build.
      Bind(_app.Storage[TProject.TypeDescriptor.StorageName]).
        Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  var tp := &Type.From<IProject>;
  var customerType := _app.Config.TypeByName('Customer');
  var customer_objecttype := _app.Config.TypeDescriptor(customerType);

//  var descr := customer_objecttype.PropertyDescriptor['Customer'];
//  _app.Config.AddProperty(tp, 'Customer', 'Customer', customerType, descr);

  var cust_prop := tp.PropertyByName('Customer');

  var data := customer_objecttype.Provider.Data(nil);

  var c := data[0]; // Customer
  var s := c.ToString;
  if s = '' then
    s := 'Test';

  var project_type := _app.Config.TypeDescriptor(&Type.From<IProject>);
  var d := project_type.Provider.Data(nil);
  var prj := d[0].AsType<IProject>;

  cust_prop.SetValue(prj, c, []);

  var v := cust_prop.GetValue(prj, []);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  var t := &Type.From<ITestObject>;

  for var p in t.GetProperties do
    ShowMessage(p.Name);

  for var m in t.GetMethods do
    ShowMessage(m.Name);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  var frm := TObjectDesignerForm.Create(Self);
  frm.Load(_app);
  frm.Show;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  var tp := &Type.From<IProject>;
  var objectProperty := tp.PropertyByName('Customer.Address');

  var ot := _app.Config.TypeDescriptor(tp);
  var d := ot.Provider.Data(nil);
  var l: IList;
  if Interfaces.Supports<IList>(d, l) then
  begin
    var prj := l[0].AsType<IProject>;
    var value := objectProperty.GetValue(prj, []);

    var descr: IPropertyDescriptor;
    if Interfaces.Supports<IPropertyDescriptor>(objectProperty, descr) then
      ShowMessage(descr.Formatter.Format(nil, value, nil));
  end;
end;

function TForm1.CreateTestFunc: TTestFunc;
begin
  Result := function(const Param: string) : string begin
    Result := 'Done';
  end;
end;

procedure TForm1.InitializeAppEnvironment;
begin
  App.Environment.impl.Environment.FormClass := TfrmObjectWindow;

  {$IFDEF FRAMEWORK_FMX}
  _app := TAppObject.Create(App.Environment.impl.Environment.Create);
  {$ENDIF}

  TProject.TypeDescriptor.Builder := TFrameBuilder.Create(TProjectFrame);
  TProject.TypeDescriptor.Binder := TFrameBinder.Create();
  TProject.TypeDescriptor.Provider := ProjectProvider.Create(TProject.TypeDescriptor);

  _app.Config.RegisterType(TProject.Type, TProject.TypeDescriptor);

  _app.Factory.RegisterType(TProject.Type, function : CObject begin
    Result := CObject.From<IProject>(TProject.Create);
  end);

  var storage := _app.AddStorage(TProject.Type, TProject.TypeDescriptor.StorageName);
  storage.Attach(TProject.TypeDescriptor.Provider.Data(nil));
end;

procedure TForm1.Initialize;
begin
  if _context = nil then
  begin
    QuickJS.Register.impl.OutputLogString := LogCallBack;

    _context := TJSContext.Create(TJSRuntime.Create);

    TJSRegisterTypedObjects.Initialize(_context);

//    TJSRegister.RegisterObject(_context, 'ObjectList', TypeInfo(List<JSObjectReference>),
//      function : Pointer begin
//        Result := CList<JSObjectReference>.Create;
//      end);

    TJSRegister.RegisterObject(_context, 'List', TypeInfo(List<CObject>),
      function : Pointer begin
        Result := CList<CObject>.Create;
      end);

    TJSRegister.RegisterObject(_context, 'ObjectModel', TypeInfo(IObjectListModelChangeTracking),
      function : Pointer begin
        Result := TObjectListModelWithChangeTracking<IJSObject>.Create(nil);
      end);

    TJSRegister.RegisterObject(_context, 'JSBinder', TypeInfo(IContentBinder),
      function : Pointer begin
        Result := TFrameBinder.Create();
      end);

    TJSRegister.RegisterObject(_context, 'JSFrameBuilder', TypeInfo(IContentBuilder),
      function : Pointer begin
        // Result := TFrameBuilder.Create(TJSGeneralFrame);
        Result := TFrameBuilder.Create(TCustomerFrame);
      end);

    TJSRegister.RegisterObject(_context, 'XMLHttpRequest', TypeInfo(IXMLHttpRequest),
      function : Pointer begin
        Result := TXMLHttpRequest.Create;
      end);

    TJSRegister.RegisterObject(_context, 'IBaseInterface', TypeInfo(IBaseInterface), nil);
    TJSRegister.RegisterObject(_context, 'IAddingNew', TypeInfo(IAddingNew), nil);
    TJSRegister.RegisterObject(_context, 'IAddRange', TypeInfo(IAddRange), nil);
    TJSRegister.RegisterObject(_context, 'ICloneable', TypeInfo(ICloneable), nil);
    TJSRegister.RegisterObject(_context, 'IEditState', TypeInfo(IEditState), nil);
    TJSRegister.RegisterObject(_context, 'IEditableModel', TypeInfo(IEditableModel), nil);
    TJSRegister.RegisterObject(_context, 'IOnItemChangedSupport', TypeInfo(IOnItemChangedSupport), nil);
    TJSRegister.RegisterObject(_context, 'IUpdatableObject', TypeInfo(IUpdatableObject), nil);
    TJSRegister.RegisterObject(_context, 'IProject', TypeInfo(IProject), nil);

    TJSRegister.RegisterLiveObject(_context, 'app', TypeInfo(IAppObject), _app);

    TJSRegister.RegisterObject(_context, 'ITestObject', TypeInfo(ITestObject),
      function : Pointer begin
        Result := TTestObject.Create;
      end);

    _test := TTestObject.Create;
    TJSRegister.RegisterLiveObject(_context, 'tst', TypeInfo(ITestObject), _test);

    // Run initialization code from mmInitialize
    _context.eval(mmInitialize.Lines.Text, '<initialize>');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeAppEnvironment;
  Initialize;
end;

procedure TForm1.LogCallBack(S: string);
begin
  TThread.Queue(nil, procedure begin
    mmLog.Lines.Add(s);
  end);
end;

procedure TForm1.TestFunc2Click(Sender: TObject);
begin
  var ctx := TRttiContext.Create;
  var m := ctx.GetType(TypeInfo(TFunc<string>)).GetMethods;
  //var m := ctx.GetType(TypeInfo(TGenFunc<string>)).GetMethods;

  var p := m[0].GetParameters[0];

  ShowMessage(p.Name);
  if p.ParamType is TRttiInterfaceType then
  begin
    var it := (p.ParamType as TRttiInterfaceType);

    if TIntfFlag.ifMethRef in it.IntfFlags then
      ShowMessage(it.Name);
  end;

//  var ii: IVirtualMethodImplementation := TVirtualMethodImplementation.Create(TypeInfo(TTestFunc));
//  _testFunc := ii.Func;
//  var o := _testFunc('test');
//  ShowMessage(o);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lblPosition.Text := Format('Position: %d:%d', [mmCode.CaretPosition.Line + 1, mmCode.CaretPosition.Pos]);
end;

procedure TForm1.TestFuncClick(Sender: TObject);
begin
  var ii: IInterface := TVirtualMethodImplementation.Create(TypeInfo(TTestFunc), nil);

  var t: IInterface;
  ii.QueryInterface(IInterface, t);

  var v: TValue;
  TValue.Make(@t, TypeInfo(TTestFunc), v);
  _testFunc := TTestFunc(Pointer(v.GetReferenceToRawData^));

  var o := _testFunc('test');
  ShowMessage(o);
end;

{ TTestObject }

procedure TTestObject.Attach(const Data: IList);
begin
  var i := 0;
  if Data <> nil then
    for var o in Data do
      inc(i);

  ShowMessage(i.ToString);
end;

constructor TTestObject.Create;
begin
  _data := CList<ITestObject>.Create;
end;

function TTestObject.get_Data: IList;
begin
  Result := _data as IList;
end;

function TTestObject.get_Names(const Value: string): string;
begin
  Result := Value;
end;

function TTestObject.NextID: Int64;
begin
  dec(_id);
  Result := _id;
end;

function TTestObject.Test(const Value: CObject): CObject;
begin
//  var jo: JSObjectReference;
//  if Value.TryGetValue<JSObjectReference>(jo) then
//    jo.Invoke('QueryInterface', nil, nil);
end;

function TTestObject.Test2(const Value: IProject): IInterface;
begin
  var t := Value.GetType;

  var props := t.GetProperties;
  for var p in props do
    ShowMessage(p.Name);

//  var b: IBaseInterface;
//  if Supports(Value, IBaseInterface, b) then
//    ShowMessage(b.GetHashCode.ToString);

//  var an: IAddNew;
//  if Interfaces.Supports<IAddNew>(Value, an) then
//    an.AddNew;
end;

function TTestObject.Test3(const Value: &Type) : CObject;
begin
  Result := Value.ToString;
end;

function TTestObject.Test4(const Value: TFunc<CObject>): CObject;
begin
  Result := 'Done';
end;

function TTestObject.Test5(const Value: TTestFunc) : CObject;
begin
  Result := Value('Test 123');
end;

end.

