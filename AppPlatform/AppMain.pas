unit AppMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  QuickJS.Register.intf, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  App.intf, System.Actions, FMX.ActnList, FMX.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, System_,
  Project.intf, System.Collections, System.Collections.Generic, System.Rtti,
  App.Environment.intf, App.Windows.intf, App.QuickJSBridge.intf, FMX.TabControl;

type
  {$M+}
  ITestObject = interface;
  TTestFunc = reference to function(const Param: string) : string;
  TGenFunc<T> = reference to function : T;

  TForm1 = class(TForm)
    Layout1: TLayout;
    mmCustomers: TMemo;
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
    Button6: TButton;
    Button7: TButton;
    TabControl1: TTabControl;
    tsCustomers: TTabItem;
    tsOpenProject: TTabItem;
    Memo1: TMemo;
    tsPolarion: TTabItem;
    mmPolarion: TMemo;
    procedure acExecuteExecute(Sender: TObject);
    procedure btnCustomerClick(Sender: TObject);
    procedure btnExecResultClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestFunc2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TestFuncClick(Sender: TObject);
  private
    procedure InitializeAppEnvironment;
    procedure Initialize;
    procedure LogCallBack(S: string);

    function CreateTestFunc : TTestFunc;
    function GetMemoFromActiveTab: TMemo;
    function GetCode: AnsiString;
    { Private declarations }

  protected
    function IsOpen(const AObject: CObject) : Boolean;
    function Open(const AObject: CObject) : Boolean;
    function Close(const AObject: CObject) : Boolean;

  public
    class var _quickJSBridge: IAppQuickJSBridge;
    class var _runtime: IJSRuntime;

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
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function get_Data: IList;
    function get_Names(const Value: string): string;
    procedure SaveToFile(const FileName: string; const Data: CObject);
    function Test(const Value: CObject) : CObject;
    function Test2 : IWindow;
    function Test3(const Value: &Type) : CObject;
    function Test4(const Value: TFunc<CObject>) : CObject;
    function Test5(const Value: TTestFunc) : CObject;

    function NextID: Int64;

    procedure Attach(const Data: IList);
    property Names[const Value: string]: string read get_Names;

    property Data: IList read get_Data;
    property ID: CObject read get_ID write set_ID;
  end;

  TTestObject = class(TBaseInterfacedObject, ITestObject)
  protected
    _nextID: Int64;

    _id: CObject;
    _data: List<ITestObject>;

    procedure Attach(const Data: IList);

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);

    function get_Data: IList;

    function get_Names(const Value: string): string;

    function NextID: Int64;

    procedure SaveToFile(const FileName: string; const Data: CObject);

    function Test(const Value: CObject) : CObject;
    function Test2 : IWindow;
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
  QuickJS.Register.impl,
  QuickJS.Register.dn4d.impl,
  App.impl,
  App.Environment.impl,
  FMX.App.MasterForm,
  Project.impl,
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
  QuickJS.VirtualMethod.impl, App.Storage.intf, App.Storage.impl,
  App.Config.intf, App.Windows.impl, App.Component.intf,
  ProjectSelection.frame;

{$R *.fmx}

function TForm1.GetMemoFromActiveTab : TMemo;

  function Search(Controls: TControlList) : TMemo;
  begin
    Result := nil;

    for var c in Controls do
    begin
      if c is TMemo then
        Result := c as TMemo else
        Result := Search(c.Controls);

      if Result <> nil then
        Exit(Result);
    end;
  end;

begin
  Result := Search(TabControl1.ActiveTab.Controls);
end;

function TForm1.GetCode : AnsiString;
begin
  var mm := GetMemoFromActiveTab;
  if mm <> nil then
    Result := AnsiString(mm.Lines.Text);
end;

procedure TForm1.acExecuteExecute(Sender: TObject);
begin
  mmLog.Lines.Clear;
  var st := TStopWatch.StartNew;
  _context.eval(GetCode, '<eval>');
  mmLog.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

procedure TForm1.btnExecResultClick(Sender: TObject);
begin
  mmLog.Lines.Clear;
  var st := TStopWatch.StartNew;

  var r := _context.eval_with_result(GetCode, '<eval>');

  mmLog.Lines.Add('result: ' + r.ToString);
  mmLog.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

procedure TForm1.btnCustomerClick(Sender: TObject);
begin
  _app.Environment.CreateWindow(TProject.Type, _app.Environment.MainWindow)
    .CreateFrame('Projects')
      .Bind(_app.Storage[TProject.TypeDescriptor.StorageName])
        .Show(nil);
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
  var tp := &Type.From<IProject>;

  var cust_prop := tp.PropertyByName('Customers');

  var dscr: IPropertyDescriptor;
  if Interfaces.Supports<IPropertyDescriptor>(cust_prop, dscr) then
  begin
    var cust_type := cust_prop.GetType;
    var dscr_type := dscr.GetType;

  end;

  var project := _app.Storage['Projects'][0];

  var o := cust_prop.GetValue(project, []);

  var customers: IList := o.AsType<IList>;

//  if not o.TryGetValue<IList>(customers) then
//  begin
//    var storage: IStorage := TStorage.Create(_app.Config.TypeByName('Customer'), 'Customers');
//    storage.Attach(CList<CObject>.Create);
//    customers := storage;
//    cust_prop.SetValue(project, CObject.From<IList>(customers), []);
//  end;

  var c := _app.Storage['Customers'][0];
  var m := dscr.Marshaller.Marshal(nil, c);
  customers.Add(m);

  ShowMessage(customers.Count.ToString);
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

procedure TForm1.Button6Click(Sender: TObject);
begin
  var t := _app.Config.TypeByName('Customer');
  var ctx := TRttiContext.Create;
  try
    ctx.GetType(t.GetTypeInfo).QualifiedName;
  finally
    ctx.Free;
  end;

  // var f := TForm1.Create(Self);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  var c := _app.Storage['Customers'][0];
  var t := c.GetType;
  var prop := t.PropertyByName('Address');

  var a := prop.GetValue(c, []);
//  for var p in props do
//    ShowMessage(p.Name);

//  var customerType := _app.Config.TypeByName('Customer');
//
//  var props := customerType.GetProperties;
//
end;

function TForm1.Close(const AObject: CObject): Boolean;
begin

end;

function TForm1.CreateTestFunc: TTestFunc;
begin
  Result := function(const Param: string) : string begin
    Result := 'Done';
  end;
end;

procedure TForm1.InitializeAppEnvironment;
begin
  {$IFDEF FRAMEWORK_FMX}
  // _app := TAppObject.Create(App.Environment.impl.Environment.Create(Self));
  _app := TAppObject.Create(App.Environment.impl.Environment.Create(TAppMasterForm, nil));
  {$ENDIF}

  _app.Config.RegisterWindow('Projects', function(const Window: IWindow) : IWindowFrame begin
    Result := TWindowFrame.Create(Window, TProjectFrame.Create(Window.Control as TComponent));
  end);

  _app.Config.RegisterWindow('ProjectSelection', function(const Window: IWindow) : IWindowFrame begin
    Result := TWindowFrame.Create(Window, TProjectSelectionFrame.Create(Window.Control as TComponent));
  end);

  _app.Config.RegisterWindow('Customers', function(const Window: IWindow) : IWindowFrame begin
    Result := TWindowFrame.Create(Window, TCustomerFrame.Create(Window.Control as TComponent));
  end);

  // TProject.TypeDescriptor.Builder := TFrameBuilder.Create(TProjectFrame);

  TProject.TypeDescriptor.Binder := TFrameBinder.Create();
  TProject.TypeDescriptor.Provider := ProjectProvider.Create(TProject.TypeDescriptor);

  _app.Factory.RegisterType(TProject.Type, function : CObject begin
    Result := CObject.From<IProject>(TProject.Create);
  end);

  _app.Factory.RegisterCollection(TProject.Type, function : CObject begin
    Result := CObject.From<List<IProject>>(CList<IProject>.Create);
  end);

  var storage := _app.AddStorage(TProject.Type, TProject.TypeDescriptor.StorageName);
  storage.Attach(TProject.TypeDescriptor.Provider.Data(nil));
end;

function TForm1.IsOpen(const AObject: CObject): Boolean;
begin

end;

procedure TForm1.Initialize;
begin
  if _context = nil then
  begin
    // Create the runtime instance
    _runtime := TJSRuntimeDN4D.Create;

    // Register types with constructors
    _runtime.RegisterObjectWithConstructor('List', TypeInfo(List<CObject>),
      function : Pointer begin
        Result := CList<CObject>.Create;
      end);

    _runtime.RegisterObjectWithConstructor('ObjectModel', TypeInfo(IObjectListModelChangeTracking),
      function : Pointer begin
        Result := TObjectListModelWithChangeTracking<IJSObject>.Create(nil);
      end);

    _runtime.RegisterObjectWithConstructor('JSBinder', TypeInfo(IContentBinder),
      function : Pointer begin
        Result := TFrameBinder.Create();
      end);

    _runtime.RegisterObjectWithConstructor('XMLHttpRequest', TypeInfo(IXMLHttpRequest),
      function : Pointer begin
        Result := TXMLHttpRequest.Create;
      end);

    // Register interface types (no constructor)
    _runtime.RegisterObjectType('IBaseInterface', TypeInfo(IBaseInterface));
    _runtime.RegisterObjectType('IComponent', TypeInfo(IComponent));
    _runtime.RegisterObjectType('IAddingNew', TypeInfo(IAddingNew));
    _runtime.RegisterObjectType('IAddRange', TypeInfo(IAddRange));
    _runtime.RegisterObjectType('ICloneable', TypeInfo(ICloneable));
    _runtime.RegisterObjectType('IEditState', TypeInfo(IEditState));
    _runtime.RegisterObjectType('IEditableModel', TypeInfo(IEditableModel));
    _runtime.RegisterObjectType('IOnItemChangedSupport', TypeInfo(IOnItemChangedSupport));
    _runtime.RegisterObjectType('IUpdatableObject', TypeInfo(IUpdatableObject));
    _runtime.RegisterObjectType('IStorage', TypeInfo(IStorage));
    _runtime.RegisterObjectType('IStorageSupport', TypeInfo(IStorageSupport));

    _runtime.RegisterObjectType('IProject', TProject.Type.GetTypeInfo);

    _runtime.RegisterObjectWithConstructor('ITestObject', TypeInfo(ITestObject),
      function : Pointer begin
        Result := TTestObject.Create;
      end);

    // Create the QuickJS bridge to automatically register types with QuickJS
    _quickJSBridge := CreateAppQuickJSBridge(_app.Config, _runtime);
    _quickJSBridge.RegisterExistingTypes;

    // Create context from runtime
    _context := _runtime.CreateContext;
    _context.LogString := LogCallBack;

    // Register live object instances
    _context.RegisterLiveInterfaceInstance('app', TypeInfo(IAppObject), _app);

    _test := TTestObject.Create;
    _context.RegisterLiveInterfaceInstance('tst', TypeInfo(ITestObject), _test);

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

function TForm1.Open(const AObject: CObject): Boolean;
begin
  Result := True;
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
  var mm := GetMemoFromActiveTab;
  if mm <> nil then
    lblPosition.Text := Format('Position: %d:%d', [mm.CaretPosition.Line + 1, mm.CaretPosition.Pos]);
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

function TTestObject.get_ID: CObject;
begin
  Result := _id;
end;

function TTestObject.get_Names(const Value: string): string;
begin
  Result := Value;
end;

function TTestObject.NextID: Int64;
begin
  dec(_nextID);
  Result := _nextID;
end;

procedure TTestObject.SaveToFile(const FileName: string; const Data: CObject);
begin
  var sl := TStringList.Create;
  sl.Text := Data.ToString;
  sl.SaveToFile(FileName);
  sl.Free;
end;

procedure TTestObject.set_ID(const Value: CObject);
begin
  _id := Value;
  Form1.mmLog.Lines.Add('ID set to: ' + Value.ToString);
end;

function TTestObject.Test(const Value: CObject): CObject;
begin
  var t := Value.GetType;
  var prop := t.PropertyByName('Address');
  var o := prop.GetValue(Value, []);

  var s: string := o.ToString;


//  var sl := TStringList.Create;
//  sl.Text := Value.ToString;
//  sl.SaveToFile('d:\Temp\Tasks.json');
//  sl.Free;
end;

function TTestObject.Test2 : IWindow;
begin
  Result := TWindow.Create(&Type.Unknown, nil);
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

