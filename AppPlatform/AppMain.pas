unit AppMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  QuickJS.Register.intf, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  App.intf, System.Actions, FMX.ActnList, FMX.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, System_;

type
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
    procedure acExecuteExecute(Sender: TObject);
    procedure btnCustomerClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure InitializeAppEnvironment;
    procedure Initialize;
    procedure LogCallBack(S: string);
    { Private declarations }

  public
    _context: IJSContext;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  QuickJS,
  QuickJS.Register.impl,
  QuickJS.Register.dn4d.impl,
  App.impl,
  App.Environment.impl,
  Project.intf,
  Project.impl,
  ObjectWindow,
  Customer.frame,
  QuickJS.Register.dn4d.intf,
  System.Collections.Generic, ADato.ObjectModel.List.Tracking.intf,
  ADato.ObjectModel.List.Tracking.impl,
  App.Content.intf,
  JSGeneral.frame, Winapi.Windows, App.Content.impl, System.Diagnostics,
  XMLHttpRequest.impl, XMLHttpRequest.intf,
  ADato.Extensions.intf,
  ADato.Extensions.impl, ObjectDesigner, Project.frame, App.Objects.intf,
  System.Collections, System.Rtti, App.PropertyDescriptor.intf, System.JSON,
  ADato.ObjectModel.impl, System.ClassHelpers;

{$R *.fmx}

procedure TForm1.acExecuteExecute(Sender: TObject);
begin
  mmLog.Lines.Clear;
  var st := TStopWatch.StartNew;

  var b: AnsiString := AnsiString(mmCode.Lines.Text);
  _context.eval_buf(PAnsiChar(b), Length(b), '<eval>', JS_EVAL_TYPE_MODULE);

  mmLog.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

procedure TForm1.btnCustomerClick(Sender: TObject);
begin
  _app.Windows.CreateWindow(Self, TProject.Type).
    Build.
      Bind(TProject.ObjectType.Provider.Data(nil)).
        Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  var tp := &Type.From<IProject>;
  var customerType := _app.Config.TypeByName('Customer');
  var customer_objecttype := _app.Config.ObjectType[customerType];

//  var descr := customer_objecttype.PropertyDescriptor['Customer'];
//  _app.Config.AddProperty(tp, 'Customer', 'Customer', customerType, descr);

  var cust_prop := tp.PropertyByName('Customer');

  var data := customer_objecttype.Provider.Data(nil).AsType<IList>;

  var c := data[0]; // Customer

  var project_type := _app.Config.ObjectType[&Type.From<IProject>];
  var d := project_type.Provider.Data(nil).AsType<IList>;
  var prj := d[0].AsType<IProject>;

  cust_prop.SetValue(prj, c, []);

  var v := cust_prop.GetValue(prj, []);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
//  var tp := &Type.From<ICustomer>;

//  _app.Windows.CreateWindow(Self, tp).
//    Build.
//      Bind(_app.Models.CreateOrGet(tp)).
//        Show;
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

  var ot := _app.Config.ObjectType[tp];
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

procedure TForm1.InitializeAppEnvironment;
begin
  App.Environment.impl.Environment.FormClass := TfrmObjectWindow;

  _app := TAppObject.Create(App.Environment.impl.Environment.Create);

  TProject.ObjectType.Builder := TFrameBuilder.Create(TProjectFrame);
  TProject.ObjectType.Binder := TFrameBinder.Create();
  TProject.ObjectType.Provider := ProjectProvider.Create;

  _app.Config.RegisterType(TProject.Type, TProject.ObjectType);
end;

procedure TForm1.Initialize;
begin
  if _context = nil then
  begin
    QuickJS.Register.impl.OutputLogString := LogCallBack;

    _context := TJSContext.Create(TJSRuntime.Create);

    TJSRegisterTypedObjects.Initialize(_context);

    TJSRegister.RegisterObject(_context.ctx, 'ObjectList', TypeInfo(List<JSObjectReference>),
      function : Pointer begin
        Result := CList<JSObjectReference>.Create;
      end);

    TJSRegister.RegisterObject(_context.ctx, 'List', TypeInfo(List<CObject>),
      function : Pointer begin
        Result := CList<CObject>.Create;
      end);

    TJSRegister.RegisterObject(_context.ctx, 'ObjectModel', TypeInfo(IObjectListModelChangeTracking),
      function : Pointer begin
        Result := TObjectListModelWithChangeTracking<JSObjectReference>.Create(nil);
      end);

    TJSRegister.RegisterObject(_context.ctx, 'JSBinder', TypeInfo(IContentBinder),
      function : Pointer begin
        Result := TFrameBinder.Create();
      end);

    TJSRegister.RegisterObject(_context.ctx, 'JSFrameBuilder', TypeInfo(IContentBuilder),
      function : Pointer begin
        // Result := TFrameBuilder.Create(TJSGeneralFrame);
        Result := TFrameBuilder.Create(TCustomerFrame);
      end);

    TJSRegister.RegisterObject(_context.ctx, 'XMLHttpRequest', TypeInfo(IXMLHttpRequest),
      function : Pointer begin
        Result := TXMLHttpRequest.Create;
      end);

    TJSRegister.RegisterLiveObject(_context.ctx, 'app', TypeInfo(IAppObject), _app);

    // Run initialization code from mmInitialize
    var b: AnsiString := AnsiString(mmInitialize.Lines.Text);
    _context.eval_buf(PAnsiChar(b), Length(b), '<initialize>', JS_EVAL_TYPE_MODULE);
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

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lblPosition.Text := Format('Position: %d:%d', [mmCode.CaretPosition.Line + 1, mmCode.CaretPosition.Pos]);
end;

end.
