unit AppMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  QuickJS.Register.intf, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  App.intf, System.Actions, FMX.ActnList, FMX.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent;

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
    procedure acExecuteExecute(Sender: TObject);
    procedure btnCustomerClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure InitializeAppEnvironment;
    procedure Initialize;
    procedure LogCallBack(S: string);
    { Private declarations }

  public
    _app: IAppObject;
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
  System_,
  Customer.intf,
  Customer.impl, ObjectWindow, Customer.frame, QuickJS.Register.dn4d.intf,
  System.Collections.Generic, ADato.ObjectModel.List.Tracking.intf,
  ADato.ObjectModel.List.Tracking.impl,
  App.Content.intf,
  JSGeneral.frame, Winapi.Windows, App.Content.impl, System.Diagnostics;

{$R *.fmx}

procedure TForm1.acExecuteExecute(Sender: TObject);
begin
  mmLog.Lines.Clear;
  var st := TStopWatch.StartNew;

  var b: AnsiString := AnsiString(mmCode.Lines.Text);
  // _context.eval_buf(PAnsiChar(b), Length(b), 'lynx', JS_EVAL_TYPE_GLOBAL); // JS_EVAL_TYPE_MODULE);
  _context.eval_buf(PAnsiChar(b), Length(b), 'lynx', JS_EVAL_TYPE_MODULE);

  mmLog.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

procedure TForm1.btnCustomerClick(Sender: TObject);
begin
  _app.Windows.CreateWindow(Self, TCustomer.CustomerType.GetType).
    Build.
      Bind(TCustomer.CustomerType.Provider.Data).
        Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if DataList <> nil then
  begin
    var o := DataList[0];
    var tp: &Type;

    var ref: IJSObjectReference;
    if o.TryAsType<IJSObjectReference>(ref) then
      tp := ref.GetType else
      tp := o.GetType;

    var prop := tp.PropertyByName('Name');
    var s: CObject;

    if prop <> nil then
      s := prop.GetValue(o, []) else
      s := o;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin

//  var tp := &Type.From<ICustomer>;

//  _app.Windows.CreateWindow(Self, tp).
//    Build.
//      Bind(_app.Models.CreateOrGet(tp)).
//        Show;
end;

procedure TForm1.InitializeAppEnvironment;
begin
  App.Environment.impl.Environment.FormClass := TfrmObjectWindow;

  _app := TAppObject.Create(App.Environment.impl.Environment.Create);

  TCustomer.CustomerType.Builder := TFrameBuilder.Create(TCustomerFrame);
  TCustomer.CustomerType.Binder := TFrameBinder.Create();
  TCustomer.CustomerType.Provider := TCustomerProvider.Create;

  _app.Config.RegisterType(TCustomer.CustomerType.GetType, TCustomer.CustomerType);
end;

procedure TForm1.Initialize;
begin
  if _context = nil then
  begin
    QuickJS.Register.impl.OutputLogString := LogCallBack;

    _context := TJSContext.Create(TJSRuntime.Create);

    TJSRegisterTypedObjects.Initialize(_context);

    TJSRegister.RegisterObject(_context.ctx, 'ObjectList', TypeInfo(List<IJSObjectReference>),
      function : Pointer begin
        Result := CList<IJSObjectReference>.Create;
      end);

    TJSRegister.RegisterObject(_context.ctx, 'List', TypeInfo(List<CObject>),
      function : Pointer begin
        Result := CList<CObject>.Create;
      end);

    TJSRegister.RegisterObject(_context.ctx, 'ObjectModel', TypeInfo(IObjectListModelChangeTracking),
      function : Pointer begin
        Result := TObjectListModelWithChangeTracking<IJSObjectReference>.Create(nil);
      end);

    TJSRegister.RegisterObject(_context.ctx, 'JSBinder', TypeInfo(IContentBinder),
      function : Pointer begin
        Result := TJSFrameBinder.Create();
      end);

    TJSRegister.RegisterObject(_context.ctx, 'JSFrameBuilder', TypeInfo(IContentBuilder),
      function : Pointer begin
        // Result := TFrameBuilder.Create(TJSGeneralFrame);
        Result := TFrameBuilder.Create(TCustomerFrame);
      end);

    TJSRegister.RegisterLiveObject(_context.ctx, 'app', TypeInfo(IAppObject), _app);
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
