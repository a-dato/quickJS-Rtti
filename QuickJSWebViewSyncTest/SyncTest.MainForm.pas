unit SyncTest.MainForm;

// Left : a list of orders in a TDataControl, with detail controls bound to the
//        list model's ObjectModelContext (the current row), as in BindSimpleTest.
// Right: webview. The page runs scripts in QuickJS, builds its own UI and binds
//        it through the SAME model: per-object contexts of its own, and the
//        list model's context for "the current item".
//
// Startup:
//   1. IList<IOrder> -> TObjectListModel<IOrder> -> DataControl1.Model
//   2. Bind the detail controls to ListModel.ObjectModelContext
//   3. TSyncBrowserConnector + QuickJS; the orders are registered as source 'order'

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.SpinBox, FMX.EditBox,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.WebBrowser,
  FMX.ScrollControl.Impl, FMX.ScrollControl.WithRows.Impl,
  FMX.ScrollControl.WithCells.Impl, FMX.ScrollControl.WithEditableCells.Impl,
  FMX.ScrollControl.DataControl.Impl,
  System_, System.Collections, System.Collections.Generic,
  ADato.ObjectModel.intf, ADato.ObjectModel.List.intf,
  QuickJS.Register.intf,
  SyncTest.OrderModel, SyncTest.Bridge, SyncTest.WebBinding, SyncTest.Trace;

type
  TfrmSyncTest = class(TForm)
    lytDelphi: TLayout;
    lblTitle: TLabel;
    lblHint: TLabel;
    DataControl1: TDataControl;
    lytListButtons: TLayout;
    btnAdd: TButton;
    btnRemove: TButton;
    btnEndEdit: TButton;
    btnCancelEdit: TButton;
    lblListState: TLabel;
    tmListState: TTimer;
    lblDetailTitle: TLabel;
    lytValues: TLayout;
    lytQuantity: TLayout;
    lblQuantityCaption: TLabel;
    spinQuantity: TSpinBox;
    lblQuantityBind: TLabel;
    lytUnitPrice: TLayout;
    lblUnitPriceCaption: TLabel;
    spinUnitPrice: TSpinBox;
    lblUnitPriceBind: TLabel;
    lytSubtotal: TLayout;
    lblSubtotalCaption: TLabel;
    lblSubtotal: TLabel;
    lblSubtotalBind: TLabel;
    lytDiscount: TLayout;
    lblDiscountCaption: TLabel;
    spinDiscount: TSpinBox;
    lblDiscountBind: TLabel;
    lytTotal: TLayout;
    lblTotalCaption: TLabel;
    lblTotal: TLabel;
    lblTotalBind: TLabel;
    lblTraceTitle: TLabel;
    btnClearLog: TButton;
    mmLog: TMemo;
    Splitter1: TSplitter;
    WebBrowser1: TWebBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnEndEditClick(Sender: TObject);
    procedure btnCancelEditClick(Sender: TObject);
    procedure tmListStateTimer(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    FOrders: IList<IOrder>;
    FNextOrderNo: Integer;
    FListModel: IObjectListModel;
    FPageContexts: TArray<IObjectModelContext>;

    FRuntime: IJSRuntime;
    FContext: IJSContext;
    FConnector: TSyncBrowserConnector;
    FConnectorIntf: IInterface;   // keeps the ref counted connector alive
    FChannel: TWebViewChannel;
    FChannelIntf: IWebViewChannel;
    FWebBindings: TWebBindingHost;
    FWebBindingsIntf: IWebBindingHost;

    // Trace lines are buffered and written every 100 ms in one go: adding them
    // to the memo and the log file one by one cost 20-50 ms per line
    FPendingLog: TStringList;
    FLogTimer: TTimer;
    procedure FlushLog(Sender: TObject);

    procedure Log(const Msg: string);
    function  FindAsset(const RelPath: string): string;
    procedure BindDetailControls;
    procedure RegisterOrdersForThePage;
    procedure InitQuickJS;
    procedure ContextPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
  end;

var
  frmSyncTest: TfrmSyncTest;

implementation

{$R *.fmx}

uses
  System.StrUtils, ADato.ObjectModel.impl, ADato.ObjectModel.List.impl, ADato.ObjectModel.Binders,
  ADato.ObjectModel.List.Tracking.impl, ADato.ObjectModel.TrackInterfaces,
  QuickJS.Register.impl;

type
  // Access to the grid's "this item changed, repaint its row" method
  TDataControlAccess = class(TDataControl);

procedure TfrmSyncTest.FormCreate(Sender: TObject);
begin
  try TFile.Delete(ChangeFileExt(ParamStr(0), '.log')); except end;
  FPendingLog := TStringList.Create;
  FLogTimer := TTimer.Create(Self);
  FLogTimer.Interval := 100;
  FLogTimer.OnTimer := FlushLog;
  Trace.Sink := procedure(const Line: string) begin Log(Line); end;

  // 1. The list and its list model (as in BindSimpleTest)
  FOrders := CList<IOrder>.Create;
  FOrders.Add(TOrder.Create('Order 1', 2, 40));
  FOrders.Add(TOrder.Create('Order 2', 5, 25, 10));
  FOrders.Add(TOrder.Create('Order 3', 1, 99));
  FOrders.Add(TOrder.Create('Order 4', 10, 12.5));
  FOrders.Add(TOrder.Create('Order 5', 3, 30));

  // The list model with change tracking (as in BindSimpleTest's "Bind+Tracking"):
  // adds IAddToList / IEditableModel / IEditState. TOrderListModel limits it to
  // 2..10 items through CanAdd/CanRemove. The creator function makes the new
  // item when something calls AddNew (DataControl, button, webview).
  FNextOrderNo := FOrders.Count;
  FListModel := TOrderListModel.Create(
    function: IOrder
    begin
      Inc(FNextOrderNo);
      Result := TOrder.Create('Order ' + FNextOrderNo.ToString, 1, 10);
    end);
  FListModel.Context := FOrders as IList;
  FListModel.ObjectModelContext.OnPropertyChanged.Add(ContextPropertyChanged);

  // 2. Detail controls follow the current row
  BindDetailControls;
  DataControl1.Model := FListModel;
  Trace.Line('DELPHI', 'DataControl1.Model := ListModel (selecting a row sets ListModel.ObjectContext)');
  FListModel.ObjectContext := FOrders[0];   // start with the first row as current item

  // 3. Webview: transport, binding host, QuickJS
  FConnector := TSyncBrowserConnector.Create;
  FConnectorIntf := FConnector as IInterface;
  FChannel := TWebViewChannel.Create(FConnector);
  FChannelIntf := FChannel;
  FConnector.OnRequest :=
    function(const RequestJson: string): string
    begin
      Result := FChannel.HandleRequest(RequestJson);   // runs the JS handler in sync-bridge.js
    end;

  FWebBindings := TWebBindingHost.Create;
  FWebBindingsIntf := FWebBindings;
  RegisterOrdersForThePage;

  InitQuickJS;

  FConnector.Initialize(WebBrowser1);
  // FMX strips 'file://' and checks FileExists on the rest, so pass a plain Windows path
  WebBrowser1.Navigate('file://' + FindAsset('web\index.html'));
end;

{ Add / remove / edit session: the list model's own operations, the same calls
  the DataControl makes for "add row" and "delete row". }

procedure TfrmSyncTest.btnAddClick(Sender: TObject);
begin
  Trace.Line('DELPHI UI', 'Add: (ListModel as IAddToList).AddNew');
  var add: IAddToList;
  if Interfaces.Supports<IAddToList>(FListModel, add) then
    add.AddNew;
end;

procedure TfrmSyncTest.btnRemoveClick(Sender: TObject);
begin
  var em: IEditableModel;
  if Interfaces.Supports<IEditableModel>(FListModel, em) and em.CanRemove then
  begin
    Trace.Line('DELPHI UI', 'Remove: (ListModel as IEditableModel).Remove');
    em.Remove;
  end;
end;

procedure TfrmSyncTest.btnEndEditClick(Sender: TObject);
begin
  Trace.Line('DELPHI UI', 'End edit: (ListModel as IEditableModel).EndEdit');
  var em: IEditableModel;
  if Interfaces.Supports<IEditableModel>(FListModel, em) then
    em.EndEdit;
end;

procedure TfrmSyncTest.btnCancelEditClick(Sender: TObject);
begin
  Trace.Line('DELPHI UI', 'Cancel edit: (ListModel as IEditableModel).CancelEdit');
  var em: IEditableModel;
  if Interfaces.Supports<IEditableModel>(FListModel, em) then
    em.CancelEdit;
end;

// Polled, like BindSimpleTest's Timer1Timer
procedure TfrmSyncTest.tmListStateTimer(Sender: TObject);
begin
  var s := Format('%d items', [FOrders.Count]);
  var em: IEditableModel;
  if Interfaces.Supports<IEditableModel>(FListModel, em) then
  begin
    s := s + Format(' (%d..%d)   can: ', [TOrderListModel.MinCount, TOrderListModel.MaxCount]);
    if em.CanAdd then s := s + 'Add ';
    if em.CanEdit then s := s + 'Edit ';
    if em.CanRemove then s := s + 'Remove';
    btnAdd.Enabled := em.CanAdd;
    btnRemove.Enabled := em.CanRemove;
  end;
  var es: IEditState;
  if Interfaces.Supports<IEditState>(FListModel, es) then
  begin
    s := s + '   state: ';
    if es.IsNew then s := s + 'New '
    else if es.IsEdit then s := s + 'Edit '
    else s := s + 'idle ';
    if es.IsChanged then s := s + '(changed)';
    btnEndEdit.Enabled := es.IsEditOrNew;
    btnCancelEdit.Enabled := es.IsEditOrNew;
  end;
  lblListState.Text := s;
end;

procedure TfrmSyncTest.BindDetailControls;

  procedure BindControl(const PropName: string; const Control: TFmxObject);
  begin
    Trace.Line('DELPHI', Format('ListModel.ObjectModelContext.Bind(%s, %s)%s', [Trace.Quote(PropName), Control.Name,
      IfThen(Length(TOrder.InputsOf(PropName)) > 0, ' + Link to ' + string.Join(', ', TOrder.InputsOf(PropName)), '')]));
    BindOrderProperty(FListModel.ObjectModelContext, PropName, TPropertyBinding.CreateBindingByControl(Control));
  end;

begin
  BindControl('Quantity',  spinQuantity);
  BindControl('UnitPrice', spinUnitPrice);
  BindControl('Subtotal',  lblSubtotal);
  BindControl('Discount',  spinDiscount);   // enabled only when TOrder says Discount is editable
  BindControl('Total',     lblTotal);
end;

procedure TfrmSyncTest.RegisterOrdersForThePage;
begin
  var source: TObjectSource;
  source.ListModel := FListModel;
  source.Items :=
    function: TArray<CObject>
    begin
      Result := [];
      for var o in FOrders do
        Result := Result + [CObject.From<IOrder>(o)];
    end;
  source.KeyOf :=
    function(Obj: CObject): string
    begin
      Result := Obj.AsType<IOrder>.Name;
    end;
  source.CaptionOf := source.KeyOf;
  source.BindProperty :=
    procedure(Ctx: IObjectModelContext; PropName: string; Binding: IPropertyBinding)
    begin
      BindOrderProperty(Ctx, PropName, Binding);
    end;
  FWebBindings.RegisterSource('order', source);

  // Contexts the page opens itself: log their changes, and tell the grid
  FWebBindings.OnContextCreated :=
    procedure(Ctx: IObjectModelContext)
    begin
      Ctx.OnPropertyChanged.Add(ContextPropertyChanged);
      FPageContexts := FPageContexts + [Ctx];
    end;
end;

procedure TfrmSyncTest.ContextPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
begin
  var name: string := AProperty.Name;
  var order: string := Context.AsType<IOrder>.Name;
  if Sender = FListModel.ObjectModelContext then
    Trace.Line('CONTEXT', Format('OnPropertyChanged(%s) on the list context (%s): DataControl repaints that row itself', [name, order]))
  else
  begin
    // The DataControl only listens to the list model's own context. An edit
    // through another context (the page's "Select one" tab) needs a nudge.
    Trace.Line('CONTEXT', Format('OnPropertyChanged(%s) on a page context (%s): tell DataControl to repaint that row', [name, order]));
    TDataControlAccess(DataControl1).DoDataItemChangedInternal(Context);
  end;
  // The Delphi detail controls are framework bindings and do not trace; show their result
  Trace.Line('DELPHI UI', Format('detail controls show: Quantity %s, UnitPrice %s, Subtotal %s, Discount %s, Total %s',
    [spinQuantity.Text, spinUnitPrice.Text, lblSubtotal.Text, spinDiscount.Text, lblTotal.Text]));
end;

procedure TfrmSyncTest.FormDestroy(Sender: TObject);
begin
  // Unbind the page's bindings and release JS functions while QuickJS is alive
  FWebBindings.Detach;
  FChannel.Detach;
  FConnector.Shutdown;
  for var ctx in FPageContexts do
    ctx.OnPropertyChanged.Remove(ContextPropertyChanged);
  FListModel.ObjectModelContext.OnPropertyChanged.Remove(ContextPropertyChanged);
  DataControl1.Model := nil;
  FListModel.ObjectModelContext.Unbind;
  FContext := nil;
  FRuntime := nil;
  FWebBindingsIntf := nil;
  FChannelIntf := nil;
  FConnectorIntf := nil;
  Trace.Sink := nil;
  FLogTimer.Enabled := False;
  FlushLog(nil);
  FPendingLog.Free;
end;

procedure TfrmSyncTest.InitQuickJS;
begin
  FRuntime := TJSRuntime.Create;
  FRuntime.RegisterObjectType('IWebBindingHost', TypeInfo(IWebBindingHost));
  FRuntime.RegisterObjectType('IWebViewChannel', TypeInfo(IWebViewChannel));

  FContext := FRuntime.CreateContext;
  if FContext = nil then
    raise Exception.Create('QuickJS could not be loaded (quickjs64.dll)');
  // console.log inside QuickJS shows up in the trace
  FContext.LogString := procedure(S: string) begin if not S.StartsWith('[QJS]') then Trace.Line('QJS console', S); end;

  // The two globals sync-bridge.js works with
  FContext.RegisterLiveInterfaceInstance('bindings', TypeInfo(IWebBindingHost), FWebBindingsIntf);
  FContext.RegisterLiveInterfaceInstance('webview', TypeInfo(IWebViewChannel), FChannelIntf);

  Trace.Enter('DELPHI -> QJS', 'run qjs\sync-bridge.js  (startup: it registers its handlers)');
  try
    FContext.eval(TFile.ReadAllText(FindAsset('qjs\sync-bridge.js'), TEncoding.UTF8), 'sync-bridge.js');
  finally
    Trace.Leave;
  end;
end;

function TfrmSyncTest.FindAsset(const RelPath: string): string;
begin
  // Next to the exe, or in the project folder when running from Win64\Debug
  var exeDir := ExtractFilePath(ParamStr(0));
  for var dir in [exeDir, exeDir + '..\..\'] do
  begin
    var p := System.IOUtils.TPath.GetFullPath(System.IOUtils.TPath.Combine(dir, RelPath));
    if TFile.Exists(p) then
      Exit(p);
  end;
  raise Exception.CreateFmt('File not found: %s', [RelPath]);
end;

procedure TfrmSyncTest.Log(const Msg: string);
begin
  FPendingLog.Add(Msg);   // written by FlushLog
end;

// Clears the trace panel; the .log file next to the exe keeps everything
procedure TfrmSyncTest.btnClearLogClick(Sender: TObject);
begin
  FPendingLog.Clear;
  mmLog.Lines.Clear;
end;

procedure TfrmSyncTest.FlushLog(Sender: TObject);
const
  MaxMemoLines = 2000;
begin
  if (FPendingLog = nil) or (FPendingLog.Count = 0) then Exit;
  try
    TFile.AppendAllText(ChangeFileExt(ParamStr(0), '.log'), FPendingLog.Text, TEncoding.UTF8);
  except
  end;
  mmLog.Lines.BeginUpdate;
  try
    mmLog.Lines.AddStrings(FPendingLog);
    while mmLog.Lines.Count > MaxMemoLines do
      mmLog.Lines.Delete(0);
  finally
    mmLog.Lines.EndUpdate;
  end;
  mmLog.GoToTextEnd;
  FPendingLog.Clear;
end;

end.
