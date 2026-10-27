unit App.WebFrameWithWindowFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.WebBrowser,
  SG.ScriptGate,
  System_, System.Collections.Generic, App.Component.intf,
  App.FrameWithWindowFrame, FMX.Controls.Presentation, FMX.Layouts;

type
  // Shared with queued callbacks so they can detect that the frame was destroyed
  TLifetimeToken = class(TInterfacedObject)
  public
    Alive: Boolean;
  end;

  TWebFrameWithWindowFrameSupport = class(TFmxFrameWithWindowFrameSupport)
    WebBrowser1: TWebBrowser;
    Layout1: TLayout;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FScriptGate: TScriptGate;
    FLifetime: TLifetimeToken;
    FLifetimeRef: IInterface;
    FScriptQueue: System.Generics.Collections.TQueue<TPair<string, TProc<string>>>;
    FScriptRunning: Boolean;
    FChildren: List<IComponent>;
    FOnChildrenLoaded: TNotifyEvent;
    FReady: Boolean;
    FCompletionProcs: System.Generics.Collections.TList<TProc>;

    procedure WebBrowserDidFinishLoad(ASender: TObject);
    procedure ExecuteScript(const Script: string);
    procedure RunScript(const Script: string; const OnResult: TProc<string>);
    procedure RunNextScript;
    procedure SetReady;

    const WaitForTimeout = 30000; // ms

    const Html =
      '<!DOCTYPE html>' +
      '<html>' +
      '<head><meta charset="utf-8"><title>Customer</title></head>' +
      '<body style="font-family: Segoe UI, sans-serif; margin: 16px;">' +
      '  <label for="Customer_Name">Name</label><br>' +
      '  <input type="text" id="Customer_Name" name="Customer_Name" style="width: 300px;">' +
      '</body>' +
      '</html>';

  protected
    // Returns a TWebComponent for each form element inside the html page,
    // as collected during the last call to RefreshChildren
    function  get_Children: List<IComponent>; override;

    // The frame is ready once the html page has loaded and its elements have been collected.
    // Without CompletionProc this blocks for at most WaitForTimeout ms and returns False on timeout.
    function  WaitFor(CompletionProc: TProc = nil) : Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Reads the form elements (and their current values) from the html page.
    // The page is queried asynchronously, OnChildrenLoaded fires when done.
    procedure RefreshChildren;

    property OnChildrenLoaded: TNotifyEvent read FOnChildrenLoaded write FOnChildrenLoaded;

  published
    // Called from the web page (through ScriptGate) when the value of an element changes.
    // Key and Value are url encoded, Sequence orders the notifications.
    procedure ElementChanged(const Key, Value: string; Sequence: Integer);
  end;

implementation

uses
  System.NetEncoding, System.Diagnostics,
  App.Component.Web.impl;

{$R *.fmx}

function HtmlToDataUrl(const Html: string): string;
begin
  // CharsPerLine = 0: no line breaks inside the base64 data
  var encoding := TBase64Encoding.Create(0);
  try
    Result := 'data:text/html;charset=utf-8;base64,' + encoding.EncodeBytesToString(TEncoding.UTF8.GetBytes(Html));
  finally
    encoding.Free;
  end;
end;

{ TWebFrameWithWindowFrameSupport }

constructor TWebFrameWithWindowFrameSupport.Create(AOwner: TComponent);
begin
  inherited;

  FLifetime := TLifetimeToken.Create;
  FLifetime.Alive := True;
  FLifetimeRef := FLifetime;

  FScriptQueue := System.Generics.Collections.TQueue<TPair<string, TProc<string>>>.Create;
  FCompletionProcs := System.Generics.Collections.TList<TProc>.Create;
  FChildren := CList<IComponent>.Create;

  FScriptGate := TScriptGate.Create(Self, WebBrowser1, 'appframe');
  WebBrowser1.OnDidFinishLoad := WebBrowserDidFinishLoad;
  // LoadFromStrings is ignored while WebView2 is still initializing, Navigate is not
  WebBrowser1.Navigate(HtmlToDataUrl(Html));
end;

destructor TWebFrameWithWindowFrameSupport.Destroy;
begin
  FLifetime.Alive := False;
  FScriptQueue.Free;
  FCompletionProcs.Free;
  inherited;
end;

procedure TWebFrameWithWindowFrameSupport.Button1Click(Sender: TObject);
begin
;
end;

procedure TWebFrameWithWindowFrameSupport.SetReady;
begin
  if FReady then
    Exit;

  FReady := True;

  var procs := FCompletionProcs.ToArray;
  FCompletionProcs.Clear;
  for var proc in procs do
    proc();
end;

function TWebFrameWithWindowFrameSupport.WaitFor(CompletionProc: TProc) : Boolean;
begin
  var p := Parent;
  while (p <> nil) do
  begin
    if ((p is TControl) and not (p as TControl).Visible) or ((p is TForm) and not (p as TForm).Visible) then
      raise Exception.Create('Window must be visible before WaitFor() can be called');
    p := p.Parent;
  end;

  if Assigned(CompletionProc) then
  begin
    if FReady then
      CompletionProc() else
      FCompletionProcs.Add(CompletionProc);
    Exit(False);
  end;

  // WebView2 initializes and loads on the main thread, keep processing messages while waiting
  var lifetime := FLifetimeRef;
  var stopwatch := TStopwatch.StartNew;
  while not FReady and (stopwatch.ElapsedMilliseconds < WaitForTimeout) do
  begin
    Application.ProcessMessages;
    CheckSynchronize(10);
    if not (lifetime as TLifetimeToken).Alive then
      Exit(False);
  end;

  Result := FReady;
end;

procedure TWebFrameWithWindowFrameSupport.ExecuteScript(const Script: string);
begin
  RunScript(Script, nil);
end;

procedure TWebFrameWithWindowFrameSupport.RunScript(const Script: string; const OnResult: TProc<string>);
begin
  // ScriptGate keeps a single result callback for Edge, so scripts must run one at a time
  FScriptQueue.Enqueue(TPair<string, TProc<string>>.Create(Script, OnResult));
  RunNextScript;
end;

procedure TWebFrameWithWindowFrameSupport.RunNextScript;
begin
  if FScriptRunning or (FScriptQueue.Count = 0) then
    Exit;

  FScriptRunning := True;
  var item := FScriptQueue.Dequeue;
  var lifetime := FLifetimeRef;

  FScriptGate.CallScript(item.Key,
    procedure(const R: string)
    begin
      // Continue outside ScriptGate's callback, it clears its result callback after we return
      TThread.ForceQueue(nil,
        procedure
        begin
          if not (lifetime as TLifetimeToken).Alive then
            Exit;
          FScriptRunning := False;
          if Assigned(item.Value) then
            item.Value(R);
          RunNextScript;
        end);
    end);
end;

function TWebFrameWithWindowFrameSupport.get_Children: List<IComponent>;
begin
  Result := FChildren;
end;

procedure TWebFrameWithWindowFrameSupport.RefreshChildren;
begin
  RunScript(TWebComponent.CollectElementsScript,
    procedure(Json: string)
    begin
      FChildren := TWebComponent.UpdateChildren(Self, FChildren, Json, ExecuteScript);
      SetReady;
      if Assigned(FOnChildrenLoaded) then
        FOnChildrenLoaded(Self);
    end);
end;

procedure TWebFrameWithWindowFrameSupport.WebBrowserDidFinishLoad(ASender: TObject);
begin
  ExecuteScript(TWebComponent.ChangeListenerScript(FScriptGate.Scheme, 'ElementChanged'));
  RefreshChildren;
end;

procedure TWebFrameWithWindowFrameSupport.ElementChanged(const Key, Value: string; Sequence: Integer);
begin
  var decodedKey := TNetEncoding.URL.Decode(Key);
  for var c in FChildren do
    if (c.GetObject is TWebComponent) and (TWebComponent(c.GetObject).Key = decodedKey) then
    begin
      TWebComponent(c.GetObject).ValueChangedInPage(TNetEncoding.URL.Decode(Value), Sequence);
      Exit;
    end;
end;

end.
