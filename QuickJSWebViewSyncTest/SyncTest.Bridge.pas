unit SyncTest.Bridge;

// Transport between the webview page and Delphi, built on the same
// TBrowserConnector / ScriptGate skeleton as the Lynx Apps integration.
//
//  page -> Delphi : location.href = 'qjs-sync:SyncRequest(<requestId>,<base64 json>)'
//                   ScriptGate calls the published SyncRequest method below
//  Delphi -> page : window.syncHost.onResponse('<requestId>', '<base64 json>', isError)
//                   window.syncHost.receive('<channel>', '<base64 json>')   (pushes)

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Diagnostics,
  ADato.AI.BrowserConnector.intf, ADato.AI.BrowserConnector.impl;

type
  TSyncRequestHandler = reference to function(const RequestJson: string): string;

  {$M+}
  TSyncBrowserConnector = class(TBrowserConnector)
  private
    FOnRequest: TSyncRequestHandler;
    procedure SendResponse(const RequestId, Json: string; IsError: Boolean);
  public
    const Scheme = 'qjs-sync';
    constructor Create;
    // Push a message to the page
    procedure Push(const Channel, Json: string);
    property OnRequest: TSyncRequestHandler read FOnRequest write FOnRequest;
  published
    // Called by ScriptGate (via RTTI, on the main thread)
    procedure SyncRequest(const RequestId: string; const PayloadBase64: string);
  end;

  // JS function registered by sync-bridge.js: request JSON in, response JSON out
  TJsRequestHandler = reference to function(const RequestJson: string): string;

  // Exposed to QuickJS as the global 'webview'
  IWebViewChannel = interface
    ['{C7D1B2E4-58A9-4F3B-9E26-0A4D8C7B1F92}']
    // Sends Json to window.syncHost.receive(Channel, data) in the page
    procedure Post(const Channel: string; const Json: string);
    // sync-bridge.js registers the function that handles page requests
    procedure SetRequestHandler(const Handler: TJsRequestHandler);
  end;
  {$M-}

  TWebViewChannel = class(TInterfacedObject, IWebViewChannel)
  private
    FConnector: TSyncBrowserConnector; // not ref counted, the form keeps it alive
    FRequestHandler: TJsRequestHandler;
  public
    constructor Create(const Connector: TSyncBrowserConnector);
    procedure Post(const Channel: string; const Json: string);
    procedure SetRequestHandler(const Handler: TJsRequestHandler);
    // Runs the JS request handler
    function  HandleRequest(const RequestJson: string): string;
    // Releases the JS handler; call before the QuickJS context goes away
    procedure Detach;
  end;

implementation

uses
  SyncTest.Trace;

{ TSyncBrowserConnector }

constructor TSyncBrowserConnector.Create;
begin
  inherited Create(Scheme);
end;

var
  LastRequestEnd: TStopwatch;   // when the previous request finished

procedure TSyncBrowserConnector.SyncRequest(const RequestId, PayloadBase64: string);
begin
  if IsShutdown or (ScriptGate = nil) then
    Exit;
  var gapMs: Int64 := -1;
  if LastRequestEnd.IsRunning then
    gapMs := LastRequestEnd.ElapsedMilliseconds;
  var sw := TStopwatch.StartNew;
  var sinkTicks := Trace.SinkTicks;
  var sinkLines := Trace.SinkLines;
  var request := DecodeBase64Param(PayloadBase64);
  Trace.Enter('WEBVIEW -> DELPHI', Format('SyncRequest(%s, %s)', [Trace.Quote(RequestId), request]));
  try
    if not Assigned(FOnRequest) then
      raise Exception.Create('No request handler');
    SendResponse(RequestId, FOnRequest(request), False);
  except
    on E: Exception do
    begin
      var err := TJSONObject.Create;
      try
        err.AddPair('ok', TJSONFalse.Create);
        err.AddPair('error', E.Message);
        SendResponse(RequestId, err.ToJSON, True);
      finally
        err.Free;
      end;
    end;
  end;
  var traceMs := (Trace.SinkTicks - sinkTicks) * 1000 div TStopwatch.Frequency;
  var lines := Trace.SinkLines - sinkLines;
  Trace.Line('TIMING', Format('%s handled in %d ms (%d ms of it writing %d trace lines); %d ms since the previous request finished',
    [RequestId, sw.ElapsedMilliseconds, traceMs, lines, gapMs]));
  Trace.Leave;
  LastRequestEnd := TStopwatch.StartNew;
end;

procedure TSyncBrowserConnector.SendResponse(const RequestId, Json: string; IsError: Boolean);
begin
  Trace.Line('DELPHI -> WEBVIEW', Format('syncHost.onResponse(%s, %s)', [Trace.Quote(RequestId), Trace.Summary(Json)]));
  CallScript(Format('window.syncHost && window.syncHost.onResponse(''%s'', ''%s'', %s)',
    [EscapeForJsString(RequestId), EncodeBase64ForJs(Json), BoolToStr(IsError, True).ToLower]));
end;

procedure TSyncBrowserConnector.Push(const Channel, Json: string);
begin
  Trace.Line('DELPHI -> WEBVIEW', Format('syncHost.receive(%s, %s)', [Trace.Quote(Channel), Trace.Summary(Json)]));
  CallScript(Format('window.syncHost && window.syncHost.receive(''%s'', ''%s'')',
    [EscapeForJsString(Channel), EncodeBase64ForJs(Json)]));
end;

{ TWebViewChannel }

constructor TWebViewChannel.Create(const Connector: TSyncBrowserConnector);
begin
  inherited Create;
  FConnector := Connector;
end;

procedure TWebViewChannel.Post(const Channel, Json: string);
begin
  Trace.Enter('QJS -> DELPHI', Format('webview.Post(%s, %s)', [Trace.Quote(Channel), Trace.Summary(Json)]));
  try
    if (FConnector <> nil) and not FConnector.IsShutdown then
      FConnector.Push(Channel, Json);
  finally
    Trace.Leave;
  end;
end;

procedure TWebViewChannel.SetRequestHandler(const Handler: TJsRequestHandler);
begin
  FRequestHandler := Handler;
  Trace.Line('QJS -> DELPHI', 'webview.SetRequestHandler(<js function>)');
end;

function TWebViewChannel.HandleRequest(const RequestJson: string): string;
begin
  if not Assigned(FRequestHandler) then
    raise Exception.Create('sync-bridge.js did not register a request handler');
  Trace.Enter('DELPHI -> QJS', 'requestHandler(' + RequestJson + ')');
  try
    Result := FRequestHandler(RequestJson);
  finally
    Trace.Leave(Trace.Summary(Result));
  end;
end;

procedure TWebViewChannel.Detach;
begin
  FConnector := nil;
  FRequestHandler := nil;
end;

end.
