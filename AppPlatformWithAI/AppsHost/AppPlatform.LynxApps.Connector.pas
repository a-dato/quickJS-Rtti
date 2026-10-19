unit AppPlatform.LynxApps.Connector;

// The ScriptGate side of the Lynx Apps pages in AppPlatform: the same protocol
// TLynxAppsBrowserConnector answers in Lynx (adato_lynx_master/LynxBot), backed
// by AppPlatform.LynxApps.Host instead of the Lynx model. The pages call
//   lynx-integration:IntegrationManagerRequest(<requestId>,<base64 { method, payload }>)
// and get the answer through window.onIntegrationManagerResponse / ...Error.
//
// Methods: LoadAssistantProfile, LoadLynxApps, SaveLynxApp, DeleteLynxApp,
// GetLynxAppTokenStatus, SetLynxAppTokens, RedactLynxAppTokens, RunIntegration
// (asynchronous, with live run logs) and CancelIntegration.

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections,
  ADato.AI.BrowserConnector.impl,
  AppPlatform.LynxApps.Host;

type
  {$M+}
  TAppPlatformLynxAppsConnector = class(TBrowserConnector)
  private
    // Token values typed in this window's editor stay staged here until the app
    // is saved, and are dropped with the window like other unsaved edits.
    _staged: TObjectDictionary<string, TTokenValues>;
    // Cancel flags of the runs in flight, by request id.
    _runs: TDictionary<string, PInteger>;

    function  DispatchRequest(const MethodName, PayloadJson: string): string;
    function  SaveAppJson(const Payload: TJSONObject): string;
    function  DeleteAppJson(const Payload: TJSONObject): string;
    procedure RunIntegrationAsync(const RequestId: string; const Payload: TJSONObject);
    function  CancelIntegrationJson(const Payload: TJSONObject): string;

    // Token broker, as TLynxAppTokenBroker in Lynx.
    function  StagedTokensFor(const AppId: string): TTokenValues;
    function  TokenValuesFor(const AppId: string): TTokenValues;
    function  TokensByName(const AppId: string; const TokenRefs: TJSONValue): TTokenValues;
    procedure CommitTokens(const StagedId, PreviousId, AppId: string; const TokenIds: TJSONValue);
    function  TokenStatusJson(const AppId: string): string;
    function  SetTokensJson(const Payload: TJSONObject): string;
    function  RedactTokensJson(const Payload: TJSONObject): string;

    procedure SendRunLog(const AMessage: string);
    procedure SendResponse(const RequestId, ResultJson: string; const IsError: Boolean = False);
  protected
    procedure DoShutdown; override;
    // Only pages served from the Lynx Apps base url get the bridge.
    function  IsTrustedPageUrl(const Url: string): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

  published
    procedure IntegrationManagerRequest(const RequestId: string; const PayloadBase64: string);
  end;

implementation

uses
  System.Classes, System.Rtti,
  QuickJS.Register.intf,
  App.intf,
  App.Tokens,
  App.TypeScripts;

function PayloadText(const Payload: TJSONObject; const Name: string): string;
begin
  Result := '';
  if Payload = nil then
    Exit;

  var value := Payload.GetValue(Name);
  if (value <> nil) and not (value is TJSONNull) then
    Result := value.Value;
end;

{ TAppPlatformLynxAppsConnector }

constructor TAppPlatformLynxAppsConnector.Create;
begin
  inherited Create('lynx-integration');
  _staged := TObjectDictionary<string, TTokenValues>.Create([doOwnsValues]);
  _runs := TDictionary<string, PInteger>.Create;
end;

destructor TAppPlatformLynxAppsConnector.Destroy;
begin
  // Runs still in flight own their cancel flag (see RunIntegrationAsync).
  _runs.Free;
  _staged.Free;
  inherited;
end;

function TAppPlatformLynxAppsConnector.IsTrustedPageUrl(const Url: string): Boolean;
begin
  Result := TLynxAppsHost.IsTrustedPageUrl(Url);
end;

procedure TAppPlatformLynxAppsConnector.DoShutdown;
begin
  for var flag in _runs.Values do
    AtomicExchange(flag^, 1);
  inherited;
end;

procedure TAppPlatformLynxAppsConnector.IntegrationManagerRequest(const RequestId: string; const PayloadBase64: string);
begin
  if IsShutdown or (ScriptGate = nil) then
    Exit;

  try
    var envelope := TJSONObject.ParseJSONValue(DecodeBase64Param(PayloadBase64)) as TJSONObject;
    try
      if envelope = nil then
        raise Exception.Create('Invalid Lynx Apps request envelope.');

      var method := PayloadText(envelope, 'method');
      if method = '' then
        raise Exception.Create('Lynx Apps request is missing method.');

      var payload := envelope.GetValue('payload') as TJSONObject;
      if method = 'RunIntegration' then
        RunIntegrationAsync(RequestId, payload)
      else if method = 'CancelIntegration' then
        SendResponse(RequestId, CancelIntegrationJson(payload))
      else
      begin
        var payloadJson := '{}';
        if payload <> nil then
          payloadJson := payload.ToJSON;
        SendResponse(RequestId, DispatchRequest(method, payloadJson));
      end;
    finally
      envelope.Free;
    end;
  except
    on E: Exception do
      SendResponse(RequestId, E.Message, True);
  end;
end;

function TAppPlatformLynxAppsConnector.DispatchRequest(const MethodName, PayloadJson: string): string;
begin
  if MethodName = 'LoadAssistantProfile' then
    Exit(LynxAppsHost.ProfileJson);

  if MethodName = 'LoadLynxApps' then
    Exit(LynxAppsHost.LoadAppsJson);

  var payload := TJSONObject.ParseJSONValue(PayloadJson) as TJSONObject;
  try
    if MethodName = 'SaveLynxApp' then
      Result := SaveAppJson(payload)
    else if MethodName = 'DeleteLynxApp' then
      Result := DeleteAppJson(payload)
    else if MethodName = 'GetLynxAppTokenStatus' then
      Result := TokenStatusJson(PayloadText(payload, 'appId'))
    else if MethodName = 'SetLynxAppTokens' then
      Result := SetTokensJson(payload)
    else if MethodName = 'RedactLynxAppTokens' then
      Result := RedactTokensJson(payload)
    else
      raise Exception.Create('Unknown Lynx Apps method: ' + MethodName);
  finally
    payload.Free;
  end;
end;

procedure TAppPlatformLynxAppsConnector.SendResponse(const RequestId, ResultJson: string; const IsError: Boolean);
begin
  var callbackName := 'window.onIntegrationManagerResponse';
  if IsError then
    callbackName := 'window.onIntegrationManagerError';

  CallScript(callbackName + '(''' + EscapeForJsString(RequestId) + ''', ''' + EncodeBase64ForJs(ResultJson) + ''')');
end;

procedure TAppPlatformLynxAppsConnector.SendRunLog(const AMessage: string);
begin
  var logJson := TJSONObject.Create;
  try
    logJson.AddPair('message', AMessage);
    CallScript('window.IntegrationManagerHost.receiveRunLog(JSON.parse(''' + EscapeForJsString(logJson.ToJSON) + '''));');
  finally
    logJson.Free;
  end;
end;

function TAppPlatformLynxAppsConnector.SaveAppJson(const Payload: TJSONObject): string;
begin
  if Payload = nil then
    raise Exception.Create('Invalid Lynx app payload.');

  var id := PayloadText(Payload, 'id');
  // clientId is the id the page used before this save (a temporary id for a
  // new app); token values typed before the first save are staged under it.
  var clientId := PayloadText(Payload, 'clientId');
  var tokenKey := id;
  if tokenKey = '' then
    tokenKey := clientId;

  // Secrets that leaked into the HTML (pasted by the user, echoed by the AI) are
  // stored as their {{token:Name}} placeholder.
  var tokens := TokensByName(tokenKey, Payload.GetValue('tokens'));
  try
    var html := RedactTokens(PayloadText(Payload, 'html'), tokens);
    var savedId := LynxAppsHost.SaveApp(id, PayloadText(Payload, 'name'), PayloadText(Payload, 'description'), html);
    CommitTokens(clientId, id, savedId, Payload.GetValue('tokenIds'));
    LynxAppsHost.StoreTokens;

    var response := TJSONObject.Create;
    try
      response.AddPair('id', savedId);
      Result := response.ToJSON;
    finally
      response.Free;
    end;
  finally
    tokens.Free;
  end;
end;

function TAppPlatformLynxAppsConnector.DeleteAppJson(const Payload: TJSONObject): string;
begin
  var id := PayloadText(Payload, 'id');
  LynxAppsHost.DeleteApp(id);
  _staged.Remove(id);
  Result := '{"ok":true}';
end;

{ Tokens }

function TAppPlatformLynxAppsConnector.StagedTokensFor(const AppId: string): TTokenValues;
begin
  if not _staged.TryGetValue(AppId, Result) then
  begin
    Result := TTokenValues.Create;
    _staged.Add(AppId, Result);
  end;
end;

// Saved values overlaid with values staged in this window, as a new dictionary
// the caller frees. A staged empty value means the token was cleared.
function TAppPlatformLynxAppsConnector.TokenValuesFor(const AppId: string): TTokenValues;
begin
  Result := TTokenValues.Create;
  if AppId = '' then
    Exit;

  if LynxAppsHost.AppExists(AppId) then
    for var entry in LynxAppsHost.SavedTokens(AppId) do
      Result.AddOrSetValue(entry.Key, entry.Value);

  var staged: TTokenValues;
  if _staged.TryGetValue(AppId, staged) then
    for var entry in staged do
      Result.AddOrSetValue(entry.Key, entry.Value);
end;

// TokenRefs is the app's [{id, name}] list; returns name -> value (caller frees):
// AppPlatform's own tokens (tokens.json), overridden by the app's.
function TAppPlatformLynxAppsConnector.TokensByName(const AppId: string; const TokenRefs: TJSONValue): TTokenValues;
begin
  Result := TLocalTokens.Values;
  if (AppId = '') or not (TokenRefs is TJSONArray) then
    Exit;

  var appTokens := TTokenValues.Create;
  var values := TokenValuesFor(AppId);
  try
    for var item in TJSONArray(TokenRefs) do
    begin
      if not (item is TJSONObject) then
        Continue;

      var tokenId := PayloadText(TJSONObject(item), 'id');
      var tokenName := PayloadText(TJSONObject(item), 'name').Trim;
      var value: string;
      if (tokenId <> '') and (tokenName <> '') and values.TryGetValue(tokenId, value) and (value <> '') then
        appTokens.AddOrSetValue(tokenName, value);
    end;

    for var entry in appTokens do
      Result.AddOrSetValue(entry.Key, entry.Value);

    // The app's tokens become AppPlatform's tokens too (tokens.json): type
    // scripts, e.g. a provider that fetches from the same API, use them by name
    if LynxAppsHost.AppExists(AppId) then
      TThread.Queue(nil, procedure begin
        try
          TLocalTokens.Merge(appTokens);
        finally
          appTokens.Free;
        end;
      end)
    else
      appTokens.Free;
  finally
    values.Free;
  end;
end;

// After a save: moves values staged under the page's id (and a previous id) to
// the saved id, and drops values of tokens the app no longer has.
procedure TAppPlatformLynxAppsConnector.CommitTokens(const StagedId, PreviousId, AppId: string; const TokenIds: TJSONValue);
begin
  var saved := LynxAppsHost.SavedTokens(AppId);

  if (PreviousId <> '') and (PreviousId <> AppId) then
  begin
    for var entry in LynxAppsHost.SavedTokens(PreviousId) do
      if not saved.ContainsKey(entry.Key) then
        saved.Add(entry.Key, entry.Value);
    LynxAppsHost.DeleteTokens(PreviousId);
  end;

  for var stagedKey in TArray<string>.Create(StagedId, PreviousId, AppId) do
  begin
    var staged: TTokenValues;
    if (stagedKey <> '') and _staged.TryGetValue(stagedKey, staged) then
    begin
      for var entry in staged do
        saved.AddOrSetValue(entry.Key, entry.Value);
      _staged.Remove(stagedKey);
    end;
  end;

  if TokenIds is TJSONArray then
  begin
    var kept := TTokenValues.Create;
    try
      for var item in TJSONArray(TokenIds) do
      begin
        var value: string;
        if saved.TryGetValue(item.Value, value) and (value <> '') then
          kept.Add(item.Value, value);
      end;

      saved.Clear;
      for var entry in kept do
        saved.Add(entry.Key, entry.Value);
    finally
      kept.Free;
    end;
  end;
end;

function TAppPlatformLynxAppsConnector.TokenStatusJson(const AppId: string): string;
begin
  var root := TJSONObject.Create;
  var values := TokenValuesFor(AppId);
  try
    var setIds := TJSONArray.Create;
    root.AddPair('set', setIds);
    for var entry in values do
      if entry.Value <> '' then
        setIds.Add(entry.Key);
    Result := root.ToJSON;
  finally
    values.Free;
    root.Free;
  end;
end;

// Payload: { appId, values: { tokenId: value | null }, commit }. Without commit
// the values are staged until the app is saved (editor); with commit they are
// stored right away (app window asking for missing tokens).
function TAppPlatformLynxAppsConnector.SetTokensJson(const Payload: TJSONObject): string;
begin
  var appId := PayloadText(Payload, 'appId');
  if appId = '' then
    raise Exception.Create('SetLynxAppTokens requires an appId.');

  var valuesJson := Payload.GetValue('values');
  if not (valuesJson is TJSONObject) then
    raise Exception.Create('SetLynxAppTokens requires a values object.');

  var commitValue := Payload.GetValue('commit');
  var commit := (commitValue is TJSONBool) and TJSONBool(commitValue).AsBoolean;

  var target: TTokenValues;
  if commit then
  begin
    if not LynxAppsHost.AppExists(appId) then
      raise Exception.Create('Token values can only be stored for a saved app.');
    target := LynxAppsHost.SavedTokens(appId);
  end else
    target := StagedTokensFor(appId);

  for var pair in TJSONObject(valuesJson) do
  begin
    var tokenId := pair.JsonString.Value;
    if tokenId = '' then
      Continue;

    if pair.JsonValue is TJSONString then
      target.AddOrSetValue(tokenId, pair.JsonValue.Value.Trim) else
      target.AddOrSetValue(tokenId, '');
  end;

  if commit then
    LynxAppsHost.StoreTokens;
  Result := TokenStatusJson(appId);
end;

// Payload: { appId, tokens: [{id, name}], text }. Lets the page scrub secrets a
// user pasted into chat before it is sent to the AI, without ever seeing them.
function TAppPlatformLynxAppsConnector.RedactTokensJson(const Payload: TJSONObject): string;
begin
  var tokens := TokensByName(PayloadText(Payload, 'appId'), Payload.GetValue('tokens'));
  var response := TJSONObject.Create;
  try
    response.AddPair('text', RedactTokens(PayloadText(Payload, 'text'), tokens));
    Result := response.ToJSON;
  finally
    response.Free;
    tokens.Free;
  end;
end;

{ Script runs }

procedure TAppPlatformLynxAppsConnector.RunIntegrationAsync(const RequestId: string; const Payload: TJSONObject);
begin
  if Payload = nil then
    raise Exception.Create('Invalid integration run payload.');

  var scriptValue := Payload.GetValue('javaScriptCode');
  if scriptValue = nil then
    raise Exception.Create('Run payload is missing javaScriptCode.');

  // appId + tokens ([{id, name}]) select the values to inject; the page itself
  // never has them. Not modified after this point, so the script thread may
  // read it; everything the run sends back is redacted with it.
  var tokens := TokensByName(PayloadText(Payload, 'appId'), Payload.GetValue('tokens'));
  var code: string;
  try
    code := InjectTokens(LynxAppsHost.RewriteModulePaths(scriptValue.Value), tokens);
  except
    tokens.Free;
    raise;
  end;

  var connector: TAppPlatformLynxAppsConnector := Self;
  // Holds the connector (and its browser bridge) alive until the run reported back.
  var connectorRef: IInterface := Self;
  var logs := TList<string>.Create;
  var cancelFlag: PInteger;
  System.New(cancelFlag);
  cancelFlag^ := 0;
  _runs.AddOrSetValue(RequestId, cancelFlag);

  LynxAppsHost.RunScript(
    procedure(const Runtime: IJSRuntime)
    begin
      var output := '';
      var error := '';
      var exceptionClass := '';

      try
        if Runtime = nil then
          raise Exception.Create('The AppPlatform script runtime is not available.');

        var context := Runtime.CreateContext;
        context.LogString :=
          procedure(Line: string)
          begin
            var redactedLine := RedactTokens(Line, tokens);
            TThread.Queue(nil,
              procedure
              begin
                logs.Add(redactedLine);
                if not connector.IsShutdown then
                  connector.SendRunLog(redactedLine);
              end);
          end;
        context.RegisterLiveInterfaceInstance('app', TypeInfo(IAppObject), _app);
        // The type scripts (js\types): what the AI's type tools work on
        if TypeScripts <> nil then
          context.RegisterLiveInterfaceInstance('types', TypeInfo(ITypeScripts), TypeScripts);

        var value := context.eval_with_result(code, '<eval>', cancelFlag);
        if not value.IsEmpty then
          output := RedactTokens(value.ToString, tokens);
      except
        on E: Exception do
        begin
          error := RedactTokens(E.Message, tokens);
          exceptionClass := E.ClassName;
        end;
      end;

      TThread.Queue(nil,
        procedure
        begin
          try
            if connector.IsShutdown then
              Exit;

            if (error <> '') and (cancelFlag^ <> 0) then
              error := 'Stopped by user. Changes made before stopping were kept.';

            var resultJson := TJSONObject.Create;
            try
              var logsArray := TJSONArray.Create;
              resultJson.AddPair('logs', logsArray);
              for var line in logs do
                logsArray.Add(line);

              resultJson.AddPair('output', output);
              if error <> '' then
              begin
                logsArray.Add(exceptionClass + ': ' + error);
                resultJson.AddPair('error', error);
                resultJson.AddPair('exceptionClass', exceptionClass);
              end;

              connector.SendResponse(RequestId, resultJson.ToJSON);
            finally
              resultJson.Free;
            end;
          finally
            connector._runs.Remove(RequestId);
            System.Dispose(cancelFlag);
            logs.Free;
            tokens.Free;
            connectorRef := nil;
          end;
        end);
    end);
end;

function TAppPlatformLynxAppsConnector.CancelIntegrationJson(const Payload: TJSONObject): string;
begin
  var flag: PInteger;
  if _runs.TryGetValue(PayloadText(Payload, 'requestId'), flag) then
  begin
    AtomicExchange(flag^, 1);
    Result := '{"ok":true}';
  end else
    Result := '{"ok":false}';
end;

end.
