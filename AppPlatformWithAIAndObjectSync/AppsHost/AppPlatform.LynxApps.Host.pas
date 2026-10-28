unit AppPlatform.LynxApps.Host;

// AppPlatformWithAI as a host of the Lynx Apps pages (manager, AI app builder and
// app runner of the IntegrationProject web service): the test host for building
// apps in an application other than Lynx.
//
// The pages are host-independent and talk to their host over ScriptGate. Lynx
// answers them with TLynxAppsBrowserConnector (adato_lynx_master); this host
// answers the same protocol with its own, small implementation:
//   AppPlatform.LynxApps.Host       what the pages need from AppPlatform (this unit)
//   AppPlatform.LynxApps.Connector  the ScriptGate protocol
//   AppPlatform.LynxApps.Window     the windows that show the pages
//
// What this unit provides:
//   - LYNX_APPS_BASE_URL: where the pages are served. For now the locally running
//     IntegrationProject server (node server.js, port 3000).
//   - The host profile: the files of the AppsProfile folder, read from disk on
//     every request so they can be edited without rebuilding.
//   - The apps, stored in apps.json next to the exe.
//   - The token values of the apps, stored in app-tokens.json next to the exe
//     (plain text: this is a local test app). AppPlatform's own tokens
//     (tokens.json, App.Tokens) are added to every run.
//   - The script thread: host scripts run one at a time on a worker thread with
//     an isolated QuickJS runtime, as in Lynx.

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.SyncObjs,
  System.Generics.Collections,
  QuickJS.Register.intf;

const
  LYNX_APPS_BASE_URL = 'http://localhost:3000/';

  // Placeholder in the scripts built by the pages (appHost.js); replaced by the
  // app's token values right before QuickJS runs the script.
  LYNX_APP_TOKENS_MARKER = '/*@@LYNX_TOKENS@@*/{}';

type
  // Token values: token id -> value, or token name -> value for a run.
  TTokenValues = TDictionary<string, string>;

  // Creates an isolated runtime with AppPlatform's types registered; called on
  // the script thread. Set by the main form.
  TLynxAppsRuntimeFactory = reference to function: IJSRuntime;

  TScriptJob = reference to procedure(const Runtime: IJSRuntime);

  TLynxAppsScriptThread = class(TThread)
  private
    _factory: TLynxAppsRuntimeFactory;
    _jobs: TQueue<TScriptJob>;
    _lock: TObject;
    _wakeUp: TEvent;

    function  TakeJob(out Job: TScriptJob): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const Factory: TLynxAppsRuntimeFactory);
    destructor Destroy; override;

    procedure Push(const Job: TScriptJob);
    procedure Stop;
  end;

  TLynxAppsHost = class
  private
    _apps: TJSONArray;
    _tokens: TObjectDictionary<string, TTokenValues>;
    _scriptThread: TLynxAppsScriptThread;

    function  AppsFile: string;
    function  TokensFile: string;
    function  FindApp(const Id: string): TJSONObject;
    procedure LoadApps;
    procedure StoreApps;
    procedure LoadTokens;
  public
    RuntimeFactory: TLynxAppsRuntimeFactory;

    constructor Create;
    destructor Destroy; override;

    class function WithScriptGateTransport(const Url: string): string; static;
    class function IsTrustedPageUrl(const Url: string): Boolean; static;

    // { "entry": "profile.js", "files": { name: source } } of the AppsProfile folder.
    function  ProfileJson: string;
    // Rewrites relative imports ('./x.js') of a host script to the files in
    // AppsProfile\host, the QuickJS modules host scripts may import.
    function  RewriteModulePaths(const JavaScriptCode: string): string;

    function  LoadAppsJson: string;
    function  AppExists(const Id: string): Boolean;
    // Returns the id of the saved app; a new id when Id is empty or unknown.
    function  SaveApp(const Id, Name, Description, Html: string): string;
    procedure DeleteApp(const Id: string);

    // The saved token values of an app (token id -> value); never nil.
    function  SavedTokens(const AppId: string): TTokenValues;
    procedure DeleteTokens(const AppId: string);
    // Writes the saved token values to app-tokens.json (after a change)
    procedure StoreTokens;

    // Runs Job on the script thread.
    procedure RunScript(const Job: TScriptJob);
  end;

function LynxAppsHost: TLynxAppsHost;

// Replaces every token value (name -> value) in Text by its {{token:Name}} placeholder.
function RedactTokens(const Text: string; const Tokens: TTokenValues): string;
// Replaces LYNX_APP_TOKENS_MARKER in the script by the name -> value JSON.
function InjectTokens(const JavaScriptCode: string; const Tokens: TTokenValues): string;

implementation

uses
  System.IOUtils, System.RegularExpressions;

var
  _host: TLynxAppsHost;

function LynxAppsHost: TLynxAppsHost;
begin
  if _host = nil then
    _host := TLynxAppsHost.Create;
  Result := _host;
end;

// Same escaping as JavaScript's JSON.stringify for string contents, so values
// are also found inside the JSON text a script returns.
function JsonEscapedText(const Value: string): string;
begin
  var builder := TStringBuilder.Create;
  try
    for var ch in Value do
      case ch of
        '"': builder.Append('\"');
        '\': builder.Append('\\');
        #8: builder.Append('\b');
        #9: builder.Append('\t');
        #10: builder.Append('\n');
        #12: builder.Append('\f');
        #13: builder.Append('\r');
      else
        if Ord(ch) < 32 then
          builder.Append('\u' + IntToHex(Ord(ch), 4).ToLower) else
          builder.Append(ch);
      end;
    Result := builder.ToString;
  finally
    builder.Free;
  end;
end;

function RedactTokens(const Text: string; const Tokens: TTokenValues): string;
begin
  Result := Text;
  if (Result = '') or (Tokens = nil) then
    Exit;

  for var entry in Tokens do
  begin
    if entry.Value.Length < 4 then
      Continue;

    var placeholder := '{{token:' + entry.Key + '}}';
    Result := Result.Replace(entry.Value, placeholder, [rfReplaceAll]);

    var escaped := JsonEscapedText(entry.Value);
    if escaped <> entry.Value then
      Result := Result.Replace(escaped, placeholder, [rfReplaceAll]);
  end;
end;

function InjectTokens(const JavaScriptCode: string; const Tokens: TTokenValues): string;
begin
  Result := JavaScriptCode;
  var markerIndex := Result.IndexOf(LYNX_APP_TOKENS_MARKER);
  if markerIndex < 0 then
    Exit;

  var json := TJSONObject.Create;
  try
    if Tokens <> nil then
      for var entry in Tokens do
        json.AddPair(entry.Key, entry.Value);

    Result := Result.Remove(markerIndex, LYNX_APP_TOKENS_MARKER.Length).Insert(markerIndex, json.ToJSON);
  finally
    json.Free;
  end;
end;

// The AppsProfile folder next to the project: the exe runs from
// AppPlatformWithAI\Win64\<config>, so the folder is found by walking up.
function ProfileFolder: string;
begin
  var dir := ExtractFileDir(ParamStr(0));
  while dir <> '' do
  begin
    var candidate := TPath.Combine(dir, 'AppsProfile');
    if TFile.Exists(TPath.Combine(candidate, 'profile.js')) then
      Exit(candidate);

    var parent := ExtractFileDir(dir);
    if parent = dir then
      Break;
    dir := parent;
  end;
  raise Exception.Create('The AppsProfile folder (with profile.js) was not found next to or above ' + ExtractFileDir(ParamStr(0)) + '.');
end;

{ TLynxAppsScriptThread }

constructor TLynxAppsScriptThread.Create(const Factory: TLynxAppsRuntimeFactory);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  _factory := Factory;
  _jobs := TQueue<TScriptJob>.Create;
  _lock := TObject.Create;
  _wakeUp := TEvent.Create(nil, False {auto reset}, False, '');
end;

destructor TLynxAppsScriptThread.Destroy;
begin
  _wakeUp.Free;
  _lock.Free;
  _jobs.Free;
  inherited;
end;

procedure TLynxAppsScriptThread.Push(const Job: TScriptJob);
begin
  TMonitor.Enter(_lock);
  try
    _jobs.Enqueue(Job);
  finally
    TMonitor.Exit(_lock);
  end;
  _wakeUp.SetEvent;
end;

procedure TLynxAppsScriptThread.Stop;
begin
  Terminate;
  _wakeUp.SetEvent;
end;

function TLynxAppsScriptThread.TakeJob(out Job: TScriptJob): Boolean;
begin
  TMonitor.Enter(_lock);
  try
    Result := _jobs.Count > 0;
    if Result then
      Job := _jobs.Dequeue();
  finally
    TMonitor.Exit(_lock);
  end;
end;

procedure TLynxAppsScriptThread.Execute;
begin
  NameThreadForDebugging('LynxAppsScriptThread');

  var runtime: IJSRuntime := nil;
  try
    if Assigned(_factory) then
      runtime := _factory();
  except
    // Every job then reports that the runtime is not available.
    runtime := nil;
  end;

  try
    while not Terminated do
    begin
      _wakeUp.WaitFor;
      var job: TScriptJob;
      while not Terminated and TakeJob(job) do
      try
        job(runtime);
      except
        // Jobs report their own errors; nothing may stop the thread.
      end;
    end;
  finally
    runtime := nil;
  end;
end;

{ TLynxAppsHost }

constructor TLynxAppsHost.Create;
begin
  inherited Create;
  _tokens := TObjectDictionary<string, TTokenValues>.Create([doOwnsValues]);
  LoadApps;
  LoadTokens;
end;

destructor TLynxAppsHost.Destroy;
begin
  if _scriptThread <> nil then
    _scriptThread.Stop;
  _scriptThread := nil;
  _tokens.Free;
  _apps.Free;
  inherited;
end;

class function TLynxAppsHost.WithScriptGateTransport(const Url: string): string;
begin
  Result := Url;
  if Result.ToLower.Contains('transport=scriptgate') then
    Exit;

  var fragment := '';
  var fragmentPos := Result.IndexOf('#');
  if fragmentPos >= 0 then
  begin
    fragment := Result.Substring(fragmentPos);
    Result := Result.Substring(0, fragmentPos);
  end;

  if Result.Contains('?') then
    Result := Result + '&transport=scriptgate' else
    Result := Result + '?transport=scriptgate';

  Result := Result + fragment;
end;

class function TLynxAppsHost.IsTrustedPageUrl(const Url: string): Boolean;
begin
  Result := (Url <> '') and Url.ToLower.StartsWith(LYNX_APPS_BASE_URL.ToLower);
end;

function TLynxAppsHost.ProfileJson: string;
begin
  var folder := ProfileFolder;
  var root := TJSONObject.Create;
  try
    root.AddPair('entry', 'profile.js');
    var files := TJSONObject.Create;
    root.AddPair('files', files);
    for var path in TDirectory.GetFiles(folder) do
    begin
      var name := ExtractFileName(path);
      if SameText(ExtractFileExt(name), '.js') or SameText(ExtractFileExt(name), '.json') then
        files.AddPair(name, TFile.ReadAllText(path, TEncoding.UTF8));
    end;
    Result := root.ToJSON;
  finally
    root.Free;
  end;
end;

function TLynxAppsHost.RewriteModulePaths(const JavaScriptCode: string): string;
begin
  Result := JavaScriptCode;
  var matches := TRegEx.Matches(JavaScriptCode, 'from\s+[''"](\./[^''"]+)[''"]');
  if matches.Count = 0 then
    Exit;

  var hostFolder := TPath.Combine(ProfileFolder, 'host');
  for var match in matches do
  begin
    var spec := match.Groups[1].Value;
    var path := TPath.Combine(hostFolder, StringReplace(spec.Substring(2), '/', PathDelim, [rfReplaceAll]));
    if not TFile.Exists(path) then
      raise Exception.Create('Host module ' + spec + ' does not exist in ' + hostFolder + '.');

    var absolute := StringReplace(path, '\', '/', [rfReplaceAll]);
    Result := Result
      .Replace('''' + spec + '''', '''' + absolute + '''')
      .Replace('"' + spec + '"', '"' + absolute + '"');
  end;
end;

function TLynxAppsHost.AppsFile: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'apps.json');
end;

function TLynxAppsHost.TokensFile: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'app-tokens.json');
end;

procedure TLynxAppsHost.LoadApps;
begin
  FreeAndNil(_apps);
  var fileName := AppsFile;
  // Apps saved before they moved next to the exe
  var oldFile := TPath.Combine(TPath.Combine(TPath.GetHomePath, 'AppPlatformWithAI'), 'apps.json');
  if not TFile.Exists(fileName) and TFile.Exists(oldFile) then
    fileName := oldFile;

  if TFile.Exists(fileName) then
  begin
    var json := TJSONObject.ParseJSONValue(TFile.ReadAllText(fileName, TEncoding.UTF8));
    if json is TJSONArray then
      _apps := TJSONArray(json)
    else
      json.Free;
  end;

  if _apps = nil then
    _apps := TJSONArray.Create;
end;

procedure TLynxAppsHost.StoreApps;
begin
  ForceDirectories(ExtractFileDir(AppsFile));
  TFile.WriteAllText(AppsFile, _apps.Format(2), TEncoding.UTF8);
end;

function TLynxAppsHost.FindApp(const Id: string): TJSONObject;
begin
  Result := nil;
  if Id = '' then
    Exit;

  for var item in _apps do
    if (item is TJSONObject) and (TJSONObject(item).GetValue<string>('id', '') = Id) then
      Exit(TJSONObject(item));
end;

function TLynxAppsHost.LoadAppsJson: string;
begin
  var root := TJSONObject.Create;
  try
    root.AddPair('apps', _apps.Clone as TJSONArray);
    Result := root.ToJSON;
  finally
    root.Free;
  end;
end;

function TLynxAppsHost.AppExists(const Id: string): Boolean;
begin
  Result := FindApp(Id) <> nil;
end;

function TLynxAppsHost.SaveApp(const Id, Name, Description, Html: string): string;
begin
  var app := FindApp(Id);
  if app = nil then
  begin
    Result := TGUID.NewGuid.ToString.Trim(['{', '}']).ToLower;
    app := TJSONObject.Create;
    app.AddPair('id', Result);
    _apps.AddElement(app);
  end else
    Result := Id;

  for var field in TArray<string>.Create('name', 'description', 'html') do
    app.RemovePair(field).Free;
  app.AddPair('name', Name);
  app.AddPair('description', Description);
  app.AddPair('html', Html);

  StoreApps;
end;

procedure TLynxAppsHost.DeleteApp(const Id: string);
begin
  var app := FindApp(Id);
  if app <> nil then
  begin
    for var i := 0 to _apps.Count - 1 do
      if _apps.Items[i] = app then
      begin
        _apps.Remove(i).Free;
        Break;
      end;
    StoreApps;
  end;
  DeleteTokens(Id);
end;

function TLynxAppsHost.SavedTokens(const AppId: string): TTokenValues;
begin
  if not _tokens.TryGetValue(AppId, Result) then
  begin
    Result := TTokenValues.Create;
    _tokens.Add(AppId, Result);
  end;
end;

procedure TLynxAppsHost.DeleteTokens(const AppId: string);
begin
  _tokens.Remove(AppId);
  StoreTokens;
end;

// app-tokens.json: { "<app id>": { "<token id>": "value" } }
procedure TLynxAppsHost.LoadTokens;
begin
  _tokens.Clear;
  if not TFile.Exists(TokensFile) then
    Exit;

  var json := TJSONObject.ParseJSONValue(TFile.ReadAllText(TokensFile, TEncoding.UTF8));
  try
    if json is TJSONObject then
      for var app in TJSONObject(json) do
        if app.JsonValue is TJSONObject then
        begin
          var values := SavedTokens(app.JsonString.Value);
          for var token in TJSONObject(app.JsonValue) do
            if (token.JsonValue is TJSONString) and (token.JsonValue.Value <> '') then
              values.AddOrSetValue(token.JsonString.Value, token.JsonValue.Value);
        end;
  finally
    json.Free;
  end;
end;

procedure TLynxAppsHost.StoreTokens;
begin
  var root := TJSONObject.Create;
  try
    for var app in _tokens do
    begin
      var values := TJSONObject.Create;
      for var token in app.Value do
        if token.Value <> '' then
          values.AddPair(token.Key, token.Value);
      if values.Count > 0 then
        root.AddPair(app.Key, values) else
        values.Free;
    end;
    TFile.WriteAllText(TokensFile, root.Format(2), TEncoding.UTF8);
  finally
    root.Free;
  end;
end;

procedure TLynxAppsHost.RunScript(const Job: TScriptJob);
begin
  if _scriptThread = nil then
    _scriptThread := TLynxAppsScriptThread.Create(RuntimeFactory);
  _scriptThread.Push(Job);
end;

initialization

finalization
  FreeAndNil(_host);

end.
