unit AppPlatform.LynxApps.Window;

// A window with one Lynx Apps page (manager, app builder or app) of the locally
// running IntegrationProject server, connected to AppPlatform over ScriptGate.
// Mirrors TFormAIAppWindow in LynxX: one window per url, the page opens further
// windows with the 'OpenAppWindow' bridge message (or window.open), and the
// builder is asked before it is closed so unsaved work is not lost.

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.WebBrowser,
  ADato.AI.BrowserConnector.intf;

type
  TAppPlatformLynxAppsWindow = class(TForm)
  private
    class var _openWindows: TDictionary<string, TAppPlatformLynxAppsWindow>;
    class constructor Create;
    class destructor Destroy;

  private
    _url: string;
    _browser: TWebBrowser;
    _connector: IBrowserConnector;
    _titleTimer: TTimer;
    _titleAttempts: Integer;
    _closeAllowed: Boolean;
    _closeQueryPending: Boolean;

    procedure Init(const Url: string; const Width, Height: Integer);
    function  IsEditorPage: Boolean;
    procedure OnBrowserLoadComplete(ASender: TObject);
    procedure OnTitleTimer(Sender: TObject);
    procedure OpenChildWindow(const Connector: IBrowserConnector; const Url: string; const Width, Height: Integer);
    procedure HandleOpenAppWindowMessage(const Connector: IBrowserConnector; const Action: string; const Params: TJSONObject);
    procedure HandleEditorCloseAllowed(const Connector: IBrowserConnector; const Action: string; const Params: TJSONObject);
    procedure HandleEditorCloseCancelled(const Connector: IBrowserConnector; const Action: string; const Params: TJSONObject);
    procedure RequestEditorClose;
    procedure WindowCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure WindowClose(Sender: TObject; var Action: TCloseAction);
    procedure Unregister;
  public
    destructor Destroy; override;

    // Opens (or brings to front) the window for Url; Width/Height in pixels, 0 = default.
    class procedure Open(const Url: string; const Width: Integer = 0; const Height: Integer = 0);
    // The app manager of the Lynx Apps server.
    class procedure OpenManager;
  end;

implementation

uses
  System.Math,
  AppPlatform.LynxApps.Host,
  AppPlatform.LynxApps.Connector;

{ TAppPlatformLynxAppsWindow }

class constructor TAppPlatformLynxAppsWindow.Create;
begin
  _openWindows := TDictionary<string, TAppPlatformLynxAppsWindow>.Create;
end;

class destructor TAppPlatformLynxAppsWindow.Destroy;
begin
  FreeAndNil(_openWindows);
end;

class procedure TAppPlatformLynxAppsWindow.OpenManager;
begin
  Open(LYNX_APPS_BASE_URL, 1100, 760);
end;

class procedure TAppPlatformLynxAppsWindow.Open(const Url: string; const Width, Height: Integer);
begin
  var key := Url.ToLower;

  var existing: TAppPlatformLynxAppsWindow;
  if _openWindows.TryGetValue(key, existing) then
  begin
    if existing.WindowState = TWindowState.wsMinimized then
      existing.WindowState := TWindowState.wsNormal;
    existing.Show;
    existing.BringToFront;
    Exit;
  end;

  var window := TAppPlatformLynxAppsWindow.CreateNew(Application);
  _openWindows.Add(key, window);
  window.Init(Url, Width, Height);
  window.Show;
end;

destructor TAppPlatformLynxAppsWindow.Destroy;
begin
  Unregister;
  if _connector <> nil then
    _connector.Shutdown;
  _connector := nil;
  inherited;
end;

procedure TAppPlatformLynxAppsWindow.Init(const Url: string; const Width, Height: Integer);
begin
  _url := Url;
  Caption := 'AppPlatform Apps';
  OnCloseQuery := WindowCloseQuery;
  OnClose := WindowClose;

  var workArea := Screen.WorkAreaRect;
  if (Width > 0) and (Height > 0) then
  begin
    ClientWidth := Min(Width, Round(workArea.Width) - 40);
    ClientHeight := Min(Height, Round(workArea.Height) - 60);
  end else
  begin
    ClientWidth := Min(900, Round(workArea.Width) - 40);
    ClientHeight := Min(640, Round(workArea.Height) - 60);
  end;
  Position := TFormPosition.ScreenCenter;
  // The Studio's end-to-end test driver: no window on screen
  if GetEnvironmentVariable('APPPLATFORM_TEST_DRIVER') <> '' then
  begin
    Position := TFormPosition.Designed;
    Left := -32000;
    Top := -32000;
  end;

  _browser := TWebBrowser.Create(Self);
  _browser.Align := TAlignLayout.Client;
  _browser.WindowsEngine := TWindowsEngine.EdgeOnly;
  _browser.OnDidFinishLoad := OnBrowserLoadComplete;
  AddObject(_browser);

  _connector := TAppPlatformLynxAppsConnector.Create;
  _connector.SetNewWindowHandler(OpenChildWindow);
  _connector.RegisterMessageHandler('OpenAppWindow', HandleOpenAppWindowMessage);
  _connector.RegisterMessageHandler('EditorCloseAllowed', HandleEditorCloseAllowed);
  _connector.RegisterMessageHandler('EditorCloseCancelled', HandleEditorCloseCancelled);
  _connector.Initialize(_browser);

  _browser.Navigate(TLynxAppsHost.WithScriptGateTransport(Url));
end;

procedure TAppPlatformLynxAppsWindow.OnBrowserLoadComplete(ASender: TObject);
begin
  if _connector = nil then
    Exit;

  _connector.OnBrowserReady;

  // The page sets its title once its scripts ran (app name, builder name).
  if _titleTimer = nil then
  begin
    _titleTimer := TTimer.Create(Self);
    _titleTimer.Interval := 500;
    _titleTimer.OnTimer := OnTitleTimer;
  end;
  _titleAttempts := 0;
  _titleTimer.Enabled := True;
end;

procedure TAppPlatformLynxAppsWindow.OnTitleTimer(Sender: TObject);
begin
  Inc(_titleAttempts);
  if (_connector = nil) or _connector.IsShutdown or (_titleAttempts > 10) then
  begin
    _titleTimer.Enabled := False;
    Exit;
  end;

  _connector.CallScript('document.title',
    procedure(const AResult: string)
    begin
      var title := AResult.Trim.DeQuotedString('"');
      if (title <> '') and (title <> 'null') then
      begin
        Caption := title;
        if _titleTimer <> nil then
          _titleTimer.Enabled := False;
      end;
    end);
end;

procedure TAppPlatformLynxAppsWindow.OpenChildWindow(const Connector: IBrowserConnector; const Url: string; const Width, Height: Integer);
begin
  TAppPlatformLynxAppsWindow.Open(Url, Width, Height);
end;

procedure TAppPlatformLynxAppsWindow.HandleOpenAppWindowMessage(const Connector: IBrowserConnector; const Action: string; const Params: TJSONObject);
begin
  if Params = nil then
    Exit;

  var url := Params.GetValue<string>('url', '');
  if url.Trim = '' then
    Exit;

  var width := Params.GetValue<Integer>('width', 0);
  var height := Params.GetValue<Integer>('height', 0);

  // The message arrives inside the browser's navigation callback; leave it
  // before creating a new form.
  TThread.ForceQueue(nil, procedure begin TAppPlatformLynxAppsWindow.Open(url, width, height); end);
end;

function TAppPlatformLynxAppsWindow.IsEditorPage: Boolean;
begin
  Result := _url.ToLower.Contains('editor.html');
end;

procedure TAppPlatformLynxAppsWindow.WindowCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if _closeAllowed or not IsEditorPage or (_connector = nil) or _connector.IsShutdown then
    Exit;

  CanClose := False;
  if _closeQueryPending then
    Exit;

  _closeQueryPending := True;
  RequestEditorClose;
end;

// The builder asks to save unsaved work and answers with EditorCloseAllowed or
// EditorCloseCancelled.
procedure TAppPlatformLynxAppsWindow.RequestEditorClose;
begin
  try
    _connector.CallScript(
      'try {' +
      '  if (typeof window.handleEditorHostClose === ''function'')' +
      '    window.handleEditorHostClose();' +
      '  else' +
      '    window.postHostCloseMessage(''EditorCloseAllowed'');' +
      '} catch (e) {' +
      '  window.postHostCloseMessage(''EditorCloseAllowed'');' +
      '}');
  except
    _closeQueryPending := False;
    _closeAllowed := True;
    Close;
  end;
end;

procedure TAppPlatformLynxAppsWindow.HandleEditorCloseAllowed(const Connector: IBrowserConnector; const Action: string; const Params: TJSONObject);
begin
  _closeQueryPending := False;
  _closeAllowed := True;
  TThread.ForceQueue(nil, procedure begin Close; end);
end;

procedure TAppPlatformLynxAppsWindow.HandleEditorCloseCancelled(const Connector: IBrowserConnector; const Action: string; const Params: TJSONObject);
begin
  _closeQueryPending := False;
end;

procedure TAppPlatformLynxAppsWindow.Unregister;
begin
  if (_openWindows = nil) or (_url = '') then
    Exit;

  var key := _url.ToLower;
  var registered: TAppPlatformLynxAppsWindow;
  if _openWindows.TryGetValue(key, registered) and (registered = Self) then
    _openWindows.Remove(key);
end;

procedure TAppPlatformLynxAppsWindow.WindowClose(Sender: TObject; var Action: TCloseAction);
begin
  Unregister;
  if _titleTimer <> nil then
    _titleTimer.Enabled := False;
  if _connector <> nil then
    _connector.Shutdown;
  // Free the browser while this window still exists: WebView2's own window is a
  // child of this one, and freeing it later sends messages to a window FMX has
  // already forgotten (EInvalidFmxHandle "Invalid argument" when the app closes)
  FreeAndNil(_browser);
  Action := TCloseAction.caFree;
end;

end.
