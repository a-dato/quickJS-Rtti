program QuickJSWebViewSyncTest;

{$IFNDEF WIN64}
  {$MESSAGE FATAL 'QuickJSWebViewSyncTest only supports Windows 64-bit (quickjs64.dll). Select the Windows 64-bit target platform.'}
{$ENDIF}

uses
  System.StartUpCopy,
  Winapi.Windows,
  System.SysUtils,
  System.IOUtils,
  FMX.Forms,
  FMX.WebBrowser.Win,
  SyncTest.Trace in 'SyncTest.Trace.pas',
  SyncTest.OrderModel in 'SyncTest.OrderModel.pas',
  SyncTest.Bridge in 'SyncTest.Bridge.pas',
  SyncTest.WebBinding in 'SyncTest.WebBinding.pas',
  SyncTest.MainForm in 'SyncTest.MainForm.pas' {frmSyncTest};

{$R *.res}

// The exe may live in the project folder (IDE default), in Win64\Debug
// (build.bat) or be deployed with the DLLs next to it. Load the runtime DLLs by
// full path before anything needs them; later loads by name reuse the loaded
// module. Must run before the form is created: streaming
// WebBrowser1.WindowsEngine = EdgeOnly already needs WebView2Loader.dll.
function PreloadRuntimeDll(const DllName: string): Boolean;
begin
  var exeDir := ExtractFilePath(ParamStr(0));
  for var dir in [exeDir, exeDir + 'redist', exeDir + '..\..\redist', exeDir + '..\redist'] do
  begin
    var path := TPath.GetFullPath(TPath.Combine(dir, DllName));
    if TFile.Exists(path) and (LoadLibrary(PChar(path)) <> 0) then
      Exit(True);
  end;
  Result := False;
end;

begin
  for var dll in ['quickjs64.dll', 'WebView2Loader.dll'] do
    if not PreloadRuntimeDll(dll) then
    begin
      MessageBox(0, PChar(dll + ' not found.'#13#10'Expected next to the exe or in the project''s redist folder.'),
        'QuickJSWebViewSyncTest', MB_ICONERROR);
      Halt(1);
    end;

  // Own WebView2 profile per running instance. Two copies (e.g. one started from
  // the IDE, one from build.bat) cannot share a profile folder at the same time.
  // Must be set before the form is created: the browser starts while it loads.
  TGlobalEdgeBrowserSettings.UserDataFolder := TPath.Combine(TPath.GetTempPath,
    'QuickJSWebViewSyncTest\WebView2-' + GetCurrentProcessId.ToString);

  Application.Initialize;
  Application.CreateForm(TfrmSyncTest, frmSyncTest);
  Application.Run;
end.
