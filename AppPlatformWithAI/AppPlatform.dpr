program AppPlatform;

uses
  System.StartUpCopy,
  FMX.Forms,
  AppMain in 'AppMain.pas' {StudioForm},
  Studio.Platform in 'Studio.Platform.pas',
  Studio.Model in 'Studio.Model.pas',
  App.TypeScripts in 'App.TypeScripts.pas',
  App.Tokens in 'App.Tokens.pas',
  Project.intf in 'DataObjects\Project.intf.pas',
  Project.impl in 'DataObjects\Project.impl.pas',
  Task.intf in 'DataObjects\Task.intf.pas',
  Task.impl in 'DataObjects\Task.impl.pas',
  BulkUpdate.intf in 'DataObjects\BulkUpdate.intf.pas',
  FMX.App.MasterForm in 'AppUI\FMX.App.MasterForm.pas',
  AppPlatform.LynxApps.Host in 'AppsHost\AppPlatform.LynxApps.Host.pas',
  AppPlatform.LynxApps.Connector in 'AppsHost\AppPlatform.LynxApps.Connector.pas',
  AppPlatform.LynxApps.Window in 'AppsHost\AppPlatform.LynxApps.Window.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TStudioForm, StudioForm);
  Application.Run;
end.
