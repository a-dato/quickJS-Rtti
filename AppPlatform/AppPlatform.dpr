program AppPlatform;



uses
  System.StartUpCopy,
  FMX.Forms,
  AppMain in 'AppMain.pas' {Form1},
  Customer.frame in 'DataObjectsFrames\Customer.frame.pas' {CustomerFrame},
  JSGeneral.frame in 'DataObjectsFrames\JSGeneral.frame.pas' {JSGeneralFrame: TFrame},
  ObjectDesigner in 'AppDialogs\ObjectDesigner.pas' {ObjectDesignerForm},
  Project.impl in 'DataObjects\Project.impl.pas',
  Project.intf in 'DataObjects\Project.intf.pas',
  Project.frame in 'DataObjectsFrames\Project.frame.pas',
  App.EditorPanel.intf in 'AppUI\App.EditorPanel.intf.pas',
  App.EditorManager.intf in 'AppUI\App.EditorManager.intf.pas',
  App.EditorManager.impl in 'AppUI\App.EditorManager.impl.pas',
  App.KeyNavigator.intf in 'AppUI\App.KeyNavigator.intf.pas',
  App.UIStateDelegate in 'AppUI\App.UIStateDelegate.pas',
  App.KeyNavigator.impl in 'AppUI\App.KeyNavigator.impl.pas',
  ProjectSelection.frame in 'DataObjectsFrames\ProjectSelection.frame.pas' {ProjectSelectionFrame: TFrame},
  FMX.App.MasterForm in 'AppUI\FMX.App.MasterForm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
