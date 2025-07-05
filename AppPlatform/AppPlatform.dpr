program AppPlatform;



uses
  System.StartUpCopy,
  FMX.Forms,
  AppMain in 'AppMain.pas' {Form1},
  App.intf in 'AppObjects\App.intf.pas',
  App.Windows.intf in 'AppObjects\App.Windows.intf.pas',
  App.Config.intf in 'AppObjects\App.Config.intf.pas',
  App.impl in 'AppObjects\App.impl.pas',
  App.Environment.intf in 'AppObjects\App.Environment.intf.pas',
  App.Config.impl in 'AppObjects\App.Config.impl.pas',
  App.Objects.intf in 'AppObjects\App.Objects.intf.pas',
  App.Objects.impl in 'AppObjects\App.Objects.impl.pas',
  App.Windows.impl in 'AppObjects\App.Windows.impl.pas',
  ObjectWindow in 'ObjectWindow.pas' {frmObjectWindow},
  App.Factory in 'AppObjects\App.Factory.pas',
  App.Environment.impl in 'AppObjects\App.Environment.impl.pas',
  App.Windows.FMX.impl in 'AppObjects\App.Windows.FMX.impl.pas',
  Customer.frame in 'DataObjectsFrames\Customer.frame.pas' {CustomerFrame},
  App.Content.intf in 'AppObjects\App.Content.intf.pas',
  App.Models.intf in 'AppObjects\App.Models.intf.pas',
  App.Models.impl in 'AppObjects\App.Models.impl.pas',
  JSGeneral.frame in 'DataObjectsFrames\JSGeneral.frame.pas' {JSGeneralFrame: TFrame},
  App.Content.impl in 'AppObjects\App.Content.impl.pas',
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
  App.PropertyDescriptor.intf in 'AppObjects\App.PropertyDescriptor.intf.pas',
  App.PropertyDescriptor.impl in 'AppObjects\App.PropertyDescriptor.impl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
