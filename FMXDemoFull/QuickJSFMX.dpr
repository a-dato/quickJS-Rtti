program QuickJSFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormFull in 'MainFormFull.pas' {Form2},
  QuickJS.Register.impl in '..\FMXShared\QuickJS.Register.impl.pas',
  XMLHttpRequest.impl in '..\FMXShared\XMLHttpRequest.impl.pas',
  XMLHttpRequest.intf in '..\FMXShared\XMLHttpRequest.intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
