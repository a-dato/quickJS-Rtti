program QuickJSFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormFull in 'MainFormFull.pas' {Form2},
  XMLHttpRequest.impl in '..\Source\XMLHttpRequest.impl.pas',
  XMLHttpRequest.intf in '..\Source\XMLHttpRequest.intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
