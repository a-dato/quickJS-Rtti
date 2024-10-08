program Browser;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form2},
  HtmlCanvas.intf in '..\FMXShared\HtmlCanvas.intf.pas',
  HtmlCanvas.impl in '..\FMXShared\HtmlCanvas.impl.pas',
  HtDocumentExtension in '..\FMXShared\HtDocumentExtension.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
