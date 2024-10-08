program Dn4DDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Dn4dMain in 'Dn4dMain.pas' {Form1},
  QuickJS.Register.dn4d.impl in 'D:\QuickJS\QuickJS-Pascal\FMXShared\QuickJS.Register.dn4d.impl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
