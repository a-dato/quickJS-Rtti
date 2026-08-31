unit FMX.App.Platform.Dialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ScrollControl.Impl,
  FMX.ScrollControl.WithRows.Impl, FMX.ScrollControl.WithCells.Impl,
  FMX.ScrollControl.WithEditableCells.Impl, FMX.ScrollControl.DataControl.Impl;

type
  TAppPlatformDialog = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    acExecute: TAction;
    mmLogging: TMemo;
    Layout2: TLayout;
    Splitter1: TSplitter;
    mmCode: TMemo;
    DataControl1: TDataControl;
    procedure FormCreate(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LogCallback(AMessage: string);
    function GetJsCode: string;

  end;

var
  AppPlatformDialog: TAppPlatformDialog;

implementation

uses
  System.Diagnostics,
  app.platform;

{$R *.fmx}

procedure TAppPlatformDialog.FormCreate(Sender: TObject);
begin
  if TAppPlatform.Logs <> nil then
    for var s in TAppPlatform.Logs do
      mmLogging.Lines.Add(s);

  TAppPlatform.OnLogMessage := LogCallback;
end;

function TAppPlatformDialog.GetJsCode: string;
begin
  Result := mmCode.Lines.Text;
end;

procedure TAppPlatformDialog.LogCallback(AMessage: string);
begin
  TThread.Queue(nil, procedure begin
    mmLogging.Lines.Add(AMessage);
  end);
end;

procedure TAppPlatformDialog.acExecuteExecute(Sender: TObject);
begin
  mmLogging.Lines.Clear;
  var st := TStopWatch.StartNew;

  TAppPlatform.Evaluate(GetJsCode);

  mmLogging.Lines.Add('done: ' + st.ElapsedMilliseconds.ToString + 'ms');
end;

end.
