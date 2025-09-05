unit SimpleRunnerWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Diagnostics,
  quickjs_ng, QuickJS.Register.intf;

type
  TForm1 = class(TForm)
    PanelTop: TPanel;
    btnRun: TButton;
    lblInfo: TLabel;
    MemoConsole: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    FRuntime: IJSRuntime;
    FContext: IJSContext;
    procedure InitializeQuickJS;
    procedure Log(const S: string);
    procedure LogCallBack(S: string);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  QuickJS.Register.impl;

{$R *.dfm}

procedure TForm1.InitializeQuickJS;
begin
  if FContext = nil then
  begin
    QuickJS.Register.impl.OutputLogString := LogCallBack;
    if FRuntime = nil then
      FRuntime := TJSRuntime.Create;
    FContext := TJSContext.Create(FRuntime);
  end;
end;

procedure TForm1.Log(const S: string);
begin
  MemoConsole.Lines.Add(S);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeQuickJS;
end;

procedure TForm1.btnRunClick(Sender: TObject);
var
  FilePath: string;
  Src: TStringList;
  SW: TStopwatch;
begin
  MemoConsole.Lines.Clear;
  FilePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'main.js';
  if not FileExists(FilePath) then
  begin
    Log('ERROR: main.js not found at: ' + FilePath);
    Exit;
  end;

  InitializeQuickJS;

  Src := TStringList.Create;
  try
    Src.LoadFromFile(FilePath, TEncoding.UTF8);
    SW := TStopwatch.StartNew;
    try
      var R := FContext.eval_with_result(Src.Text, 'main.js');
      Log('result: ' + R.ToString);
      Log('done: ' + SW.ElapsedMilliseconds.ToString + 'ms');
    except
      on E: Exception do
      begin
        Log('ERROR: ' + E.ClassName + ': ' + E.Message);
      end;
    end;
  finally
    Src.Free;
  end;
end;

procedure TForm1.LogCallBack(S: string);
begin
  MemoConsole.Lines.Add(S);
end;

end.
