program QuickNodeJSConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  quickjs_ng,
  QuickJS.Register.intf,
  QuickJS.Register.impl;

var
  FRuntime: IJSRuntime;
  FContext: IJSContext;

procedure InitializeQuickJS;
begin
  QuickJS.Register.impl.OutputLogString := TProc<string>(
    procedure(S: string)
    begin
      Writeln(S);
    end);
  FRuntime := TJSRuntime.Create;
  FContext := TJSContext.Create(FRuntime);
end;

procedure WaitForExitWithTimeout;
var
  hIn: THandle;
  ir: INPUT_RECORD;
  readCount: DWORD;
  start: UInt64;
  keepOpen: Boolean;
  vk: Word;
  ch: WideChar;
begin
  Writeln('Auto-closing in 10 seconds. Press Enter to exit, or press S to stay open...');
  hIn := GetStdHandle(STD_INPUT_HANDLE);
  start := GetTickCount64;
  keepOpen := False;

  while True do
  begin
    readCount := 0;
    if PeekConsoleInput(hIn, ir, 1, readCount) and (readCount > 0) then
    begin
      ReadConsoleInput(hIn, ir, 1, readCount);
      if ir.EventType = KEY_EVENT then
      begin
        if ir.Event.KeyEvent.bKeyDown then
        begin
          vk := ir.Event.KeyEvent.wVirtualKeyCode;
          ch := ir.Event.KeyEvent.UnicodeChar;

          if vk = VK_RETURN then
            Exit;

          if (ch = 's') or (ch = 'S') then
          begin
            if not keepOpen then
            begin
              keepOpen := True;
              Writeln('Stay-open enabled. Press Enter to exit.');
            end;
          end;
        end;
      end;
    end;

    if not keepOpen then
    begin
      if (GetTickCount64 - start) >= 10000 then
        Exit;
    end;

    Sleep(50);
  end;
end;

procedure RunJavaScriptFile(const FilePath: string);
var
  Src: TStringList;
  SW: TStopwatch;
begin
  if not FileExists(FilePath) then
  begin
    Writeln('ERROR: File not found: ' + FilePath);
    ExitCode := 1;
    Exit;
  end;

  Src := TStringList.Create;
  try
    Src.LoadFromFile(FilePath, TEncoding.UTF8);
    SW := TStopwatch.StartNew;
    try
      var R := FContext.eval_with_result(Src.Text, ExtractFileName(FilePath));
      Writeln('Result: ' + R.ToString);
      Writeln('Execution time: ' + SW.ElapsedMilliseconds.ToString + 'ms');
    except
      on E: Exception do
      begin
        Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Src.Free;
  end;
end;

begin
  try
    if ParamCount < 1 then
    begin
      Writeln('Usage: QuickNodeJSConsole <javascript-file>');
      Writeln('Example: QuickNodeJSConsole main.js');
      ExitCode := 1;
      Exit;
    end;

    InitializeQuickJS;
    RunJavaScriptFile(ParamStr(1));
    WaitForExitWithTimeout;
  except
    on E: Exception do
    begin
      Writeln('FATAL ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
