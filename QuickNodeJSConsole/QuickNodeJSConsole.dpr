program QuickNodeJSConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
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

function EscapeJS(const S: string): string;
begin
  Result := StringReplace(S, '\\', '\\\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
end;

function QuoteJS(const S: string): string;
begin
  Result := '"' + EscapeJS(S) + '"';
end;

function ParseEnvFile(const FilePath: string): TDictionary<string, string>;
var
  Lines: TStringList;
  Line, Key, Value: string;
  P: Integer;
begin
  Result := TDictionary<string, string>.Create;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath, TEncoding.UTF8);
    for var I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if (Line = '') or (Line[1] = '#') then
        Continue;
      if (Length(Line) >= 7) and SameText(Copy(Line, 1, 7), 'export ') then
        Line := Trim(Copy(Line, 8, MaxInt));
      P := Pos('=', Line);
      if P <= 1 then
        Continue;
      Key := Trim(Copy(Line, 1, P - 1));
      Value := Trim(Copy(Line, P + 1, MaxInt));
      if (Value <> '') and ((Value[1] = '"') or (Value[1] = '''')) then
      begin
        var q := Value[1];
        if (Length(Value) >= 2) and (Value[Length(Value)] = q) then
          Value := Copy(Value, 2, Length(Value) - 2);
        Value := StringReplace(Value, '\n', #10, [rfReplaceAll]);
        Value := StringReplace(Value, '\r', #13, [rfReplaceAll]);
        if q = '"' then
          Value := StringReplace(Value, '\"', '"', [rfReplaceAll])
        else
          Value := StringReplace(Value, '\''', '\''', [rfReplaceAll]);
      end;
      if Key <> '' then
        Result.AddOrSetValue(Key, Value);
    end;
  finally
    Lines.Free;
  end;
end;

procedure InjectEnvDictionaryToJS(const Env: TDictionary<string, string>);
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine('if (!globalThis.process) globalThis.process = {};');
    SB.AppendLine('if (!globalThis.process.env) globalThis.process.env = {};');
    for var Pair in Env do
    begin
      SB.Append('globalThis.process.env[');
      SB.Append(QuoteJS(Pair.Key));
      SB.Append('] = ');
      SB.Append(QuoteJS(Pair.Value));
      SB.AppendLine(';');
    end;
    FContext.eval(SB.ToString, 'env.js');
  finally
    SB.Free;
  end;
end;

procedure InjectEnvFromFile(const EnvFile: string);
var
  Env: TDictionary<string, string>;
begin
  if not FileExists(EnvFile) then
  begin
    Writeln('ERROR: .env file not found: ' + EnvFile);
    ExitCode := 1;
    Halt(ExitCode);
  end;
  Env := ParseEnvFile(EnvFile);
  try
    InjectEnvDictionaryToJS(Env);
  finally
    Env.Free;
  end;
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
      FContext.eval(Src.Text, ExtractFileName(FilePath));
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
      Writeln('Usage: QuickNodeJSConsole <javascript-file> [env-file]');
      Writeln('Example: QuickNodeJSConsole main.js .env');
      ExitCode := 1;
      Exit;
    end;

    InitializeQuickJS;
    if ParamCount >= 2 then
      InjectEnvFromFile(ParamStr(2));
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
