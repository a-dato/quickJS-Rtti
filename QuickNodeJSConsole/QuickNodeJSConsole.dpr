program QuickNodeJSConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.StrUtils,
  quickjs_ng,
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  TestObjects.intf,
  TestObjects.impl,
  QuickJS.Register.dn4d.impl,
  TestObjectsDefinitionsTest.impl;

type
  TQuickJSConsole = class
  private
    // Shared runtime is now managed internally by QuickJS.Register
    FContext: IJSContext;
    FTestObject: ITestObject;
    FTestObject2: ITestObject2;
    FTestObject3: ITestObject3;
    FJavaScriptFilePath: string;
    
    function EscapeJS(const S: string): string;
    function QuoteJS(const S: string): string;
    function ParseEnvFile(const FilePath: string): TDictionary<string, string>;
    procedure InjectEnvDictionaryToJS(const Env: TDictionary<string, string>);
    procedure ClearConsole;
    procedure ShowHeader(const JSFilePath: string; const EnvFilePath: string = '');
  public
    procedure InitializeQuickJS;
    procedure InjectEnvFromFile(const EnvFile: string);
    procedure RunJavaScriptFile(const FilePath: string; TimeoutSeconds: Integer = 0);
    procedure WaitForExitWithAutoClose(AutoCloseSeconds: Integer);
    procedure WaitForExit;
    procedure Execute(const JSFilePath, EnvFilePath: string; TimeoutSeconds, AutoCloseSeconds: Integer);
  end;

var
  Console: TQuickJSConsole;

{ TQuickJSConsole }

procedure TQuickJSConsole.ClearConsole;
begin
  // Clear the console screen
  System.Write(#27'[2J'#27'[H');
end;

procedure TQuickJSConsole.ShowHeader(const JSFilePath: string; const EnvFilePath: string = '');
begin
  Writeln('================================================================================');
  Writeln('                            QuickJS Console Runner                             ');
  Writeln('================================================================================');
  Writeln('JavaScript File: ' + JSFilePath);
  if EnvFilePath <> '' then
    Writeln('Environment File: ' + EnvFilePath)
  else
    Writeln('Environment File: (none)');
  Writeln('Execution Time: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  Writeln('================================================================================');
  Writeln('');
end;

procedure TQuickJSConsole.InitializeQuickJS;
begin
  QuickJS.Register.impl.OutputLogString := TProc<string>(
    procedure(S: string)
    begin
      Writeln(S);
    end);
  FContext := TJSRegister.CreateContext;

 TJSRegisterTypedObjects.Initialize(FContext);


  // Create and register the test object
  // Use TTestObject3 which inherits from ITestObject2 and ITestObject
  // This will allow testing inheritance - the object supports all three interfaces
  FTestObject3 := TTestObject3.Create;
  FTestObject2 := FTestObject3 as ITestObject2;
  FTestObject := FTestObject3 as ITestObject;

  TJSRegister.RegisterLiveObject(FContext, 'testObj', TypeInfo(ITestObject), FTestObject);
  TJSRegister.RegisterLiveObject(FContext, 'testObj2', TypeInfo(ITestObject2), FTestObject2);
  TJSRegister.RegisterLiveObject(FContext, 'testObj3', TypeInfo(ITestObject3), FTestObject3);
end;

function TQuickJSConsole.EscapeJS(const S: string): string;
begin
  Result := StringReplace(S, '\\', '\\\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
end;

function TQuickJSConsole.QuoteJS(const S: string): string;
begin
  Result := '"' + EscapeJS(S) + '"';
end;

function TQuickJSConsole.ParseEnvFile(const FilePath: string): TDictionary<string, string>;
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

procedure TQuickJSConsole.InjectEnvDictionaryToJS(const Env: TDictionary<string, string>);
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

procedure TQuickJSConsole.InjectEnvFromFile(const EnvFile: string);
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

procedure TQuickJSConsole.WaitForExitWithAutoClose(AutoCloseSeconds: Integer);
var
  hIn: THandle;
  ir: INPUT_RECORD;
  readCount: DWORD;
  start: UInt64;
  keepOpen: Boolean;
  vk: Word;
  ch: WideChar;
  timeoutMs: UInt64;
begin
  timeoutMs := UInt64(AutoCloseSeconds) * 1000;
  Writeln(Format('Auto-closing in %d seconds. Press Enter to exit, S to stay open, or Ctrl+R to re-run...', [AutoCloseSeconds]));
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

          // Check for Ctrl+R
          if (vk = Ord('R')) and ((ir.Event.KeyEvent.dwControlKeyState and (LEFT_CTRL_PRESSED or RIGHT_CTRL_PRESSED)) <> 0) then
          begin
            Writeln('Re-running JavaScript file...');
            Self.RunJavaScriptFile(FJavaScriptFilePath, 0); // No timeout for re-run
            Continue;
          end;

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
      if (GetTickCount64 - start) >= timeoutMs then
        Exit;
    end;

    Sleep(50);
  end;
end;

procedure TQuickJSConsole.WaitForExit;
var
  hIn: THandle;
  ir: INPUT_RECORD;
  readCount: DWORD;
  vk: Word;
  ch: WideChar;
begin
  Writeln('Press Enter to exit or Ctrl+R to re-run...');
  hIn := GetStdHandle(STD_INPUT_HANDLE);

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

          // Check for Ctrl+R
          if (vk = Ord('R')) and ((ir.Event.KeyEvent.dwControlKeyState and (LEFT_CTRL_PRESSED or RIGHT_CTRL_PRESSED)) <> 0) then
          begin
            ClearConsole;
            ShowHeader(FJavaScriptFilePath, ''); // We don't store the env file path for re-runs
            Writeln('Re-running JavaScript file...');
            Writeln('');
            Self.RunJavaScriptFile(FJavaScriptFilePath, 0); // No timeout for re-run
            Continue;
          end;
        end;
      end;
    end;

    Sleep(50);
  end;
end;

procedure TQuickJSConsole.RunJavaScriptFile(const FilePath: string; TimeoutSeconds: Integer = 0);
var
  Src: TStringList;
  SW: TStopwatch;
  ExecutionThread: TThread;
  ExecutionCompleted: Boolean;
  ExecutionException: Exception;
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
    
    if TimeoutSeconds > 0 then
    begin
      // Execute with timeout using a thread
      ExecutionCompleted := False;
      ExecutionException := nil;
      
      ExecutionThread := TThread.CreateAnonymousThread(
        procedure
        begin
          try
            FContext.eval(Src.Text, ExtractFileName(FilePath));
          except
            on E: Exception do
            begin
              ExecutionException := Exception.Create(E.Message);
              ExecutionException.Message := E.ClassName + ': ' + E.Message;
            end;
          end;
          ExecutionCompleted := True;
        end);
      
      ExecutionThread.Start;
      
      // Wait for completion or timeout
      var StartTime := GetTickCount64;
      var TimeoutMs := UInt64(TimeoutSeconds) * 1000;
      
      while not ExecutionCompleted do
      begin
        if (GetTickCount64 - StartTime) >= TimeoutMs then
        begin
          Writeln(Format('ERROR: JavaScript execution timed out after %d seconds', [TimeoutSeconds]));
          ExecutionThread.Terminate;
          ExitCode := 1;
          Exit;
        end;
        Sleep(50);
      end;
      
      ExecutionThread.WaitFor;
      ExecutionThread.Free;
      
      if Assigned(ExecutionException) then
      begin
        Writeln('ERROR: ' + ExecutionException.Message);
        ExecutionException.Free;
        ExitCode := 1;
        Exit;
      end;
    end
    else
    begin
      // Execute without timeout
      try
        FContext.eval(Src.Text, ExtractFileName(FilePath));
      except
        on E: Exception do
        begin
          Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
          ExitCode := 1;
          Exit;
        end;
      end;
    end;
    
    Writeln('Execution time: ' + SW.ElapsedMilliseconds.ToString + 'ms');
  finally
    Src.Free;
  end;
end;

procedure TQuickJSConsole.Execute(const JSFilePath, EnvFilePath: string; TimeoutSeconds, AutoCloseSeconds: Integer);
begin
  ClearConsole;
  ShowHeader(JSFilePath, EnvFilePath);
  
  InitializeQuickJS;
  FJavaScriptFilePath := JSFilePath;
  
  if EnvFilePath <> '' then
    InjectEnvFromFile(EnvFilePath);
    
  RunJavaScriptFile(FJavaScriptFilePath, TimeoutSeconds);
  
  // Only wait if execution was successful (ExitCode = 0)
  if ExitCode = 0 then
  begin
    if AutoCloseSeconds > 0 then
      WaitForExitWithAutoClose(AutoCloseSeconds)
    else
      WaitForExit;
  end;
end;

var
  JSFilePath, EnvFilePath: string;
  TimeoutSeconds, AutoCloseSeconds: Integer;
  I: Integer;
  Param: string;

begin
  try
    if ParamCount < 1 then
    begin
      Writeln('Usage: QuickNodeJSConsole <javascript-file> [env-file] [--timeout <seconds>] [--auto-close <seconds>]');
      Writeln('');
      Writeln('Parameters:');
      Writeln('  <javascript-file>     JavaScript file to execute');
      Writeln('  [env-file]           Optional environment file (.env)');
      Writeln('  --timeout <seconds>  Interrupt JavaScript execution after specified seconds');
      Writeln('  --auto-close <seconds>  Auto-close console after JavaScript completes');
      Writeln('');
      Writeln('Examples:');
      Writeln('  QuickNodeJSConsole main.js');
      Writeln('  QuickNodeJSConsole main.js .env');
      Writeln('  QuickNodeJSConsole main.js --timeout 30');
      Writeln('  QuickNodeJSConsole main.js .env --auto-close 10');
      Writeln('  QuickNodeJSConsole main.js --timeout 60 --auto-close 5');
      Writeln('');
      Writeln('Note: Neither timeout nor auto-close are active by default');
      ExitCode := 1;
      Exit;
    end;

    // Parse command line parameters
    JSFilePath := ParamStr(1);
    EnvFilePath := '';
    TimeoutSeconds := 0;    // No timeout by default
    AutoCloseSeconds := 0;  // No auto-close by default

    I := 2;
    while I <= ParamCount do
    begin
      Param := ParamStr(I);
      if SameText(Param, '--timeout') then
      begin
        Inc(I);
        if I <= ParamCount then
        begin
          if not TryStrToInt(ParamStr(I), TimeoutSeconds) then
          begin
            Writeln('ERROR: Invalid timeout value: ' + ParamStr(I));
            ExitCode := 1;
            Exit;
          end;
          if TimeoutSeconds <= 0 then
          begin
            Writeln('ERROR: Timeout must be greater than 0');
            ExitCode := 1;
            Exit;
          end;
        end
        else
        begin
          Writeln('ERROR: --timeout requires a value');
          ExitCode := 1;
          Exit;
        end;
      end
      else if SameText(Param, '--auto-close') then
      begin
        Inc(I);
        if I <= ParamCount then
        begin
          if not TryStrToInt(ParamStr(I), AutoCloseSeconds) then
          begin
            Writeln('ERROR: Invalid auto-close value: ' + ParamStr(I));
            ExitCode := 1;
            Exit;
          end;
          if AutoCloseSeconds <= 0 then
          begin
            Writeln('ERROR: Auto-close time must be greater than 0');
            ExitCode := 1;
            Exit;
          end;
        end
        else
        begin
          Writeln('ERROR: --auto-close requires a value');
          ExitCode := 1;
          Exit;
        end;
      end
      else if (EnvFilePath = '') and not StartsText('--', Param) then
      begin
        EnvFilePath := Param;
      end
      else
      begin
        Writeln('ERROR: Unknown parameter: ' + Param);
        ExitCode := 1;
        Exit;
      end;
      Inc(I);
    end;

    Console := TQuickJSConsole.Create;
    try
      Console.Execute(JSFilePath, EnvFilePath, TimeoutSeconds, AutoCloseSeconds);
    finally
      Console.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('FATAL ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
