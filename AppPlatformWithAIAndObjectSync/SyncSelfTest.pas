unit SyncSelfTest;

// Headless self-test of live app sessions (AppPlatform.AppSession), without a
// browser and without showing a window:
//
//   set APPPLATFORM_SYNC_SELFTEST=<session script file>
//   AppPlatform.exe
//
// The Studio runs it at the end of FormCreate (before the window is shown) and
// exits. The session script is what the builder sends for an app's host block
// (appHost.js buildAppSessionScript). It plays two app pages (A and B) and the
// Studio's inspector against the same objects, and writes every request,
// answer and push to <exe>.selftest.log.

interface

procedure RunSyncSelfTest(const ScriptFile: string);

implementation

uses
  System.SysUtils, System.StrUtils, System.IOUtils, System.JSON, System.Generics.Collections,
  System_, ADato.ObjectModel.intf, ADato.ObjectModel.impl,
  App.intf,
  AppPlatform.AppSync, AppPlatform.AppSession, AppPlatform.LynxApps.Host,
  QuickJS.Register.intf, System.Classes;

var
  _log: string;

procedure Log(const Text: string);
begin
  TFile.AppendAllText(_log, Text + sLineBreak, TEncoding.UTF8);
end;

function Session(const Name: string; const Params: TArray<TSessionParam>) : TAppSession;
begin
  Result := TAppSession.Create(Name, Params,
    procedure(const Channel, Json: string)
    begin
      Log(Format('  push to %s: %s %s', [Name, Channel, Json]));
    end,
    function(Text: string): string
    begin
      Result := Text;
    end);
end;

procedure Request(const S: TAppSession; const Json: string);
begin
  Log(Format('%s <- %s', [S.Id, Json]));
  Log(Format('%s -> %s', [S.Id, S.HandleRequest(Json)]));
end;

function FindType(const Name: string) : &Type;
begin
  for var t in _app.Config.Types do
    if SameText(t.Name, Name) then
      Exit(t);
  raise Exception.Create('No type ' + Name);
end;

// A steps file instead of a script:
//   { "script": "<session script>", "pages": { "A": { "project": "IProject:3" }, "B": {} },
//     "steps": [ ["A", { "op": "open", "object": "project" }],
//                ["studio", { "object": "IProject:3", "property": "Description", "value": "x" }],
//                ["log", "text"] ] }
procedure RunSteps(const StepsFile: string);
begin
  var root := TJSONObject.ParseJSONValue(TFile.ReadAllText(StepsFile, TEncoding.UTF8)) as TJSONObject;
  var pages := TDictionary<string, TAppSession>.Create;
  var refs := TList<IAppSessionHost>.Create;
  try
    var code := TFile.ReadAllText(root.GetValue<string>('script'), TEncoding.UTF8);
    for var page in root.GetValue('pages') as TJSONObject do
    begin
      var params: TArray<TSessionParam> := [];
      for var param in page.JsonValue as TJSONObject do
      begin
        var p: TSessionParam;
        var err := TAppSession.ResolveObject(param.JsonValue.Value, p);
        if err <> '' then
          raise Exception.Create(err);
        p.Name := param.JsonString.Value;
        params := params + [p];
      end;
      var s := Session(page.JsonString.Value, params);
      pages.Add(s.Id, s);
      refs.Add(s);
      var output := AppSessionRunner(s, code, 'selftest-' + s.Id + '.js');
      Log(Format('%s started: %s, load error: "%s", output: %s', [s.Id, BoolToStr(s.Started, True), s.LoadError, output.Trim]));
    end;

    for var step in root.GetValue('steps') as TJSONArray do
    begin
      var who := TJSONArray(step).Items[0].Value;
      var what := TJSONArray(step).Items[1];
      if who = 'log' then
        Log('--- ' + what.Value)
      else if who = 'main' then
        // JavaScript on the main runtime (where app sessions run)
        Log('main runtime: ' + AppSessionQuery(what.Value))
      else if who = 'worker' then
      begin
        // JavaScript on the Lynx Apps worker runtime (where host_eval and
        // AppPlatformWithAI's app actions run), as RunIntegration does it
        var workerCode := what.Value;
        var answer := '';
        var done := False;
        LynxAppsHost.RunScript(
          procedure(const Runtime: IJSRuntime)
          begin
            try
              var context := Runtime.CreateContext;
              context.RegisterLiveInterfaceInstance('app', TypeInfo(IAppObject), _app);
              answer := context.eval_with_result(workerCode, '<worker>').ToString;
            except
              on E: Exception do answer := 'error: ' + E.Message;
            end;
            done := True;
          end);
        var deadline := Now + 30 / SecsPerDay;
        while not done and (Now < deadline) do
        begin
          CheckSynchronize(10);   // host calls are marshalled to this thread
        end;
        Log('worker runtime: ' + IfThen(done, answer, '(timed out)'));
      end
      else if who = 'studio' then
      begin
        // As the Studio's inspector: a context on the shared model
        var target: TSessionParam;
        var err := TAppSession.ResolveObject(what.GetValue<string>('object'), target);
        if err <> '' then
          raise Exception.Create(err);
        var context: IObjectModelContext := TObjectModelContext.Create(ModelFor(target.ValueType));
        context.Context := target.Value;
        var binding := TCallbackBinding.Create(nil);
        var ref: IPropertyBinding := binding;
        context.Bind(what.GetValue<string>('property'), ref);
        Log('studio edits ' + what.ToJSON);
        var value := (what as TJSONObject).GetValue('value');
        if value is TJSONBool then
          binding.Edit(TJSONBool(value).AsBoolean)
        else
          binding.Edit(value.Value);
        context.Unbind;
      end
      else
        Request(pages[who], what.ToJSON);
    end;

    for var s in pages.Values do
      s.Detach;
    Log('done');
  finally
    refs.Free;
    pages.Free;
    root.Free;
  end;
end;

procedure RunSyncSelfTest(const ScriptFile: string);
begin
  _log := ChangeFileExt(ParamStr(0), '.selftest.log');
  if SameText(ExtractFileExt(ScriptFile), '.json') then
  begin
    TFile.WriteAllText(_log, '', TEncoding.UTF8);
    try
      RunSteps(ScriptFile);
    except
      on E: Exception do
        Log('SELFTEST FAILED: ' + E.ClassName + ': ' + E.Message);
    end;
    Exit;
  end;
  TFile.WriteAllText(_log, '', TEncoding.UTF8);
  try
    var code := TFile.ReadAllText(ScriptFile, TEncoding.UTF8);

    // Page A is opened with project 3 (as from the Studio's Open in app); B without
    var p: TSessionParam;
    var err := TAppSession.ResolveObject('IProject:3', p);
    Log('resolve IProject:3 -> ' + IfThen(err = '', 'ok', err));
    p.Name := 'project';

    var a := Session('A', [p]);
    var aRef: IAppSessionHost := a;
    var b := Session('B', []);
    var bRef: IAppSessionHost := b;

    // What the session's script sees of the session object
    var probe := Session('probe', []);
    var probeRef: IAppSessionHost := probe;
    Log('probe: ' + AppSessionRunner(probe,
      'console.log("typeof __appSession:", typeof globalThis.__appSession);' +
      'try { console.log("Params:", globalThis.__appSession.Params()); } catch (e) { console.log("Params failed:", String(e)); }' +
      'try { console.log("SessionId:", globalThis.__appSession.SessionId()); } catch (e) { console.log("SessionId failed:", String(e)); }' +
      'console.log("app:", typeof app, "items:", typeof items);',
      'selftest-probe.js').Trim);
    Log('probe started: ' + BoolToStr(probe.Started, True));
    probe.Detach;

    for var s in [a, b] do
    begin
      var output := AppSessionRunner(s, code, 'selftest-' + s.Id + '.js');
      Log(Format('%s started: %s, load error: "%s", output: %s', [s.Id, BoolToStr(s.Started, True), s.LoadError, output.Trim]));
    end;

    Log('--- both pages open their project (A: project 3 from the param, B: the first)');
    Request(a, '{"op":"open","object":"project"}');
    Request(b, '{"op":"open","object":"project"}');

    Log('--- B switches to project 3 as well');
    Request(b, '{"op":"run","action":"selectProject","args":{"id":"3"}}');

    Log('--- A edits the description: B must get a push');
    Request(a, '{"op":"edit","object":"project","property":"Description","value":"Renamed by page A"}');

    Log('--- the Studio inspector edits it (context on the shared model): A and B must get pushes');
    var project := p.Value;
    var studio: IObjectModelContext := TObjectModelContext.Create(ModelFor(FindType('IProject')));
    studio.Context := project;
    var studioHeard := 0;
    var studioBinding := TCallbackBinding.Create(procedure begin Inc(studioHeard); end);
    var studioRef: IPropertyBinding := studioBinding;
    studio.Bind('Description', studioRef);
    studioBinding.Armed := True;
    studioBinding.Edit('Renamed in the Studio');
    Log('--- B edits it: the Studio must hear it');
    Request(b, '{"op":"edit","object":"project","property":"Description","value":"Renamed by page B"}');
    var description := '';
    for var prop in _app.Config.GetProperties(FindType('IProject')) do
      if SameText(prop.Name, 'Description') then
        description := prop.GetValue(project, []).ToString;
    Log(Format('Studio heard %d change(s); project 3 is now "%s"', [studioHeard, description]));

    Log('--- an action changes it with page.update: everyone must hear it');
    Request(a, '{"op":"run","action":"renameViaUpdate","args":{"text":"Renamed by an action"}}');
    Log(Format('Studio heard %d change(s)', [studioHeard]));

    Log('--- A takes edit away from its page: A gets the new shape, its edit is refused');
    Request(a, '{"op":"run","action":"makeReadOnly"}');
    Request(a, '{"op":"edit","object":"project","property":"Description","value":"Should be refused"}');

    Log('--- a script type: A exposes a Customer and edits its name; B shows the same customer');
    Request(a, '{"op":"run","action":"exposeCustomer"}');
    Request(b, '{"op":"run","action":"exposeCustomer"}');
    Request(a, '{"op":"open","object":"customer"}');
    Request(b, '{"op":"open","object":"customer"}');
    Request(a, '{"op":"edit","object":"customer","property":"Name","value":"Acme Renamed"}');
    Request(a, '{"op":"edit","object":"customer","property":"Active","value":false}');

    Log('--- errors the page can cause');
    Request(a, '{"op":"open","object":"nothing"}');
    Request(a, '{"op":"run","action":"nope"}');
    Request(a, '{"op":"run","action":"fails"}');
    Request(a, '{"op":"edit","object":"project","property":"ID","value":"x"}');
    Request(a, '{"op":"run","action":"echo","args":{"n":1}}');

    studio.Unbind;
    a.Detach;
    b.Detach;
    Log('--- after Detach');
    Request(a, '{"op":"run","action":"echo"}');
    Log('done');
  except
    on E: Exception do
      Log('SELFTEST FAILED: ' + E.ClassName + ': ' + E.Message);
  end;
end;

end.
