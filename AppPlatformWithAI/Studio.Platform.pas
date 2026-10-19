unit Studio.Platform;

// Starts the app platform the Studio shows. No UI in here.
//
//   1. The 'app' object (App.intf._app) with the Delphi type IProject and its
//      storage 'Projects'.
//   2. The main QuickJS runtime, with the globals 'app' and 'types'.
//   3. The type scripts: every js\types\<Name>.js defines one JavaScript type
//      (App.TypeScripts); they run on the main runtime.
//   4. The runtime factory for Lynx Apps: the host scripts of AI-built apps run
//      on their own runtime, on a worker thread, against the same 'app'.

interface

uses
  System.SysUtils;

type
  TPlatform = class
  public
    // Logs go to OnLog (script console.log output and platform messages).
    class var OnLog: TProc<string>;

    class procedure Start; static;
    // Runs JavaScript on the main runtime. CodeName shows up in error messages
    // and is the base for relative imports.
    class procedure Run(const Code: string; const CodeName: string); static;
    // Runs Code like Run and returns what it logged, errors included.
    class function RunCaptured(const Code: string; const CodeName: string) : string; static;
    // Evaluates a JavaScript expression on the main runtime; its value as text.
    class function Query(const Expression: string) : string; static;
    // The js folder: platform.js and the type scripts (types\).
    class function ScriptFolder: string; static;
  end;

procedure Log(const Text: string);

implementation

uses
  System.Classes,
  System.IOUtils,
  System_,
  System.Collections,
  System.Collections.Generic,
  System.ComponentModel,
  ADato.StatusCode,
  ADato.ObjectModel.TrackInterfaces,
  ADato.ObjectModel.List.Tracking.intf,
  ADato.ObjectModel.List.Tracking.impl,
  // Creates the global ExtensionManager: without it app.Config.AddProperty
  // silently does nothing
  ADato.Extensions.impl,
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  QuickJS.Register.dn4d.intf,
  QuickJS.Register.dn4d.impl,
  XMLHttpRequest.intf,
  XMLHttpRequest.impl,
  App.intf,
  App.impl,
  App.Component.intf,
  App.Content.intf,
  App.Environment.impl,
  App.Storage.intf,
  App.QuickJSBridge.intf,
  FMX.App.MasterForm,
  Project.intf,
  Project.impl,
  App.TypeScripts,
  App.Tokens,
  AppPlatform.LynxApps.Host;

var
  _runtime: IJSRuntime;
  _context: IJSContext;
  _bridge: IAppQuickJSBridge;
  _lynxAppsBridge: IAppQuickJSBridge;
  // Collects the log output of RunCaptured
  _capture: TStringBuilder;

procedure Log(const Text: string);
begin
  if Assigned(TPlatform.OnLog) then
    TPlatform.OnLog(Text);
end;

// The Delphi side of the platform: IProject, its factory and its storage.
procedure RegisterDelphiTypes;
begin
  TProject.TypeDescriptor.Binder := TFrameBinder.Create();
  TProject.TypeDescriptor.Provider := ProjectProvider.Create(TProject.TypeDescriptor);

  _app.Config.RegisterType(TProject.Type, TProject.TypeDescriptor);

  _app.Factory.RegisterType(TProject.Type, function : CObject begin
    Result := CObject.From<IProject>(TProject.Create);
  end);

  _app.Factory.RegisterCollection(TProject.Type, function : CObject begin
    Result := CObject.From<List<IProject>>(CList<IProject>.Create);
  end);

  var storage := _app.AddStorage(TProject.Type, TProject.TypeDescriptor.StorageName);
  storage.Attach(TProject.TypeDescriptor.Provider.Data(nil));
end;

// The types both runtimes know: lists, XMLHttpRequest (fetch) and the
// interfaces of the platform objects.
procedure RegisterSharedTypes(const Runtime: IJSRuntime);
begin
  Runtime.RegisterObjectWithConstructor('List', TypeInfo(List<CObject>),
    function : Pointer begin
      Result := CList<CObject>.Create;
    end);

  Runtime.RegisterObjectWithConstructor('XMLHttpRequest', TypeInfo(IXMLHttpRequest),
    function : Pointer begin
      Result := TXMLHttpRequest.Create;
    end);

  Runtime.RegisterObjectType('IBaseInterface', TypeInfo(IBaseInterface));
  Runtime.RegisterObjectType('IAddingNew', TypeInfo(IAddingNew));
  Runtime.RegisterObjectType('IAddRange', TypeInfo(IAddRange));
  Runtime.RegisterObjectType('ICloneable', TypeInfo(ICloneable));
  Runtime.RegisterObjectType('IEditState', TypeInfo(IEditState));
  Runtime.RegisterObjectType('IEditableModel', TypeInfo(IEditableModel));
  Runtime.RegisterObjectType('IUpdatableObject', TypeInfo(IUpdatableObject));
  Runtime.RegisterObjectType('IStorage', TypeInfo(IStorage));
  Runtime.RegisterObjectType('IStorageSupport', TypeInfo(IStorageSupport));
  Runtime.RegisterObjectType('IProject', TProject.Type.GetTypeInfo);
end;

procedure RegisterValueTypes(const Runtime: IJSRuntime);
begin
  Runtime.RegisterObjectType('DateTime', TypeInfo(CDateTime));
  Runtime.RegisterObjectType('StatusCode', TypeInfo(StatusCode));
end;

procedure CreateMainRuntime;
begin
  _runtime := TJSRuntimeDN4D.Create;
  RegisterSharedTypes(_runtime);

  // Used by the JavaScript type descriptors
  _runtime.RegisterObjectWithConstructor('ObjectModel', TypeInfo(IObjectListModelChangeTracking),
    function : Pointer begin
      Result := TObjectListModelWithChangeTracking<IJSObject>.Create(nil);
    end);

  _runtime.RegisterObjectWithConstructor('JSBinder', TypeInfo(IContentBinder),
    function : Pointer begin
      Result := TFrameBinder.Create();
    end);

  _runtime.RegisterObjectType('IComponent', TypeInfo(IComponent));
  _runtime.RegisterObjectType('IOnItemChangedSupport', TypeInfo(IOnItemChangedSupport));

  _bridge := CreateAppQuickJSBridge(_app.Config, _runtime);
  _bridge.RegisterExistingTypes;

  _context := _runtime.CreateContext;
  _context.LogString := procedure(S: string) begin
    // Captured output belongs to the caller of RunCaptured (a type script
    // load or save), which reports what matters
    if _capture <> nil then
      _capture.AppendLine(S)
    else
      TThread.Queue(nil, procedure begin Log(S); end);
  end;

  _context.RegisterLiveInterfaceInstance('app', TypeInfo(IAppObject), _app);
end;

// Called once by Lynx Apps, on its script thread.
function CreateLynxAppsRuntime: IJSRuntime;
begin
  var runtime := TJSRuntimeDN4D.Create;
  // Calls into platform objects run on the main thread.
  runtime.MarshalHostCalls := True;
  Result := runtime;

  RegisterSharedTypes(Result);

  _lynxAppsBridge := CreateAppQuickJSBridge(_app.Config, Result);
  _lynxAppsBridge.RegisterExistingTypes;

  RegisterValueTypes(Result);
end;

procedure PublishTokens;
begin
  _context.eval('globalThis.tokens = Object.freeze(' + TLocalTokens.ToJson + ');', '<tokens>');
end;

{ TPlatform }

class function TPlatform.ScriptFolder: string;
begin
  // The exe runs from Win64\Debug (or similar): look upwards for js\platform.js.
  var dir := ExtractFilePath(ParamStr(0));
  while dir <> '' do
  begin
    var candidate := TPath.Combine(dir, 'js');
    if TFile.Exists(TPath.Combine(candidate, 'platform.js')) then
      Exit(candidate);

    var parent := ExtractFileDir(ExcludeTrailingPathDelimiter(dir));
    if (parent = '') or SameText(parent, ExcludeTrailingPathDelimiter(dir)) then
      Break;
    dir := IncludeTrailingPathDelimiter(parent);
  end;

  raise Exception.Create('js\platform.js not found next to or above ' + ExtractFilePath(ParamStr(0)));
end;

class procedure TPlatform.Run(const Code: string; const CodeName: string);
begin
  _context.eval(Code, CodeName);
end;

class function TPlatform.RunCaptured(const Code: string; const CodeName: string) : string;
begin
  // A run inside a run (a type saved from a script) keeps its own output
  var outer := _capture;
  _capture := TStringBuilder.Create;
  try
    Run(Code, CodeName);
    Result := _capture.ToString;
  finally
    _capture.Free;
    _capture := outer;
  end;
end;

class function TPlatform.Query(const Expression: string) : string;
begin
  Result := _context.eval_with_result('var resultValue = ' + Expression + ';', '<query>').ToString;
end;

class procedure TPlatform.Start;
begin
  _app := TAppObject.Create(App.Environment.impl.Environment.Create(TAppMasterForm, nil));

  RegisterDelphiTypes;
  CreateMainRuntime;

  RegisterValueTypes(_runtime);

  TypeScriptStore := TTypeScripts.Create(TPath.Combine(ScriptFolder, 'types'), RunCaptured, Query);
  TypeScripts := TypeScriptStore;
  // Save state writes here; at startup a type with a state file loads from it
  TypeScriptStore.StateFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'state');
  _context.RegisterLiveInterfaceInstance('types', TypeInfo(ITypeScripts), TypeScripts);

  // tokens['Name'] for type scripts (a provider fetching from an API); a
  // change on the Tokens tab reloads the scripts that use them
  PublishTokens;
  TLocalTokens.OnChanged := procedure begin
    PublishTokens;
    for var name in TypeScriptStore.Reload('tokens') do
      Log('Reloaded (it uses tokens): ' + name);
  end;

  Log('Loading type scripts from ' + TypeScriptStore.Folder);
  TypeScriptStore.LoadAll(procedure(S: string) begin Log(S); end);

  LynxAppsHost.RuntimeFactory := CreateLynxAppsRuntime;end;

end.
