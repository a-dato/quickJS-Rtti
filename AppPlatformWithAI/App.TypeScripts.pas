unit App.TypeScripts;

// Type scripts: JavaScript types defined at runtime, one script per type.
//
//   js\types\<Name>.js   defines the type <Name>: its class, provider and type
//                        descriptor, handed to registerType() (js\platform.js)
//
// The store loads every script at startup and runs a saved script right away,
// replacing the previous definition. It is a live object in JavaScript, as
// 'types', on the main runtime and on the runtime of Lynx Apps host scripts, so
// the AI app builder can list, read, create, edit and delete types:
//
//   types.List()               -> JSON [{ name, origin, storage, objects, properties: [{ name, type }] }]
//   types.Get(name)            -> the source of the script, '' when there is none
//   types.Save(name, source)   -> JSON { ok, error?, output? }
//   types.Delete(name)         -> JSON { ok, error? }
//   types.Import(name, json, key) -> JSON { ok, added, updated, warnings? }:
//                                 adds objects to a script type's storage (an
//                                 app that fetched them, e.g. with its tokens)
//   types.Objects(name)        -> JSON { ok, storage, objects: [...] }
//   types.SaveState()          -> JSON { ok, output }: writes every storage to
//                                 the state folder (state\<Type>.json)
//
// State: at startup a type with a state file gets its saved objects (IDs
// included) instead of its provider's data; Delphi objects get their saved
// property values back, matched by ID. References are saved through the
// Marshaller of the referenced type ({ "ID", "Type" }).
//
// A script can add properties to a Delphi type (the descriptor's Extends);
// they are added when the script runs and removed when it is redefined.
//
// Results are JSON text and the methods never raise: an exception would not
// reach the calling script (see the QuickJS host-call notes).

interface

uses
  System.SysUtils,
  System.JSON,
  System_,
  System.Generics.Collections,
  App.PropertyDescriptor.intf,
  App.Storage.intf;

type
  // Runs Code on the main runtime and returns what it logged (errors included).
  TScriptRunner = reference to function(const Code, CodeName: string) : string;
  // Evaluates a JavaScript expression on the main runtime, as text.
  TScriptQuery = reference to function(const Expression: string) : string;

  {$M+}
  ITypeScripts = interface(IBaseInterface)
    ['{6E0B7A51-3D4C-4E86-9F1B-2C7A8D5E4F10}']
    function List: string;
    function Get(const Name: string): string;
    function Save(const Name: string; const Source: string): string;
    function Delete(const Name: string): string;
    function Import(const Name: string; const ItemsJson: string; const Key: string): string;
    function Objects(const Name: string): string;
    function SaveState: string;
  end;

  // A property added to a Delphi type, by a script or in the Studio ('')
  TTypeExtension = record
    Script: string;
    Owner: &Type;
    Prop: _PropertyInfo;
    // A list of objects of ItemType (Extends ... List: true)
    IsList: Boolean;
    ItemType: &Type;
    // The label the script gave it
    Caption: string;
  end;

  TTypeScripts = class(TBaseInterfacedObject, ITypeScripts)
  private
    _folder: string;
    _run: TScriptRunner;
    _query: TScriptQuery;
    // globalThis.__typeSchemas (js\platform.js): the declared property types
    _schemas: TJSONObject;
    _version: Integer;
    _loading: Boolean;
    // What the last Define logged (warnings of a script that ran fine)
    _lastOutput: string;
    _extensions: TList<TTypeExtension>;
    _stateFolder: string;

    function  ScriptFile(const Name: string) : string;
    function  FindType(const Name: string; out AType: &Type) : Boolean;
    procedure RemoveType(const Name: string);
    // Runs Source as the definition of Name; '' when Name got defined.
    function  Define(const Name, Source: string) : string;
    function  Dependents(const Name: string) : TArray<string>;
    procedure Changed(const Name: string);
    procedure RefreshSchemas;
    // globalThis.__delphiTypes: the types scripts can refer to but not define
    procedure PublishDelphiTypes;
    // Adds the properties the script of Name declares for Delphi types
    // (Extends); returns warnings.
    function  ApplyExtensions(const Name: string) : string;
    procedure RemoveExtensions(const Script: string);

    function  StateFile(const TypeName: string) : string;
    function  StorageOfType(const AType: &Type) : IStorage;
    function  ItemMarshaller(const ItemType: &Type) : IMarshaller;
    function  MarshalValue(const ItemType: &Type; const Value: CObject) : TJSONValue;
    function  UnmarshalValue(const ItemType: &Type; const Json: TJSONValue) : CObject;
    function  FindExtension(const Owner: &Type; const PropName: string; out Ext: TTypeExtension) : Boolean;
    function  DelphiState(const AType: &Type; const Storage: IStorage) : TJSONObject;
    procedure PublishSavedState;
    procedure RestoreDelphiState(const Log: TProc<string>);
  public
    // Called on the main thread after a type was saved or deleted.
    class var OnChanged: TProc<string>;

    constructor Create(const Folder: string; const Run: TScriptRunner; const Query: TScriptQuery);
    destructor Destroy; override;

    // All scripts, in dependency order (a script whose types are not defined
    // yet is retried after the others). Errors go to Log.
    procedure LoadAll(const Log: TProc<string>);
    function  IsScripted(const Name: string) : Boolean;
    function  ScriptNames: TArray<string>;
    // The type a script declared for a property ('string', 'Address', ...);
    // '' when TypeName is not scripted. JavaScript property types are not
    // visible to Delphi otherwise.
    function  DeclaredType(const TypeName, PropName: string) : string;
    property  Folder: string read _folder;
    // Where SaveState writes and LoadAll reads the saved state
    property  StateFolder: string read _stateFolder write _stateFolder;
    // Removes the saved state: the next start loads from the providers
    function  DiscardState: string;
    // A property added to a Delphi type in the Studio: until the app closes
    function  AddRuntimeProperty(const Owner: &Type; const Name, ALabel: string; const PropType: &Type;
      const Descriptor: IPropertyDescriptor) : _PropertyInfo;
    // Runs the scripts that mention Word again (e.g. 'tokens' after a token
    // changed); returns their names.
    function  Reload(const Word: string) : TArray<string>;
    // True when Owner.PropName is a list a script added (List: true); ItemType
    // is the type of its objects
    function  ExtensionItemType(const Owner: &Type; const PropName: string; out ItemType: &Type) : Boolean;
    // The label of a property a script added ('' when none did)
    function  ExtensionLabel(const Owner: &Type; const PropName: string) : string;
    // The script that added Owner.PropName ('' when none did)
    function  ExtendedBy(const Owner: &Type; const PropName: string) : string;

    // ITypeScripts
    function List: string;
    function Get(const Name: string): string;
    function Save(const Name: string; const Source: string): string;
    function Delete(const Name: string): string;
    function Import(const Name: string; const ItemsJson: string; const Key: string): string;
    function Objects(const Name: string): string;
    function SaveState: string;
  end;

var
  TypeScripts: ITypeScripts;
  TypeScriptStore: TTypeScripts;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.IOUtils,
  System.RegularExpressions,
  System.Collections,
  System.Collections.Generic,
  ADato.Extensions.intf,
  App.intf,
  App.TypeDescriptor.intf,
  App.Tokens;

function JsonResult(const Ok: Boolean; const Error: string = ''; const Output: string = '') : string;
begin
  var obj := TJSONObject.Create;
  try
    obj.AddPair('ok', TJSONBool.Create(Ok));
    if Error <> '' then
      obj.AddPair('error', TLocalTokens.Redact(Error));
    if Output.Trim <> '' then
      obj.AddPair('output', TLocalTokens.Redact(Output.Trim));
    Result := obj.ToJSON;
  finally
    obj.Free;
  end;
end;

{ TTypeScripts }

constructor TTypeScripts.Create(const Folder: string; const Run: TScriptRunner; const Query: TScriptQuery);
begin
  inherited Create;
  _folder := Folder;
  _run := Run;
  _query := Query;
  _schemas := TJSONObject.Create;
  _extensions := TList<TTypeExtension>.Create;
  ForceDirectories(_folder);
end;

destructor TTypeScripts.Destroy;
begin
  _extensions.Free;
  _schemas.Free;
  inherited;
end;

procedure TTypeScripts.RefreshSchemas;
begin
  var value: TJSONValue := nil;
  try
    value := TJSONObject.ParseJSONValue(_query('JSON.stringify(globalThis.__typeSchemas ?? {})'));
  except
  end;

  if value is TJSONObject then
  begin
    _schemas.Free;
    _schemas := value as TJSONObject;
  end
  else
    value.Free;
end;

function TTypeScripts.DeclaredType(const TypeName, PropName: string) : string;
begin
  Result := '';
  var schema := _schemas.GetValue(TypeName) as TJSONObject;
  if schema = nil then
    Exit;
  var props := schema.GetValue('properties') as TJSONObject;
  if props = nil then
    Exit;
  var prop := props.GetValue(PropName) as TJSONObject;
  if prop <> nil then
    prop.TryGetValue<string>('type', Result);
end;

function TTypeScripts.ScriptFile(const Name: string) : string;
begin
  Result := TPath.Combine(_folder, Name + '.js');
end;

function TTypeScripts.ScriptNames: TArray<string>;
begin
  Result := [];
  for var f in TDirectory.GetFiles(_folder, '*.js') do
    Result := Result + [TPath.GetFileNameWithoutExtension(f)];
  TArray.Sort<string>(Result);
end;

function TTypeScripts.IsScripted(const Name: string) : Boolean;
begin
  Result := IsValidIdent(Name) and TFile.Exists(ScriptFile(Name));
end;

function TTypeScripts.FindType(const Name: string; out AType: &Type) : Boolean;
begin
  for var t in _app.Config.Types do
    if SameText(t.Name, Name) then
    begin
      AType := t;
      Exit(True);
    end;
  Result := False;
end;

// Unregisters the type and drops its storage, so the script can define it again.
procedure TTypeScripts.RemoveType(const Name: string);
begin
  RemoveExtensions(Name);

  // The JavaScript side (js\platform.js) forgets the class and its schema
  _run('globalThis.__forgetType?.(''' + Name + ''');', '<forget ' + Name + '>');

  var t: &Type;
  if not FindType(Name, t) then
    Exit;

  var descriptor := _app.Config.TypeDescriptor(t);
  if descriptor <> nil then
  try
    var storageName: string := descriptor.StorageName;
    if storageName <> '' then
      _app.RemoveStorage(storageName);
  except
    // descriptor without storage
  end;

  _app.Config.UnregisterType(t);
end;

function TTypeScripts.Define(const Name, Source: string) : string;
begin
  RemoveType(Name);
  PublishDelphiTypes;

  // A new module name for every run: QuickJS keeps evaluated modules. The
  // folder part keeps './' and '../' imports relative to js\types.
  inc(_version);
  var codeName := ScriptFile(Name).Replace('\', '/') + '?v=' + _version.ToString;
  var output: string;
  try
    output := _run(Source, codeName);
  except
    // A Delphi exception from a host call (e.g. app.Storage of a storage that
    // does not exist) does not become a JavaScript error: it ends the run here.
    // It is this script's error, not a reason to stop loading the others.
    on E: Exception do
      output := E.ClassName + ': ' + E.Message;
  end;
  if not _loading then
    RefreshSchemas;

  // QuickJS reports an uncaught error twice (as exception and as result)
  _lastOutput := '';
  for var block in output.Replace(#13#10, #10).Split([#10#10], TStringSplitOptions.ExcludeEmpty) do
    if not _lastOutput.Contains(block.Trim) then
      _lastOutput := _lastOutput + block.Trim + sLineBreak + sLineBreak;
  _lastOutput := _lastOutput.Trim;

  var t: &Type;
  if FindType(Name, t) then
  begin
    var warnings := ApplyExtensions(Name);
    if warnings <> '' then
      _lastOutput := (_lastOutput + sLineBreak + warnings).Trim;
    Exit('');
  end;

  Result := _lastOutput;

  if Result = '' then
    Result := Format('The script ran, but did not define a type named %s. End it with registerType(%s, new %sType()).', [Name, Name, Name]);
end;

// The scripts that use Name as a property type: they hold the old class and
// are run again after Name changed.
function TTypeScripts.Dependents(const Name: string) : TArray<string>;
begin
  Result := [];
  // { type: 'Name' } (defineType) or { Type: 'Name' } (a property descriptor)
  var pattern := '\b[Tt]ype\s*:\s*[''"]' + Name + '[''"]';
  for var other in ScriptNames do
    if not SameText(other, Name) and TRegEx.IsMatch(TFile.ReadAllText(ScriptFile(other), TEncoding.UTF8), pattern) then
      Result := Result + [other];
end;

procedure TTypeScripts.Changed(const Name: string);
begin
  if not _loading and Assigned(OnChanged) then
    OnChanged(Name);
end;

procedure TTypeScripts.LoadAll(const Log: TProc<string>);
begin
  _loading := True;
  try
    PublishSavedState;
    var pending := TList<string>.Create;
    var errors := TDictionary<string, string>.Create;
    try
      pending.AddRange(ScriptNames);

      // Retry until a pass defines nothing new: a script may use a type of a
      // script that comes later in the alphabet.
      var progress := True;
      while progress and (pending.Count > 0) do
      begin
        progress := False;
        for var name in pending.ToArray do
        begin
          var error := Define(name, TFile.ReadAllText(ScriptFile(name), TEncoding.UTF8));
          if error = '' then
          begin
            // Loaded, but with something to say (a provider that failed, a
            // reference that matches nothing)
            if _lastOutput <> '' then
              Log(Format('Type script %s.js: %s', [name, TLocalTokens.Redact(_lastOutput)]));
            pending.Remove(name);
            errors.Remove(name);
            progress := True;
          end
          else
            errors.AddOrSetValue(name, error);
        end;
      end;

      for var name in pending do
        Log(Format('Type script %s.js failed: %s', [name, errors[name]]));
      RefreshSchemas;
      // Saved state no script used (its script failed) is not kept around
      _run('globalThis.__savedState = {};', '<saved state>');
      RestoreDelphiState(Log);
    finally
      errors.Free;
      pending.Free;
    end;
  finally
    _loading := False;
  end;
end;

{ State: the objects and property values, saved to the state folder }

function TTypeScripts.StateFile(const TypeName: string) : string;
begin
  Result := TPath.Combine(_stateFolder, TypeName + '.json');
end;

function TTypeScripts.ItemMarshaller(const ItemType: &Type) : IMarshaller;
begin
  Result := nil;
  try
    var typeDescriptor := _app.Config.TypeDescriptor(ItemType);
    if typeDescriptor <> nil then
    begin
      var descriptor := typeDescriptor.PropertyDescriptor[ItemType.Name];
      if descriptor <> nil then
        Result := descriptor.Marshaller;
    end;
  except
    Result := nil;
  end;
end;

// A value as JSON: an object of a script type through its type's Marshaller
// ({ "ID", "Type" }), plain values as they are; nil when it cannot be saved.
function TTypeScripts.MarshalValue(const ItemType: &Type; const Value: CObject) : TJSONValue;
begin
  if Value.IsNull then
    Exit(TJSONNull.Create);

  if IsScripted(ItemType.Name) then
  begin
    var marshaller := ItemMarshaller(ItemType);
    if marshaller = nil then
      Exit(nil);
    var text := marshaller.Marshal(Default(CObject), Value).ToString;
    Result := TJSONObject.ParseJSONValue(text);
    if Result = nil then
      Result := TJSONString.Create(text);
    Exit;
  end;

  if Value.IsBoolean then
    Result := TJSONBool.Create(Value.AsType<Boolean>)
  else if Value.IsNumber then
    Result := TJSONNumber.Create(Value.AsType<Double>)
  else if Value.IsString then
    Result := TJSONString.Create(Value.ToString)
  else
    Result := nil;
end;

function TTypeScripts.UnmarshalValue(const ItemType: &Type; const Json: TJSONValue) : CObject;
begin
  Result := Default(CObject);
  if (Json = nil) or (Json is TJSONNull) then
    Exit;

  if IsScripted(ItemType.Name) then
  begin
    var marshaller := ItemMarshaller(ItemType);
    if marshaller <> nil then
      Result := marshaller.Unmarshal(Default(CObject), CObject.From<string>(Json.ToJSON));
    Exit;
  end;

  if Json is TJSONBool then
    Result := CObject.From<Boolean>(TJSONBool(Json).AsBoolean)
  else if Json is TJSONNumber then
  begin
    if ItemType = &Type.From<Integer> then
      Result := CObject.From<Integer>(TJSONNumber(Json).AsInt) else
      Result := CObject.From<Double>(TJSONNumber(Json).AsDouble);
  end
  else
    Result := CObject.From<string>(Json.Value);
end;

// The ID of a Delphi object as text, through its type's properties
function ObjectId(const Props: List<_PropertyInfo>; const Item: CObject) : string;
begin
  Result := '';
  for var prop in Props do
    if SameText(prop.Name.ToString, 'ID') then
      Exit(prop.GetValue(Item, []).ToString);
end;

function TTypeScripts.FindExtension(const Owner: &Type; const PropName: string; out Ext: TTypeExtension) : Boolean;
begin
  for var e in _extensions do
    if (e.Owner = Owner) and (e.Script <> '') and SameText(e.Prop.Name.ToString, PropName) then
    begin
      Ext := e;
      Exit(True);
    end;
  Result := False;
end;

// { "type", "storage", "objects": [{ "ID", <property>: value }] } of a Delphi
// type: its writable plain properties and the properties scripts added
function TTypeScripts.DelphiState(const AType: &Type; const Storage: IStorage) : TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('type', AType.Name);
  Result.AddPair('storage', Storage.Name);
  var objects := TJSONArray.Create;
  Result.AddPair('objects', objects);

  var props := _app.Config.GetProperties(AType);
  for var i := 0 to Storage.Count - 1 do
  begin
    var item := Storage[i];
    var obj := TJSONObject.Create;
    objects.AddElement(obj);
    obj.AddPair('ID', ObjectId(props, item));

    for var prop in props do
    begin
      var name := prop.Name.ToString;
      if SameText(name, 'ID') then
        Continue;
      try
        var ext: TTypeExtension;
        var value := prop.GetValue(item, []);
        var json: TJSONValue := nil;
        if FindExtension(AType, name, ext) then
        begin
          if ext.IsList then
          begin
            var arr := TJSONArray.Create;
            var list: IList;
            if not value.IsNull and value.TryGetValue<IList>(list) and (list <> nil) then
              for var j := 0 to list.Count - 1 do
              begin
                var element := MarshalValue(ext.ItemType, list[j]);
                if element <> nil then
                  arr.AddElement(element);
              end;
            json := arr;
          end
          else
            json := MarshalValue(ext.ItemType, value);
        end
        else if prop.CanWrite and not value.IsNull and (value.IsString or value.IsNumber or value.IsBoolean) then
          json := MarshalValue(prop.GetType, value);

        if json <> nil then
          obj.AddPair(name, json);
      except
        // a property that cannot be read is not saved
      end;
    end;
  end;
end;

function TTypeScripts.SaveState: string;
begin
  try
    ForceDirectories(_stateFolder);
    var saved: TArray<string> := [];
    for var t in _app.Config.Types do
    begin
      var storage := StorageOfType(t);
      if storage = nil then
        Continue;

      var text: string;
      if IsScripted(t.Name) then
        text := _query('__exportState(' + TJSONString.Create(t.Name).ToJSON + ')')
      else
      begin
        var state := DelphiState(t, storage);
        try
          text := state.Format(2);
        finally
          state.Free;
        end;
      end;

      if text <> '' then
      begin
        TFile.WriteAllText(StateFile(t.Name), text, TEncoding.UTF8);
        saved := saved + [Format('%s (%d)', [t.Name, storage.Count])];
      end;
    end;
    Result := JsonResult(True, '', 'Saved ' + string.Join(', ', saved) + ' to ' + _stateFolder);
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

function TTypeScripts.DiscardState: string;
begin
  try
    var count := 0;
    if TDirectory.Exists(_stateFolder) then
      for var f in TDirectory.GetFiles(_stateFolder, '*.json') do
      begin
        TFile.Delete(f);
        inc(count);
      end;
    Result := JsonResult(True, '', Format('Removed %d saved state files; the types load from their providers at the next start.', [count]));
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

function TTypeScripts.StorageOfType(const AType: &Type) : IStorage;
begin
  Result := nil;
  var descriptor := _app.Config.TypeDescriptor(AType);
  if descriptor = nil then
    Exit;
  try
    var storageName: string := descriptor.StorageName;
    if (storageName = '') or not _app.TryGetStorage(storageName, Result) then
      Result := nil;
  except
    Result := nil;
  end;
end;

// Before the scripts load: globalThis.__savedState = { <type>: state } for the
// scripted types with a state file; registerType uses it instead of the
// provider's data.
procedure TTypeScripts.PublishSavedState;
begin
  var root := TJSONObject.Create;
  try
    if TDirectory.Exists(_stateFolder) then
      for var name in ScriptNames do
        if TFile.Exists(StateFile(name)) then
        begin
          var value := TJSONObject.ParseJSONValue(TFile.ReadAllText(StateFile(name), TEncoding.UTF8));
          if value is TJSONObject then
            root.AddPair(name, value)
          else
            value.Free;
        end;
    _run('globalThis.__savedState = ' + root.ToJSON + ';', '<saved state>');
  finally
    root.Free;
  end;
end;

// After the scripts loaded: the saved values of the Delphi objects, matched by ID
procedure TTypeScripts.RestoreDelphiState(const Log: TProc<string>);
begin
  for var t in _app.Config.Types do
  begin
    if IsScripted(t.Name) or not TFile.Exists(StateFile(t.Name)) then
      Continue;
    var storage := StorageOfType(t);
    if storage = nil then
      Continue;

    var state := TJSONObject.ParseJSONValue(TFile.ReadAllText(StateFile(t.Name), TEncoding.UTF8));
    try
      var objects: TJSONArray := nil;
      if state is TJSONObject then
        objects := TJSONObject(state).GetValue('objects') as TJSONArray;
      if objects = nil then
        Continue;

      var props := _app.Config.GetProperties(t);
      var restored := 0;
      for var savedValue in objects do
      begin
        if not (savedValue is TJSONObject) then
          Continue;
        var saved := TJSONObject(savedValue);
        var id := saved.GetValue<string>('ID', '');

        var item: CObject := Default(CObject);
        for var i := 0 to storage.Count - 1 do
          if ObjectId(props, storage[i]) = id then
          begin
            item := storage[i];
            Break;
          end;
        if item.IsNull then
          Continue;

        for var pair in saved do
        begin
          var name := pair.JsonString.Value;
          // Empty is where a property starts (and a descriptor-wrapped one
          // cannot be set to nil)
          if SameText(name, 'ID') or (pair.JsonValue is TJSONNull) then
            Continue;
          try
            var ext: TTypeExtension;
            for var prop in props do
              if SameText(prop.Name.ToString, name) then
              begin
                if FindExtension(t, name, ext) then
                begin
                  if ext.IsList then
                  begin
                    var list: IList := CList<CObject>.Create;
                    if pair.JsonValue is TJSONArray then
                      for var element in TJSONArray(pair.JsonValue) do
                      begin
                        var v := UnmarshalValue(ext.ItemType, element);
                        if not v.IsNull then
                          list.Add(v);
                      end;
                    prop.SetValue(item, CObject.From<IList>(list), []);
                  end
                  else
                    prop.SetValue(item, UnmarshalValue(ext.ItemType, pair.JsonValue), []);
                end
                else if prop.CanWrite then
                  prop.SetValue(item, UnmarshalValue(prop.GetType, pair.JsonValue), []);
                Break;
              end;
          except
            on E: Exception do
              Log(Format('State of %s %s: %s not restored: %s', [t.Name, id, name, E.Message]));
          end;
        end;
        inc(restored);
      end;
      Log(Format('Restored the saved state of %d %s objects', [restored, t.Name]));
    finally
      state.Free;
    end;
  end;
end;

procedure TTypeScripts.PublishDelphiTypes;
begin
  var obj := TJSONObject.Create;
  try
    for var t in _app.Config.Types do
    begin
      if IsScripted(t.Name) then
        Continue;
      var info := TJSONObject.Create;
      obj.AddPair(t.Name, info);
      var descriptor := _app.Config.TypeDescriptor(t);
      if descriptor <> nil then
      try
        var storageName: string := descriptor.StorageName;
        if storageName <> '' then
          info.AddPair('storage', storageName);
      except
      end;
    end;
    _run('globalThis.__delphiTypes = ' + obj.ToJSON + ';', '<delphi types>');
  finally
    obj.Free;
  end;
end;

function TTypeScripts.ApplyExtensions(const Name: string) : string;
begin
  Result := '';
  var value := TJSONObject.ParseJSONValue(_query('JSON.stringify(globalThis.__typeSchemas?.[' +
    TJSONString.Create(Name).ToJSON + ']?.extends ?? [])'));
  try
    if not (value is TJSONArray) then
      Exit;

    for var item in TJSONArray(value) do
    begin
      var ownerName := item.GetValue<string>('owner', '');
      var propName := item.GetValue<string>('name', '');
      var typeName := item.GetValue<string>('type', '');
      var propLabel := item.GetValue<string>('label', propName);
      var isList := item.GetValue<Boolean>('list', False);

      var owner: &Type;
      if not FindType(ownerName, owner) or IsScripted(ownerName) then
      begin
        Result := Result + Format('Warning: Extends.%s: %s is not a Delphi type; %s is not added.', [ownerName, ownerName, propName]) + sLineBreak;
        Continue;
      end;

      var exists := False;
      for var prop in _app.Config.GetProperties(owner) do
        if SameText(prop.Name.ToString, propName) then
          exists := True;
      if exists then
      begin
        var by := ExtendedBy(owner, propName);
        Result := Result + Format('Warning: %s already has a property %s (%s); not added again.',
          [ownerName, propName, IfThen(by <> '', 'added by ' + by + '.js', 'in Delphi')]) + sLineBreak;
        Continue;
      end;

      var propType: &Type;
      var descriptor: IPropertyDescriptor := nil;
      if (typeName = 'string') or (typeName = 'date') then
        propType := &Type.From<string>
      else if typeName = 'number' then
        propType := &Type.From<Double>
      else if typeName = 'integer' then
        propType := &Type.From<Integer>
      else if typeName = 'boolean' then
        propType := &Type.From<Boolean>
      else if FindType(typeName, propType) then
      begin
        // Editor, text and choices of the property come from its type
        var typeDescriptor := _app.Config.TypeDescriptor(propType);
        if typeDescriptor <> nil then
          descriptor := typeDescriptor.PropertyDescriptor[typeName];
      end
      else
      begin
        Result := Result + Format('Warning: Extends.%s.%s: there is no type %s; not added.', [ownerName, propName, typeName]) + sLineBreak;
        Continue;
      end;

      var ext: TTypeExtension;
      ext.Script := Name;
      ext.Caption := propLabel;
      ext.Owner := owner;
      ext.IsList := isList;
      // The type of the value (of each object, for a list): for saving state
      ext.ItemType := propType;
      if isList then
      begin
        // The value is an IList of the chosen objects; the item type's
        // descriptor is for single values, so the list goes without
        ext.ItemType := propType;
        propType := &Type.From<IList>;
        descriptor := nil;
      end;
      ext.Prop := _app.Config.AddProperty(owner, propName, propLabel, propType, descriptor);
      if ext.Prop <> nil then
        _extensions.Add(ext);
    end;
  finally
    value.Free;
  end;
  Result := Result.Trim;
end;

procedure TTypeScripts.RemoveExtensions(const Script: string);
begin
  // (&Type has an = operator; TList.Contains would compare the records' bytes)
  var owners: TArray<&Type> := [];
  for var i := _extensions.Count - 1 downto 0 do
    if SameText(_extensions[i].Script, Script) then
    begin
      var known := False;
      for var owner in owners do
        if owner = _extensions[i].Owner then
          known := True;
      if not known then
        owners := owners + [_extensions[i].Owner];
      _extensions.Delete(i);
    end;

  // The extension manager has no remove: define what is left
  for var owner in owners do
  begin
    var props: PropertyInfoArray := [];
    for var ext in _extensions do
      if ext.Owner = owner then
        props := props + [ext.Prop];
    if ExtensionManager <> nil then
      ExtensionManager.DefineProperties(owner, props);
  end;
end;

function TTypeScripts.AddRuntimeProperty(const Owner: &Type; const Name, ALabel: string; const PropType: &Type;
  const Descriptor: IPropertyDescriptor) : _PropertyInfo;
begin
  Result := _app.Config.AddProperty(Owner, Name, ALabel, PropType, Descriptor);
  if Result <> nil then
  begin
    var ext: TTypeExtension;
    ext.Script := '';
    ext.Owner := Owner;
    ext.Prop := Result;
    _extensions.Add(ext);
  end;
end;

function TTypeScripts.Reload(const Word: string) : TArray<string>;
begin
  Result := [];
  for var name in ScriptNames do
  begin
    var source := Get(name);
    if not TRegEx.IsMatch(source, '\b' + Word + '\b') then
      Continue;
    var error := Define(name, source);
    if error <> '' then
      Result := Result + [name + ' (failed: ' + TLocalTokens.Redact(error) + ')']
    else if _lastOutput <> '' then
      Result := Result + [name + ': ' + TLocalTokens.Redact(_lastOutput)]
    else
      Result := Result + [name];
    Changed(name);
  end;
end;

function TTypeScripts.ExtensionItemType(const Owner: &Type; const PropName: string; out ItemType: &Type) : Boolean;
begin
  for var ext in _extensions do
    if ext.IsList and (ext.Owner = Owner) and SameText(ext.Prop.Name.ToString, PropName) then
    begin
      ItemType := ext.ItemType;
      Exit(True);
    end;
  Result := False;
end;

function TTypeScripts.ExtensionLabel(const Owner: &Type; const PropName: string) : string;
begin
  var ext: TTypeExtension;
  if FindExtension(Owner, PropName, ext) then
    Result := ext.Caption else
    Result := '';
end;

function TTypeScripts.ExtendedBy(const Owner: &Type; const PropName: string) : string;
begin
  Result := '';
  for var ext in _extensions do
    if (ext.Owner = Owner) and (ext.Script <> '') and SameText(ext.Prop.Name.ToString, PropName) then
      Exit(ext.Script);
end;

function TTypeScripts.Import(const Name: string; const ItemsJson: string; const Key: string): string;
begin
  try
    if not IsScripted(Name) then
      Exit(JsonResult(False, 'There is no script type ' + Name + '; Import works for script types only.'));

    var items := TJSONObject.ParseJSONValue(ItemsJson);
    try
      if not (items is TJSONArray) then
        Exit(JsonResult(False, 'items must be a JSON array of objects with property values, e.g. [{"Name":"x"}].'));

      Result := TLocalTokens.Redact(_query(Format('__importObjects(%s, %s, %s)',
        [TJSONString.Create(Name).ToJSON, items.ToJSON, TJSONString.Create(Key).ToJSON])));
    finally
      items.Free;
    end;
    Changed(Name);
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

function TTypeScripts.Objects(const Name: string): string;
begin
  try
    if not IsScripted(Name) then
      Exit(JsonResult(False, 'There is no script type ' + Name + '.'));
    Result := TLocalTokens.Redact(_query(Format('__exportObjects(%s)', [TJSONString.Create(Name).ToJSON])));
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

function TTypeScripts.List: string;
begin
  try
    var arr := TJSONArray.Create;
    try
      for var t in _app.Config.Types do
      begin
        var obj := TJSONObject.Create;
        arr.AddElement(obj);
        obj.AddPair('name', t.Name);
        obj.AddPair('origin', IfThen(IsScripted(t.Name), 'script', 'delphi'));

        var descriptor := _app.Config.TypeDescriptor(t);
        var storage: IStorage := nil;
        if descriptor <> nil then
        try
          var storageName: string := descriptor.StorageName;
          if (storageName <> '') and _app.TryGetStorage(storageName, storage) then
          begin
            obj.AddPair('storage', storageName);
            obj.AddPair('objects', TJSONNumber.Create(storage.Count));
          end;
        except
        end;

        var props := TJSONArray.Create;
        obj.AddPair('properties', props);
        for var prop in _app.Config.GetProperties(t) do
        begin
          var p := TJSONObject.Create;
          props.AddElement(p);
          p.AddPair('name', prop.Name.ToString);
          var addedBy := ExtendedBy(t, prop.Name.ToString);
          if addedBy <> '' then
            p.AddPair('addedBy', addedBy + '.js');
          var declared := DeclaredType(t.Name, prop.Name.ToString);
          if declared <> '' then
            p.AddPair('type', declared)
          else
          try
            p.AddPair('type', prop.GetType.Name);
          except
            p.AddPair('type', '?');
          end;
        end;
      end;
      Result := TLocalTokens.Redact(arr.ToJSON);
    finally
      arr.Free;
    end;
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

function TTypeScripts.Get(const Name: string): string;
begin
  try
    if IsScripted(Name) then
      Result := TFile.ReadAllText(ScriptFile(Name), TEncoding.UTF8) else
      Result := '';
  except
    Result := '';
  end;
end;

function TTypeScripts.Save(const Name: string; const Source: string): string;
begin
  try
    if not IsValidIdent(Name) then
      Exit(JsonResult(False, 'The type name must be an identifier, e.g. Invoice.'));

    var existing: &Type;
    if FindType(Name, existing) and not IsScripted(Name) then
      Exit(JsonResult(False, Name + ' is defined in Delphi and cannot be replaced by a script.'));

    var previous := Get(Name);

    var error := Define(Name, Source);
    if error <> '' then
    begin
      // Put the previous definition back
      if previous <> '' then
        Define(Name, previous);
      Exit(JsonResult(False, error));
    end;

    // UTF-8 without BOM, like the scripts it sits next to
    TFile.WriteAllBytes(ScriptFile(Name), TEncoding.UTF8.GetBytes(Source));

    // Warnings of the script itself (e.g. a reference in its data that
    // matches nothing), then of the types that use it
    var output := _lastOutput;
    if output <> '' then
      output := output + sLineBreak;
    for var dependent in Dependents(Name) do
    begin
      var dependentError := Define(dependent, Get(dependent));
      if dependentError <> '' then
        output := output + Format('%s.js (uses %s) failed to reload: %s', [dependent, Name, dependentError]) + sLineBreak
      else if _lastOutput <> '' then
        output := output + Format('%s.js (uses %s) reloaded: %s', [dependent, Name, _lastOutput]) + sLineBreak;
    end;

    Changed(Name);
    Result := JsonResult(True, '', output);
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

function TTypeScripts.Delete(const Name: string): string;
begin
  try
    if not IsScripted(Name) then
      Exit(JsonResult(False, 'There is no type script named ' + Name + '.'));

    var users := Dependents(Name);
    if Length(users) > 0 then
      Exit(JsonResult(False, Format('%s is used by %s; change those types first.', [Name, string.Join(', ', users)])));

    RemoveType(Name);
    TFile.Delete(ScriptFile(Name));
    RefreshSchemas;
    Changed(Name);
    Result := JsonResult(True);
  except
    on E: Exception do
      Result := JsonResult(False, E.Message);
  end;
end;

end.
