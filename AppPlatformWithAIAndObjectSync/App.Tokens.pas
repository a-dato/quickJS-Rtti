unit App.Tokens;

// Tokens: API keys and other secrets this AppPlatform uses, by name.
//
//   <exe folder>\tokens.json   { "TrelloKey": "...", "TrelloToken": "..." }
//
// Plain text on this machine, next to the exe, edited on the Studio's Tokens
// tab. They are available as tokens['Name']:
//   - to type scripts (globalThis.tokens on the main runtime), e.g. a provider
//     that fetches Trello cards;
//   - to the host scripts of AI-built apps and the AI's host calls, next to
//     the app's own tokens from the builder (which win on equal names).
// Their values are replaced by {{token:Name}} in what goes back to the AI.

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TLocalTokens = class
  private
    class var _values: TDictionary<string, string>;
    class var _loaded: Boolean;
    class procedure EnsureLoaded; static;
    class procedure Store; static;
  public
    // Called after the tokens changed (on the main thread)
    class var OnChanged: TProc;

    class destructor Destroy;

    class function FileName: string; static;
    class function Names: TArray<string>; static;
    class function TryGetValue(const Name: string; out Value: string) : Boolean; static;
    // Adds or replaces a token; an empty value removes it.
    class procedure SetValue(const Name, Value: string); static;
    class procedure Remove(const Name: string); static;
    // Adds or replaces several tokens; stores (and reports a change) only when
    // a value is new or different.
    class procedure Merge(const Values: TDictionary<string, string>); static;
    // A copy, name -> value (the caller frees it)
    class function Values: TDictionary<string, string>; static;
    // { "Name": "value", ... }
    class function ToJson: string; static;
    // Text with every token value replaced by {{token:Name}}
    class function Redact(const Text: string) : string; static;
  end;

implementation

uses
  System.IOUtils,
  System.JSON;

class destructor TLocalTokens.Destroy;
begin
  _values.Free;
end;

class function TLocalTokens.FileName: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'tokens.json');
end;

class procedure TLocalTokens.EnsureLoaded;
begin
  if _loaded then
    Exit;
  _loaded := True;
  _values := TDictionary<string, string>.Create;

  if not TFile.Exists(FileName) then
    Exit;

  var value := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName, TEncoding.UTF8));
  try
    if value is TJSONObject then
      for var pair in TJSONObject(value) do
        if (pair.JsonString.Value.Trim <> '') and (pair.JsonValue is TJSONString) and (pair.JsonValue.Value <> '') then
          _values.AddOrSetValue(pair.JsonString.Value.Trim, pair.JsonValue.Value);
  finally
    value.Free;
  end;
end;

class procedure TLocalTokens.Store;
begin
  TFile.WriteAllText(FileName, ToJson, TEncoding.UTF8);
  if Assigned(OnChanged) then
    OnChanged();
end;

class function TLocalTokens.Names: TArray<string>;
begin
  EnsureLoaded;
  Result := _values.Keys.ToArray;
  TArray.Sort<string>(Result);
end;

class function TLocalTokens.TryGetValue(const Name: string; out Value: string) : Boolean;
begin
  EnsureLoaded;
  Result := _values.TryGetValue(Name, Value);
end;

class procedure TLocalTokens.SetValue(const Name, Value: string);
begin
  EnsureLoaded;
  if Value = '' then
    _values.Remove(Name.Trim) else
    _values.AddOrSetValue(Name.Trim, Value);
  Store;
end;

class procedure TLocalTokens.Remove(const Name: string);
begin
  SetValue(Name, '');
end;

class procedure TLocalTokens.Merge(const Values: TDictionary<string, string>);
begin
  EnsureLoaded;
  var changed := False;
  for var entry in Values do
  begin
    var current: string;
    if (entry.Key.Trim <> '') and (entry.Value <> '') and
       not (_values.TryGetValue(entry.Key.Trim, current) and (current = entry.Value)) then
    begin
      _values.AddOrSetValue(entry.Key.Trim, entry.Value);
      changed := True;
    end;
  end;
  if changed then
    Store;
end;

class function TLocalTokens.Values: TDictionary<string, string>;
begin
  EnsureLoaded;
  Result := TDictionary<string, string>.Create(_values);
end;

class function TLocalTokens.ToJson: string;
begin
  EnsureLoaded;
  var obj := TJSONObject.Create;
  try
    for var name in Names do
      obj.AddPair(name, _values[name]);
    Result := obj.Format(2);
  finally
    obj.Free;
  end;
end;

class function TLocalTokens.Redact(const Text: string) : string;
begin
  EnsureLoaded;
  Result := Text;
  if Result = '' then
    Exit;
  for var entry in _values do
    // Very short values would replace ordinary text
    if entry.Value.Length >= 4 then
      Result := Result.Replace(entry.Value, '{{token:' + entry.Key + '}}', [rfReplaceAll]);
end;

end.
