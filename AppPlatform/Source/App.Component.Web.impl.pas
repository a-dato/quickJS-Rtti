unit App.Component.Web.impl;

interface

uses
  System.Classes,
  System_,
  System.JSON,
  App.Component.intf,
  App.Component.impl,
  ADato.ObjectModel.Binders,
  System.Collections.Generic;

type
  // Executes a piece of JavaScript inside the web page hosting the element
  TWebScriptProc = reference to procedure(const Script: string);

  // Reflects a single form element (input, select, textarea, button) inside a web page
  {$M+}
  TWebComponent = class(App.Component.impl.TComponent, IControlWithBindingSupport)
  protected
    _parent: IComponent;
    _executeScript: TWebScriptProc;
    _key: string;
    _name: string;
    _elementId: string;
    _tagName: string;
    _inputType: string;
    _value: string;
    _lastChangeSequence: Integer;
    _onChange: TNotifyEvent;

    function  get_Parent: IComponent; override;
    function  get_Children: List<IComponent>; override;
    function  get_Name: CString; override;

    // IControlWithBindingSupport
    function  get_OnChange: TNotifyEvent;
    procedure set_OnChange(const Value: TNotifyEvent);
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);

    procedure Update(const Element: TJSONObject);
    procedure DoChange;

  public
    constructor Create(const Parent: IComponent; const Element: TJSONObject; const ExecuteScript: TWebScriptProc);

    // Called when the value was changed inside the web page. Sequence orders the notifications,
    // older notifications that arrive late are ignored.
    procedure ValueChangedInPage(const NewValue: string; Sequence: Integer);

    // Script that returns a JSON array describing all form elements in the page
    class function CollectElementsScript: string; static;
    // Script that reports every value change inside the page by calling
    // Scheme:MethodName(Key, Value, Sequence), Key and Value are url encoded twice
    class function ChangeListenerScript(const Scheme, MethodName: string) : string; static;
    // Returns a TWebComponent for each element returned by CollectElementsScript,
    // components already in Existing are updated and reused
    class function UpdateChildren(const Parent: IComponent; const Existing: List<IComponent>; const ElementsJson: string; const ExecuteScript: TWebScriptProc) : List<IComponent>; static;

    // Identifies the element inside the page: its id, or its name when it has no id
    property Key: string read _key;
    property ElementId: string read _elementId;
    property TagName: string read _tagName;
    property InputType: string read _inputType;
    // Value as last read from or reported by the page, or as last set through this property.
    // Setting it updates the page but does not fire OnChange.
    property Value: CObject read get_Value write set_Value;
    // Fires when the value is changed inside the web page
    property OnChange: TNotifyEvent read get_OnChange write set_OnChange;
  end;

implementation

uses
  System.SysUtils;

function JavaScriptString(const Value: string): string;
begin
  var s := TJSONString.Create(Value);
  try
    Result := s.ToJSON;
  finally
    s.Free;
  end;
end;

{ TWebComponent }

constructor TWebComponent.Create(const Parent: IComponent; const Element: TJSONObject; const ExecuteScript: TWebScriptProc);
begin
  inherited Create(Parent);
  _parent := Parent;
  _executeScript := ExecuteScript;
  Update(Element);
end;

procedure TWebComponent.Update(const Element: TJSONObject);
begin
  _elementId := Element.GetValue<string>('id', '');
  _name := Element.GetValue<string>('name', '');
  if _name = '' then
    _name := _elementId;
  if _elementId <> '' then
    _key := _elementId else
    _key := _name;
  _tagName := Element.GetValue<string>('tag', '');
  _inputType := Element.GetValue<string>('type', '');

  var newValue := Element.GetValue<string>('value', '');
  if newValue <> _value then
  begin
    _value := newValue;
    DoChange;
  end;
end;

class function TWebComponent.CollectElementsScript: string;
begin
  Result :=
    '(function() {' +
    '  var result = [];' +
    '  document.querySelectorAll("input, select, textarea, button").forEach(function(e) {' +
    '    result.push({ tag: e.tagName.toLowerCase(), type: e.type || "", id: e.id || "", name: e.name || "", value: e.value || "" });' +
    '  });' +
    '  return JSON.stringify(result);' +
    '})()';
end;

class function TWebComponent.ChangeListenerScript(const Scheme, MethodName: string) : string;
begin
  // ScriptGate url-decodes the call once and then splits it on ( , and ), the inner
  // encoding also escapes the characters encodeURIComponent leaves alone: ! ' ( ) *
  Result :=
    '(function() {' +
    '  if (window.__appFrameListening) return "ok";' +
    '  window.__appFrameListening = true;' +
    '  window.__appFrameSequence = 0;' +
    '  function encode(s) {' +
    '    var inner = encodeURIComponent(s).replace(/[!''()*]/g, function(c) { return "%" + c.charCodeAt(0).toString(16).toUpperCase(); });' +
    '    return encodeURIComponent(inner);' +
    '  }' +
    '  function notify(e) {' +
    '    var t = e.target;' +
    '    if (window.__appFrameSetting || !t || !("value" in t)) return;' +
    '    var key = t.id || t.name;' +
    '    if (!key) return;' +
    '    window.__appFrameSequence++;' +
    '    window.location.href = ' + JavaScriptString(Scheme + ':' + MethodName + '(') +
    '      + encode(key) + "," + encode(t.value) + "," + window.__appFrameSequence + ")";' +
    '  }' +
    '  document.addEventListener("input", notify, true);' +
    '  document.addEventListener("change", notify, true);' +
    '  return "ok";' +
    '})()';
end;

class function TWebComponent.UpdateChildren(const Parent: IComponent; const Existing: List<IComponent>; const ElementsJson: string; const ExecuteScript: TWebScriptProc) : List<IComponent>;
begin
  Result := CList<IComponent>.Create;

  var json := TJSONValue.ParseJSONValue(ElementsJson);
  try
    if not (json is TJSONArray) then
      Exit;

    for var item in TJSONArray(json) do
    begin
      if not (item is TJSONObject) then
        continue;

      var element := TJSONObject(item);
      var key := element.GetValue<string>('id', '');
      if key = '' then
        key := element.GetValue<string>('name', '');

      // Reuse the existing component so OnChange handlers stay attached
      var component: IComponent := nil;
      if (key <> '') and (Existing <> nil) then
        for var c in Existing do
          if (c.GetObject is TWebComponent) and (TWebComponent(c.GetObject).Key = key) then
          begin
            component := c;
            break;
          end;

      if component <> nil then
        TWebComponent(component.GetObject).Update(element) else
        component := TWebComponent.Create(Parent, element, ExecuteScript);

      Result.Add(component);
    end;
  finally
    json.Free;
  end;
end;

procedure TWebComponent.ValueChangedInPage(const NewValue: string; Sequence: Integer);
begin
  if Sequence <= _lastChangeSequence then
    Exit;
  _lastChangeSequence := Sequence;

  if NewValue <> _value then
  begin
    _value := NewValue;
    DoChange;
  end;
end;

procedure TWebComponent.DoChange;
begin
  if Assigned(_onChange) then
    _onChange(Self);
end;

function TWebComponent.get_Parent: IComponent;
begin
  Result := _parent;
end;

function TWebComponent.get_Children: List<IComponent>;
begin
  // Form elements have no child elements
  Result := CList<IComponent>.Create;
end;

function TWebComponent.get_Name: CString;
begin
  Result := _name;
end;

function TWebComponent.get_OnChange: TNotifyEvent;
begin
  Result := _onChange;
end;

procedure TWebComponent.set_OnChange(const Value: TNotifyEvent);
begin
  _onChange := Value;
end;

function TWebComponent.get_Value: CObject;
begin
  Result := _value;
end;

procedure TWebComponent.set_Value(const Value: CObject);
begin
  if Value = nil then
    _value := '' else
    _value := Value.ToString;

  // __appFrameSetting keeps the change listener from reporting this change back
  if Assigned(_executeScript) then
    _executeScript(
      '(function() {' +
      '  var e = document.getElementById(' + JavaScriptString(_elementId) + ') || document.getElementsByName(' + JavaScriptString(_name) + ')[0];' +
      '  if (!e) return;' +
      '  window.__appFrameSetting = true;' +
      '  try {' +
      '    e.value = ' + JavaScriptString(_value) + ';' +
      '    e.dispatchEvent(new Event("input", { bubbles: true }));' +
      '  } finally {' +
      '    window.__appFrameSetting = false;' +
      '  }' +
      '})()');
end;

end.
