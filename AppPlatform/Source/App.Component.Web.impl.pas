unit App.Component.Web.impl;

interface

uses
  System.Classes,
  System.UITypes,
  FMX.Types,
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
    _onMouseDown: TMouseEvent;

    function  get_Parent: IComponent; override;
    function  get_Children: List<IComponent>; override;
    function  get_Name: CString; override;

    // IControlWithBindingSupport
    function  get_OnChange: TNotifyEvent;
    procedure set_OnChange(const Value: TNotifyEvent);
    function  get_OnMouseDown: TMouseEvent;
    procedure set_OnMouseDown(const Value: TMouseEvent);
    function  get_Value: CObject; virtual;
    procedure set_Value(const Value: CObject); virtual;

    procedure Update(const Element: TJSONObject);
    procedure DoChange;

  public
    constructor Create(const Parent: IComponent; const Element: TJSONObject; const ExecuteScript: TWebScriptProc);

    // Called when the value was changed inside the web page. Sequence orders the notifications,
    // older notifications that arrive late are ignored.
    procedure ValueChangedInPage(const NewValue: string; Sequence: Integer);
    // Called when the element was clicked inside the web page. Button and Shift use the
    // JavaScript encoding: button 0 = left, 1 = middle, 2 = right; Shift bits 1 = shift, 2 = alt, 4 = ctrl.
    // X and Y are relative to the element.
    procedure MouseDownInPage(Button, Shift, X, Y: Integer);

    // Script that returns a JSON array describing all form elements in the page
    class function CollectElementsScript: string; static;
    // Script that reports events inside the page by calling
    //   Scheme:ChangeMethod(Key, Value, Sequence) for every value change, and
    //   Scheme:MouseDownMethod(Key, Button, Shift, X, Y) when an element is clicked.
    // Key and Value are url encoded twice.
    class function EventListenerScript(const Scheme, ChangeMethod, MouseDownMethod: string) : string; static;
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
    // Fires when the element is clicked inside the web page
    property OnMouseDown: TMouseEvent read get_OnMouseDown write set_OnMouseDown;
  end;

  // Reflects a drop down (select element) inside a web page. Each picklist item is shown
  // as an option with item.ToString as its text and value.
  TWebComboboxComponent = class(TWebComponent, IComboboxControlWithBindingSupport)
  protected
    _picklist: List<CObject>;

    // Returns the picklist item matching the selected option, or the option text
    // when there is no matching item
    function  get_Value: CObject; override;

    // IComboboxControlWithBindingSupport
    function  get_Picklist: List<CObject>;
    procedure set_Picklist(const Value: List<CObject>);

  public
    // Setting it replaces the options of the drop down in the page. Options without
    // a value (like a '(none)' option) are kept, so is the current selection when it
    // is still in the list.
    property Picklist: List<CObject> read get_Picklist write set_Picklist;
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

class function TWebComponent.EventListenerScript(const Scheme, ChangeMethod, MouseDownMethod: string) : string;
begin
  // ScriptGate url-decodes the call once and then splits it on ( , and ), the inner
  // encoding also escapes the characters encodeURIComponent leaves alone: ! ' ( ) *
  // Calls reach Delphi through (cancelled) navigations; a navigation started right after
  // another one can cancel it, so calls are sent one at a time with a short pause in between.
  Result :=
    '(function() {' +
    '  if (window.__appFrameListening) return "ok";' +
    '  window.__appFrameListening = true;' +
    '  window.__appFrameSequence = 0;' +
    '  var pending = [];' +
    '  var sending = false;' +
    '  function encode(s) {' +
    '    var inner = encodeURIComponent(s).replace(/[!''()*]/g, function(c) { return "%" + c.charCodeAt(0).toString(16).toUpperCase(); });' +
    '    return encodeURIComponent(inner);' +
    '  }' +
    '  function sendNext() {' +
    '    if (pending.length == 0) { sending = false; return; }' +
    '    sending = true;' +
    '    window.location.href = pending.shift();' +
    '    setTimeout(sendNext, 30);' +
    '  }' +
    '  function send(url) {' +
    '    pending.push(url);' +
    '    if (!sending) sendNext();' +
    '  }' +
    '  function elementOf(t) {' +
    '    var e = t && t.closest ? t.closest("input, select, textarea, button") : null;' +
    '    if (!e && t && t.tagName == "LABEL" && t.control) e = t.control;' +
    '    return e && (e.id || e.name) ? e : null;' +
    '  }' +
    '  function notifyChange(e) {' +
    '    var t = e.target;' +
    '    if (window.__appFrameSetting || !t || !("value" in t)) return;' +
    '    var key = t.id || t.name;' +
    '    if (!key) return;' +
    '    window.__appFrameSequence++;' +
    '    send(' + JavaScriptString(Scheme + ':' + ChangeMethod + '(') +
    '      + encode(key) + "," + encode(t.value) + "," + window.__appFrameSequence + ")");' +
    '  }' +
    '  function notifyMouseDown(e) {' +
    '    var t = elementOf(e.target);' +
    '    if (!t) return;' +
    '    var r = t.getBoundingClientRect();' +
    '    var shift = (e.shiftKey ? 1 : 0) | (e.altKey ? 2 : 0) | (e.ctrlKey ? 4 : 0);' +
    '    send(' + JavaScriptString(Scheme + ':' + MouseDownMethod + '(') +
    '      + encode(t.id || t.name) + "," + e.button + "," + shift + "," + Math.round(e.clientX - r.left) + "," + Math.round(e.clientY - r.top) + ")");' +
    '  }' +
    '  document.addEventListener("input", notifyChange, true);' +
    '  document.addEventListener("change", notifyChange, true);' +
    '  document.addEventListener("mousedown", notifyMouseDown, true);' +
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
        TWebComponent(component.GetObject).Update(element)
      else if element.GetValue<string>('tag', '') = 'select' then
        component := TWebComboboxComponent.Create(Parent, element, ExecuteScript)
      else
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

procedure TWebComponent.MouseDownInPage(Button, Shift, X, Y: Integer);
begin
  if not Assigned(_onMouseDown) then
    Exit;

  var mouseButton: TMouseButton;
  case Button of
    1: mouseButton := TMouseButton.mbMiddle;
    2: mouseButton := TMouseButton.mbRight;
  else
    mouseButton := TMouseButton.mbLeft;
  end;

  var shiftState: TShiftState := [];
  case mouseButton of
    TMouseButton.mbLeft: Include(shiftState, ssLeft);
    TMouseButton.mbRight: Include(shiftState, ssRight);
    TMouseButton.mbMiddle: Include(shiftState, ssMiddle);
  end;
  if Shift and 1 <> 0 then Include(shiftState, ssShift);
  if Shift and 2 <> 0 then Include(shiftState, ssAlt);
  if Shift and 4 <> 0 then Include(shiftState, ssCtrl);

  _onMouseDown(Self, mouseButton, shiftState, X, Y);
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

function TWebComponent.get_OnMouseDown: TMouseEvent;
begin
  Result := _onMouseDown;
end;

procedure TWebComponent.set_OnMouseDown(const Value: TMouseEvent);
begin
  _onMouseDown := Value;
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

{ TWebComboboxComponent }

function TWebComboboxComponent.get_Value: CObject;
begin
  if (_picklist <> nil) and (_value <> '') then
    for var item in _picklist do
      if (item <> nil) and (string(item.ToString) = _value) then
        Exit(item);

  Result := inherited;
end;

function TWebComboboxComponent.get_Picklist: List<CObject>;
begin
  Result := _picklist;
end;

procedure TWebComboboxComponent.set_Picklist(const Value: List<CObject>);
begin
  _picklist := Value;

  // The selection is kept when it is still in the list, otherwise nothing is selected
  var found := _value = '';

  var items := TJSONArray.Create;
  try
    if _picklist <> nil then
      for var item in _picklist do
        if item <> nil then
        begin
          var text: string := item.ToString;
          items.Add(text);
          if text = _value then
            found := True;
        end;

    if not found then
      _value := '';

    if Assigned(_executeScript) then
      _executeScript(
        '(function() {' +
        '  var e = document.getElementById(' + JavaScriptString(_elementId) + ') || document.getElementsByName(' + JavaScriptString(_name) + ')[0];' +
        '  if (!e || !e.options) return;' +
        '  var items = ' + items.ToJSON + ';' +
        '  for (var i = e.options.length - 1; i >= 0; i--)' +
        '    if (e.options[i].value !== "") e.remove(i);' +
        '  items.forEach(function(text) {' +
        '    var option = document.createElement("option");' +
        '    option.value = text;' +
        '    option.text = text;' +
        '    e.add(option);' +
        '  });' +
        '  e.value = ' + JavaScriptString(_value) + ';' +
        '})()');
  finally
    items.Free;
  end;
end;

end.
