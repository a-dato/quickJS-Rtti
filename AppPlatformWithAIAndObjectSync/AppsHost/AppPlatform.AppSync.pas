unit AppPlatform.AppSync;

// Live objects shared by every window that shows them (the Studio, app pages),
// ported from QuickJSWebViewSyncTestSimplified (Simple.ObjectModels,
// Simple.WebPropertyBinding, Simple.WebParams).
//
//   ModelFor(&Type)          one IObjectModel per type for the whole app. Contexts
//                            made from the same model share its property
//                            wrappers, so an edit through one context reaches the
//                            bindings of every other context showing that object.
//   TPagePropertyBinding     one property shown in an app page: an
//                            IPropertyBinding whose "control" lives in the page.
//                              model -> page : SetValue -> push 'property'
//                                              {object, property, value, editable}
//                              page -> model : PageEdited -> NotifyModel (the
//                                              call a TEdit binding makes)
//   TCallbackBinding         a binding that calls back when the property changed
//                            somewhere else, and edits through the model (the
//                            Studio's inspector).

interface

uses
  System.SysUtils, System.JSON, FMX.Controls,
  System_, ADato.ObjectModel.intf, ADato.ObjectModel.Binders, ADato.PropertyAccessibility.Intf;

type
  TPushProc = reference to procedure(const Channel, Json: string);

  // Also implements IControlBinding (with no Delphi control): the framework
  // compares bindings with TControlBinding.Equals, which casts the other
  // binding to IControlBinding.
  TPagePropertyBinding = class(TPropertyBinding, IControlBinding)
  private
    FObjectName: string;
    FAppCanEdit: TFunc<Boolean>;  // the host block exposed it as 'edit'
    FPush: TPushProc;
    FPageValue: CObject;          // last value the page sent (GetValue)
    FShown: CObject;              // what the page shows
    FShownEditable: Boolean;
    FHasShown: Boolean;
    procedure Push(const Value: CObject);
  protected
    function  GetValue: CObject; override;
    procedure SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject); override;
    procedure ExecuteOriginalOnChangeEvent; override;
    // IControlBinding
    function  get_Control: TControl;
    procedure UpdateControlEditability(IsEditable: Boolean);
    procedure OnFreeNotificationDestroy;
  public
    constructor Create(const ObjectName: string; const AppCanEdit: TFunc<Boolean>; const PushProc: TPushProc);
    // The host block allows it, and the object (IPropertyAccessibility, as
    // TControlBinding.CheckControlEditability), or the property is writable
    function  IsEditable: Boolean;
    // The current value and editability, as the page now shows them: {value, editable}
    function  Snapshot: TJSONObject;
    procedure PageEdited(const Value: CObject);
    procedure Detach;
    // The bound property (the model's wrapper)
    function  Prop: _PropertyInfo;
  end;

  TCallbackBinding = class(TPropertyBinding, IControlBinding)
  private
    FValue: CObject;
    FOnChanged: TProc;
    FArmed: Boolean;
  protected
    function  GetValue: CObject; override;
    procedure SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject); override;
    procedure ExecuteOriginalOnChangeEvent; override;
    function  get_Control: TControl;
    procedure UpdateControlEditability(IsEditable: Boolean);
    procedure OnFreeNotificationDestroy;
  public
    constructor Create(const OnChanged: TProc);
    // Writes Value through the model: every other context showing the object hears it
    procedure Edit(const Value: CObject);
    procedure Detach;
    // Set after Bind: binding pushes the current value into the binding, which
    // is not a change (calling back for it rebuilds whoever listens, endlessly)
    property Armed: Boolean read FArmed write FArmed;
  end;

// One model per type for the whole app
function ModelFor(const AType: &Type) : IObjectModel;

var
  // Called after an app page or a host block changed an object (the Studio
  // re-reads its object grid: it only binds the object in its inspector)
  OnObjectsEdited: TProc;

procedure ObjectsEdited;

// A CObject as JSON: null, Boolean, number or string
function ValueToJson(const Value: CObject) : TJSONValue;

implementation

uses
  System.Generics.Collections, ADato.ObjectModel.impl;

type
  TModelEntry = record
    AType: &Type;
    Model: IObjectModel;
  end;

var
  _Models: TDictionary<string, TModelEntry>;

function ModelFor(const AType: &Type) : IObjectModel;
begin
  if _Models = nil then
    _Models := TDictionary<string, TModelEntry>.Create;

  // By name: script types share one TypeInfo. A type script that ran again is a
  // new type with the same name and gets a new model.
  var key := AType.Name.ToLower;
  var entry: TModelEntry;
  if _Models.TryGetValue(key, entry) and (entry.AType = AType) then
    Exit(entry.Model);

  entry.AType := AType;
  entry.Model := TObjectModel.Create(AType);
  _Models.AddOrSetValue(key, entry);
  Result := entry.Model;
end;

procedure ObjectsEdited;
begin
  if Assigned(OnObjectsEdited) then
    OnObjectsEdited();
end;

function ValueToJson(const Value: CObject) : TJSONValue;
begin
  if Value = nil then
    Result := TJSONNull.Create
  else if Value.IsBoolean then   // (IsOfType<Boolean> raises on a string value)
    Result := TJSONBool.Create(Value.AsType<Boolean>)
  else if Value.IsNumber then
    Result := TJSONNumber.Create(Value.AsType<Double>)
  else
    Result := TJSONString.Create(string(Value.ToString));
end;

{ TPagePropertyBinding }

constructor TPagePropertyBinding.Create(const ObjectName: string; const AppCanEdit: TFunc<Boolean>; const PushProc: TPushProc);
begin
  inherited Create;
  FObjectName := ObjectName;
  FAppCanEdit := AppCanEdit;
  FPush := PushProc;
end;

function TPagePropertyBinding.GetValue: CObject;
begin
  Result := FPageValue;
end;

procedure TPagePropertyBinding.ExecuteOriginalOnChangeEvent;
begin
end;

function TPagePropertyBinding.get_Control: TControl;
begin
  Result := nil;   // the "control" lives in the page
end;

procedure TPagePropertyBinding.UpdateControlEditability(IsEditable: Boolean);
begin
end;

procedure TPagePropertyBinding.OnFreeNotificationDestroy;
begin
end;

function TPagePropertyBinding.IsEditable: Boolean;
begin
  if not Assigned(FAppCanEdit) or not FAppCanEdit() then
    Exit(False);
  var obj: CObject := nil;
  if _ObjectModelContext <> nil then
    obj := _ObjectModelContext.Context;
  if obj = nil then
    Exit(False);
  var pa: IPropertyAccessibility;
  if obj.TryAsType<IPropertyAccessibility>(pa) then
    Result := pa.PropertyState(__PropertyInfo.Name).IsEditable
  else
    Result := __PropertyInfo.CanWrite;
end;

// Called by the framework, exactly like it calls TEditControlBinding.SetValue:
//  - AProperty is our bound property -> its value changed
//  - AProperty is another property   -> we are Linked to it: re-read our value
//                                       and whether we can be edited
procedure TPagePropertyBinding.SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject);
begin
  if IsUpdating then Exit;   // our own edit coming back, the page already shows it
  if IsBoundProperty(AProperty) then
    Push(Value)
  else
  begin
    // As TControlBinding.ExecuteFromLink: tell the object, so a presenter drops
    // the answer it cached for our property before we ask again
    var listener: IPropertyLinkChangeListener;
    if (Obj <> nil) and Obj.TryAsType<IPropertyLinkChangeListener>(listener) then
      listener.PropertyLinkChanged(AProperty, __PropertyInfo);
    Push(__PropertyInfo.GetValue(Obj, []));
  end;
end;

procedure TPagePropertyBinding.Push(const Value: CObject);
begin
  var editable := IsEditable;
  if FHasShown and CObject.Equals(Value, FShown) and (editable = FShownEditable) then
    Exit;   // nothing new for the page
  // Before the page has the object (during Open), it gets values in the snapshot
  var send := FHasShown;
  FShown := Value;
  FShownEditable := editable;
  FHasShown := True;
  if not send or not Assigned(FPush) then
    Exit;

  var o := TJSONObject.Create;
  try
    o.AddPair('object', FObjectName);
    o.AddPair('property', string(__PropertyInfo.Name));
    o.AddPair('value', ValueToJson(Value));
    o.AddPair('editable', TJSONBool.Create(editable));
    FPush('property', o.ToJSON);
  finally
    o.Free;
  end;
end;

function TPagePropertyBinding.Snapshot: TJSONObject;
begin
  FShown := __PropertyInfo.GetValue(_ObjectModelContext.Context, []);
  FShownEditable := IsEditable;
  FHasShown := True;
  Result := TJSONObject.Create;
  Result.AddPair('value', ValueToJson(FShown));
  Result.AddPair('editable', TJSONBool.Create(FShownEditable));
end;

procedure TPagePropertyBinding.PageEdited(const Value: CObject);
begin
  FPageValue := Value;
  FShown := Value;     // the page shows this already
  // Same path a TEdit/TCheckBox binding takes on its OnChange
  NotifyModel(nil);
end;

function TPagePropertyBinding.Prop: _PropertyInfo;
begin
  Result := __PropertyInfo;
end;

procedure TPagePropertyBinding.Detach;
begin
  FPush := nil;
  FAppCanEdit := nil;
end;

{ TCallbackBinding }

constructor TCallbackBinding.Create(const OnChanged: TProc);
begin
  inherited Create;
  FOnChanged := OnChanged;
end;

function TCallbackBinding.GetValue: CObject;
begin
  Result := FValue;
end;

procedure TCallbackBinding.SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject);
begin
  if IsUpdating or not FArmed then Exit;   // our own edit, or Bind showing the current value
  if Assigned(FOnChanged) then
    FOnChanged();
end;

procedure TCallbackBinding.ExecuteOriginalOnChangeEvent;
begin
end;

function TCallbackBinding.get_Control: TControl;
begin
  Result := nil;
end;

procedure TCallbackBinding.UpdateControlEditability(IsEditable: Boolean);
begin
end;

procedure TCallbackBinding.OnFreeNotificationDestroy;
begin
end;

procedure TCallbackBinding.Edit(const Value: CObject);
begin
  FValue := Value;
  NotifyModel(nil);
end;

procedure TCallbackBinding.Detach;
begin
  FOnChanged := nil;
end;

initialization

finalization
  _Models.Free;

end.
