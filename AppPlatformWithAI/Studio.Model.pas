unit Studio.Model;

// What the Studio shows about a type, read from the app platform (_app):
// its storage, its properties and how to show a value.

interface

uses
  System.SysUtils,
  System_,
  System.Collections,
  App.Storage.intf;

type
  TProperties = TArray<_PropertyInfo>;

  TStudioModel = class
  public
    // The storage holding the objects of AType, or nil when it has none.
    class function StorageOf(const AType: &Type) : IStorage; static;
    // The values Prop can be set to, or nil when it takes free input:
    // the picklist of its descriptor, else the storage of its type.
    class function ChoicesFor(const Prop: _PropertyInfo; const PropType: &Type) : IList; static;
    // True when values of PropType go through a descriptor (marshaller,
    // formatter): a registered type, which a text field cannot produce.
    class function IsRegisteredType(const PropType: &Type) : Boolean; static;
    // All properties of AType, including properties added at runtime.
    class function PropertiesOf(const AType: &Type) : TProperties; static;
    class function PropertyLabel(const AType: &Type; const Prop: _PropertyInfo) : string; static;
    class function IsList(const AType: &Type) : Boolean; static;
    // A new, empty list of objects
    class function NewList: IList; static;
    // The type of Prop: for a type script the declared one (Address,
    // Customer, ...), which Delphi cannot see on a JavaScript property.
    class function PropertyTypeOf(const Owner: &Type; const Prop: _PropertyInfo) : &Type; static;
    // The type of Prop as text, as declared when Owner is scripted
    class function PropertyTypeName(const Owner: &Type; const Prop: _PropertyInfo) : string; static;
    class function FindType(const Name: string; out AType: &Type) : Boolean; static;
    // The value as text; never raises.
    class function Format(const Value: CObject) : string; static;
    // The value of Prop on Item as text; never raises.
    class function ValueText(const Item: CObject; const Prop: _PropertyInfo) : string; static;
    class function TypeName(const AType: &Type) : string; static;
  end;

implementation

uses
  App.intf,
  App.TypeScripts,
  App.PropertyDescriptor.intf,
  App.TypeDescriptor.intf,
  System.StrUtils,
  System.Collections.Generic;

class function TStudioModel.StorageOf(const AType: &Type) : IStorage;
begin
  Result := nil;

  var descriptor: ITypeDescriptor;
  try
    descriptor := _app.Config.TypeDescriptor(AType);
  except
    Exit;
  end;

  if descriptor = nil then
    Exit;

  var name: string;
  try
    name := descriptor.StorageName;
  except
    // JavaScript descriptors without a StorageName
    Exit;
  end;

  if (name = '') or not _app.TryGetStorage(name, Result) then
    Result := nil;
end;

class function TStudioModel.ChoicesFor(const Prop: _PropertyInfo; const PropType: &Type) : IList;
begin
  Result := nil;

  var descriptor: IPropertyDescriptor;
  if Interfaces.Supports<IPropertyDescriptor>(Prop, descriptor) then
  try
    var picklist := descriptor.Picklist;
    if picklist <> nil then
    begin
      var items := picklist.Items(CString(''));
      if not items.IsNull and items.TryGetValue<IList>(Result) and (Result <> nil) then
        Exit;
    end;
  except
    // A descriptor without a (working) picklist: fall back to the storage
  end;

  Result := StorageOf(PropType);
end;

class function TStudioModel.IsRegisteredType(const PropType: &Type) : Boolean;
begin
  try
    Result := _app.Config.TypeDescriptor(PropType) <> nil;
  except
    Result := False;
  end;
end;

class function TStudioModel.PropertiesOf(const AType: &Type) : TProperties;
begin
  Result := [];
  for var prop in _app.Config.GetProperties(AType) do
    Result := Result + [prop];
end;

class function TStudioModel.PropertyLabel(const AType: &Type; const Prop: _PropertyInfo) : string;
begin
  // A property a script added to a Delphi type (its descriptor would name the
  // property's type instead)
  if TypeScriptStore <> nil then
  begin
    Result := TypeScriptStore.ExtensionLabel(AType, Prop.Name);
    if Result <> '' then
      Exit;
  end;

  try
    Result := _app.Config.PropertyDisplayName(AType, Prop.Name);
  except
    Result := '';
  end;

  if Result = '' then
    Result := Prop.Name;
end;

class function TStudioModel.FindType(const Name: string; out AType: &Type) : Boolean;
begin
  for var t in _app.Config.Types do
    if SameText(t.Name, Name) then
    begin
      AType := t;
      Exit(True);
    end;
  Result := False;
end;

class function TStudioModel.PropertyTypeOf(const Owner: &Type; const Prop: _PropertyInfo) : &Type;
begin
  if TypeScriptStore <> nil then
  begin
    var declared := TypeScriptStore.DeclaredType(TypeName(Owner), Prop.Name);
    if (declared <> '') and FindType(declared, Result) then
      Exit;
  end;
  Result := Prop.GetType;
end;

class function TStudioModel.PropertyTypeName(const Owner: &Type; const Prop: _PropertyInfo) : string;
begin
  Result := '';
  if TypeScriptStore <> nil then
    Result := TypeScriptStore.DeclaredType(TypeName(Owner), Prop.Name);
  if Result = '' then
  try
    Result := TypeName(Prop.GetType);
  except
    on E: Exception do
      Result := '<' + E.Message + '>';
  end;
end;

class function TStudioModel.NewList: IList;
begin
  Result := CList<CObject>.Create;
end;

class function TStudioModel.IsList(const AType: &Type) : Boolean;
begin
  Result := AType.IsOfType<IList>;
end;

class function TStudioModel.Format(const Value: CObject) : string;
begin
  try
    if Value.IsNull then
      Exit('');

    var list: IList;
    if Value.TryGetValue<IList>(list) then
    begin
      if list = nil then
        Exit('');
      // A few objects of a registered type (IProject.TrelloTasks): their texts
      if (list.Count > 0) and (list.Count <= 3) and not list[0].IsNull and list[0].IsInterface
        and IsRegisteredType(list[0].GetType) then
      begin
        Result := '';
        for var i := 0 to list.Count - 1 do
          Result := Result + IfThen(i > 0, ', ') + Format(list[i]);
        Exit;
      end;
      Exit(list.Count.ToString + ' items');
    end;

    Result := Value.ToString;

    // A Delphi object (e.g. an IProject a script type links to) has no
    // ToString of its own: show its Description or Name instead of its class
    if Value.IsInterface and (TypeScriptStore <> nil) and not TypeScriptStore.IsScripted(TypeName(Value.GetType)) then
      for var t in _app.Config.Types do
      begin
        var cast: CObject;
        if TypeScriptStore.IsScripted(TypeName(t)) or not Value.TryCast(t, cast) then
          Continue;
        for var propName in ['Description', 'Name'] do
          for var prop in PropertiesOf(t) do
            if SameText(prop.Name, propName) then
              Exit(prop.GetValue(cast, []).ToString);
      end;
  except
    on E: Exception do
      Result := '<' + E.Message + '>';
  end;
end;

class function TStudioModel.ValueText(const Item: CObject; const Prop: _PropertyInfo) : string;
begin
  try
    Result := Format(Prop.GetValue(Item, []));
  except
    on E: Exception do
      Result := '<' + E.Message + '>';
  end;
end;

class function TStudioModel.TypeName(const AType: &Type) : string;
begin
  try
    Result := AType.Name;
  except
    Result := '?';
  end;
end;

end.
