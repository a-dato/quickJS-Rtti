unit App.Config.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.TypeDescriptor.intf,
  App.TypeDescriptor.impl,
  QuickJS.Register.dn4d.intf,
  App.Content.intf,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.PropertyDescriptor.intf;

type
  TAppConfig = class(TBaseInterfacedObject, IAppConfig)
  protected
    _Types: Dictionary<&Type, ITypeDescriptor>;
    _WindowTypes: Dictionary<string {Name}, IWindowType>;

    function get_Types: List<&Type>;

    function  AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type; const Descriptor: IPropertyDescriptor) : _PropertyInfo;
    procedure RegisterType(const AType: &Type; const TypeDescriptor: ITypeDescriptor);
    procedure RegisterWindow(const Name: string; const CreateFunc: TFrameCreateFunc);

    function  TypeByName(const Name: string) : &Type;
    function  TypeDescriptor(const AType: &Type): ITypeDescriptor;
    function  TypeDescriptorByName(const Name: string) : ITypeDescriptor;

    function  TryGetWindowType(const Name: string; out WindowType : IWindowType) : Boolean;
  public
    constructor Create;
  end;

implementation

uses
  App.Windows.impl,
  System.ClassHelpers,
  System.Rtti,
  ADato.Extensions.intf,
  ADato.ObjectModel.impl, App.PropertyDescriptor.impl, System.Collections;

{ TAppConfig }

constructor TAppConfig.Create;
begin
  _Types := CDictionary<&Type, ITypeDescriptor>.Create(10, TypeEqualityComparer.Create);
  _WindowTypes := CDictionary<string, IWindowType>.Create;
end;

function TAppConfig.TypeDescriptor(const AType: &Type): ITypeDescriptor;
begin
  {$IFDEF DEBUG}
  if not _Types.TryGetValue(AType, Result) then
    _Types.TryGetValue(AType, Result);
  {$ELSE}
  _Types.TryGetValue(AType, Result);
  {$ENDIF}
end;

function TAppConfig.get_Types: List<&Type>;
begin
  Result := CList<&Type>.Create(_Types.Keys);
end;

function TAppConfig.AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type; const Descriptor: IPropertyDescriptor) : _PropertyInfo;
begin
  {$IFDEF APP_PLATFORM}
  if ExtensionManager <> nil then
  begin
    var prop: _PropertyInfo := CustomProperty.Create(OwnerType, Name, ALabel, PropType);

    if Descriptor <> nil then
    begin
      if PropType.IsOfType<IList> then
        prop := TCollectionPropertyWithDescriptor.Create(prop, descriptor) else
        prop := TPropertyWithDescriptor.Create(prop, descriptor);
    end;

    if Name.Contains('.') then
    begin
      var sub_properties := Name.Split(['.']);

      var path: TArray<_PropertyInfo>;
      SetLength(path, Length(sub_properties));
      var property_type := OwnerType;

      for var i := 0 to High(sub_properties) do
      begin
        var sub_property := property_type.PropertyByName(sub_properties[i]);
        path[i] := sub_property;
        property_type := sub_property.GetType;
      end;

      prop := TPathProperty.Create(prop, path);
    end;

    ExtensionManager.AddProperty(OwnerType, prop);

    Result := prop;
  end;
  {$ENDIF}
end;

procedure TAppConfig.RegisterType(const AType: &Type; const TypeDescriptor: ITypeDescriptor);
begin
  _Types[AType] := TypeDescriptor {can be nil};
end;

function TAppConfig.TypeByName(const Name: string): &Type;
begin
  for var entry in _Types do
    if CString.Equals(entry.Key.Name, Name) then
      Exit(entry.Key);
end;

procedure TAppConfig.RegisterWindow(const Name: string; const CreateFunc: TFrameCreateFunc);
begin
  _WindowTypes[Name] := TWindowType.Create(Name, CreateFunc);
end;

function TAppConfig.TypeDescriptorByName(const Name: string): ITypeDescriptor;
begin
  Result := TypeDescriptor(TypeByName(Name));
end;

function TAppConfig.TryGetWindowType(const Name: string; out WindowType : IWindowType) : Boolean;
begin
  Result := _WindowTypes.TryGetValue(Name, WindowType);
end;

end.

