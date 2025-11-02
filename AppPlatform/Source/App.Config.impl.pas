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
  ADato.ObjectModel.List.intf,
  App.PropertyDescriptor.intf;

type
  TAppConfig = class(TBaseInterfacedObject, IAppConfig)
  protected
    _Types: Dictionary<&Type, ITypeDescriptor>;

    function get_Types: List<&Type>;

    function  AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type; const Descriptor: IPropertyDescriptor) : _PropertyInfo;
    procedure RegisterType(const AType: &Type; const TypeDescriptor: ITypeDescriptor);
    function  TypeByName(const Name: string) : &Type;
    function  TypeDescriptor(const AType: &Type): ITypeDescriptor;
    function  TypeDescriptorByName(const Name: string) : ITypeDescriptor;
  public
    constructor Create;
  end;

//  TJSObjectType = class(TTypeDescriptor)
//  protected
//    _JSProto: IJSObject;
//
//    function  get_Binder: IContentBinder; override;
//    function  get_Builder: IContentBuilder; override;
//    function  get_Provider: IContentProvider; override;
//    function  get_PropertyDescriptor(const Name: string) : IPropertyDescriptor; override;
//
//    //function  get_PropertyDescriptor: IPropertyDescriptors; override;
//
//  public
//    constructor Create(const JSProto: IJSObject);
//
//    function  GetType: &Type; override;
//  end;

implementation

uses
  App.Windows.impl,
  System.ClassHelpers,
  System.Rtti,
  ADato.Extensions.intf,
  ADato.ObjectModel.impl, App.PropertyDescriptor.impl;

{ TAppConfig }

constructor TAppConfig.Create;
begin
  _Types := CDictionary<&Type, ITypeDescriptor>.Create(10, TypeEqualityComparer.Create);
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
      prop := TPropertyWithDescriptor.Create(prop, descriptor);

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

function TAppConfig.TypeDescriptorByName(const Name: string): ITypeDescriptor;
begin
  Result := TypeDescriptor(TypeByName(Name));
end;

//{ TJSObjectType }
//
//constructor TJSObjectType.Create(const JSProto: IJSObject);
//begin
//  _JSProto := JSProto;
//end;
//
//function TJSObjectType.GetType: &Type;
//begin
// Result := _JSProto.GetType;
//end;
//
//function TJSObjectType.get_Binder: IContentBinder;
//begin
//  var v := _JSProto.Invoke('Binder', nil, TypeInfo(IContentBinder));
//  Interfaces.Supports<IContentBinder>(v.AsInterface, Result);
//end;
//
//function TJSObjectType.get_Builder: IContentBuilder;
//begin
//  var v := _JSProto.Invoke('Builder', nil, TypeInfo(IContentBuilder));
//  Interfaces.Supports<IContentBuilder>(v.AsInterface, Result);
//end;
//
//function TJSObjectType.get_PropertyDescriptor(const Name: string) : IPropertyDescriptor;
//begin
//  var v := _JSProto.Invoke('PropertyDescriptor', [Name], TypeInfo(IPropertyDescriptor));
//  Interfaces.Supports<IPropertyDescriptor>(v.AsInterface, Result);
//end;
//
//function TJSObjectType.get_Provider: IContentProvider;
//begin
//  var v := _JSProto.Invoke('Provider', nil, TypeInfo(IContentProvider));
//  Interfaces.Supports<IContentProvider>(v.AsInterface, Result);
//end;

end.

