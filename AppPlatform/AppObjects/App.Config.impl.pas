unit App.Config.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Objects.intf,
  App.Objects.impl,
  QuickJS.Register.dn4d.intf, App.Content.intf, ADato.ObjectModel.List.intf,
  App.PropertyDescriptor.intf;

type
  TAppConfig = class(TBaseInterfacedObject, IAppConfig)
  protected
    _Types: Dictionary<&Type, IObjectType>;

    function get_ObjectType(const AType: &Type): IObjectType;
    function get_Types: List<&Type>;

    procedure AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type);
    procedure RegisterJSType(const JSObjectType: JSObjectReference);
    procedure RegisterType(const AType: &Type; const ObjectType: IObjectType);
    function  TypeByName(const Name: string) : &Type;

  public
    constructor Create;
  end;

  TJSObjectType = class(ObjectType)
  protected
    _JSProto: JSObjectReference;

    function  get_Binder: IContentBinder; override;
    function  get_Builder: IContentBuilder; override;
    function  get_Provider: IContentProvider; override;
    function  get_PropertyDescriptor(const Name: CString) : IPropertyDescriptor; override;

    //function  get_PropertyDescriptor: IPropertyDescriptors; override;

  public
    constructor Create(const JSProto: JSObjectReference);

    function  GetType: &Type; override;
  end;

implementation

uses
  App.Windows.impl, System.ClassHelpers, System.Rtti, ADato.Extensions.intf;

{ TAppConfig }

constructor TAppConfig.Create;
begin
  _Types := CDictionary<&Type, IObjectType>.Create(10, TypeEqualityComparer.Create);
end;

function TAppConfig.get_ObjectType(const AType: &Type): IObjectType;
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

procedure TAppConfig.AddProperty(const OwnerType: &Type; const Name: CString; const ALabel: CString; const PropType: &Type);
begin
  if ExtensionManager <> nil then
  begin
    var objectType := get_ObjectType(OwnerType);
    var dataObjectType := objectType.GetType;

    var prop: _PropertyInfo := CustomProperty.Create(dataObjectType, Name, ALabel, PropType);
    ExtensionManager.AddProperty(dataObjectType, prop);
  end;
end;


procedure TAppConfig.RegisterJSType(const JSObjectType: JSObjectReference);
begin
  var tp := JSObjectType.GetType;
  RegisterType(tp, TJSObjectType.Create(JSObjectType));
end;

procedure TAppConfig.RegisterType(const AType: &Type; const ObjectType: IObjectType);
begin
  _Types[AType] := ObjectType {can be nil};
end;

function TAppConfig.TypeByName(const Name: string): &Type;
begin
  for var entry in _Types do
    if CString.Equals(entry.Key.Name, Name) then
      Exit(entry.Key);
end;

{ TJSObjectType }

constructor TJSObjectType.Create(const JSProto: JSObjectReference);
begin
  _JSProto := JSProto;
end;

function TJSObjectType.GetType: &Type;
begin
 Result := _JSProto.Invoke<&Type>('GetType');
end;

function TJSObjectType.get_Binder: IContentBinder;
begin
  Result := _JSProto.Invoke<IContentBinder>('Binder');
end;

function TJSObjectType.get_Builder: IContentBuilder;
begin
  Result := _JSProto.Invoke<IContentBuilder>('Builder');
end;

function TJSObjectType.get_PropertyDescriptor(const Name: CString) : IPropertyDescriptor;
begin
  Result := _JSProto.Invoke<IPropertyDescriptor>('PropertyDescriptor', Name);
end;

function TJSObjectType.get_Provider: IContentProvider;
begin
  Result := _JSProto.Invoke<IContentProvider>('Provider');
end;

end.
