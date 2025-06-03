unit App.Config.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Objects.intf,
  App.Objects.impl,
  QuickJS.Register.dn4d.intf, App.Content.intf, ADato.ObjectModel.List.intf;

type
  TAppConfig = class(TBaseInterfacedObject, IAppConfig)
  protected
    _Types: Dictionary<&Type, IObjectType>;

    function get_ObjectType(const AType: &Type): IObjectType;
    function get_Types: List<&Type>;

    procedure RegisterJSType(const JSObjectType: IJSObjectReference);
    procedure RegisterType(const AType: &Type; const ObjectType: IObjectType);
    function  TypeByName(const Name: string) : &Type;

  public
    constructor Create;
  end;

  TJSObjectType = class(ObjectType)
  protected
    _JSProto: IJSObjectReference;

    function  get_Binder: IContentBinder; override;
    function  get_Builder: IContentBuilder; override;
    function  get_Provider: IContentProvider; override;

  public
    constructor Create(const JSProto: IJSObjectReference);

    function  GetType: &Type; override;
  end;

implementation

uses
  App.Windows.impl, System.ClassHelpers;

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

procedure TAppConfig.RegisterJSType(const JSObjectType: IJSObjectReference);
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

constructor TJSObjectType.Create(const JSProto: IJSObjectReference);
begin
  _JSProto := JSProto;
end;

function TJSObjectType.GetType: &Type;
begin
  var o := _JSProto.Invoke('GetType', TypeInfo(&Type));
  if o <> nil then
    o.TryAsType<&Type>(Result);
end;

function TJSObjectType.get_Binder: IContentBinder;
begin
  var o := _JSProto.Invoke('Binder', TypeInfo(IJSObjectReference));
  if o <> nil then
    o.TryAsType<IContentBinder>(Result);
end;

function TJSObjectType.get_Builder: IContentBuilder;
begin
  var o := _JSProto.Invoke('Builder', TypeInfo(IJSObjectReference));
  if o <> nil then
    o.TryAsType<IContentBuilder>(Result);
end;

function TJSObjectType.get_Provider: IContentProvider;
begin
  var o := _JSProto.Invoke('Provider', TypeInfo(IJSObjectReference));
  if o <> nil then
    o.TryAsType<IContentProvider>(Result);
end;

end.
