unit App.TypeDescriptor.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Content.intf,
  App.TypeDescriptor.intf,
  App.PropertyDescriptor.intf,
  App.TypeMetadata;

type
//  TPropertyDescriptors = class(TBaseInterfacedObject, IPropertyDescriptors)
//  protected
//    _descriptors: Dictionary<CString, IPropertyDescriptor>;
//
//    procedure AddPropertyDescriptor(const Name: CString; const AProperty: IPropertyDescriptor);
//    function  get_Property(const Name: CString) : IPropertyDescriptor;
//
//  public
//    constructor Create;
//  end;

  TTypeDescriptor = class(TBaseInterfacedObject, ITypeDescriptor)
  protected
    _className: CString;
    _storageName: CString;
    _type: &Type;
    _binder: IContentBinder;
    _provider: IContentProvider;
    _PropertyDescriptor: Dictionary<string, IPropertyDescriptor>;
    {$IFDEF APP_PLATFORM_MD}
    _SupportedInterfaces: List<ISupportedInterfaceMetadata>;
    {$ENDIF}

    {$IFDEF APP_PLATFORM_MD}
    function  GetType: &Type; override;

    procedure AddSupportedInterface(const AType: &Type);
    {$ENDIF}

    function  AddPropertyDescriptor(const Name: string; const Value: IPropertyDescriptor) : Boolean; virtual;

    function  get_ClassName: CString;
    function  get_FullName: CString;
    function  get_PropertyDescriptor(const Name: string) : IPropertyDescriptor; virtual;
    function  get_StorageName: CString; virtual;
    function  get_Binder: IContentBinder; virtual;
    procedure set_Binder(const Value: IContentBinder); virtual;
    function  get_Provider: IContentProvider; virtual;
    procedure set_Provider(const Value: IContentProvider); virtual;
    {$IFDEF APP_PLATFORM_MD}
    function  get_SupportedInterfaces: List<ISupportedInterfaceMetadata>;
    {$ENDIF}

  public
    constructor Create(const AType: &Type; const ClassName: CString; const StorageName: CString);
  end;

implementation

uses
  System.ClassHelpers;

{ ObjectType }
constructor TTypeDescriptor.Create(const AType: &Type; const ClassName: CString; const StorageName: CString);
begin
  _type := AType;
  _className := ClassName;
  _storageName := StorageName;
  _PropertyDescriptor := CDictionary<string, IPropertyDescriptor>.Create;
end;

{$IFDEF APP_PLATFORM_MD}
function TTypeDescriptor.GetType: &Type;
begin
  Result := _type;
end;

procedure TTypeDescriptor.AddSupportedInterface(const AType: &Type);
begin
  for var supportedInterface in get_SupportedInterfaces do
    if CObject.Equals(supportedInterface.InterfaceType, AType) then
      Exit;

  get_SupportedInterfaces.Add(TTypeMetadata.SupportedInterface(AType));
end;
{$ENDIF}

function TTypeDescriptor.AddPropertyDescriptor(const Name: string; const Value: IPropertyDescriptor): Boolean;
begin
  _PropertyDescriptor[Name] := Value;
  Result := True;
end;

function TTypeDescriptor.get_Binder: IContentBinder;
begin
  Result := _binder;
end;

function TTypeDescriptor.get_ClassName: CString;
begin
  Result := _className;
end;

function TTypeDescriptor.get_FullName: CString;
begin
  Result := get_ClassName;
end;

{$IFDEF APP_PLATFORM_MD}
function TTypeDescriptor.get_SupportedInterfaces: List<ISupportedInterfaceMetadata>;
begin
  if _SupportedInterfaces = nil then
    _SupportedInterfaces := CList<ISupportedInterfaceMetadata>.Create;

  Result := _SupportedInterfaces;
end;
{$ENDIF}

function TTypeDescriptor.get_PropertyDescriptor(const Name: string): IPropertyDescriptor;
begin
  _PropertyDescriptor.TryGetValue(Name, Result);
end;

function TTypeDescriptor.get_StorageName: CString;
begin
  Result := _storageName;
end;

function TTypeDescriptor.get_Provider: IContentProvider;
begin
  Result := _provider;
end;

procedure TTypeDescriptor.set_Binder(const Value: IContentBinder);
begin
  _Binder := Value;
end;

procedure TTypeDescriptor.set_Provider(const Value: IContentProvider);
begin
  _provider := Value;
end;

end.

