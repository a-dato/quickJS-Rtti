unit App.Objects.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.Objects.intf,
  App.Windows.intf,
  App.Content.intf,
  ADato.ObjectModel.List.intf;

type
  TPicklist = class(TBaseInterfacedObject, IPicklist)
  protected
    function Items(const Filter: CString) : CObject; overload; // List, array or else...
    function Items(const Pred: Predicate<CObject>) : CObject; overload; // List, array or else...
    function Format(const Item: CObject) : CString;
  end;

  TPropertyDescriptor = class(TBaseInterfacedObject, IPropertyDescriptor)
  protected
    _Picklist: IPicklist;
    _EditorType: EditorType;
    _Visible: Boolean;

    function  get_Picklist: IPicklist;
    procedure set_Picklist(const Value: IPicklist);
    function  get_EditorType: EditorType;
    procedure set_EditorType(const Value: EditorType);
    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);
  end;

  TPropertyDescriptors = class(TBaseInterfacedObject, IPropertyDescriptors)
  protected
    _descriptors: Dictionary<CString, IPropertyDescriptor>;

    procedure AddPropertyDescriptor(const Name: CString; const AProperty: IPropertyDescriptor);
    function  get_Property(const Name: CString) : IPropertyDescriptor;

  public
    constructor Create;
  end;

  ObjectType = class(TBaseInterfacedObject, IObjectType)
  protected
    _binder: IContentBinder;
    _builder: IContentBuilder;
    _provider: IContentProvider;
    _PropertyDescriptor: IPropertyDescriptors;

    function  get_Name: CString; virtual;
    function  get_Binder: IContentBinder; virtual;
    procedure set_Binder(const Value: IContentBinder); virtual;
    function  get_Builder: IContentBuilder; virtual;
    procedure set_Builder(const Value: IContentBuilder); virtual;
    function  get_Provider: IContentProvider; virtual;
    procedure set_Provider(const Value: IContentProvider); virtual;
    function  get_PropertyDescriptor: IPropertyDescriptors; virtual;
    procedure set_PropertyDescriptor(const Value: IPropertyDescriptors);
  end;

implementation

uses
  System.ClassHelpers;

{ TObjectsConfig }

{ ObjectType }

function ObjectType.get_Binder: IContentBinder;
begin
  Result := _binder;
end;

function ObjectType.get_Builder: IContentBuilder;
begin
  Result := _builder;
end;

function ObjectType.get_Name: CString;
begin
  Result := GetType().Name;
end;

function ObjectType.get_PropertyDescriptor: IPropertyDescriptors;
begin
  if _PropertyDescriptor = nil then
    _PropertyDescriptor := TPropertyDescriptors.Create;

  Result := _PropertyDescriptor;
end;

function ObjectType.get_Provider: IContentProvider;
begin
  Result := _provider;
end;

procedure ObjectType.set_Binder(const Value: IContentBinder);
begin
  _Binder := Value;
end;

procedure ObjectType.set_Builder(const Value: IContentBuilder);
begin
  _builder := Value;
end;

procedure ObjectType.set_PropertyDescriptor(const Value: IPropertyDescriptors);
begin
  _PropertyDescriptor := Value;
end;

procedure ObjectType.set_Provider(const Value: IContentProvider);
begin
  _provider := Value;
end;

{ TPropertyDescriptor }

function TPropertyDescriptor.get_EditorType: EditorType;
begin
  Result := _EditorType;
end;

function TPropertyDescriptor.get_Picklist: IPicklist;
begin
  Result := _Picklist;
end;

function TPropertyDescriptor.get_Visible: Boolean;
begin
  Result := _Visible;
end;

procedure TPropertyDescriptor.set_EditorType(const Value: EditorType);
begin
  _EditorType := Value;
end;

procedure TPropertyDescriptor.set_Picklist(const Value: IPicklist);
begin
  _PickList := Value;
end;

procedure TPropertyDescriptor.set_Visible(const Value: Boolean);
begin
  _Visible := Value;
end;

{ TPropertyDescriptors }

procedure TPropertyDescriptors.AddPropertyDescriptor(const Name: CString; const AProperty: IPropertyDescriptor);
begin
  _descriptors[Name] := AProperty;
end;

constructor TPropertyDescriptors.Create;
begin
  _descriptors := CDictionary<CString, IPropertyDescriptor>.Create;
end;

function TPropertyDescriptors.get_Property(const Name: CString): IPropertyDescriptor;
begin
  _descriptors.TryGetValue(Name, Result);
end;

{ TPicklist }

function TPicklist.Format(const Item: CObject): CString;
begin

end;

function TPicklist.Items(const Filter: CString): CObject;
begin

end;

function TPicklist.Items(const Pred: Predicate<CObject>): CObject;
begin

end;

end.
