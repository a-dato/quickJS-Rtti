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
  ObjectType = class(TBaseInterfacedObject, IObjectType)
  protected
    _binder: IContentBinder;
    _builder: IContentBuilder;
    _provider: IContentProvider;

    function  get_Name: CString; virtual;
    function  get_Binder: IContentBinder; virtual;
    procedure set_Binder(const Value: IContentBinder); virtual;
    function  get_Builder: IContentBuilder; virtual;
    procedure set_Builder(const Value: IContentBuilder); virtual;
    function  get_Provider: IContentProvider; virtual;
    procedure set_Provider(const Value: IContentProvider); virtual;
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

procedure ObjectType.set_Provider(const Value: IContentProvider);
begin
  _provider := Value;
end;

end.
