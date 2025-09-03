unit App.Content.impl;

interface

uses
  System_,
  System.Collections,
  App.TypeDescriptor.intf,
  App.Storage.intf,
  App.Content.intf;

type
  TContentProvider = class(TBaseInterfacedObject, IContentProvider)
  protected
    [unsafe]_descriptor: ITypeDescriptor;

    function CreateInstance : CObject; overload; virtual;
    function CreateInstance(const Param1: CObject) : CObject; overload; virtual;
    function CreateInstance(const Param1: CObject; const Param2: CObject) : CObject; overload; virtual;
    function CreateInstance(const Param1: CObject; const Param2: CObject; const Param3: CObject) : CObject; overload; virtual;
    function CreateInstance(const Param1: CObject; const Param2: CObject; const Param3: CObject; const Param4: CObject) : CObject; overload; virtual;

    function CreateStorage(const Name: string): IAppStorage; virtual;
    function Data(const Filter: CObject): IList; virtual;
  public
    constructor Create(const Descriptor: ITypeDescriptor);
  end;

implementation

uses
  App.intf, App.Storage.impl;

{ TContentProvider }
constructor TContentProvider.Create(const Descriptor: ITypeDescriptor);
begin
  inherited Create;
  _descriptor := Descriptor;
end;

function TContentProvider.CreateInstance: CObject;
begin
  Result := _app.Factory.CreateInstance(_descriptor.GetType);
end;

function TContentProvider.CreateInstance(const Param1: CObject): CObject;
begin
  Result := _app.Factory.CreateInstance(_descriptor.GetType, Param1);
end;

function TContentProvider.CreateInstance(const Param1, Param2: CObject): CObject;
begin
  Result := _app.Factory.CreateInstance(_descriptor.GetType, Param1, Param2);
end;

function TContentProvider.CreateInstance(const Param1, Param2, Param3: CObject): CObject;
begin
  Result := _app.Factory.CreateInstance(_descriptor.GetType, Param1, Param2, Param3);
end;

function TContentProvider.CreateInstance(const Param1, Param2, Param3, Param4: CObject): CObject;
begin
  Result := _app.Factory.CreateInstance(_descriptor.GetType, Param1, Param2, Param3, Param4);
end;

function TContentProvider.CreateStorage(const Name: string): IAppStorage;
begin
  Result := TAppStorage.Create(_descriptor.GetType, Name);
end;

function TContentProvider.Data(const Filter: CObject): IList;
begin

end;

end.

