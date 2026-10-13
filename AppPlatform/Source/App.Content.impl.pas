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

    function CreateStorage: IStorage; virtual;
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

function TContentProvider.CreateStorage: IStorage;
begin
  Result := TStorage.Create(_descriptor.GetType, _descriptor.StorageName);
end;

function TContentProvider.Data(const Filter: CObject): IList;
begin

end;

end.

