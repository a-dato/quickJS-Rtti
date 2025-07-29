unit App.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Storage.intf,
  App.Windows.intf,
  App.Environment.intf,
  ADato.ObjectModel.List.intf, ADato.AI.SpaceAccessor.intf;

type
  {$M+}
  IAppObject = interface(IBaseInterface)
    ['{0519E4B9-3CD3-4B42-A776-7E62173F5967}']
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Storage(const Name: string): IAppStorage;
    function get_Windows: IWindows;
    function get_SpaceAccessor: ISpaceAccessor;

    function  AddStorage(const DataType: &Type; const Name: string) : IAppStorage;
    function  HasStorage(const Name: string): Boolean;
    function  TryGetStorage(const Name: string; out Value: IAppStorage) : Boolean;
    function  RemoveStorage(const Name: string) : Boolean;

    property Config: IAppConfig read get_Config;
    property Environment: IEnvironment read get_Environment;
    property Storage[const Value: string]: IAppStorage read get_Storage;
    property Windows: IWindows read get_Windows;
    property SpaceAccessor: ISpaceAccessor read get_SpaceAccessor;
  end;

var
  _app: IAppObject;

implementation

end.

