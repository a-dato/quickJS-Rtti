unit App.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Storage.intf,
  App.Windows.intf,
  App.Environment.intf,
  ADato.ObjectModel.List.intf,
  App.Factory.intf;

type
  {$M+}
  IAppObject = interface(IStorageSupport)
    ['{0519E4B9-3CD3-4B42-A776-7E62173F5967}']
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Factory: IAppFactory;

    property Config: IAppConfig read get_Config;
    property Environment: IEnvironment read get_Environment;
    property Factory: IAppFactory read get_Factory;
  end;

var
  _app: IAppObject;

implementation

end.

