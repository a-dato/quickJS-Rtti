unit App.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Windows.intf,
  App.Environment.intf,
  ADato.ObjectModel.List.intf;

type
  {$M+}
  IAppObject = interface(IBaseInterface)
    ['{0519E4B9-3CD3-4B42-A776-7E62173F5967}']
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Windows: IWindows;

    property Config: IAppConfig read get_Config;
    property Environment: IEnvironment read get_Environment;
    property Windows: IWindows read get_Windows;
  end;

var
  _app: IAppObject;

implementation

end.

