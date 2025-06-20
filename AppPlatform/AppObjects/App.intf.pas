unit App.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Windows.intf,
  App.Environment.intf,
  QuickJS.Register.dn4d.intf,
  ADato.ObjectModel.List.intf;

type
  {$M+}
  IDataObject = interface(IBaseInterface)
    ['{E17A1549-79AC-4861-AB2F-C446986DEAE6}']
    function  get_Name: string;
    procedure set_Name(const Value: string);

    property Name: string read get_Name write set_Name;
  end;

  {$M+}
  IAppObject = interface(IBaseInterface)
    ['{0519E4B9-3CD3-4B42-A776-7E62173F5967}']
    function  Test(const Value: CObject) : Boolean;

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
