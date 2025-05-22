unit App.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Config.intf,
  App.Models.intf,
  App.Windows.intf,
  App.Environment.intf,
  QuickJS.Register.dn4d.intf,
  ADato.ObjectModel.List.intf;

type
  {$M+}
  IAppObject = interface(IBaseInterface)
    ['{0519E4B9-3CD3-4B42-A776-7E62173F5967}']
    function  Test(const Data: CObject; const Index: Integer) : CObject;
    procedure Register(const Value: IJSObjectReference);
    procedure RegisterType(const AType: &Type);
    procedure Show(const Value: IObjectListModel);

    function get_AppModels: IAppModels;
    function get_Config: IAppConfig;
    function get_Environment: IEnvironment;
    function get_Windows: IWindows;

    property Config: IAppConfig read get_Config;
    property Environment: IEnvironment read get_Environment;
    property Models: IAppModels read get_AppModels;
    property Windows: IWindows read get_Windows;
  end;

implementation

end.
