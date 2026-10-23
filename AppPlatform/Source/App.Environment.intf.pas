unit App.Environment.intf;

interface

uses
  System_,
  App.Component.intf,
  App.Windows.intf, System.Collections.Generic;

type
  IAppActions = interface(IBaseInterface)
    function IsOpen(const AObject: CObject) : Boolean;
    function Open(const AObject: CObject) : Boolean;
    function Close(const AObject: CObject) : Boolean;
  end;

  {$M+}
  IAppPersistence = interface(IBaseInterface)
    ['{2AA39066-C87D-4CF9-BE74-F843CE24E014}']
    function SaveChanges: Boolean;
  end;

  IEnvironment = interface(IAppActions)
    ['{9A009A38-C920-4F43-A1BE-1E1131D918E1}']
    function get_MainWindow: IWindow;
    function get_Windows: List<IWindow>;
    function get_TickCount: Integer;
    function get_Persistence: IAppPersistence;

    function CreateWindow(const AType: &Type; const AOwner: IComponent)  : IWindow;

    property MainWindow: IWindow read get_MainWindow;
    property Windows: List<IWindow> read get_Windows;
    property TickCount: Integer read get_TickCount;
    property Persistence: IAppPersistence read get_Persistence;
  end;

implementation

end.

