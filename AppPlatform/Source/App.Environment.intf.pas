unit App.Environment.intf;

interface

uses
  System_,
  App.Component.intf,
  App.Windows.intf;

type
  IAppActions = interface(IBaseInterface)
    function IsOpen(const AObject: CObject) : Boolean;
    function Open(const AObject: CObject) : Boolean;
    function Close(const AObject: CObject) : Boolean;
  end;

  {$M+}
  IEnvironment = interface(IAppActions)
    ['{9A009A38-C920-4F43-A1BE-1E1131D918E1}']
    function get_MainWindow: IWindow;
    function get_TickCount: Integer;

    function CreateWindow(const AType: &Type; const AOwner: IComponent)  : IWindow;

    property MainWindow: IWindow read get_MainWindow;
    property TickCount: Integer read get_TickCount;
  end;

implementation

end.

