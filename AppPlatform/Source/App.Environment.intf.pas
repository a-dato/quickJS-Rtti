unit App.Environment.intf;

interface

uses
  System_,
  App.TypeDescriptor.intf,
  App.Windows.intf;

type
  {$M+}
  IApplication = interface(IBaseInterface)

  end;

  {$M+}
  IEnvironment = interface(IBaseInterface)

    function get_Application: IApplication;
    function get_MainForm: IWindow;

    function get_TickCount: Integer;

    function CreateWindowFrame(const AOwner: CObject; const AType: &Type) : IWindowFrame;

    property Application: IApplication read get_Application;
    property MainForm: IWindow read get_MainForm;
    property TickCount: Integer read get_TickCount;
  end;

implementation

end.

