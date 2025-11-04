unit App.Environment.intf;

interface

uses
  System_,
  App.TypeDescriptor.intf,
  App.Windows.intf;

type
  {$M+}
  IEnvironment = interface(IBaseInterface)
    ['{9A009A38-C920-4F43-A1BE-1E1131D918E1}']
    function get_MainForm: IWindow;
    function get_TickCount: Integer;

    function CreateWindow(const AType: &Type; const AOwner: CObject)  : IWindow;

    property MainForm: IWindow read get_MainForm;
    property TickCount: Integer read get_TickCount;
  end;

implementation

end.

