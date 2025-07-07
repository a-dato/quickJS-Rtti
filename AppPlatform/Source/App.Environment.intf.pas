unit App.Environment.intf;

interface

uses
  System_,
  App.TypeDescriptor.intf,
  App.Windows.intf;

type
  {$M+}
  IEnvironment = interface(IBaseInterface)

    function get_TickCount: Integer;

    function CreateWindowFrame(const AOwner: CObject; const AType: &Type) : IWindowFrame;

    property TickCount: Integer read get_TickCount;
  end;

implementation

end.

