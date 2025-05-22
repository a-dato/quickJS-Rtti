unit QuickJS.Register.dn4d.intf;

interface

uses
  System_,
  System.TypInfo,
  quickjs;

type
  IJSObjectReference = interface(IBaseInterface)
    ['{75E40F2F-F7A1-4467-96F9-65887F1B800A}']
    function get_Reference: JSValueConst;

    function Invoke(const Func: AnsiString; ResultType: PTypeInfo) : CObject; overload;
    function Invoke(const Func: AnsiString; const P1: CObject; ResultType: PTypeInfo) : CObject; overload;
    function Invoke(const Func: AnsiString; const P1, P2: CObject; ResultType: PTypeInfo) : CObject; overload;

    property Reference: JSValueConst read get_Reference;
  end;

  IJSRegisteredObject = interface
    function GetType: &Type;
  end;

  IJSCapturedObject = interface
    function Ctx: JSContext;
    function Target: JSValueConst;
  end;

implementation

end.
