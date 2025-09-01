unit App.Factory.intf;

interface

uses
  System_,
  System.SysUtils;

type
  {$M+}
  TCreatorFunc_0 = reference to function: CObject;
  TCreatorFunc_1 = reference to function(const Param0: CObject) : CObject;
  TCreatorFunc_2 = reference to function(const Param0: CObject; const Param1: CObject) : CObject;

  IAppFactory = interface(IBaseInterface)
    ['{ED9DEEB4-F0FD-4996-8CF4-B8B817D2EFCB}']

    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_0); overload;
    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_1); overload;
    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_2); overload;

    function  CreateInstance(const AType: &Type) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject) : CObject; overload;
  end;

implementation

end.

