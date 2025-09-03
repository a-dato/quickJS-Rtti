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
  TCreatorFunc_3 = reference to function(const Param0: CObject; const Param1: CObject; const Param2: CObject) : CObject;
  TCreatorFunc_4 = reference to function(const Param0: CObject; const Param1: CObject; const Param2: CObject; const Param3: CObject) : CObject;

  IAppFactory = interface(IBaseInterface)
    ['{ED9DEEB4-F0FD-4996-8CF4-B8B817D2EFCB}']

    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_0);
    procedure RegisterType_1(const AType: &Type; const Func: TCreatorFunc_1);
    procedure RegisterType_2(const AType: &Type; const Func: TCreatorFunc_2);
    procedure RegisterType_3(const AType: &Type; const Func: TCreatorFunc_3);
    procedure RegisterType_4(const AType: &Type; const Func: TCreatorFunc_4);

    function  CreateInstance(const AType: &Type) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject; const Param2: CObject) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject; const Param2: CObject; const Param3: CObject) : CObject; overload;

    function  NextID: Int64;
  end;

implementation

end.

