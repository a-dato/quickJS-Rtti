unit App.Factory.impl;

interface

uses
  System_,
  System.Rtti,
  System.Collections.Generic,
  App.Factory.intf;

type
  TAppFactory = class(TBaseInterfacedObject, IAppFactory)
  protected
    class var _id: Int64;
    class var _Instance: IAppFactory;

    var _dict: Dictionary<&Type, TValue>;

    // IAppFactory
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
    procedure ResetID(const Value: Int64);
  public
    constructor Create;

    class property Instance: IAppFactory read _Instance write _Instance;
  end;

implementation

uses
  System.SysUtils;

{ TAppFactory }

constructor TAppFactory.Create;
begin
  _dict := CDictionary<&Type, TValue>.Create(10, TypeEqualityComparer.Create);
end;

procedure TAppFactory.RegisterType(const AType: &Type; const Func: TCreatorFunc_0);
begin
  _dict[AType] := TValue.From<TCreatorFunc_0>(Func);
end;

procedure TAppFactory.RegisterType_1(const AType: &Type; const Func: TCreatorFunc_1);
begin
  _dict[AType] := TValue.From<TCreatorFunc_1>(Func);
end;

procedure TAppFactory.RegisterType_2(const AType: &Type; const Func: TCreatorFunc_2);
begin
  _dict[AType] := TValue.From<TCreatorFunc_2>(Func);
end;

procedure TAppFactory.RegisterType_3(const AType: &Type; const Func: TCreatorFunc_3);
begin
  _dict[AType] := TValue.From<TCreatorFunc_3>(Func);
end;

procedure TAppFactory.RegisterType_4(const AType: &Type; const Func: TCreatorFunc_4);
begin
  _dict[AType] := TValue.From<TCreatorFunc_4>(Func);
end;

function TAppFactory.CreateInstance(const AType: &Type): CObject;
begin
  var func := _dict[AType].AsType<TCreatorFunc_0>();
  Result := func();
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0: CObject): CObject;
begin
  var func := _dict[AType].AsType<TCreatorFunc_1>();
  Result := func(Param0);
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0, Param1: CObject): CObject;
begin
  var func := _dict[AType].AsType<TCreatorFunc_2>();
  Result := func(Param0, Param1);
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0, Param1, Param2: CObject): CObject;
begin
  var func := _dict[AType].AsType<TCreatorFunc_3>();
  Result := func(Param0, Param1, Param2);
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0, Param1, Param2, Param3: CObject): CObject;
begin
  var func := _dict[AType].AsType<TCreatorFunc_4>();
  Result := func(Param0, Param1, Param2, Param3);
end;

function TAppFactory.NextID: Int64;
begin
  dec(_id);
  Result := _id;
end;

procedure TAppFactory.ResetID(const Value: Int64);
begin
  _id := Value;
end;

end.

