unit App.Factory.impl;

interface

uses
  System_,
  System.Rtti,
  System.Collections.Generic,
  App.Factory.intf;

type
  TCreatorFunc<T> = reference to function : T;
  TCreatorFunc<T, P0> = reference to function(const Param0: P0) : T;
  TCreatorFunc<T, P0, P1> = reference to function(const Param0: P0; const Param1: P1) : T;

  TAppFactory = class(TBaseInterfacedObject, IAppFactory)
  protected
    class var _Instance: IAppFactory;

    var _dict: Dictionary<&Type, TValue>;

    class function get_GenericInstance: TAppFactory; static;

    // IAppFactory
    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_0); overload;
    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_1); overload;
    procedure RegisterType(const AType: &Type; const Func: TCreatorFunc_2); overload;

    function  CreateInstance(const AType: &Type) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject) : CObject; overload;
    function  CreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject) : CObject; overload;

  public
    constructor Create;

    procedure RegisterType<T>(const Func: TCreatorFunc<T>); overload;
    procedure RegisterType<T, P0>(const Func: TCreatorFunc<T, P0>); overload;
    procedure RegisterType<T, P0, P1>(const Func: TCreatorFunc<T, P0, P1>); overload;

    function  CreateInstance<T> : T; overload;
    function  CreateInstance<T, P0>(const Param0: P0) : T; overload;
    function  CreateInstance<T, P0, P1>(const Param0: P0; const Param1: P1) : T; overload;

    class property Instance: IAppFactory read _Instance write _Instance;
    class property Generic: TAppFactory read get_GenericInstance;
  end;

implementation

uses
  System.SysUtils;

{ TAppFactory }

class function TAppFactory.get_GenericInstance: TAppFactory;
begin
  Result := TAppFactory(_Instance);
end;

constructor TAppFactory.Create;
begin
  _dict := CDictionary<&Type, TValue>.Create;
end;

procedure TAppFactory.RegisterType(const AType: &Type; const Func: TCreatorFunc_0);
begin
  _dict[AType] := TValue.From<TCreatorFunc_0>(Func);
end;

procedure TAppFactory.RegisterType(const AType: &Type; const Func: TCreatorFunc_1);
begin
  _dict[AType] := TValue.From<TCreatorFunc_1>(Func);
end;

procedure TAppFactory.RegisterType(const AType: &Type; const Func: TCreatorFunc_2);
begin
  _dict[AType] := TValue.From<TCreatorFunc_2>(Func);
end;

function TAppFactory.CreateInstance(const AType: &Type): CObject;
begin
  var func := _dict[AType];
  var cf_0: TCreatorFunc_0;
  if func.TryAsType<TCreatorFunc_0>(cf_0) then
    Result := cf_0();
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

procedure TAppFactory.RegisterType<T>(const Func: TCreatorFunc<T>);
begin
  var tp := &Type.From<T>;
  _dict[tp] := TValue.From<TCreatorFunc<T>>(Func);
end;

procedure TAppFactory.RegisterType<T, P0>(const Func: TCreatorFunc<T, P0>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T, P0>>(Func);
  {$ELSE}
  _dict[&Type.From<T>] := TValue.From<TCreatorFunc<T, P0>>(Func);
  {$ENDIF}
end;

procedure TAppFactory.RegisterType<T, P0, P1>(const Func: TCreatorFunc<T, P0, P1>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T, P0, P1>>(Func);
  {$ELSE}
  _dict[&Type.From<T>] := TValue.From<TCreatorFunc<T, P0, P1>>(Func);
  {$ENDIF}
end;

function TAppFactory.CreateInstance<T>: T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T>>();
  {$ELSE}
  var func := _dict[&Type.From<T>].AsType<TCreatorFunc<T>>();
  {$ENDIF}
  Result := func();
end;

function TAppFactory.CreateInstance<T, P0>(const Param0: P0): T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T, P0>>();
  {$ELSE}
  var func := _dict[&Type.From<T>].AsType<TCreatorFunc<T, P0>>();
  {$ENDIF}
  Result := func(Param0);
end;

function TAppFactory.CreateInstance<T, P0, P1>(const Param0: P0; const Param1: P1): T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T, P0, P1>>();
  {$ELSE}
  var func := _dict[&Type.From<T>].AsType<TCreatorFunc<T, P0, P1>>();
  {$ENDIF}
  Result := func(Param0, Param1);
end;

end.

