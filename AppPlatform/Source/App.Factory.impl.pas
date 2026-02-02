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

    function GetConstructor(const AType: &Type) : TValue;
    function TryGetConstructor(const AType: &Type; out AConstructor: TValue) : Boolean;
    function GetConstructorFunc<T>(const AType: &Type; const AConstructor: TValue) : T;

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

    function  TryCreateInstance(const AType: &Type; out AObject: CObject) : Boolean; overload;
    function  TryCreateInstance(const AType: &Type; const Param0: CObject; out AObject: CObject) : Boolean; overload;
    function  TryCreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject; out AObject: CObject) : Boolean; overload;
    function  TryCreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject; const Param2: CObject; out AObject: CObject) : Boolean; overload;
    function  TryCreateInstance(const AType: &Type; const Param0: CObject; const Param1: CObject; const Param2: CObject; const Param3: CObject; out AObject: CObject) : Boolean; overload;

  protected
    // Collections
    var _collectiondict: Dictionary<&Type, TValue>;

    function GetCollectionConstructor(const AType: &Type) : TValue;
    function TryGetCollectionConstructor(const AType: &Type; out AConstructor: TValue) : Boolean;
    function GetCollectionConstructorFunc<T>(const AType: &Type; const AConstructor: TValue) : T;

    procedure RegisterCollection(const AType: &Type; const Func: TCreatorFunc_0);   // Returns collection of AType (IList<T>, ICollection<T>)
    function  CreateCollection(const AType: &Type) : CObject; overload;
    function  TryCreateCollection(const AType: &Type; out AObject: CObject) : Boolean; overload;

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
  _collectiondict := CDictionary<&Type, TValue>.Create(10, TypeEqualityComparer.Create);
end;

function TAppFactory.GetConstructor(const AType: &Type) : TValue;
begin
  if not _dict.TryGetValue(AType, Result) then
    raise ETypeNotSupported.Create(AType.Name);
end;

function TAppFactory.TryGetConstructor(const AType: &Type; out AConstructor: TValue) : Boolean;
begin
  Result := _dict.TryGetValue(AType, AConstructor)
end;

function TAppFactory.GetConstructorFunc<T>(const AType: &Type; const AConstructor: TValue) : T;
begin
  if not AConstructor.TryAsType<T>(Result) then
  begin
    if AConstructor.IsType<TCreatorFunc_0> then
      raise EConstructorNotSupported.Create(CString.Format('Constructor for type {0} should be called without arguments', AType.Name))
    else if AConstructor.IsType<TCreatorFunc_1> then
      raise EConstructorNotSupported.Create(CString.Format('Constructor for type {0} requires a single argument', AType.Name))
    else if AConstructor.IsType<TCreatorFunc_2> then
      raise EConstructorNotSupported.Create(CString.Format('Constructor for type {0} requires 2 arguments', AType.Name))
    else if AConstructor.IsType<TCreatorFunc_3> then
      raise EConstructorNotSupported.Create(CString.Format('Constructor for type {0} requires 3 arguments', AType.Name))
    else if AConstructor.IsType<TCreatorFunc_4> then
      raise EConstructorNotSupported.Create(CString.Format('Constructor for type {0} requires 4 arguments', AType.Name));
  end;
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
  Result := GetConstructorFunc<TCreatorFunc_0>(AType, GetConstructor(AType))();
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0: CObject): CObject;
begin
  Result := GetConstructorFunc<TCreatorFunc_1>(AType, GetConstructor(AType))(Param0);
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0, Param1: CObject): CObject;
begin
  Result := GetConstructorFunc<TCreatorFunc_2>(AType, GetConstructor(AType))(Param0, Param1);
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0, Param1, Param2: CObject): CObject;
begin
  Result := GetConstructorFunc<TCreatorFunc_3>(AType, GetConstructor(AType))(Param0, Param1, Param2);
end;

function TAppFactory.CreateInstance(const AType: &Type; const Param0, Param1, Param2, Param3: CObject): CObject;
begin
  Result := GetConstructorFunc<TCreatorFunc_4>(AType, GetConstructor(AType))(Param0, Param1, Param2, Param3);
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

function TAppFactory.TryCreateInstance(const AType: &Type; out AObject: CObject): Boolean;
begin
  var c: TValue;
  if TryGetConstructor(AType, c) then
  begin
    AObject := GetConstructorFunc<TCreatorFunc_0>(AType, c)();
    Result := True;
  end else
    Result := False;
end;

function TAppFactory.TryCreateInstance(const AType: &Type; const Param0: CObject; out AObject: CObject): Boolean;
begin
  var c: TValue;
  if TryGetConstructor(AType, c) then
  begin
    AObject := GetConstructorFunc<TCreatorFunc_1>(AType, c)(Param0);
    Result := True;
  end else
    Result := False;
end;

function TAppFactory.TryCreateInstance(const AType: &Type; const Param0,
  Param1: CObject; out AObject: CObject): Boolean;
begin
  var c: TValue;
  if TryGetConstructor(AType, c) then
  begin
    AObject := GetConstructorFunc<TCreatorFunc_2>(AType, c)(Param0, Param1);
    Result := True;
  end else
    Result := False;
end;

function TAppFactory.TryCreateInstance(const AType: &Type; const Param0, Param1,
  Param2: CObject; out AObject: CObject): Boolean;
begin
  var c: TValue;
  if TryGetConstructor(AType, c) then
  begin
    AObject := GetConstructorFunc<TCreatorFunc_3>(AType, c)(Param0, Param1, Param2);
    Result := True;
  end else
    Result := False;
end;

function TAppFactory.TryCreateInstance(const AType: &Type; const Param0, Param1,
  Param2, Param3: CObject; out AObject: CObject): Boolean;
begin
  var c: TValue;
  if TryGetConstructor(AType, c) then
  begin
    AObject := GetConstructorFunc<TCreatorFunc_4>(AType, c)(Param0, Param1, Param2, Param3);
    Result := True;
  end else
    Result := False;
end;

// Collections
function TAppFactory.GetCollectionConstructor(const AType: &Type) : TValue;
begin
  if not _collectiondict.TryGetValue(AType, Result) then
    raise ETypeNotSupported.Create(AType.Name);
end;

function TAppFactory.TryGetCollectionConstructor(const AType: &Type; out AConstructor: TValue) : Boolean;
begin
  Result := _collectiondict.TryGetValue(AType, AConstructor)
end;

function TAppFactory.GetCollectionConstructorFunc<T>(const AType: &Type; const AConstructor: TValue) : T;
begin
  if not AConstructor.TryAsType<T>(Result) then
    raise EConstructorNotSupported.Create(CString.Format('Constructor for collection of type {0} should be called without arguments', AType.Name))
end;

function TAppFactory.CreateCollection(const AType: &Type): CObject;
begin
  Result := GetCollectionConstructorFunc<TCreatorFunc_0>(AType, GetCollectionConstructor(AType))();
end;

procedure TAppFactory.RegisterCollection(const AType: &Type; const Func: TCreatorFunc_0);
begin
  _collectiondict[AType] := TValue.From<TCreatorFunc_0>(Func);
end;

function TAppFactory.TryCreateCollection(const AType: &Type; out AObject: CObject): Boolean;
begin
  var c: TValue;
  if TryGetCollectionConstructor(AType, c) then
  begin
    AObject := GetCollectionConstructorFunc<TCreatorFunc_0>(AType, c)();
    Result := True;
  end else
    Result := False;
end;

end.

