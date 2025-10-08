unit QuickJS.VirtualMethod.impl;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections, QuickJS.Register.intf;

type
  { TVirtualMethodImplementation: Creates an implementation of an interface at runtime.
    All methods in the Interface are marshaled through a generic stub function
    that raises the OnInvoke event.}
  TVirtualMethodImplementation = class(TInterfacedObject, IInterface)
  private type
    { TImplInfo: Helper class that keeps a reference to the TRttiMethod metadata
      as well as the implemention of the stub function }
    TImplInfo = class
    private
      FImpl: TMethodImplementation;
      FMethod: TRttiMethod;
      function GetCodeAddress: Pointer;
        function GetVirtualIndex: SmallInt;
    public
      constructor Create(AMethod: TRttiMethod;
        const ACallback: TMethodImplementationCallback);
      destructor Destroy; override;
      property CodeAddress: Pointer read GetCodeAddress;
      property VirtualIndex: SmallInt read GetVirtualIndex;
    end;

  private
    FCallback: IJsObject;
    VTable: PPointer;       // Constructed VTable for Interface
    FContext: TRttiContext; // Local reference to Context so metadata doesn't expire
    FIntercept: TImplInfo;

    { Functions for the generated VTable. When these are invoked, the Self
      pointer is pointing to the generated VTable. In order to access members
      of the TVirtualMethodImplementation class (such as the reference count), this pointer
      has to be adjusted before actually performing these actions}
    function _AddRefFromIntf: Integer; stdcall;
    function _ReleaseFromIntf: Integer; stdcall;
    function _QIFromIntf(const IID: TGUID; out Obj): HResult; stdcall;

    // Stub function called by all methods in the interface.
    procedure RawCallback(UserData: Pointer {TImplInfo}; const Args: TArray<TValue>;
      out Result: TValue); virtual;
    procedure ErrorProc;
  protected
    // IInterface methods. Make them virtual so derived classes can do their own
    // lifetime management if needed.
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    // function  Func : T;

  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    constructor Create(AType: PTypeInfo; const CallbackFunc: IJSObject); overload;

    destructor Destroy; override;
  end;

{$POINTERMATH ON}
  PVtablePtr = ^Pointer;
{$POINTERMATH OFF}
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

implementation

uses
  System.RTLConsts, System.SysUtils;

{ TVirtualMethodImplementation }

constructor TVirtualMethodImplementation.Create(AType: PTypeInfo; const CallbackFunc: IJSObject);
var
  method: TRttiMethod;

begin
  inherited Create;

  FCallback := CallbackFunc;

  FContext := TRttiContext.Create;

  // 'reference methods types' have only one method -> 'Invoke'
  var methods := FContext.GetType(AType).GetMethods;
  if Length(methods) < 1 then
    raise Exception.Create('Cannot get ''Invoke'' method for type: ' + AType.Name);

  method := methods[0];
  FIntercept := TImplInfo.Create(method, RawCallBack);

  VTable := AllocMem(SizeOf(Pointer) * 4);
  PVtablePtr(VTable)[0] := @TVirtualMethodImplementation._QIFromIntf;
  PVtablePtr(VTable)[1] := @TVirtualMethodImplementation._AddRefFromIntf;
  PVtablePtr(VTable)[2] := @TVirtualMethodImplementation._ReleaseFromIntf;
  PVtablePtr(VTable)[3] := FIntercept.CodeAddress;

//  for I := 0 to FIntercepts.Count-1 do
//    PVtablePtr(VTable)[FIntercepts[I].VirtualIndex] := FIntercepts[I].CodeAddress;
//  for I := 3 to MaxIndex do
//    if PVtablePtr(VTable)[I] = nil then
//      PVtablePtr(VTable)[I] := @TVirtualMethodImplementation.ErrorProc;
end;

destructor TVirtualMethodImplementation.Destroy;
begin
  if VTable <> nil then
    FreeMem(VTable);

  inherited;
end;

procedure TVirtualMethodImplementation.RawCallback(UserData: Pointer; const Args: TArray<TValue>; out Result: TValue);
begin
  var m := TImplInfo(UserData).FMethod;
  if m.ReturnType <> nil then
    FCallback.Call(Copy(Args, 1), m.ReturnType.Handle) else
    FCallback.Call(Copy(Args, 1), nil);
end;

procedure TVirtualMethodImplementation.ErrorProc;
begin
  raise EInsufficientRtti.Create(SInsufficientRtti) at ReturnAddress;
end;

procedure TVirtualMethodImplementation.Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  var name: AnsiString;
  name := Method.Name;
  Result := name;
end;

//function TVirtualMethodImplementation.Func : T;
//begin
//  var ii: IInterface;
//  var v: TValue;
//  QueryInterface(IInterface, ii);
//  TValue.Make(@ii, TypeInfo(T), v);
//  Result := v.AsType;
//end;

function TVirtualMethodImplementation._AddRefFromIntf: Integer;
begin
  Result := TVirtualMethodImplementation(PByte(Self) -
    (PByte(@Self.VTable) - PByte(Self)))._AddRef;
end;

function TVirtualMethodImplementation._ReleaseFromIntf: Integer;
begin
  Result := TVirtualMethodImplementation(PByte(Self) -
    (PByte(@Self.VTable) - PByte(Self)))._Release;
end;

function TVirtualMethodImplementation._QIFromIntf(const IID: TGUID; out Obj): HResult;
begin
  Result := TVirtualMethodImplementation(PByte(Self) -
    (PByte(@Self.VTable) - PByte(Self))).QueryInterface(IID, Obj);
end;

function TVirtualMethodImplementation._AddRef: Integer;
begin
  Result := inherited
end;

function TVirtualMethodImplementation._Release: Integer;
begin
  Result := inherited
end;

function TVirtualMethodImplementation.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  _AddRef;
  Pointer(Obj) := @VTable;
  Result := S_OK;
end;

{ TVirtualMethodImplementation.TImplInfo }

constructor TVirtualMethodImplementation.TImplInfo.Create(AMethod: TRttiMethod;
  const ACallback: TMethodImplementationCallback);
begin
  FImpl := AMethod.CreateImplementation(Pointer(Self), ACallback);
  FMethod := AMethod;
end;

destructor TVirtualMethodImplementation.TImplInfo.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TVirtualMethodImplementation.TImplInfo.GetCodeAddress: Pointer;
begin
  Result := FImpl.CodeAddress;
end;

function TVirtualMethodImplementation.TImplInfo.GetVirtualIndex: SmallInt;
begin
  Result := FMethod.VirtualIndex;
end;

end.
