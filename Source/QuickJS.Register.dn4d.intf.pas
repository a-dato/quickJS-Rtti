unit QuickJS.Register.dn4d.intf;

interface

uses
  System_,
  System.TypInfo,
  System.SysUtils,
  quickjs, System.Rtti;

type
  IJSRegisteredObject = interface
    function GetType: &Type;
  end;

  IJSCapturedObject = interface
    function ctx: JSContext;
    function value: JSValueConst;
  end;

  JSObjectReference = record
  public
    class var GetTypeFromJSObjectFunc: TFunc<JSContext, JSValueConst, &Type>;

  private
    _jsValue: IJSCapturedObject;

    function  InternalInvoke(const Func: AnsiString; argc:Integer; argv: PJSValueConstArr) : JSValue;
    function get_Value: JSValueConst;

  public
    function Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue; overload;
    function Invoke<T>(const Func: AnsiString) : T; overload;
    function Invoke<T>(const Func: AnsiString; const P1: CObject) : T; overload;
    function Invoke<T>(const Func: AnsiString; const P1, P2: CObject) : T; overload;

    property Value: JSValueConst read get_Value;

  public
    constructor Create(ctx: JSContext; Value: JSValueConst);

    class function Empty: JSObjectReference; static;
    function GetType: &Type;

    // property Items[const Index: Integer]: CObject read get_Items;
  end;

  TCaptureJSObject = class(TInterfacedObject, IJSCapturedObject)
  protected
    _ctx: JSContext;
    _jsValue: JSValueConst;

    function ctx: JSContext;
    function value: JSValueConst;

  public
    constructor Create(Ctx: JSContext; ATarget: JSValueConst);
    destructor Destroy; override;
  end;

implementation

uses
  QuickJS.Register.impl;

{ JSValueRec }

constructor JSObjectReference.Create(ctx: JSContext; Value: JSValueConst);
begin
  _jsValue := TCaptureJSObject.Create(ctx, Value);
end;

class function JSObjectReference.Empty: JSObjectReference;
begin
  Result := default(JSObjectReference);
end;

function JSObjectReference.GetType: &Type;
begin
  Result := GetTypeFromJSObjectFunc(_jsValue.ctx, _jsValue.value);
end;

function JSObjectReference.get_Value: JSValueConst;
begin
  Result := _jsValue.Value;
end;

function JSObjectReference.InternalInvoke(const Func: AnsiString; argc: Integer; argv: PJSValueConstArr): JSValue;
begin
  Result := JS_GetPropertyStr(_jsValue.ctx, _jsValue.value, PAnsiChar(Func));
  if not TJSRuntime.Check(_jsValue.ctx) then Exit;

  if JS_IsFunction(_jsValue.ctx, Result) then
  begin
    var tmp := Result;
    Result := JS_Call(_jsValue.ctx, tmp, _jsValue.Value, argc, argv);
    JS_FreeValue(_jsValue.ctx, tmp);
    if not TJSRuntime.Check(_jsValue.ctx) then Exit;
  end;
end;

function JSObjectReference.Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue;
begin
  var argv: array of JSValueConst;
  SetLength(argv, Length(Args));
  for var i := 0 to High(Args) do
    argv[i] := JSConverter.Instance.TValueToJSValue(_jsValue.ctx, Args[i]);

  var js_val := InternalInvoke(Func, Length(argv), PJSValueConstArr(argv));
  Result := JSConverter.Instance.JSValueToTValue(_jsValue.ctx, js_val, ReturnType);
  JS_FreeValue(_jsValue.ctx, js_val);

  for var i := 0 to High(Args) do
    JS_FreeValue(_jsValue.ctx, argv[i]);
end;

function JSObjectReference.Invoke<T>(const Func: AnsiString): T;
begin
  var js_val := InternalInvoke(Func, 0, nil);
  JSConverter.Instance.JSValueToTValue(_jsValue.ctx, js_val, TypeInfo(T)).TryAsType<T>(Result);
  JS_FreeValue(_jsValue.ctx, js_val);
end;

function JSObjectReference.Invoke<T>(const Func: AnsiString; const P1: CObject): T;
begin
  var argv: array of JSValueConst;
  SetLength(argv, 1);
  argv[0] := JSConverter.Instance.TValueToJSValue(_jsValue.ctx, P1.AsType<System.Rtti.TValue>);

  var js_val := InternalInvoke(Func, 1, PJSValueConstArr(argv));
  JSConverter.Instance.JSValueToTValue(_jsValue.ctx, js_val, TypeInfo(T)).TryAsType<T>(Result);

  JS_FreeValue(_jsValue.ctx, argv[0]);
  JS_FreeValue(_jsValue.ctx, js_val);
end;

function JSObjectReference.Invoke<T>(const Func: AnsiString; const P1, P2: CObject): T;
begin
  var argv: array of JSValueConst;
  SetLength(argv, 2);
  argv[0] := JSConverter.Instance.TValueToJSValue(_jsValue.ctx, P1.AsType<System.Rtti.TValue>);
  argv[1] := JSConverter.Instance.TValueToJSValue(_jsValue.ctx, P1.AsType<System.Rtti.TValue>);

  var js_val := InternalInvoke(Func, 2, PJSValueConstArr(argv));
  JSConverter.Instance.JSValueToTValue(_jsValue.ctx, js_val, TypeInfo(T)).TryAsType<T>(Result);

  JS_FreeValue(_jsValue.ctx, argv[0]);
  JS_FreeValue(_jsValue.ctx, argv[1]);
  JS_FreeValue(_jsValue.ctx, js_val);
end;

{ TCaptureJSObject }

constructor TCaptureJSObject.Create(Ctx: JSContext; ATarget: JSValueConst);
begin
  _ctx := Ctx;
  _jsValue := JS_DupValue(ctx, ATarget);
end;

function TCaptureJSObject.ctx: JSContext;
begin
  Result := _ctx;
end;

destructor TCaptureJSObject.Destroy;
begin
  JS_FreeValue(_ctx, _jsValue);
  inherited;
end;

function TCaptureJSObject.value: JSValueConst;
begin
  Result := _jsValue;
end;

end.
