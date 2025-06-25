unit QuickJS.Register.dn4d.intf;

interface

uses
  System_,
  System.TypInfo,
  System.SysUtils,
  quickjs,
  QuickJS.Variant, System.Rtti;

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

    function InternalInvoke(const Func: AnsiString; argc:Integer; argv: PJSValueConstArr) : JSValue;
    function get_Ctx: JSContext;
    function get_Value: JSValueConst;

  public
    function Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue; overload;
    function Invoke<T>(const Func: AnsiString) : T; overload;
    function Invoke<T>(const Func: AnsiString; const P1: CObject) : T; overload;
    function Invoke<T>(const Func: AnsiString; const P1, P2: CObject) : T; overload;

    property Ctx: JSContext read get_Ctx;
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

  function JSVariant(const Value: IInterface) : Variant;
  function JSVariantIsNull(const Value: Variant) : Boolean;
  function JSVariantIsUndefined(const Value: Variant) : Boolean;
  function WrapIJSObjectInVirtualInterface(Target: PTypeInfo; obj_ref: JSObjectReference) : TValue;

implementation

uses
  QuickJS.Register.impl, System.Variants;

function JSVariant(const Value: IInterface) : Variant;
begin
  Result := VarJSVariantCreate(Value);
end;

function JSVariantIsNull(const Value: Variant) : Boolean;
begin
  Result := VarJSVariantIsNull(Value);
end;

function JSVariantIsUndefined(const Value: Variant) : Boolean;
begin
  Result := VarJSVariantIsUndefined(Value);
end;

function WrapIJSObjectInVirtualInterface(Target: PTypeInfo; obj_ref: JSObjectReference) : TValue;
begin
  var virtual_interface := TVirtualInterface.Create(Target,
    procedure(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue)
    begin
      // Args[0] = Self

      var name: AnsiString;

      if Method.Name.StartsWith('get_') then
      begin
        // Getter with indexer called (like Object['ID']) --> Call ID property
//        if (Length(Args) > 1) then
//        begin
//          if Args[1].IsType<string> then
//            name := Args[1].AsString
//          else if Args[1].IsType<CString> then
//            name := Args[1].AsType<CString>
//          else
//            raise Exception.Create('Invalid parameter in call to: ' + Method.Name);
//        end else
        name := Method.Name.Substring(4);
      end else
        name := Method.Name;

      var rt: PTypeInfo := nil;
      if Method.ReturnType <> nil then
        rt := Method.ReturnType.Handle;

      Result := obj_ref.Invoke(name, Copy(Args, 1), rt);
    end);

  var ii: IInterface;
  if Interfaces.Supports(virtual_interface, Target.TypeData.GUID, ii) then
    TValue.Make(@ii, Target, Result);
end;


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

function JSObjectReference.get_Ctx: JSContext;
begin
  Result := _jsValue.ctx;
end;

function JSObjectReference.get_Value: JSValueConst;
begin
  Result := _jsValue.Value;
end;

function JSObjectReference.InternalInvoke(const Func: AnsiString; argc: Integer; argv: PJSValueConstArr): JSValue;
begin
  Result := JS_GetPropertyStr(_jsValue.ctx, _jsValue.value, PAnsiChar(Func));
  if not TJSRuntime.Check(_jsValue.ctx) then Exit;

  // Property returns a function, call function to get actual value
  if JS_IsFunction(_jsValue.ctx, Result) then
  begin
    var tmp := Result;
    Result := JS_Call(_jsValue.ctx, tmp, _jsValue.Value, argc, argv);
    JS_FreeValue(_jsValue.ctx, tmp);
    if not TJSRuntime.Check(_jsValue.ctx) then Exit;
  end
  // Call on property with sub-properties
  // obj.ObjectWithProps['SUB-PROPERTY']
  else if (argc = 1) and JS_IsObject(Result) then
  begin
    // Result := JS_GetPropertyStr(_jsValue.ctx, Result, PAnsiChar('Customer'));
    var js_obj := Result;
    var prop_name := JSConverter.Instance.JSValueToTValue(_jsValue.ctx, argv[0], TypeInfo(string));
    if not prop_name.IsEmpty then
    begin
      var s: AnsiString := prop_name.ToString;
      Result := JS_GetPropertyStr(_jsValue.ctx, js_obj, PAnsiChar(s));
    end;
    JS_FreeValue(_jsValue.ctx, js_obj);
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
