unit QuickJS.Fetch.impl;

interface

uses
  System_, quickjs;

type
  IPromise<T> = interface
    function get_Value: JSValue;

    property Value: JSValue read get_Value;
  end;

  IResponse = interface

  end;

  IFetch = interface

    function get_Promise: IPromise<IFetch>;

    property Promise: IPromise<IFetch> read get_Promise;
  end;

  TPromise<T> = class(TBaseInterfacedObject, IPromise<T>)
  protected
    _ctx: JSContext;
    _value: JSValue;

    function get_Value: JSValue;

  public
    constructor Create(Ctx: JSContext; argv: PJSValueConstArr);
  end;

  TResponse = class(TBaseInterfacedObject, IResponse)

  end;

  TFetch = class(TBaseInterfacedObject, IFetch)
  protected
    _ctx: JSContext;
    _argv: array of JSValueConst;
    _promise: IPromise<IFetch>;

    function get_Promise: IPromise<IFetch>;

  public
    constructor Create(Ctx: JSContext; argc: Integer; argv: PJSValueConstArr);
    destructor Destroy; override;

    class function job(ctx: JSContext; argc: Integer; argv: PJSValueConst): JSValue; cdecl; static;
  end;

implementation

uses
  System.SysUtils,
  System.Net.HttpClientComponent;

{ TFetch }

constructor TFetch.Create(Ctx: JSContext; argc: Integer; argv: PJSValueConstArr);
begin
  _ctx := Ctx;

  SetLength(_argv, 3 + argc);

  // _argv[0] -> Self
  // _argv[1] -> Resolve()
  // _argv[2] -> Reject()
  _AddRef; // Keep ourselves till job ends

  // Store Self
  var f: IFetch;
  Interfaces.Supports<IFetch>(Self, f);
  _argv[0] := JS_NewInt64(Ctx, Int64(Pointer(f)));
  _promise := TPromise<IFetch>.Create(ctx, @_argv[1]);

  // Copy other args
  for var i := 3 to argc + 2 do
    _argv[i] := JS_DupValue(Ctx, argv[i - 3]);

  JS_EnqueueJob(_ctx, @job, Length(_argv), PJSValueConst(_argv));
end;

destructor TFetch.Destroy;
begin
  inherited;
end;

function TFetch.get_Promise: IPromise<IFetch>;
begin
  result := _promise;
end;

class function TFetch.job(ctx: JSContext; argc: Integer; argv: PJSValueConst): JSValue;
begin
  var f: IFetch;
  var http := TNetHTTPClient.Create(nil);
  try
    // argv[0] -> Self(IFetch)
    // argv[1] -> Resolve()
    // argv[2] -> Reject()
    var i64: Int64;
    JS_ToInt64(ctx, @i64, PJSValueConstArr(argv)[0]);
    f := IFetch(Pointer(i64));

    // Url
    var url: PAnsiChar;
    if (argc >= 4) and JS_IsString(PJSValueConstArr(argv)[3]) then
      url := JS_ToCString(ctx, PJSValueConstArr(argv)[3]) else
      Exit(JS_EXCEPTION);

    // Read options?
    if (argc >= 5) then
    begin
      var arg := PJSValueConstArr(argv)[4];
      if JS_IsObject(arg) then
      begin
        var method := JS_GetPropertyStr(ctx, arg, 'method');
        var headers := JS_GetPropertyStr(ctx, arg, 'headers');
        var classID := JS_GetClassID(headers);

        var body := JS_GetPropertyStr(ctx, arg, 'headers');

      end;

//      var s: PAnsiChar;
//      if JS_IsString(PJSValueConstArr(argv)[4]) then
//      begin
//        s := JS_ToCString(ctx, PJSValueConstArr(argv)[4]);
//      end;
    end;

    var argv_inner: array of JSValueConst;
    SetLength(argv_inner, 1);
    argv_inner[0] := JS_NewInt32(ctx, 32);

    JS_Call(ctx, PJSValueConstArr(argv)[1], JS_UNDEFINED, 1, PJSValueConstArr(argv_inner));
  finally
    TFetch(f)._Release;
    http.Free;
  end;
end;

{ TPromise<T> }

constructor TPromise<T>.Create(Ctx: JSContext; argv: PJSValueConstArr);
begin
  _ctx := Ctx;
  _value := JS_NewPromiseCapability(ctx, PJSValue(argv));
end;

function TPromise<T>.get_Value: JSValue;
begin
  Result := _value;
end;

end.

