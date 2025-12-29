unit MainFormFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  quickjs, System.Collections.Generic, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, QuickJS.Register.intf,
  FMX.Memo.Types, System.Actions, FMX.ActnList;

type
  {$M+}
  IResource = interface;

  TForm2 = class(TForm)
    Layout1: TLayout;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Memo3: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    NetHTTPRequest1: TNetHTTPRequest;
    NetHTTPClient1: TNetHTTPClient;
    Button5: TButton;
    ActionList1: TActionList;
    acExecute: TAction;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure NetHTTPRequest1RequestCompleted(const Sender: TObject; const
        AResponse: IHTTPResponse);
  private
    e: TNotifyEvent;
    _context: IJSContext;

    procedure Initialize;
    function  CreateResourceList: List<IResource>;

    { Private declarations }
  public
    procedure LogCallBack(S: string);
    { Public declarations }
  end;

  IResource = interface
    ['{92D88022-DD9D-4DB0-A84A-03C6A8125515}']
    function  get_ID: Integer;
    procedure set_ID(const Value: Integer);
    function  get_Name: string;
    procedure set_Name(const Value: string);

    function GetOwner: IResource;

    property ID: Integer read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

  {$M+}
  TResource = class(TInterfacedObject, IResource)
  protected
    FID: Integer;
    FName: string;
    FOwner: IResource;

    function  get_ID: Integer;
    procedure set_ID(const Value: Integer);
    function  get_Name: string;
    procedure set_Name(const Value: string);

  public
    constructor Create(AID: Integer; AName: string);
    destructor Destroy; override;
    function GetOwner: IResource;

  published
    property ID: Integer read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

  {$M+}
  TUser = class
  private
    FOwner: TUser;
    FName: string;
    FID: Integer;

  public
    constructor Create;
    destructor Destroy; override;
    function Concat(S1, S2: string) : string;
    function GetOwner: TUser;
    function GetResource: IResource;
    function GetResources: List<IResource>;
    function GetArray: TArray<Byte>;

  published
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
  end;

  TTestObject = class
  private
    class var API_Class_id : JSClassID;
    class var API_Class_Proto : JSValue;
    class var JClass : JSClassDef; // = (class_name:'ApiHook';finalizer:nil;gc_mark:nil;call:nil;exotic:nil);
    class var tab : array [0..2] of JSCFunctionListEntry;

    class procedure Register;
  protected
    FName: AnsiString;
    FID: Integer;
    FTT: TTestObject;
    FResource: IResource;

  private
    function  get_TT: TTestObject;
    procedure set_TT(const Value: TTestObject);
    function  get_Resource: IResource;
    procedure set_Resource(const Value: IResource);

  protected
    function  get_Name: AnsiString;
    procedure set_Name(Value: AnsiString);

  public
    function Concat() : string;

  published
    property Name: AnsiString read get_Name write set_Name;
    property ID: Integer read FID write FID;
    property TT: TTestObject read get_TT write set_TT;
    property Resource: IResource read get_Resource write set_Resource;
  end;

var
  exotics: JSClassExoticMethods;
  Form2: TForm2;

implementation

uses
  QuickJS.Register.impl, System.Rtti, System_, ADato.Parser.intf,
  ADato.Parser.impl, System.Diagnostics, System.Threading, XMLHttpRequest.impl,
  System.TypInfo, XMLHttpRequest.intf, Winapi.Windows, System.Math;

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Initialize;
end;

procedure TForm2.acExecuteExecute(Sender: TObject);
begin
  var b: AnsiString := AnsiString(Memo1.Lines.Text);
  // _context.eval_buf(PAnsiChar(b), Length(b), 'lyx', JS_EVAL_TYPE_GLOBAL);
  _context.eval_buf(PAnsiChar(b), Length(b), 'lynx', JS_EVAL_TYPE_GLOBAL or JS_EVAL_TYPE_MODULE);
  //_context.eval_buf(PAnsiChar(b), Length(b), 'application', JS_EVAL_TYPE_GLOBAL);
end;

procedure TForm2.Initialize;
var
  rt: IJSRuntime;
begin
  if _context = nil then
  begin
    rt := TJSRuntime.Create;
    rt.LogString := LogCallBack;
    _context := rt.CreateContext;

    TJSRegister.RegisterObject(_context.ctx, 'XMLHttpRequest', TypeInfo(IXMLHttpRequest), function : Pointer begin Result := TXMLHttpRequest.Create; end);
    TJSRegister.RegisterObject(_context.ctx, 'Resource', TypeInfo(IResource), function : Pointer begin Result := TResource.Create(-1, ''); end);
    TJSRegister.RegisterObject(_context.ctx, 'User', TypeInfo(TUser));

    var u := TUser.Create;
    TJSRegister.RegisterLiveObject(_context.ctx, 'usr', u, True);
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Initialize;
  var b: AnsiString := AnsiString(Memo3.Lines.Text);
  _context.eval_buf(PAnsiChar(b), Length(b), 'application', JS_EVAL_TYPE_MODULE);
end;

function TForm2.CreateResourceList: List<IResource>;
begin
  Result := CList<IResource>.Create;
  for var i := 0 to 999 do
    Result.Add(TResource.Create(i, 'Resource ' + i.ToString));
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  var global := JS_GetGlobalObject(_context.ctx);
  var func := JS_GetPropertyStr(_context.ctx, global, 'func');
  JS_FreeValue(_context.ctx, global);

  if JS_IsFunction(_context.ctx, func) then
  begin
    var argc := 2;

    var argv: PJSValueConstArr := js_malloc(_context.ctx, 2 * SizeOf(JSValue));

    argv[0] := JS_NewInt32(_context.ctx, 20);
    argv[1] := JS_NewInt32(_context.ctx, 40);

    var res := JS_Call(_context.ctx, func, JS_UNDEFINED, argc, argv);
    if JS_IsNumber(res) then
    begin
      var i64: Int64;
      JS_ToInt64(_context.ctx, @i64, res);
    end;

    JS_FreeValue(_context.ctx, argv[0]);
    JS_FreeValue(_context.ctx, argv[1]);
    js_free(_context.ctx, argv);
    Exit;

    var o: CObject := CreateResourceList;

    var st := TStopWatch.Create;
    st.Start;

//    TParallel.&For(0, 100, procedure(I: Integer) begin
//      argv[0] := JSConverter.TValueToJSValue(_context.ctx, o.AsType<TValue>);
//      var res := JS_Call(_context.ctx, func, JS_UNDEFINED, 1 {argc}, PJSValueConstArr(argv));
//
////      if JS_IsNumber(res) then
////      begin
////        var i64: Int64;
////        JS_ToInt64(ctx, @i64, res);
////        if i64 <> 499500 then
////          raise Exception.Create('Invalid value');
////      end else
////        raise Exception.Create('Invalid value');
//
//      JS_FreeValue(_context.ctx, argv[0]);
//      JS_FreeValue(_context.ctx, res);
//    end);

    st.Stop;

    ShowMessage(st.ElapsedMilliseconds.ToString);
  end;

  JS_FreeValue(_context.ctx, func);
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  p: IParser;
begin
  p := TParser.Create;
  p.Context := CreateresourceList;
  p.Expression := 'sum(all(ID))';

  var s := TStopWatch.Create;
  s.Start;
  var o := P.ResultValue;
  s.Stop;

  var i: Int64;
  if not o.TryAsType<Int64>(i) or (i <> 499500) then
      raise Exception.Create('Invalid value');

  ShowMessage(s.ElapsedMilliseconds.ToString);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  var exp := JS_GetException(_context.ctx);

  var t := JS_VALUE_GET_TAG(exp);

  if JS_IsObject(exp) then
  begin
    var msg: string;
    var stack: string;

    var prop := JS_GetPropertyStr(_context.ctx, exp, 'message');
    if not JS_IsNull(prop) then
    begin
      var res := JS_Call(_context.ctx, prop, exp, 0, nil);
      if not JS_IsNull(res) then
      begin
        msg := JS_ToCString(_context.ctx, res);
        JS_FreeValue(_context.ctx, res);
      end;
      JS_FreeValue(_context.ctx, prop);
    end;

    prop := JS_GetPropertyStr(_context.ctx, exp, 'stack');
    if not JS_IsNull(prop) then
    begin
      var res := JS_Call(_context.ctx, prop, exp, 0, nil);
      if not JS_IsNull(res) then
      begin
        stack := JS_ToCString(_context.ctx, res);
        JS_FreeValue(_context.ctx, res);
      end;
      JS_FreeValue(_context.ctx, prop);
    end;

    ShowMessage('Message: ' + msg + #10#13 + 'Stack: ' + stack);
  end;

  JS_FreeValue(_context.ctx, exp);
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  Security : TSecurityAttributes;
  outputPipeRead, outputPipeWrite: THandle;

begin
  Security.nlength := SizeOf(TSecurityAttributes) ;
  Security.binherithandle := true;
  Security.lpsecuritydescriptor := nil;
  if CreatePipe(outputPipeRead, outputPipeWrite, @Security, 0) then
    SetStdHandle(STD_OUTPUT_HANDLE, outputPipeWrite) else
    ShowMessage ('Error in creating pipe');

  TThread.CreateAnonymousThread(procedure begin
    while True do
    try
      Sleep(100);

      Flush(output);

      var bytesavail: Cardinal;

      PeekNamedPipe(outputPipeRead, nil, 0, nil, @bytesavail, nil);
      while bytesavail > 0 do
      begin
        var buffer: array[0..500] of AnsiChar;
        var size := Min(500 - 1, bytesavail);
        var bytesread: Cardinal;
        if ReadFile(outputPipeRead, buffer, size, bytesread, nil) then
        begin
          bytesavail := bytesavail - bytesread;

          buffer[bytesread] := #0;
          var s := AnsiString(buffer);
          TThread.Queue(nil, procedure begin
            Memo2.Lines.Add(s);
          end);
        end;
      end
    except
      on E: Exception do
        TThread.Queue(nil, procedure begin
          Memo2.Lines.Add(E.Message);
        end);
    end;
  end).Start;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  WriteLn('Hello from here');
end;

procedure TForm2.LogCallBack(S: string);
begin
  Memo2.Lines.Add(S);
end;

procedure TForm2.NetHTTPRequest1RequestCompleted(const Sender: TObject; const
    AResponse: IHTTPResponse);
begin

end;

{ TTestObject }
function CConstructor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; cdecl;
begin
  var s := JS_ToCString(ctx, new_target);
  if s = '' then;

  Result := JS_NewObjectClass(ctx, TTestObject.API_Class_id);
  JS_SetOpaque(Result, TTestObject.Create);
//  // New Array for every new instance.
//  JS_DefinePropertyValueStr(ctx,Result,'args',JS_NewArray(ctx),JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE);
end;

procedure CFinalize(rt : JSRuntime; this_val : JSValue); cdecl;
begin
  var p := JS_GetOpaque(this_val, TTestObject.API_Class_id);
  if p <> nil then
    TObject(p).Free;
end;

function CGetID(ctx: JSContext; this_val:JSValueConst; magic:Integer) : JSValue; cdecl;
begin
  var p := JS_GetOpaque2(ctx, this_val, TTestObject.API_Class_id);
  if p = nil then
    Result := JS_EXCEPTION else
    Result := JS_NewInt32(ctx, TTestObject(p).ID);
end;

function CSetID(ctx: JSContext; this_val:JSValueConst; val:JSValueConst; magic:Integer) : JSValue; cdecl;
begin
  var p := JS_GetOpaque2(ctx, this_val, TTestObject.API_Class_id);
  var i: Integer;

  if (p = nil) or (JS_ToInt32(ctx, @i, val) <> 0) then
      Result := JS_EXCEPTION
  else begin
    TTestObject(p).ID := i;
    Result := JS_UNDEFINED;
  end;
end;

function CConcat(ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConst; magic : Integer) : JSValue; cdecl;
begin
  var p := JS_GetOpaque2(ctx, this_val, TTestObject.API_Class_id);

end;

function CGetTT(ctx: JSContext; this_val:JSValueConst; magic:Integer) : JSValue; cdecl;
begin
  var p := JS_GetOpaque2(ctx, this_val, TTestObject.API_Class_id);
  if p = nil then
    Result := JS_EXCEPTION
  else
  begin
    Result := JS_NewObjectClass(ctx, TTestObject.API_Class_id);
    // Result := JS_NewObjectProto(ctx, TTestObject.API_Class_Proto);
    JS_SetOpaque(Result, TTestObject(p).TT);

//    JS_SetClassProto(ctx, TTestObject.API_Class_id, Result);
//    // Set list of Properties to the prototype Object.
//    JS_SetPropertyFunctionList(ctx,Result,@TTestObject.tab,Length(TTestObject.tab));

    // Set the Prototype to the Class.
    // JS_SetClassProto(ctx, API_Class_id, API_Class_Proto);

    // Result := JS_NewInt32(ctx, TTestObject(p).ID);
  end;
end;

function CSetTT(ctx: JSContext; this_val:JSValueConst; val:JSValueConst; magic:Integer) : JSValue; cdecl;
begin
  var p := JS_GetOpaque2(ctx, this_val, TTestObject.API_Class_id);
  var i: Integer;

  if (p = nil) or (JS_ToInt32(ctx, @i, val) <> 0) then
      Result := JS_EXCEPTION
  else begin
    TTestObject(p).ID := i;
    Result := JS_UNDEFINED;
  end;
end;

function JSClassCall(ctx : JSContext; func_obj : JSValueConst; this_val : JSValueConst;
                              argc : Integer; argv : PJSValueConst) : JSValue; cdecl;
begin
  Result := JS_UNDEFINED;
end;

function get_property(ctx: JSContext; this_val:JSValueConst; atom:JSAtom; receiver:JSValueConst):JSValue;cdecl;
begin
  Result := JS_NewInt64(ctx, 10001);
end;

function set_property(ctx: JSContext; this_val:JSValueConst; atom:JSAtom; value:JSValueConst; receiver:JSValueConst; flags:Integer):Integer; cdecl;
begin
  Result := 0;
end;

function xmlhttprequest_open(ctx : JSContext; this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
begin
  // Result := JS_
end;

function TTestObject.get_Resource: IResource;
begin
  if FResource = nil then
    FResource := TResource.Create(-1, 'No name');

  Result := FResource;
end;

function TTestObject.get_TT: TTestObject;
begin
  if FTT = nil then
    FTT := TTestObject.Create;

  Result := FTT;
end;

class procedure TTestObject.Register;
var
  obj,global, obj2 : JSValue;

begin
//  FillChar(exotics, sizeof(exotics), #0);
//
//  exotics.get_property := get_property;
//  exotics.set_property := set_property;
//
//  JClass.class_name := 'TestObject';
//  JClass.finalizer := @CFinalize;
//  JClass.gc_mark := nil;
//  JClass.call := nil; //@JSClassCall;
//  JClass.exotic := nil; // @exotics;
//
//  // Create New Class id.
//  JS_NewClassID(@API_Class_id);
//
//  // Create the Class Name and other stuff.
//  JS_NewClass(JS_GetRuntime(_context.ctx),API_Class_id,@JClass);
//
//  // New Object act as Prototype for the Class.
//  API_Class_Proto := JS_NewObject(ctx);
//
//  JS_SetPropertyStr(ctx, API_Class_Proto, 'open', JS_NewCFunction(ctx, @xmlhttprequest_open, 'open', 2));
//
//  JS_DefinePropertyValueStr(ctx, API_Class_Proto, 'readyState', JS_NewInt64(ctx, 21),
//    JS_PROP_HAS_CONFIGURABLE or JS_PROP_CONFIGURABLE or JS_PROP_HAS_WRITABLE or JS_PROP_HAS_VALUE);
//
////  // Properties list.
////  //  tab[0] := JS_PROP_INT32_DEF('ID', 1337, JS_PROP_CONFIGURABLE);
//  tab[0] := JS_CGETSET_MAGIC_DEF('ID', CGetID, CSetID, 1);
//  tab[1] := JS_CGETSET_MAGIC_DEF('TT', CGetTT, CSetTT, 2);
//  tab[2] := JS_CFUNC_MAGIC_DEF('Concat', 0, CConcat, 11);
//
//  // Set list of Properties to the prototype Object.
//  JS_SetPropertyFunctionList(ctx,API_Class_Proto,@tab,Length(tab));
//
//  // Set the Prototype to the Class.
//  JS_SetClassProto(ctx, API_Class_id, API_Class_Proto);
//
//  // Set the Class native constructor.
//  obj := JS_NewCFunction2(ctx, @CConstructor, 'TestObject', 1, JS_CFUNC_constructor, 0);
//
//  // Add the Class to Global Object so we can use it.
//  global := JS_GetGlobalObject(ctx);
//  JS_SetPropertyStr(ctx,global,'TestObject',obj);
//  JS_FreeValue(ctx,global);
end;

function TTestObject.Concat: string;
begin
  Result := 'Concat';
end;

function TTestObject.get_Name: AnsiString;
begin
  Result := FName;
end;

procedure TTestObject.set_Name(Value: AnsiString);
begin
  FName := Value;
end;

procedure TTestObject.set_Resource(const Value: IResource);
begin
  FResource := Value;
end;

procedure TTestObject.set_TT(const Value: TTestObject);
begin
  FTT.Free;
  FTT := Value;
end;

{ TResource }

constructor TResource.Create(AID: Integer; AName: string);
begin
  FID := AID;
  FName := AName;
end;

destructor TResource.Destroy;
begin
  inherited;
end;

function TResource.GetOwner: IResource;
begin
  Result := TResource.Create(FID + 1, 'Owner of: ' + FName);
end;

function TResource.get_ID: Integer;
begin
  Result := FID;
end;

function TResource.get_Name: string;
begin
  Result := FName;
end;

procedure TResource.set_ID(const Value: Integer);
begin
  FID := Value;
end;

procedure TResource.set_Name(const Value: string);
begin
  FName := Value;
end;

{ TUser }

function TUser.Concat(S1, S2: string): string;
begin
  Result := S1 + S2;
end;

constructor TUser.Create;
begin
  FName := 'TUser.Name: ' + CDateTime.Now.ToString;
  // + Char($20A1) + ' ' + Char($2211);
end;

destructor TUser.Destroy;
begin
  inherited;
end;

function TUser.GetArray: TArray<Byte>;
begin
  var ts := TStringStream.Create('Hello', TEncoding.Unicode);
  var arr: TArray<Byte>;
  SetLength(arr, ts.Size);
  ts.Position := 0;
  ts.Read(arr, Length(arr));
  ts.Free;
  Result := arr;
end;

function TUser.GetOwner: TUser;
begin
  if FOwner = nil then
  begin
    FOwner := TUser.Create;
    FOwner.ID := FID + 1;
    FOwner.Name := 'Owner of: ' + FName;
  end;

  Result := FOwner;
end;

function TUser.GetResource: IResource;
begin
  Result := TResource.Create(-1, 'Resource for user: ' + Name);
end;

function TUser.GetResources: List<IResource>;
begin
  Result := CList<IResource>.Create;

  for var i := 0 to 10 do
  begin
    var r: IResource := TResource.Create(i, 'Resource ' + i.ToString);
    Result.Add(r);
  end;
end;

end.

