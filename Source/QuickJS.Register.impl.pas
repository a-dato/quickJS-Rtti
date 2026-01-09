unit QuickJS.Register.impl;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  
  QuickJS.Register.intf, QuickJS.Register.ObjectBridge.intf,
  QuickJS.Register.PropertyDescriptors.impl,
  QuickJS.Register.PropertyDescriptors.intf, quickjs_ng;

type
  TProc_0 = TProc;
  TProc_1 = TProc<Pointer>;
  TProc_Double = TProc<Double>;

  // Forward declare TRegistrationInfo before TJSRuntime
  TRegistrationInfo = record
    ClassName: string;
    TypeInfo: PTypeInfo;
    ConstructorFunc: TObjectConstuctor;
  end;

  TJSRuntime = class;  // Forward declaration

  TJSObject = class(TInterfacedObject, IJSObject)
  protected
    _Runtime: TJSRuntime;
    _ctx: JSContext;
    _value: JSValueConst;
    _ImplementingInterfaces: TList<TInterfaceRef>;

    function get_Ctx: JSContext;
    function get_Value: JSValueConst;

    function QueryInterface(const IID: TGUID; out Obj): HResult; reintroduce; stdcall;

    function InternalInvoke(const Func: AnsiString; argc: Integer; argv: PJSValueConstArr): JSValue;
    function InternalCall(const this_obj: JSValueConst; const func_obj: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue;

    function Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo): TValue;
    function Call(const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue; overload;
    function Call(const JSFunc: JSValueConst; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue; overload;

  public
    constructor Create(const Runtime: TJSRuntime; const Ctx: JSContext; const Value: JSValueConst);
    destructor  Destroy; override;
  end;


  TJSRuntime = class(TInterfacedObject, IJSRuntime)
  protected
    var _rt: JSRuntime;
    var _LogString: TProc<string>;

    // Per-runtime instance state
    var FAutoRegister: Boolean;
    var FExotics: JSClassExoticMethods;
    var FRegisteredObjectsByClassID: TDictionary<JSClassID, IRegisteredObject>;
    var FRegisteredObjectsByType: TDictionary<PTypeInfo, IRegisteredObject>;
    var FRegisteredObjectsByConstructor: TDictionary<JSValueConst, IRegisteredObject>;
    var FRegisteredJSObjects: TDictionary<JSValueConst, IRegisteredObject>;
    var FRegisteredInterfaces: TDictionary<TGuid, IRegisteredObject>;
    var FObjectBridgeResolver: IObjectBridgeResolver;
    var FOwnedContexts: TList<Pointer {Unsafe IJSContext pointer}>;
    var FPendingRegistrations: TList<TRegistrationInfo>;
    var FRegisteredEnums: TDictionary<PTypeInfo, string>;
    var FCustomObjectFactory: TCustomObjectFactory;
    var FFirst_ClassID: JSClassID;

    function  get_rt: JSRuntime;
    function  get_LogString: TProc<string>;
    procedure set_LogString(const Value: TProc<string>);
    function  get_ObjectBridgeResolver: IInterface;
    function  get_AutoRegister: Boolean;
    procedure set_AutoRegister(const Value: Boolean);
    procedure OutputLog(const Value: string);

    class function get_Context(const Value: JSContext) : IJSContext; static;

    procedure InitializeExotics;
    procedure AddRegisteredObjectWithClassID(const RegisteredObject: IRegisteredObject);
    procedure InternalRegisterType(const ctx: IJSContext; const Reg: IRegisteredObject; ClassName: string); virtual;
    procedure DoInternalRegisterType(const ctx: IJSContext; const Reg: IRegisteredObject; ClassName: string);
    procedure InternalRegisterInterface(const ctx: IJSContext; const Reg: IRegisteredObject; ClassName: string); virtual;
    procedure InternalRegisterEnum(const ctx: IJSContext; EnumTypeInfo: PTypeInfo; const EnumJSName: string); virtual;
    function  CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject; virtual;
    function  CreateRegisteredJSObject(ctx: JSContext; JSConstructor: JSValueConst; ATypeInfo: PTypeInfo): IRegisteredObject; virtual;

    // Callback methods for QuickJS - must be static for cdecl callbacks
    class function GenericInvokeCallBack(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericGetItem(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericSetItem(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericClassIterator(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue): JSValue; cdecl; static;
    class function GenericIteratorNext(ctx : JSContext; this_val : JSValueConst;
      argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
    class function GenericMethodCallData(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericPropertyGetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericPropertySetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericIndexedPropertyGetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericIndexedPropertySetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function ExtendablePropertyGetter(ctx: JSContext; obj: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function ExtendablePropertySetter(ctx: JSContext; obj: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;

    class function  CConstructor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr; magic : Integer): JSValue; cdecl; static;
    class procedure CFinalize(rt : JSRuntime; this_val : JSValue); cdecl; static;

    class function get_own_property(ctx: JSContext; desc: PJSPropertyDescriptor; obj: JSValueConst; prop: JSAtom) : Integer; cdecl; static;
    class function get_own_property_names (ctx: JSContext; ptab: PPJSPropertyEnum; plen: pUInt32; obj: JSValueConst) : Integer; cdecl; static;
    class function delete_property(ctx: JSContext; obj: JSValueConst; prop: JSAtom) : Integer; cdecl; static;
    class function define_own_property(ctx: JSContext; obj: JSValueConst; prop: JSAtom;
      val:JSValueConst; getter:JSValueConst;
      setter:JSValueConst; flags:Integer):Integer; cdecl; static;

  public
    procedure InitializeGlobals;
    procedure FinalizeGlobals;
    constructor Create;
    procedure BeforeDestruction; override;

    // Returns False on JS_EXCEPTION or JS_ERROR
    class function Check(ctx: JSContext; Value: JSValue) : Boolean; overload; static;
    class function Check(ctx: JSContext) : Boolean; overload; static;
    class procedure Check(FuncResult: Integer); overload; static;
    class function WaitForJobs(Ctx: JSContext; APromise: JSValue) : JSValue; static;

    function CreateContext: IJSContext;

    // IJSRuntime type registration methods (interface methods use distinct names)
    function  RegisterObjectType(ClassName: string; TypeInfo: PTypeInfo): IRegisteredObject;
    function  RegisterObjectWithConstructor(ClassName: string; TypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject;
    function  RegisterJSObject(const ctx: IJSContext; JSConstructor: JSValueConst; TypeInfo: PTypeInfo): IRegisteredObject;


    function  TryGetRegisteredObjectFromJSValue(Value: JSValue; out AObject: TRegisteredObjectWithPtr): Boolean; overload;
    function  TryGetRegisteredObjectFromJSValue(ctx: JSContext; Value: JSValue; out AObject: TRegisteredObjectWithPtr): Boolean; overload;
    function  TryGetRegisteredObjectFromClassID(ClassID: JSClassID; out RegisteredObject: IRegisteredObject): Boolean;
    function  TryGetRegisteredObjectFromTypeInfo(TypeInfo: PTypeInfo; out RegisteredObject: IRegisteredObject): Boolean;
    function  TryGetRegisteredObjectFromConstructor(JSConstructor: JSValueConst; out RegisteredObject: IRegisteredObject): Boolean;
    function  TryGetRegisteredJSObject(JSConstructor: JSValueConst; out RegisteredObject: IRegisteredObject): Boolean;
    function  TryGetRegisteredInterface(const IID: TGuid; out RegisteredObject: IRegisteredObject): Boolean;

    procedure SetCustomObjectFactory(const Factory: TCustomObjectFactory);
    procedure RegisterContext(const Context: IJSContext);
    procedure UnregisterContext(const Context: IJSContext);

    // Converter methods - virtual, override in derived runtimes (e.g. dn4d)
    function JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue; virtual;
    function TValueToJSValue(ctx: JSContext; const Value: TValue): JSValue; virtual;
    function TestParamsAreCompatible(ctx: JSContext; const Param: TRttiParameter; Value: JSValue; out ParamIsGenericValue: Boolean): Boolean; virtual;
    function GetDefaultValue(const Param: TRttiParameter): TValue; virtual;

    // Helper methods (moved from TJSRuntime)
    procedure Clear(ClearAll: Boolean);
    function  CreateCallback_0(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_0;
    function  CreateCallback_Double(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_Double;
    class function Describe(ctx: JSContext; Value: JSValue): string; static;

    class function JS_NewDate(ctx: JSContext; epoch_ms: Double) : JSValue; static;
    class function GetClassID(Value: JSValueConst) : JSClassID; static;
    class function GetClassName(Value: PTypeInfo) : string; static;
    class function GetObjectFromJSValue(Value: JSValueConst; PointerIsAnObject: Boolean) : Pointer; static;

    class function GetRuntimeFromContext(ctx: JSContext): TJSRuntime; static;
    function  TryLoadQuickJS: Boolean;

    class property Context[const Value: JSContext]: IJSContext read get_Context;
    
    property AutoRegister: Boolean read get_AutoRegister write set_AutoRegister;
    property LogString: TProc<string> read get_LogString write set_LogString;
    property ObjectBridgeResolver: IObjectBridgeResolver read FObjectBridgeResolver;
    property ObjectBridgeResolverIntf: IObjectBridgeResolver read FObjectBridgeResolver;
  end;

  TJSContext = class(TInterfacedObject, IJSContext)
  protected
    _ctx: JSContext;
    _runtime: IJSRuntime;
    _LogString: TProc<string>;

    function  get_ctx: JSContext;
    function  get_Runtime: IJSRuntime;
    function  get_LogString: TProc<string>;
    procedure set_LogString(const Value: TProc<string>);

    function  eval_internal(Buf: PAnsiChar; buf_len: Integer; FileName: PAnsiChar; eval_flags: Integer): Integer;

    function  eval_with_result(const Code: string; const CodeContext: string): TValue;


    class function logme(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
    // class function fetch(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
    procedure Initialize;

  public
    constructor Create(const Runtime: IJSRuntime);
    procedure eval(const Code: string; const CodeContext: string);
    procedure OutputLog(const Value: string);
    procedure BeforeDestruction; override;

    procedure RegisterLiveObjectInstance(ObjectName: string; AObject: TObject; OwnsObject: Boolean);
    procedure RegisterLiveInterfaceInstance(ObjectName: string; TypeInfo: PTypeInfo; const AInterface: IInterface);
  end;

  TJSIndexedPropertyAccessor = class
  protected
    _Runtime: TJSRuntime;
    _ctx: JSContext;
    _this_obj: JSValue;
    _prop: IPropertyDescriptor;

  public
    constructor Create(const Runtime: TJSRuntime; ctx: JSContext; this_obj: JSValue; prop: IPropertyDescriptor);
    destructor Destroy; override;
  end;


  TObjectReference = class
  public
    ObjectRef: TObject;
    constructor Create(AObject: TObject);
  end;

  TAutoReference = class(TObjectReference)
  public
    procedure BeforeDestruction; override;
  end;

  TRecordReference = class
  public
    Value: TValue; // Holds the record captured, frees Record when no longer in use

    constructor Create(APointer: Pointer; PType: PTypeInfo); overload;
    constructor Create(const AValue: TValue); overload;

    destructor Destroy; override;
  end;


  TRegisteredObject = class(TInterfacedObject, IRegisteredObject)
  protected
    var FRuntime: TJSRuntime;
    var FOnGetMemberByName: TOnGetMemberByName;
    var FOnGetMemberNames: TOnGetMemberNames;

  protected
    FClassID: JSClassID;
    FTypeInfo: PTypeInfo;
    FConstructor: TObjectConstuctor;  // Delphi constructor
    FJSConstructor: JSValue;          // JS constructor, used when a 'type' must be passed to QuickJS
    FRttiDescriptorCache: TDictionary<string, IPropertyDescriptor>;
    FExtensionProperties: TDictionary<string, string>;
    FObjectSupportsExtension: TObjectSupportsExtension;

  protected
    function  get_ClassID: JSClassID;
    procedure set_ClassID(const Value: JSClassID);
    function  get_JSConstructor: JSValue;
    procedure set_JSConstructor(const Value: JSValue);
    function  get_IsObject: Boolean;
    function  get_IsInterface: Boolean;
    function  get_IsIterator: Boolean;
    function  get_IsIndexedPropertyAccessor: Boolean;
    function  get_Kind: TTypeKind;
    function  get_ObjectSupportsEnumeration: Boolean; virtual;
    function  get_ObjectSupportsExtension: TObjectSupportsExtension; virtual;
    procedure set_ObjectSupportsExtension(const Value: TObjectSupportsExtension); virtual;
    function  get_ObjectSupportsIndexing: Boolean; virtual;

    procedure Finalize(Ptr: Pointer);
    function  CallConstructor : Pointer; virtual;
    function  CreateInstance(ctx: JSContext; argc: Integer; argv: PJSValueConstArr) : Pointer;
    function  DoOnGetMemberByName(const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean) : IPropertyDescriptor; virtual;
    function  DoOnGetMemberNames(MemberTypes: TMemberTypes; var Handled: Boolean) : TArray<string>; virtual;
    function  GetArrayIndexer: IPropertyDescriptor; virtual;
    function  GetIterator: IPropertyDescriptor; virtual;
    function  GetIteratorNext: IPropertyDescriptor; virtual;
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IPropertyDescriptor; virtual;
    function  GetMemberNames(MemberTypes: TMemberTypes) : TArray<string>; virtual;
    function  GetTypeInfo: PTypeInfo;

    function  TryGetRttiDescriptor(const PropName: string; out RttiMember: IPropertyDescriptor) : Boolean;
    procedure AddRttiDescriptor(const PropName: string; const RttiMember: IPropertyDescriptor);
    function  TryGetExtensionProperty(const PropName: string; out PropertyName: string) : Boolean;
    procedure AddExtensionProperty(const PropName: string; const PropertyName: string);

  public
    constructor Create(const Runtime: TJSRuntime; ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor);
    destructor  Destroy; override;

    property OnGetMemberByName: TOnGetMemberByName read FOnGetMemberByName write FOnGetMemberByName;
    property OnGetMemberNames: TOnGetMemberNames read FOnGetMemberNames write FOnGetMemberNames;
  end;

  function AtomToString(ctx: JSContext; Atom: JSAtom) : string;

// To keep the RTTI Pool alive and avoid continuously creating/destroying it
// See also https://stackoverflow.com/questions/27368556/trtticontext-multi-thread-issue
// _RttiContext is declared in QuickJS.Register.intf for shared access

implementation
uses
  System.Classes, QuickJS.VirtualMethod.impl, QuickJS.Register.ObjectBridge.impl,
  QuickJS.Register.ObjectBridgeDefaultDefinitions.impl;

function AtomToString(ctx: JSContext; Atom: JSAtom) : string;
begin
  var jv := JS_AtomToString(ctx, Atom);
  var ansistr := JS_ToCString(ctx, jv);
  Result := UTF8ToString(ansistr);
  JS_FreeCString(ctx, ansistr);
  JS_FreeValue(ctx, jv);
end;

constructor TJSObject.Create(const Runtime: TJSRuntime; const Ctx: JSContext; const Value: JSValueConst);
begin
  _Runtime := Runtime;
  _ctx := Ctx;
  _value := JS_DupValue(Ctx, Value);
  _ImplementingInterfaces := nil; // Initialize to nil, created on demand
end;

destructor TJSObject.Destroy;
begin
  if Assigned(_ImplementingInterfaces) then
    _ImplementingInterfaces.Free;
    
  if _ctx <> nil then
    JS_FreeValue(_ctx, _value);
    
  inherited;
end;

function TJSObject.get_Ctx: JSContext;
begin
  Result := _ctx;
end;

function TJSObject.get_Value: JSValueConst;
begin
  Result := _value;
end;

function TJSObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited;

  if Result <> S_OK then
  begin
    var ii_ref: TInterfaceRef;

    if _ImplementingInterfaces <> nil then
    begin
      for var i := 0 to _ImplementingInterfaces.Count - 1 do
      begin
        if IsEqualGUID(IID, _ImplementingInterfaces[i].IID) then
        begin
          ii_ref := _ImplementingInterfaces[i];
          break;
        end;
      end;

      if not ii_ref.IID.IsEmpty then
      begin
        // ii can still be nil!
        if ii_ref.ii <> nil then
        begin
          IInterface(Obj) := ii_ref.ii;
          Result := S_OK;
        end;

        Exit;
      end;
    end;

    // Call QuickJS, we expect an object to be returned. This object will be wrapped inside
    // an TJSVirtualInterface object as well and can be cast to the requested type
    var reg: IRegisteredObject;
    if _Runtime.TryGetRegisteredInterface(IID, reg) then
    begin
      var js_type := TValue.From<JSValue>(reg.JSConstructor);
      var val := Invoke('QueryInterface', [js_type], reg.GetTypeInfo);
      if not val.IsEmpty then
      begin
        ii_ref.ii := IInterface(val.GetReferenceToRawData^);
        IInterface(Obj) := ii_ref.ii;
        Result := S_OK;
      end;
    end;

    ii_ref.IID := IID;
    if _ImplementingInterfaces = nil then
      _ImplementingInterfaces := TList<TInterfaceRef>.Create;
    _ImplementingInterfaces.Add(ii_ref);
  end;
end;

function TJSObject.InternalCall(const this_obj: JSValueConst; const func_obj: JSValueConst; argc: Integer; argv: PJSValueConstArr) : JSValue;
begin
  var val := JS_Call(_ctx, func_obj, this_obj, argc, argv);
  TJSRuntime.Check(_ctx, val);
  Result := TJSRuntime.WaitForJobs(_ctx, val);
  TJSRuntime.Check(_ctx, Result);
end;

function TJSObject.InternalInvoke(const Func: AnsiString; argc: Integer; argv: PJSValueConstArr): JSValue;
begin
  {$IFDEF DEBUG}
  var obj_descr := TJSRuntime.Describe(_ctx, _value);
  {$ENDIF}

  Result := JS_GetPropertyStr(_ctx, _value, PAnsiChar(Func));
  TJSRuntime.Check(_ctx, Result);
  Result := TJSRuntime.WaitForJobs(_ctx, Result);
  TJSRuntime.Check(_ctx, Result);

  // Property returns a function, call function to get actual value
  if JS_IsFunction(_ctx, Result) then
  begin
    var tmp_func := Result;
    Result := InternalCall(_value, tmp_func, argc, argv);
    JS_FreeValue(_ctx, tmp_func);
    if not TJSRuntime.Check(_ctx) then Exit;
  end
  // Call on property with sub-properties
  // obj.ObjectWithProps['SUB-PROPERTY']
  else if (argc = 1) and JS_IsObject(Result) then
  begin
    // Result := JS_GetPropertyStr(_jsValue.ctx, Result, PAnsiChar('Customer'));
    var js_obj := Result;
    var prop_name := _Runtime.JSValueToTValue(_ctx, argv[0], TypeInfo(string));
    if not prop_name.IsEmpty then
    begin
      var prop_name_utf8: UTF8String := UTF8Encode(prop_name.ToString);
      Result := JS_GetPropertyStr(_ctx, js_obj, PAnsiChar(prop_name_utf8));
      TJSRuntime.Check(_ctx, Result);
      Result := TJSRuntime.WaitForJobs(_ctx, Result);
      TJSRuntime.Check(_ctx, Result);
    end;
    JS_FreeValue(_ctx, js_obj);
  end;
end;

function TJSObject.Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo): TValue;
begin
  var argv: array of JSValueConst;
  SetLength(argv, Length(Args));
  for var i := 0 to High(Args) do
    argv[i] := _Runtime.TValueToJSValue(_ctx, Args[i]);

  var js_val := InternalInvoke(Func, Length(argv), PJSValueConstArr(argv));
  Result := _Runtime.JSValueToTValue(_ctx, js_val, ReturnType);
  JS_FreeValue(_ctx, js_val);

  for var i := 0 to High(Args) do
    JS_FreeValue(_ctx, argv[i]);
end;

function TJSObject.Call(const JSFunc: JSValueConst; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue;
begin
  var argv: array of JSValueConst;
  SetLength(argv, Length(Args));
  for var i := 0 to High(Args) do
    argv[i] := _Runtime.TValueToJSValue(_ctx, Args[i]);

  var js_val := InternalCall(_value {this_obj}, JSFunc, Length(argv), PJSValueConstArr(argv));
  Result := _Runtime.JSValueToTValue(_ctx, js_val, ReturnType);
  JS_FreeValue(_ctx, js_val);

  for var i := 0 to High(Args) do
    JS_FreeValue(_ctx, argv[i]);
end;

function TJSObject.Call(const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue;
begin
  var argv: array of JSValueConst;
  SetLength(argv, Length(Args));
  for var i := 0 to High(Args) do
    argv[i] := _Runtime.TValueToJSValue(_ctx, Args[i]);

  var js_val := InternalCall(JS_Null, _value, Length(argv), PJSValueConstArr(argv));
  Result := _Runtime.JSValueToTValue(_ctx, js_val, ReturnType);
  JS_FreeValue(_ctx, js_val);

  for var i := 0 to High(Args) do
    JS_FreeValue(_ctx, argv[i]);
end;

class function TJSRuntime.GetRuntimeFromContext(ctx: JSContext): TJSRuntime;
begin
  var jsctx := TJSRuntime.Context[ctx];
  if jsctx <> nil then
    Result := jsctx.Runtime as TJSRuntime
  else
    Result := nil;
end;

{ TJSRuntime - Converter methods }

function TJSRuntime.GetDefaultValue(const Param: TRttiParameter): TValue;
begin
  var attr := Param.GetAttributes;
  for var a in attr do
  begin
    if a is DefaultValueAttribute then
    begin
      Result := TValue.FromVariant((a as DefaultValueAttribute).Value);
      Exit;
    end;
  end;

  // Create default (empty) parameter
  TValue.Make(nil, Param.ParamType.Handle, Result);
end;

function TJSRuntime.JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue;
begin
  case Target.Kind of
    // tkUnknown:
    tkInteger:
    begin
      var v: Integer;
      TJSRuntime.Check(JS_ToInt32(ctx, @v, Value));
      Result := TValue.From<Integer>(v);
    end;

//    tkChar:
    tkEnumeration, tkSet:
    begin
      if Target = TypeInfo(Boolean) then
      begin
        if JS_IsBool(value) and (JS_ToBool(ctx, value) <> 0) then
          Result := TValue.From<Boolean>(True) else
          Result := TValue.From<Boolean>(False);
      end
      else if JS_IsNumber(Value) then
      begin
        var v: Integer;
        TJSRuntime.Check(JS_ToInt32(ctx, @v, Value));
        TValue.Make(v, Target, Result);
      end;
    end;

    tkFloat:
    begin
      if JS_IsNumber(Value) then
      begin
        var v: Double;
        TJSRuntime.Check(JS_ToFloat64(ctx, @v, Value));
        Result := TValue.From<Double>(v);
      end;
    end;

    tkString, tkUString:
    begin
      var str: PAnsiChar := JS_ToCString(ctx, Value);
      if Assigned(str) then
      try
        Result := TValue.From<string>(UTF8ToString(str));
      finally
        JS_FreeCString(ctx, str);
      end;
    end;
//    tkSet:
//    tkClass:
//    tkMethod:
        // Assignement of Delphi event handler (like TNotifyEvent)
        // Value assigned must be a TMethod
//    tkWChar:
//    tkLString:
//    tkWString:
//    tkVariant:
    tkClass, tkArray:
    begin
      if TJSRuntime.GetClassID(Value) = JS_CLASS_ARRAY_BUFFER then
      begin
        // array buffer -> stream
        if Target.TypeData.ClassType.InheritsFrom(TStream) then
        begin
          var size: size_t;
          var ptr := JS_GetArrayBuffer(ctx, @size, Value);
          var m := TMemoryStream.Create;
          m.Write(ptr^, size);
          m.Position := 0;
          Result := TValue.From<TStream>(m);
        end
      end
      else if JS_IsObject(Value) then
      begin
        var ptr := TJSRuntime.GetObjectFromJSValue(Value, True {Is object type?});
        if ptr <> nil then
          TValue.Make(@ptr, Target, Result);
      end;
    end;

    tkRecord:
    begin
      // Assume record is actually an enumeration
      if Target.TypeData.RecSize in [1, 2, 4] then
      begin
        var v: Integer;
        TJSRuntime.Check(JS_ToInt32(ctx, @v, Value));
        Assert(Target.TypeData.ManagedFldCount = 0);
        TValue.Make(@v, Target, Result);
      end;
    end;

    tkInterface:
    begin
      if JS_IsNull(Value) or JS_IsUndefined(Value) then
        Exit;

      // Assign a JS function to a method reference?
      if JS_IsFunction(ctx, Value) then
      begin
        if TIntfFlag.ifMethRef in Target.TypeData.IntfFlags then
        begin
          var ii: IInterface := TVirtualMethodImplementation.Create(Target, TJSObject.Create(Self, ctx, Value));
          var t: IInterface;
          ii.QueryInterface(IInterface, t);
          TValue.Make(@t, Target, Result);
        end;
      end

      else if JS_IsObject(Value) then
      begin
        var ptr := TJSRuntime.GetObjectFromJSValue(Value, False {Is NOT object type?});
        if ptr <> nil then
          TValue.Make(@ptr, Target, Result);
      end;
    end;

    tkInt64:
    begin
      var v: Int64;
      TJSRuntime.Check(JS_ToBigInt64(ctx, @v, Value));
      Result := TValue.From<Int64>(v);
    end;

//    tkDynArray:
//    tkUString,
//    tkClassRef:
//    tkPointer:
//    tkProcedure:
//    tkMRecord:
  end;
end;

function TJSRuntime.TValueToJSValue(ctx: JSContext; const Value: TValue): JSValue;

  function GetRegisteredObjectFromTypeInfo(PInfo: PTypeInfo): IRegisteredObject;
  begin
    var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
    if runtime = nil then
      Exit(nil);
      
    TMonitor.Enter(runtime.FRegisteredObjectsByType);
    try
      if not runtime.FRegisteredObjectsByType.TryGetValue(PInfo, Result) then
      begin
        if not runtime.FAutoRegister then Exit(nil); // Result = undefined

        runtime.RegisterObjectType(TJSRuntime.GetClassName(PInfo), PInfo);
        runtime.FRegisteredObjectsByType.TryGetValue(PInfo, Result);
      end;
    finally
      TMonitor.Exit(runtime.FRegisteredObjectsByType);
    end;
  end;

begin
  Result := JS_UNDEFINED;

  case Value.Kind of
    // tkUnknown:
    tkInteger:
      Result := JS_NewInt32(ctx, Value.AsInteger);
//    tkChar:
    tkEnumeration:
    begin
      if Value.TypeInfo = TypeInfo(Boolean) then
        Result := JS_NewBool(ctx, Value.AsBoolean)
      else
      begin
        // Handle other enumerations as integers
        var v: Integer := Value.AsOrdinal;
        Result := JS_NewInt32(ctx, v);
      end;
    end;
    
    tkFloat:
    begin
      Result := JS_NewFloat64(ctx, Value.AsExtended);
    end;
    
    tkString, tkUString:
    begin
      var s := UTF8Encode(Value.AsString);
      Result := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));
    end;
//    tkSet:
    tkClass:
    begin
      var obj := Value.AsObject;
      var reg := GetRegisteredObjectFromTypeInfo(obj.ClassInfo);
      Result := JS_NewObjectClass(ctx, reg.ClassID);
      // Objects retrieved from get_property are passed by reference
      // They will not be freed when CFinalize is called
      obj := TObjectReference.Create(obj);
      JS_SetOpaque(Result, obj);
    end;

//    tkMethod:
//    tkWChar:
//    tkLString:
//    tkWString:
//    tkVariant:
//    tkArray:
    tkDynArray:
    begin
      var arr: TArray<Byte>;
      if Value.TryAsType<TArray<Byte>>(arr) and (arr <> nil) then
        Result := JS_NewArrayBufferCopy(ctx, Pointer(arr), Length(arr));
    end;
    tkRecord:
    // Records are copied and then returned as 'objects'
    begin
      if Value.TypeInfo = TypeInfo(JSValue) then
        Result := Value.AsType<JSValue>

      else
      begin
        var reg := GetRegisteredObjectFromTypeInfo(Value.TypeInfo);
        Result := JS_NewObjectClass(ctx, reg.ClassID);
        // Objects retrieved from get_property are passed by reference
        // They will not be freed when CFinalize is called
        var rec := TRecordReference.Create(Value);
        JS_SetOpaque(Result, rec);
      end;
    end;
    tkInterface:
    begin
      if not Value.IsEmpty then
      begin
        // Property holds a reference to callback procedure
        if string(Value.TypeInfo.Name).StartsWith('TProc') then
        begin
          Result := JS_NewCFunctionData(ctx, @TJSRuntime.GenericInvokeCallBack, 0, 999, 0, nil);
        end
        else
        begin
          // Resolve interface mapping if available
          var typeInfoToUse := Value.TypeInfo;
          var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
          if (runtime <> nil) and Assigned(runtime.FObjectBridgeResolver) then
            typeInfoToUse := runtime.FObjectBridgeResolver.ResolveInterfaceMapping(Value.TypeInfo, Value.AsInterface);
          
          var reg := GetRegisteredObjectFromTypeInfo(typeInfoToUse);
          if reg <> nil then
          begin
            Result := JS_NewObjectClass(ctx, reg.ClassID);
            // Supports will bump reference count (required for QuickJS to manage the object lifetime)
            var ptr: Pointer;
            Supports(Value.AsInterface, typeInfoToUse.TypeData.Guid, ptr);
            JS_SetOpaque(Result, ptr);
          end;

        end;
      end;
    end;

    tkInt64:
      Result := JS_NewBigInt64(ctx, Value.AsInt64);

//    tkDynArray:
    // tkClassRef:
//    tkPointer:
//    tkProcedure:
//    tkMRecord:
  end;
end;

function TJSRuntime.TestParamsAreCompatible(ctx: JSContext; const Param: TRttiParameter; Value: JSValue; out ParamIsGenericValue: Boolean): Boolean;
begin
  Result := False;
  ParamIsGenericValue := False;

  case Param.ParamType.TypeKind of
    // tkUnknown:
    tkInteger:
      Result := JS_IsNumber(Value) or JS_IsBigInt(Value);

//    tkChar:
    tkEnumeration:
      if Param.ParamType.Handle = System.TypeInfo(Boolean) then
        Result := JS_IsBool(Value) or JS_IsNumber(Value) or JS_IsString(Value) else
        Result := JS_IsNumber(Value);

    tkFloat:
      Result := JS_IsNumber(Value);
    tkString, tkUString:
      Result := JS_IsString(Value);

//    tkSet:
//    tkClass:
//    tkMethod:
        // Assignement of Delphi event handler (like TNotifyEvent)
        // Value assigned must be a TMethod
//    tkWChar:
//    tkLString:
//    tkWString:
//    tkVariant:
    tkClass {Must be TStream}, tkArray:
    begin
      var size: size_t;
      var ptr := JS_GetArrayBuffer(ctx, @size, Value);
      Result := ptr <> nil;
    end;

    tkRecord:
      Result := True;
//      if Param.Handle = TypeInfo(TValue) then
//      begin
//        ParamIsGenericValue := True;
//        Result := True;
//      end;

    tkInterface:
      Result := JS_IsNull(Value) or JS_IsObject(Value) or
                (JS_IsFunction(ctx, Value) and (Param.ParamType is TRttiInterfaceType) and (TIntfFlag.ifMethRef in (Param.ParamType as TRttiInterfaceType).IntfFlags));

    tkInt64:
      Result := JS_IsNumber(Value) or JS_IsBigInt(Value);

//    tkDynArray:
//    tkUString,
//    tkClassRef:
//    tkPointer:
//    tkProcedure:
//    tkMRecord:
  end;
end;

procedure TJSRuntime.Clear(ClearAll: Boolean);
begin
  if FRegisteredJSObjects <> nil then
    FRegisteredJSObjects.Clear;
end;

function TJSRuntime.CreateCallback_0(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_0;
begin
  Result := procedure begin
    JS_Call(ctx, JSValue, JS_UNDEFINED, 0 {argc}, nil {PJSValueConstArr(argv)});
  end;
end;

function TJSRuntime.CreateCallback_Double(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_Double;
begin
  Result := procedure(D: Double) begin
    var argc := 1;
    var argv: array of JSValueConst;
    SetLength(argv, 1);
    argv[0] := JS_NewFloat64(ctx, D);
    JS_Call(ctx, JSValue, JS_UNDEFINED, argc, PJSValueConstArr(argv));
    TJSRuntime.Check(ctx);
    JS_FreeValue(ctx, argv[0]);

//    var argv: PJSValueConstArr := js_malloc(ctx, 1 * SizeOf(JSValue));
//    argv[0] := JS_NewFloat64(ctx, D);
//
//    TJSRuntime.Check(ctx, JS_Call(ctx, JSValue, JS_UNDEFINED, argc, argv));
//
//    JS_FreeValue(ctx, argv[0]);
//    js_free(ctx, argv);
  end;
end;

class function TJSRuntime.GenericInvokeCallBack(ctx: JSContext; this_val: JSValueConst;
  argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue;
begin
  Result := JS_NewBigInt64(ctx, 500);
end;

class function TJSRuntime.GenericMethodCallData(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, PJSValueConstArr(func_data)[1]));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var m_descr: IMethodsPropertyDescriptor := nil;

  if Supports(descr, IMethodsPropertyDescriptor, m_descr) then
  begin
    var ptr: Pointer;
    var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
    if not JS_IsUndefined(this_val) then
      ptr := TJSRuntime.GetObjectFromJSValue( this_val, descr.TypeInfo.Kind <> tkInterface) else
      // Get self
      ptr := TJSRuntime.GetObjectFromJSValue( PJSValueConstArr(func_data)[0], descr.TypeInfo.Kind <> tkInterface);

    Result := m_descr.Call(ctx, ptr, argc, argv);
  end;

  JS_FreeValue(ctx, PJSValueConstArr(func_data)[1]);
end;

class function TJSRuntime.GenericGetItem(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var ptr := TJSRuntime.GetObjectFromJSValue( this_val, False {Ptr is an IInterface} );
  var vt := descr.GetValue(ptr, [magic]);
  JS_FreeValue(ctx, func_data^);
  Result := runtime.TValueToJSValue(ctx, vt);
end;

class function TJSRuntime.GenericSetItem(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var ptr := TJSRuntime.GetObjectFromJSValue( this_val, False {Ptr is an IInterface} );

  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');

  var v := runtime.JSValueToTValue(ctx, PJSValueConstArr(argv)[0], descr.PropertyType);

  descr.SetValue(ptr, [magic], v);
  JS_FreeValue(ctx, func_data^);
  Result := JS_UNDEFINED;
end;

class function TJSRuntime.GenericPropertyGetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);

  // Return TJSIndexedPropertyAccessor object to access property value
  if descr.MemberType = TMemberType.IndexedProperty then
  begin
    var reg_iter := runtime.FRegisteredObjectsByType[System.TypeInfo(TJSIndexedPropertyAccessor)];
    Result := JS_NewObjectClass(ctx, reg_iter.ClassID);
    var idx_access := TJSIndexedPropertyAccessor.Create(runtime, ctx, JS_DupValue(ctx, this_val), descr);
    JS_SetOpaque(Result, TAutoReference.Create(idx_access));
  end
  else
  begin
    var ptr := TJSRuntime.GetObjectFromJSValue( this_val, not descr.IsInterface {Ptr is an IInterface} );
    var vt := descr.GetValue(ptr, []);
    JS_FreeValue(ctx, func_data^);
    Result := runtime.TValueToJSValue(ctx, vt);
  end;
end;

class function TJSRuntime.GenericPropertySetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var ptr := TJSRuntime.GetObjectFromJSValue( this_val, not descr.IsInterface {Ptr is an IInterface} );

  // Return TJSIndexedPropertyAccessor object to access property value
  if descr.MemberType = TMemberType.IndexedProperty then
  begin
    var reg_iter := runtime.FRegisteredObjectsByType[System.TypeInfo(TJSIndexedPropertyAccessor)];
    Result := JS_NewObjectClass(ctx, reg_iter.ClassID);
    var idx_access := TJSIndexedPropertyAccessor.Create(runtime, ctx, JS_DupValue(ctx, this_val), descr);
    JS_SetOpaque(Result, TAutoReference.Create(idx_access));
  end
  else
  begin
    if argc <> 1 then
      raise Exception.Create('Invalid number of arguments');
    var v := runtime.JSValueToTValue(ctx, PJSValueConstArr(argv)[0], descr.PropertyType);

    descr.SetValue(ptr, [], v);
    JS_FreeValue(ctx, func_data^);
    Result := JS_UNDEFINED;
  end;
end;

class function TJSRuntime.GenericIndexedPropertyGetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var propertyIndex := JS_ToCString(ctx, func_data^);
  var prop_access := TJSIndexedPropertyAccessor(TJSRuntime.GetObjectFromJSValue( this_val, True));
  var ptr := TJSRuntime.GetObjectFromJSValue( prop_access._this_obj, not prop_access._prop.IsInterface {Ptr is an IInterface} );
  var vt := prop_access._prop.GetValue(ptr, [UTF8ToString(propertyIndex)]);
  JS_FreeValue(ctx, func_data^);
  Result := runtime.TValueToJSValue(ctx, vt);
end;

class function TJSRuntime.GenericIndexedPropertySetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  Assert(False);

  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRuntime.GetObjectFromJSValue( this_val, not descr.IsInterface {Ptr is an IInterface} );

  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');
  var v := runtime.JSValueToTValue(ctx, PJSValueConstArr(argv)[0], descr.PropertyType);

  descr.SetValue(ptr, [], v);
  JS_FreeValue(ctx, func_data^);
  Result := JS_UNDEFINED;
end;

class function TJSRuntime.ExtendablePropertyGetter(ctx : JSContext; obj: JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data: PJSValue ): JSValue;

begin
  Result := JS_UNDEFINED;

  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var reg: IRegisteredObject;
  if runtime.FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var ext: IJSExtendableObject;
    var ptr := TJSRuntime.GetObjectFromJSValue( obj, False {Is object type?});
    var vt: TValue;
    TValue.Make(@ptr, reg.GetTypeInfo, vt);
    if Supports(vt.AsInterface, IJSExtendableObject, ext) then
    begin
      var prop_name := JS_ToCString(ctx, func_data^);
      if Assigned(prop_name) then
      begin
        Result := ext.GetValue(ctx, UTF8ToString(prop_name));
        JS_FreeCString(ctx, prop_name);
      end;
    end;
  end;
end;

class function TJSRuntime.ExtendablePropertySetter(ctx : JSContext; obj: JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  Result := JS_UNDEFINED;

  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');

  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var reg: IRegisteredObject;
  if runtime.FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var ext: IJSExtendableObject;
    var ptr := TJSRuntime.GetObjectFromJSValue( obj, False {Is object type?});
    var vt: TValue;
    TValue.Make(@ptr, reg.GetTypeInfo, vt);
    if Supports(vt.AsInterface, IJSExtendableObject, ext) then
    begin
      var prop_name := JS_ToCString(ctx, func_data^);
      if Assigned(prop_name) then
      begin
        ext.SetValue(ctx, UTF8ToString(prop_name), PJSValueConstArr(argv)[0]);
        JS_FreeCString(ctx, prop_name);
      end;
    end;
  end;
end;

class function TJSRuntime.get_own_property(ctx: JSContext; desc: PJSPropertyDescriptor; obj: JSValueConst; prop: JSAtom) : Integer;
var
  rtti_descriptor: IPropertyDescriptor;
  runtime: TJSRuntime;

  procedure SetRttiArrayIndexProperty(Index: Integer);
  begin
    var data: JSValue := JS_NewBigInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @TJSRuntime.GenericGetItem, 0 {length}, Index {magic=index}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @TJSRuntime.GenericSetItem, 0 {length}, Index {magic=index}, 1 {data_len}, @data {PJSValueConst});
  end;

  procedure SetRttiPropertyDescriptorCallBack;
  begin
    var data: JSValue := JS_NewBigInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @TJSRuntime.GenericPropertyGetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @TJSRuntime.GenericPropertySetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
  end;

  procedure SetIndexedPropertyCallBack(const Index: string);
  begin
    var s := UTF8Encode(Index);
    var data: JSValue := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @TJSRuntime.GenericIndexedPropertyGetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @TJSRuntime.GenericIndexedPropertySetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
  end;

  procedure SetRttiMethodCallBack;
  begin
    var data: array of JSValueConst;
    SetLength(data, 2);
    data[0] := obj; // Add Self pointer
    // 7/6/2025 Do not increase ref count --> dubugging shows that this creates a dangling object
    // data[0] := JS_DupValue(ctx, obj); // Add Self pointer
    data[1] := JS_NewBigInt64(ctx, Int64(Pointer(rtti_descriptor))); // Method to call

    desc^.flags := JS_PROP_HAS_VALUE or JS_PROP_ENUMERABLE;
    desc^.value := JS_NewCFunctionData(ctx, @TJSRuntime.GenericMethodCallData, 0 {length}, 999 {magic}, 2, PJSValueConst(data) {PJSValueConst});
    desc^.getter := JS_UNDEFINED;
    desc^.setter := JS_UNDEFINED;
  end;

  procedure SetIteratorProperty;
  begin
    var data: JSValue := JS_NewBigInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_HAS_GET or JS_PROP_ENUMERABLE;
    desc^.value := JS_NewCFunctionData(ctx, @TJSRuntime.GenericClassIterator, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.getter := JS_UNDEFINED;
    desc^.setter := JS_UNDEFINED;
  end;

  procedure SetIteratorNextProperty;
  begin
    var data: JSValue := JS_NewBigInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_HAS_GET or JS_PROP_ENUMERABLE;
    desc^.value := JS_NewCFunctionData(ctx, @TJSRuntime.GenericIteratorNext, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.getter := JS_UNDEFINED;
    desc^.setter := JS_UNDEFINED;
  end;

  procedure SetExtendableObjectGetterSetter(const PropertyName: string);
  begin
    // data holds the name of the property
    var s := UTF8Encode(PropertyName);
    var data: JSValue := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @TJSRuntime.ExtendablePropertyGetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @TJSRuntime.ExtendablePropertySetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
  end;

  function TestObjectSupportsExtension(const reg: IRegisteredObject; const PropertyName: string): Boolean;
  begin
    if reg.ObjectSupportsExtension = TObjectSupportsExtension.Unknown then
    begin
      reg.ObjectSupportsExtension := TObjectSupportsExtension.NotSupported;

      if reg.GetTypeInfo.Kind in [tkInterface, tkClass] then
      begin
        var isObject := reg.GetTypeInfo.Kind = tkClass;
        var ptr := TJSRuntime.GetObjectFromJSValue(obj, isObject);
        if ptr <> nil then
        begin
          var ext: IJSExtendableObject;
          if isObject then
          begin
            if not Supports(TObject(ptr), IJSExtendableObject, ext) then
              Exit(False);
          end
          else // Interface
          begin
            var v: TValue;
            TValue.Make(@ptr, reg.GetTypeInfo, v);
            if not Supports(v.AsInterface, IJSExtendableObject, ext) then
              Exit(False);
          end;

          if ext.define_own_property(Ctx, PropertyName) then
            reg.ObjectSupportsExtension := TObjectSupportsExtension.Supported;
        end;
      end;
    end;

    Result := reg.ObjectSupportsExtension = TObjectSupportsExtension.Supported;
  end;

begin
  Result := 0;

  // for(var x in n) {} first calls get_own_property_names first.
  // Then this method for per property twice.
  // Can't explain why it calls this method a second time (with desc = nil), for now ignore second call
  if desc = nil then Exit(1);

  var member_name := AtomToString(ctx, prop);
  runtime := TJSRuntime.GetRuntimeFromContext(ctx);

  {$IFDEF DEBUG}
  var obj_type: string;
  var r: IRegisteredObject;
  if runtime.FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), r) then
    obj_type := string(r.GetTypeInfo.Name);
  var ptr := TJSRuntime.GetObjectFromJSValue( obj, r.GetTypeInfo.Kind <> tkInterface);
  {$ENDIF}

  var reg: IRegisteredObject;

  if runtime.FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    if reg.IsIndexedPropertyAccessor then
    begin
      SetIndexedPropertyCallBack(member_name);
      Exit(1);
    end;

    var item_index: Integer := -1;
    if CharInSet(member_name[1], ['0'..'9']) then
    begin
      item_index := StrToInt(member_name);
      member_name := 'get_Item';
    end;

    if not reg.TryGetRttiDescriptor(member_name, rtti_descriptor) then
    begin
      // First try ObjectBridge resolver
      var handled := False;
      if Assigned(runtime.FObjectBridgeResolver) then
        rtti_descriptor := runtime.FObjectBridgeResolver.OnGetMemberByName(reg, member_name, [TMemberType.Methods, TMemberType.Property], handled);

      // If ObjectBridge resolver didn't handle it, use standard resolution
      if not handled then
        rtti_descriptor := reg.GetMemberByName(member_name, [TMemberType.Methods, TMemberType.Property]);

      if rtti_descriptor = nil then
      begin
        if TestObjectSupportsExtension(reg, member_name) then
          rtti_descriptor := TRttiExtensionPropertyDescriptor.Create(member_name) else
          Exit;
      end;

      reg.AddRttiDescriptor(member_name, rtti_descriptor);
    end;

    // The first call on a indexed property should return the property itself.
    // The getter/setter for this property will return a TJSIndexedPropertyAccessor
    // which handles the get/set on the property
    if rtti_descriptor.MemberType in [TMemberType.Property, TMemberType.IndexedProperty] then
      SetRttiPropertyDescriptorCallBack
    else if rtti_descriptor.MemberType = TMemberType.Methods then
      SetRttiMethodCallBack
    else if rtti_descriptor.MemberType = TMemberType.Iterator then
      SetIteratorProperty
    else if rtti_descriptor.MemberType = TMemberType.IteratorNext then
      SetIteratorNextProperty
    else if rtti_descriptor.MemberType = TMemberType.ArrayIndexer then
      SetRttiArrayIndexProperty(item_index)
    else if rtti_descriptor.MemberType = TMemberType.ExtensionProperty then
      SetExtendableObjectGetterSetter(member_name)
    else
      Exit(0);

    Exit(1);
  end;
end;

// Called to retrieve the properties of a object
class function TJSRuntime.get_own_property_names (ctx: JSContext; ptab: PPJSPropertyEnum; plen: pUInt32; obj: JSValueConst):Integer;
begin
  Result := 0; // OK

  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var reg: IRegisteredObject;
  if runtime.FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var names := reg.GetMemberNames([TMemberType.Methods, TMemberType.Property]);

    var arr: PJSPropertyEnum := js_malloc(ctx, Length(names) * SizeOf(JSPropertyEnum));
    var p := arr;
    var name: string;

    for name in names do
    begin
      p^.is_enumerable := False; // Not used?
      var name_utf8 := UTF8Encode(name);
      p^.atom := JS_NewAtom(ctx, PAnsiChar(name_utf8));
      inc(p);
    end;

    plen^ := Length(names);
    ptab^ := arr;
  end;
end;

class function TJSRuntime.delete_property(ctx: JSContext; obj:JSValueConst; prop:JSAtom):Integer;
begin
  Assert(False, 'Not implemented');
  Result := 0;
end;

class function TJSRuntime.JS_NewDate(ctx: JSContext; epoch_ms: Double) : JSValue;
begin
  // Always get the Date constructor from the current context, not from a cached global
  // Each context (including sandboxes) has its own Date constructor
  // If this is not done, the "instanceOf Date" operator will return false
  var dateConstructor := JS_NULL;
  var global := JS_NULL;

  try
    global := JS_GetGlobalObject(ctx);
    dateConstructor := JS_GetPropertyStr(ctx, global, 'Date');
    if not JS_IsConstructor(ctx, dateConstructor) then
      raise Exception.Create('Date constructor not found');

    var argc := 1;
    var argv: array of JSValueConst;
    SetLength(argv, 1);
    argv[0] := JS_NewFloat64(ctx, epoch_ms);
    Result := JS_CallConstructor(ctx, dateConstructor, argc, PJSValueConst(argv));
    JS_FreeValue(ctx, argv[0]);
  finally
    JS_FreeValue(ctx, global);
    JS_FreeValue(ctx, dateConstructor);
  end;
end;

class function TJSRuntime.GetClassID(Value: JSValueConst): JSClassID;
begin
  Result := JS_GetClassID(Value);
end;

class function TJSRuntime.GetClassName(Value: PTypeInfo) : string;
begin
  Result := string(Value.Name);
//  if Result[1] = 'T' then
//    Result := Result.Substring(1);
end;

class function TJSRuntime.GetObjectFromJSValue(Value: JSValueConst; PointerIsAnObject: Boolean) : Pointer;
begin
  var classID := GetClassID(Value);
  if classID > 0 then
  begin
    Result := JS_GetOpaque(Value, classID);
    if (Result <> nil) and PointerIsAnObject then
    begin
      if (TObject(Result) is TObjectReference) then
        Result := (TObject(Result) as TObjectReference).ObjectRef
      else if (TObject(Result) is TRecordReference) then
        Result := (TObject(Result) as TRecordReference).Value.GetReferenceToRawData;
    end;
  end else
    Result := nil;
end;

function TJSRuntime.TryGetRegisteredObjectFromJSValue(ctx: JSContext; Value: JSValue; out AObject: TRegisteredObjectWithPtr) : Boolean;
begin
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  AObject.Reg := nil;
  AObject.Ptr := nil;

  var classID := GetClassID(Value);
  if classID >= runtime.FFirst_ClassID then
  begin
    if not runtime.TryGetRegisteredObjectFromClassID(classID, AObject.Reg) then
      raise EArgumentException.Create('Class not registered');

    AObject.Ptr := JS_GetOpaque(Value, classID);
    if AObject.Reg.IsObject then
    begin
      if (TObject(AObject.Ptr) is TObjectReference) then
        AObject.Ptr := (TObject(AObject.Ptr) as TObjectReference).ObjectRef
      else if (TObject(AObject.Ptr) is TRecordReference) then
        AObject.Ptr := (TObject(AObject.Ptr) as TRecordReference).Value.GetReferenceToRawData;
    end;

    Exit(True);
  end;
  Exit(False);
end;

class function TJSRuntime.define_own_property(ctx: JSContext; obj:JSValueConst; prop:JSAtom;
  val:JSValueConst; getter:JSValueConst;
  setter:JSValueConst; flags:Integer): Integer;
begin
  // method called when a property does not exist on a object
  // However, this situation is already handled in get_own_property..
  Result := 0;
end;

class function TJSRuntime.CConstructor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr; magic : Integer): JSValue;
var
  reg: IRegisteredObject;

begin
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  if runtime.FRegisteredObjectsByClassID.TryGetValue(magic, reg) then
  begin
    Result := JS_NewObjectClass(ctx, reg.ClassID);
    var ptr := reg.CreateInstance(ctx, argc, argv);
    // Do not wrap object inside TObjectReference
    // Finalize will free instance when no longer in use
    JS_SetOpaque(Result, ptr);
  end else
    Result := JS_UNDEFINED;
end;

class procedure TJSRuntime.CFinalize(rt: JSRuntime; this_val: JSValue); cdecl;
begin
  var cid := GetClassID(this_val);
  var ptr := JS_GetOpaque(this_val, cid);

  if ptr <> nil then
  begin
    // Get the TJSRuntime from the JSRuntime opaque data
    var runtime := TJSRuntime(JS_GetRuntimeOpaque(rt));
    if runtime <> nil then
    begin
      var reg: IRegisteredObject;
      if runtime.FRegisteredObjectsByClassID.TryGetValue(cid, reg) then
        reg.Finalize(ptr);
    end;
  end;
end;

class function TJSRuntime.GenericClassIterator(ctx: JSContext; this_val: JSValueConst;
  argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue): JSValue; cdecl;
begin
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var prtti: Int64;
  TJSRuntime.Check(JS_ToBigInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRuntime.GetObjectFromJSValue( this_val, False {Ptr is an IInterface} );
  var iter := descr.GetValue(ptr, []);
  if not iter.IsEmpty then
  begin
    var reg_iter := runtime.FRegisteredObjectsByType[System.TypeInfo(TJSIterator)];
    Result := JS_NewObjectClass(ctx, reg_iter.ClassID);
    // Do not wrap object inside TObjectReference
    // Finalize will free instance when no longer in use
    JS_SetOpaque(Result, iter.AsObject);
  end;
  // JS_FreeValue(ctx, this_val);
end;

class function TJSRuntime.GenericIteratorNext(ctx : JSContext; this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
begin
  var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
  var cid := TJSRuntime.GetClassID(this_val);
  var iter := TJSIterator(JS_GetOpaque(this_val, cid));

  Result := JS_NewObject(ctx);

  if iter.MoveNext then
  begin
    JS_SetPropertyStr(ctx, Result, 'value', runtime.TValueToJSValue(ctx, iter.Current));
    JS_SetPropertyStr(ctx, Result, 'done', JS_FALSE);
  end else
    JS_SetPropertyStr(ctx, Result, 'done', JS_TRUE);

  // JS_FreeValue(ctx, this_val);
end;

class function TJSRuntime.Describe(ctx: JSContext; Value: JSValue): string;
type
  JSPropertyEnumArr  = array[0..(MaxInt div SizeOf(JSPropertyEnum))-1] of JSPropertyEnum;
  PJSPropertyEnumArr = ^JSPropertyEnumArr;

  function GetClassName(c: JSValue) : string;
  begin
    var n := JS_GetPropertyStr(ctx, c, 'name');
    var name := JS_ToCString(ctx, n);
    Result := UTF8ToString(name);
    JS_FreeCString(ctx, name);
    JS_FreeValue(ctx, n);
  end;

begin
  var proto: JSValue;

  if JS_IsConstructor(ctx, Value) then
  begin
    proto := JS_GetPropertyStr(ctx, Value, 'prototype');
    Result := GetClassName(Value) + ': ';
  end
  else if JS_IsObject(Value) then
  begin
    var c := JS_GetPropertyStr(ctx, Value, 'constructor');
    Result := GetClassName(c) + ': ';
    JS_FreeValue(ctx, c);
    proto := JS_GetPrototype(ctx, Value);
  end;

  var p_enum: PJSPropertyEnum := nil;
  var p_len: UInt32;
  JS_GetOwnPropertyNames(ctx, @p_enum, @p_len, proto, JS_PROP_C_W_E);

  if p_len > 0 then
  begin
    for var i := 0 to p_len -1 do
    begin
      var name := AtomToString(ctx, PJSPropertyEnumArr(p_enum)[i].atom);

      if i = 0 then
        Result := Result + name else
        Result := Result + ', ' + name;
    end;
  end;

  js_free(ctx, p_enum);

  JS_FreeValue(ctx, proto);
end;

function TJSRuntime.TryLoadQuickJS: Boolean;
begin
  Result := False;
  
  // Try to load QuickJS library
  if not IsQuickJSLoaded then
  begin
    if not LoadQuickJS then
      Exit(False);
  end;
  
  Result := True;
end;

{ TRegisteredObject }

function TRegisteredObject.TryGetExtensionProperty(const PropName: string; out PropertyName: string) : Boolean;
begin
  Result := FExtensionProperties.TryGetValue(PropName, PropertyName);
end;

procedure TRegisteredObject.AddExtensionProperty(const PropName: string; const PropertyName: string);
begin
  FExtensionProperties.Add(PropName, PropertyName);
end;

procedure TRegisteredObject.AddRttiDescriptor(const PropName: string; const RttiMember: IPropertyDescriptor);
begin
  FRttiDescriptorCache.Add(PropName, RttiMember);
end;

procedure TRegisteredObject.Finalize(Ptr: Pointer);
begin
  if get_IsInterface then
    IInterface(Ptr)._Release else
    TObject(Ptr).Free;  // Frees TObjectReference when object is passed in through get_property
                        // Frees actual object when object is created in CConstructor
end;

function TRegisteredObject.get_ClassID: JSClassID;
begin
  Result := FClassID;
end;

procedure TRegisteredObject.set_ClassID(const Value: JSClassID);
begin
  FClassID := Value;
end;

procedure TRegisteredObject.set_JSConstructor(const Value: JSValue);
begin
  FJSConstructor := Value;
end;

constructor TRegisteredObject.Create(const Runtime: TJSRuntime; ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor);
begin
  FRuntime := Runtime;
  FTypeInfo := ATypeInfo;
  FConstructor := AConstructor;
  FRttiDescriptorCache := TDictionary<string, IPropertyDescriptor>.Create;
  FExtensionProperties := TDictionary<string, string>.Create;
end;

destructor TRegisteredObject.Destroy;
begin
  inherited;
  FRttiDescriptorCache.Free;
  FExtensionProperties.Free;
end;

function TRegisteredObject.CallConstructor : Pointer;
begin
  if Assigned(FConstructor) then
  begin
    Result := FConstructor;

    if Result = nil then
      raise Exception.Create('Failed to object of type: ' + FTypeInfo.Name + ', Constructor returned nil');

    if get_IsInterface then
    try
      // NO need to call _AddRef, Supports will bump reference count
      Supports(TObject(Result), FTypeInfo^.TypeData.Guid, Result)
    except
      on E: Exception do
        raise Exception.Create(string.Format('Query interface failed for interface %s', [FTypeInfo.Name]));
    end;
  end else
    Result := nil;
end;

function TRegisteredObject.CreateInstance(ctx: JSContext; argc: Integer; argv: PJSValueConstArr): Pointer;
var
  arr: TArray<TValue>;

  function TryCreateViaFactory: Pointer;
  begin
    Result := nil;
    var runtime := TJSRuntime.GetRuntimeFromContext(ctx);
    if (runtime = nil) or not Assigned(runtime.FCustomObjectFactory) then
      Exit;

    // Don't send records to the factory
    if FTypeInfo.Kind = tkRecord then
      Exit;

    // Convert JSValues to TValue array
    SetLength(arr, argc);
    for var i := 0 to argc - 1 do
    begin
      var jsVal := PJSValueConstArr(argv)[i];
      arr[i] := FRuntime.JSValueToTValue(ctx, jsVal, nil);
    end;

    // Might raise an exception when wrong constructor is called
    Result := runtime.FCustomObjectFactory(FTypeInfo, arr);
  end;
  
  function TryCreateViaRTTI: Pointer;
  begin
    var rttiType := TRttiContext.Create.GetType(FTypeInfo);
    var rtti_method: TRttiMethod := nil;

    // Look for constructor with matching parameter count
    for var method in rttiType.GetMethods do
    begin
      if method.IsConstructor and (Length(method.GetParameters) = argc) then
      begin
       rtti_method := method;
       break;
      end;
    end;

    if rtti_method = nil then
      raise Exception.Create('No constructor could be found');

    // Convert arguments
    if argc > 0 then
    begin
      var params := rtti_method.GetParameters;
      SetLength(arr, argc);
      for var i := 0 to argc - 1 do
      begin
        if High(params) < i then
          raise Exception.Create('Too many parameters in call to constructor');

        arr[i] := FRuntime.JSValueToTValue(ctx, PJSValueConstArr(argv)[i], params[i].ParamType.Handle);
      end;
    end;

    if FTypeInfo.Kind = tkRecord then
    begin
      // For records, create uninitialized record and invoke constructor
      var rec_ptr: Pointer;
      GetMem(rec_ptr, FTypeInfo.TypeData.RecSize);
      try
        FillChar(rec_ptr^, FTypeInfo.TypeData.RecSize, 0);
        var rec_val: TValue;
        TValue.Make(rec_ptr, FTypeInfo, rec_val);
        
        // Invoke constructor on the record (modifies rec_val in place)
        rtti_method.Invoke(rec_val, arr);
        
        // Wrap in TRecordReference (TValue contains a copy of the record)
        Result := TRecordReference.Create(rec_val);
      finally
        // Free the temporary memory (TValue has made a copy)
        FreeMem(rec_ptr);
      end;
    end
    else
    begin
      // For classes, invoke constructor
      Result := rtti_method.Invoke(PTypeInfo(FTypeInfo)^.TypeData.ClassType, arr).AsObject;
    end;
  end;
  
  procedure HandleResult(var Ptr: Pointer);
  begin
    if Ptr = nil then
      Exit;
    
    // Interfaces are already properly ref-counted, just return
    if get_IsInterface then
      Exit;
      
    // For records, wrap in TRecordReference (but only if not already wrapped)
    if FTypeInfo.Kind = tkRecord then
    begin
      // Check if already wrapped by TryCreateViaRTTI
      if TObject(Ptr) is TRecordReference then
        Exit; // Already wrapped, don't wrap again
        
      var rec_val: TValue;
      TValue.Make(Ptr, FTypeInfo, rec_val);
      Ptr := TRecordReference.Create(rec_val);
      Exit;
    end;
    
    // For classes, check if interface support needed
    var ii: IInterface;
    if Supports(TObject(Ptr), IInterface, ii) then
      ii._AddRef;
  end;

begin
  // Try creation methods in order of preference
  Result := TryCreateViaFactory;
  
  if Result = nil then
    Result := CallConstructor;
    
  if Result = nil then
    Result := TryCreateViaRTTI;
  
  // Handle the result consistently regardless of creation method
  HandleResult(Result);
end;

function TRegisteredObject.GetArrayIndexer: IPropertyDescriptor;
begin
  Result := TRttiArrayIndexDescriptor.Create(FTypeInfo);
end;

function TRegisteredObject.GetIterator: IPropertyDescriptor;
begin
  Result := TRttiIteratorDescriptor.Create(FTypeInfo);
end;

function TRegisteredObject.GetIteratorNext: IPropertyDescriptor;
begin
  Result := TRttiIteratorNextDescriptor.Create(FTypeInfo);;
end;

function TRegisteredObject.DoOnGetMemberByName(const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean) : IPropertyDescriptor;
begin
  Handled := False;

  if Assigned(FOnGetMemberByName) then
    Result := FOnGetMemberByName(Self, AName, MemberTypes, {var} Handled);
end;

function TRegisteredObject.DoOnGetMemberNames(MemberTypes: TMemberTypes; var Handled: Boolean) : TArray<string>;
begin
  Handled := False;

  if Assigned(FOnGetMemberNames) then
    Result := FOnGetMemberNames(Self, MemberTypes, {var} Handled);
end;

function TRegisteredObject.GetMemberByName(const AName: string; MemberTypes: TMemberTypes): IPropertyDescriptor;
begin
  var handled := False;
  Result := DoOnGetMemberByName(AName, MemberTypes, handled);
  if handled then Exit;

  if AName = 'Symbol.iterator' then
  begin
    if get_ObjectSupportsEnumeration then
      Result := GetIterator;
    Exit;
  end;

  if (AName = 'next') and get_IsIterator then
  begin
    Result := GetIteratorNext;
    Exit;
  end;

  var rttictx := _RttiContext;
  var rttiType := rttictx.GetType(FTypeInfo);

  if (AName = 'get_Item') then
  begin
    var getter := rttiType.GetMethod('get_Item');
    if getter <> nil then
      Result := GetArrayIndexer;
    Exit;
  end;

  if TMemberType.Methods in MemberTypes then
  begin
    var methods := rttiType.GetMethods(AName);
    if methods <> nil then
    begin
      Result := TRttiMethodPropertyDescriptor.Create(FTypeInfo, methods, FTypeInfo.Kind = tkInterface);
      Exit;
    end;
  end;

  if TMemberType.Property in MemberTypes then
  begin
    if FTypeInfo.Kind in [tkInterface, tkRecord, tkClass] then
    begin
      // First check for accessor methods (get_/set_)
      var getter := rttiType.GetMethod('get_' + AName);
      var setter := rttiType.GetMethod('set_' + AName);

      if ((getter <> nil) and (Length(getter.GetParameters) > 0)) or ((setter <> nil) and (Length(setter.GetParameters) > 1)) then
      begin
        Result := TRttiIndexedAccessorPropertyDescriptor.Create(FTypeInfo, getter, setter);
        Exit;
      end
      else if (getter <> nil) or (setter <> nil) then
      begin
        Result := TRttiAccessorPropertyDescriptor.Create(FTypeInfo, getter, setter);
        Exit;
      end;
      
      // If no accessor methods found, check for regular properties (for tkRecord and tkClass)
      if FTypeInfo.Kind in [tkRecord, tkClass] then
      begin
        var prop := rttiType.GetProperty(AName);
        if prop <> nil then
        begin
          Result := TRttiStandardPropertyDescriptor.Create(FTypeInfo, prop);
          Exit;
        end;

        var field := rttiType.GetField(AName);
        if field <> nil then
          Result := TRttiStandardPropertyDescriptor.Create(FTypeInfo, field);
      end;
    end
    else
    begin
      var prop := rttiType.GetProperty(AName);
      if prop <> nil then
        Result := TRttiStandardPropertyDescriptor.Create(FTypeInfo, prop);
    end;
  end;
end;

function TRegisteredObject.GetMemberNames(MemberTypes: TMemberTypes) : TArray<string>;
begin
  var handled := False;
  Result := DoOnGetMemberNames(MemberTypes, handled);
  if handled then Exit;

  var rttiType := _RttiContext.GetType(FTypeInfo);

  var methods := rttiType.GetMethods;
  var properties := rttiType.GetProperties;
  var i := 0;

  SetLength(Result, Length(methods) + Length(properties));

  var m: TRttiMethod;
  for m in methods do
  begin
    Result[i] := m.Name;
    inc(i);
  end;

  var p: TRttiProperty;
  for p in properties do
  begin
    Result[i] := p.Name;
    inc(i);
  end;
end;

function TRegisteredObject.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TRegisteredObject.get_IsInterface: Boolean;
begin
  Result := FTypeInfo.Kind = tkInterface;
end;

function TRegisteredObject.get_IsObject: Boolean;
begin
  Result := FTypeInfo.Kind = tkClass;
end;

function TRegisteredObject.get_ObjectSupportsEnumeration: Boolean;
begin
  var tp := _RttiContext.GetType(FTypeInfo);
  Result := tp.GetMethod('GetEnumerator') <> nil;
end;

function TRegisteredObject.get_IsIterator : Boolean;
begin
  Result := FTypeInfo.TypeData.ClassType.InheritsFrom(TJSIterator);
end;

function TRegisteredObject.get_IsIndexedPropertyAccessor: Boolean;
begin
  Result := FTypeInfo = TypeInfo(TJSIndexedPropertyAccessor);
end;

function TRegisteredObject.get_Kind: TTypeKind;
begin
  Result := FTypeInfo.Kind;
end;

function TRegisteredObject.get_JSConstructor: JSValue;
begin
  Result := FJSConstructor;
end;

function TRegisteredObject.get_ObjectSupportsExtension: TObjectSupportsExtension;
begin
  Result := FObjectSupportsExtension;
end;

function TRegisteredObject.get_ObjectSupportsIndexing: Boolean;
begin
  Assert(False);
end;

procedure TRegisteredObject.set_ObjectSupportsExtension(const Value: TObjectSupportsExtension);
begin
  FObjectSupportsExtension := Value;
end;

function TRegisteredObject.TryGetRttiDescriptor(const PropName: string; out RttiMember: IPropertyDescriptor): Boolean;
begin
  Result := FRttiDescriptorCache.TryGetValue(PropName, RttiMember);
end;

{ TObjectReference }

constructor TObjectReference.Create(AObject: TObject);
begin
  ObjectRef := AObject;
end;

{ TAutoReference }

procedure TAutoReference.BeforeDestruction;
begin
  inherited;
  TObject(ObjectRef).Free;
end;

{ TRecordReference }
constructor TRecordReference.Create(APointer: Pointer; PType: PTypeInfo);
begin
  TValue.Make(APointer, PType, Value);
end;

constructor TRecordReference.Create(const AValue: TValue);
begin
  Value := AValue;
end;

destructor TRecordReference.Destroy;
begin
  inherited;
end;


{ TJSRuntime }

procedure TJSRuntime.BeforeDestruction;
begin
  inherited;

  // Free registration state
  FRegisteredObjectsByClassID.Free;
  FRegisteredObjectsByType.Free;
  FRegisteredObjectsByConstructor.Free;
  FRegisteredJSObjects.Free;
  FRegisteredInterfaces.Free;
  FOwnedContexts.Free;
  FPendingRegistrations.Free;
  FRegisteredEnums.Free;
  FObjectBridgeResolver := nil;

  if (_rt <> nil) and IsQuickJSLoaded then
  begin
    try
      js_std_free_handlers(_rt);
      JS_FreeRuntime(_rt);
    except

    end;
  end;
end;

class procedure TJSRuntime.Check(FuncResult: Integer);
begin
  if FuncResult <> 0 then
    raise Exception.Create('Error in conversion');
end;

class function TJSRuntime.Check(ctx: JSContext; Value: JSValue) : Boolean;
begin
  Result := Check(ctx);  // Check for Exception

  if JS_IsError(ctx, Value) then
  begin
    var err: string;

    // var s := Describe(ctx, Value);
    var name := JS_GetPropertyStr(ctx, Value, 'name');
    if not JS_IsUndefined(name) then
    begin
      var str: PAnsiChar := JS_ToCString(ctx, name);
      err := UTF8ToString(str);
      JS_FreeCString(ctx, str);
      JS_FreeValue(ctx, name);
    end;

    var msg := JS_GetPropertyStr(ctx, Value, 'message');
    if not JS_IsUndefined(msg) then
    begin
      var str: PAnsiChar := JS_ToCString(ctx, msg);
      err := err + ': ' + UTF8ToString(str);
      JS_FreeCString(ctx, str);
      JS_FreeValue(ctx, msg);
    end;

    var stack := JS_GetPropertyStr(ctx, Value, 'stack');
    if not JS_IsUndefined(stack) then
    begin
      var str: PAnsiChar := JS_ToCString(ctx, stack);
      err := err + #13#10 + UTF8ToString(str);
      JS_FreeCString(ctx, str);
      JS_FreeValue(ctx, stack);
    end;

    var jsctx := TJSRuntime.Context[ctx];
    if jsctx <> nil then
      jsctx.OutputLog(err);
    Result := False;
  end;
end;

class function TJSRuntime.Check(ctx: JSContext): Boolean;
begin
  Result := True;

  if JS_HasException(ctx) then
  begin
    var exp := JS_GetException(ctx);
    try
      var jsctx := TJSRuntime.Context[ctx];
      if jsctx = nil then
        Exit(False);

      // Prefer rich error details (name/message/stack) when possible.
      if JS_IsError(ctx, exp) then
      begin
        var err: string := '';

        var name := JS_GetPropertyStr(ctx, exp, 'name');
        if not JS_IsUndefined(name) then
        begin
          var str: PAnsiChar := JS_ToCString(ctx, name);
          err := UTF8ToString(str);
          JS_FreeCString(ctx, str);
          JS_FreeValue(ctx, name);
        end;

        var msg := JS_GetPropertyStr(ctx, exp, 'message');
        if not JS_IsUndefined(msg) then
        begin
          var str: PAnsiChar := JS_ToCString(ctx, msg);
          err := err + ': ' + UTF8ToString(str);
          JS_FreeCString(ctx, str);
          JS_FreeValue(ctx, msg);
        end;

        var stack := JS_GetPropertyStr(ctx, exp, 'stack');
        if not JS_IsUndefined(stack) then
        begin
          var str: PAnsiChar := JS_ToCString(ctx, stack);
          err := err + #13#10 + UTF8ToString(str);
          JS_FreeCString(ctx, str);
          JS_FreeValue(ctx, stack);
        end;

        jsctx.OutputLog(err);
      end
      else
      begin
        var str := JS_ToCString(ctx, exp);
        try
          jsctx.OutputLog(UTF8ToString(str));
        finally
          JS_FreeCString(ctx, str);
        end;
      end;
    finally
      JS_FreeValue(ctx, exp);
    end;

    Result := False;
  end;
end;

procedure TJSRuntime.InitializeGlobals;
begin
  // Load QuickJS library if not already loaded
  if not IsQuickJSLoaded then
    LoadQuickJS;
end;

procedure TJSRuntime.FinalizeGlobals;
begin
  // No global state to clean up - context tracking is now per-context via JS_SetContextOpaque
end;

constructor TJSRuntime.Create;
begin
  // Load QuickJS library if not already loaded
  if not IsQuickJSLoaded then
    LoadQuickJS;
  
  _rt := JS_NewRuntime;
  js_std_init_handlers(_rt);
  
  // Set a generous native stack size limit.
  // The stack_top is recorded at runtime creation, but JS may be called from
  // much deeper in the Delphi call stack, causing false "Maximum call stack size exceeded" errors.
  // Setting to 64MB accommodates deep call stacks while still protecting against infinite JS recursion.
  // Note: Setting to 0 would disable the check entirely.
  JS_SetMaxStackSize(_rt, 64 * 1024 * 1024);
  
  // Store Self in runtime opaque data for static callback access
  JS_SetRuntimeOpaque(_rt, Self);
  
  // Initialize registration state
  FAutoRegister := True;
  FFirst_ClassID := 0;
  FRegisteredObjectsByClassID := TDictionary<JSClassID, IRegisteredObject>.Create;
  FRegisteredObjectsByType := TDictionary<PTypeInfo, IRegisteredObject>.Create;
  FRegisteredObjectsByConstructor := TDictionary<JSValueConst, IRegisteredObject>.Create;
  FRegisteredJSObjects := TDictionary<JSValueConst, IRegisteredObject>.Create;
  FRegisteredInterfaces := TDictionary<TGuid, IRegisteredObject>.Create;
  FOwnedContexts := TList<Pointer>.Create;
  FPendingRegistrations := TList<TRegistrationInfo>.Create;
  FRegisteredEnums := TDictionary<PTypeInfo, string>.Create;

  // Initialize ObjectBridge resolver
  FObjectBridgeResolver := TObjectBridgeResolver.Create;

  // Initialize exotics
  InitializeExotics;

  // Register default ObjectBridge mappings (e.g. forEach)
  TObjectBridgeDefaultDefinitions.Initialize(Self);

  // Register internal QuickJS types on this runtime
  RegisterObjectWithConstructor('JSIterator', System.TypeInfo(TJSIterator), nil);
  RegisterObjectWithConstructor('JSIndexedPropertyAccessor', System.TypeInfo(TJSIndexedPropertyAccessor), nil);
end;

function TJSRuntime.CreateContext: IJSContext;
begin
  if (_rt = nil) or not IsQuickJSLoaded then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := TJSContext.Create(Self);
end;

function TJSRuntime.get_LogString: TProc<string>;
begin
  Result := _LogString;
end;

function TJSRuntime.get_rt: JSRuntime;
begin
  Result := _rt;
end;

procedure TJSRuntime.OutputLog(const Value: string);
begin
  if Assigned(_LogString) then
    _LogString(Value);
end;

class function TJSRuntime.get_Context(const Value: JSContext): IJSContext;
begin
  // Use QuickJS's built-in context opaque data
  // We store the IJSContext interface pointer directly (not object pointer)
  // This allows retrieval without knowing concrete class, avoiding circular dependencies
  var ptr := JS_GetContextOpaque(Value);
  if ptr <> nil then
    Result := IJSContext(ptr)  // Direct interface pointer retrieval
  else
    Result := nil;
end;

procedure TJSRuntime.set_LogString(const Value: TProc<string>);
begin
  _LogString := Value;
end;

function TJSRuntime.get_ObjectBridgeResolver: IInterface;
begin
  Result := FObjectBridgeResolver;
end;

function TJSRuntime.get_AutoRegister: Boolean;
begin
  Result := FAutoRegister;
end;

procedure TJSRuntime.set_AutoRegister(const Value: Boolean);
begin
  FAutoRegister := Value;
end;

procedure TJSRuntime.InitializeExotics;
begin
  FExotics.get_own_property := TJSRuntime.get_own_property;
  FExotics.get_own_property_names := TJSRuntime.get_own_property_names;
  FExotics.delete_property := TJSRuntime.delete_property;
  FExotics.define_own_property := TJSRuntime.define_own_property;
  FExotics.has_property := nil;
  FExotics.get_property := nil;
  FExotics.set_property := nil;
end;

procedure TJSRuntime.AddRegisteredObjectWithClassID(const RegisteredObject: IRegisteredObject);
begin
  FRegisteredObjectsByClassID.Add(RegisteredObject.ClassID, RegisteredObject);
end;

function TJSRuntime.CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject;
begin
  Result := TRegisteredObject.Create(Self, ATypeInfo, AConstructor);
end;

function TJSRuntime.CreateRegisteredJSObject(ctx: JSContext; JSConstructor: JSValueConst; ATypeInfo: PTypeInfo): IRegisteredObject;
begin
  raise ENotSupportedException.Create('Wrapping of JS objects requires dn4d extensions');
end;

procedure TJSRuntime.InternalRegisterType(const ctx: IJSContext; const Reg: IRegisteredObject; ClassName: string);
begin
  DoInternalRegisterType(ctx, Reg, ClassName);
end;

procedure TJSRuntime.DoInternalRegisterType(const ctx: IJSContext; const Reg: IRegisteredObject; ClassName: string);
var
  obj,global : JSValue;
  JClass : JSClassDef;
  classID: JSClassID;
  classProto: JSValue;
  jsctx: JSContext;
  needsRuntimeRegistration: Boolean;
begin
  var s: UTF8String := UTF8Encode(ClassName);
  JClass.class_name := PAnsiChar(s);
  JClass.finalizer := @TJSRuntime.CFinalize;
  JClass.gc_mark := nil;
  JClass.call := nil;
  JClass.exotic := @FExotics;

  jsctx := ctx.ctx;

  // Check if this needs runtime-level registration (first time only)
  needsRuntimeRegistration := Reg.ClassID = 0;
  
  if needsRuntimeRegistration then
  begin
    // Runtime-level registration (only once)
    // Create New Class ID
    classID := 0;
    JS_NewClassID(JS_GetRuntime(jsctx), @classID);

    if FFirst_ClassID = 0 then
      FFirst_ClassID := classID;

    Reg.ClassID := classID;
    
    // Register the class at runtime level (only once per ClassID)
    JS_NewClass(JS_GetRuntime(jsctx), classID, @JClass);
  end
  else
    classID := Reg.ClassID;

  // Context-level registration (once per context)
  // New Object act as Prototype for the Class.
  classProto := JS_NewObject(jsctx);

  // Set the Prototype to the Class.
  JS_SetClassProto(jsctx, classID, classProto);

  // Set the Class native constructor.
  obj := JS_NewCFunction2(jsctx, @TJSRuntime.CConstructor, JClass.class_name, 1, JS_CFUNC_constructor_magic, classID);

  // Add the Class to Global Object so we can use it.
  global := JS_GetGlobalObject(jsctx);
  JS_SetPropertyStr(jsctx, global, JClass.class_name, obj);
  JS_FreeValue(jsctx, global);

  // Store JSConstructor only on first registration
  if needsRuntimeRegistration then
    Reg.JSConstructor := JS_DupValue(jsctx, obj);

  if Reg.IsInterface then
    InternalRegisterInterface(ctx, Reg, ClassName);
end;

procedure TJSRuntime.InternalRegisterInterface(const ctx: IJSContext; const Reg: IRegisteredObject; ClassName: string);
begin
  var g := Reg.GetTypeInfo.TypeData.GUID;
  if not g.IsEmpty then
  begin
    TMonitor.Enter(FRegisteredInterfaces);
    try
      if not FRegisteredInterfaces.ContainsKey(g) then
        FRegisteredInterfaces.Add(g, Reg);
    finally
      TMonitor.Exit(FRegisteredInterfaces);
    end;
  end;
end;

procedure TJSRuntime.InternalRegisterEnum(const ctx: IJSContext; EnumTypeInfo: PTypeInfo; const EnumJSName: string);
begin
  var jsctx := ctx.ctx;
  var global := JS_GetGlobalObject(jsctx);
  
  // Create a plain JavaScript object to hold enum values
  var enumObj := JS_NewObject(jsctx);
  
  // Get enum type data
  var typeData := GetTypeData(EnumTypeInfo);
  var minValue := typeData.MinValue;
  var maxValue := typeData.MaxValue;
  
  // Add each enum value as a property
  for var i := minValue to maxValue do
  begin
    var enumName := GetEnumName(EnumTypeInfo, i);
    if enumName <> '' then
    begin
      var propName := UTF8Encode(enumName);
      var enumValue := JS_NewInt32(jsctx, i);
      JS_SetPropertyStr(jsctx, enumObj, PAnsiChar(propName), enumValue);
    end;
  end;
  
  // Add the enum object to global scope
  var globalName := UTF8Encode(EnumJSName);
  JS_SetPropertyStr(jsctx, global, PAnsiChar(globalName), enumObj);
  JS_FreeValue(jsctx, global);
end;

procedure TJSRuntime.RegisterContext(const Context: IJSContext);
begin
  TMonitor.Enter(FOwnedContexts);
  try
    FOwnedContexts.Add(Pointer(Context));
  finally
    TMonitor.Exit(FOwnedContexts);
  end;

  // Apply all registered enums to this new context
  TMonitor.Enter(FRegisteredEnums);
  try
    for var kvp in FRegisteredEnums do
      InternalRegisterEnum(Context, kvp.Key, kvp.Value);
  finally
    TMonitor.Exit(FRegisteredEnums);
  end;

  // Apply all pending registrations to this new context
  TMonitor.Enter(FPendingRegistrations);
  try
    for var i := 0 to FPendingRegistrations.Count - 1 do
    begin
      var regInfo := FPendingRegistrations[i];
      var reg: IRegisteredObject;
      
      // Check if this type was already registered at the runtime level
      if not FRegisteredObjectsByType.TryGetValue(regInfo.TypeInfo, reg) then
      begin
        reg := CreateRegisteredObject(regInfo.TypeInfo, regInfo.ConstructorFunc);
        FRegisteredObjectsByType.Add(regInfo.TypeInfo, reg);
      end;
      
      // Always register the type in this new context
      InternalRegisterType(Context, reg, regInfo.ClassName);
      
      // Add to runtime collections only once
      if not FRegisteredObjectsByClassID.ContainsKey(reg.ClassID) then
        AddRegisteredObjectWithClassID(reg);
      
      if not FRegisteredObjectsByConstructor.ContainsKey(reg.JSConstructor) then
        FRegisteredObjectsByConstructor.Add(reg.JSConstructor, reg);
    end;
  finally
    TMonitor.Exit(FPendingRegistrations);
  end;
end;

procedure TJSRuntime.UnregisterContext(const Context: IJSContext);
begin
  TMonitor.Enter(FOwnedContexts);
  try
    FOwnedContexts.Remove(Pointer(Context));
  finally
    TMonitor.Exit(FOwnedContexts);
  end;
end;

// Interface method implementations (with distinct names to avoid Delphi interface overload issues)
function TJSRuntime.RegisterObjectType(ClassName: string; TypeInfo: PTypeInfo): IRegisteredObject;
begin
  Result := RegisterObjectWithConstructor(ClassName, TypeInfo, nil);
end;

function TJSRuntime.RegisterObjectWithConstructor(ClassName: string; TypeInfo: PTypeInfo; AConstructor: TObjectConstuctor): IRegisteredObject;
begin
  // Handle enum types specially
  if TypeInfo.Kind = tkEnumeration then
  begin
    TMonitor.Enter(FRegisteredEnums);
    try
      if FRegisteredEnums.ContainsKey(TypeInfo) then
        Exit(nil); // Already registered
      FRegisteredEnums.Add(TypeInfo, ClassName);
    finally
      TMonitor.Exit(FRegisteredEnums);
    end;
    
    // Apply to all existing contexts
    TMonitor.Enter(FOwnedContexts);
    try
      for var i := 0 to FOwnedContexts.Count - 1 do
      begin
        var ctx: IJSContext := IJSContext(FOwnedContexts[i]);
        InternalRegisterEnum(ctx, TypeInfo, ClassName);
      end;
    finally
      TMonitor.Exit(FOwnedContexts);
    end;
    
    Exit(nil); // Enums don't return a registered object
  end;

  TMonitor.Enter(FRegisteredObjectsByType);
  try
    if FRegisteredObjectsByType.TryGetValue(TypeInfo, Result) then
      Exit; // Already registered
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;

  // Store registration info for future contexts
  var regInfo: TRegistrationInfo;
  regInfo.ClassName := ClassName;
  regInfo.TypeInfo := TypeInfo;
  regInfo.ConstructorFunc := AConstructor;
  
  TMonitor.Enter(FPendingRegistrations);
  try
    FPendingRegistrations.Add(regInfo);
  finally
    TMonitor.Exit(FPendingRegistrations);
  end;

  Result := CreateRegisteredObject(TypeInfo, AConstructor);
  
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    FRegisteredObjectsByType.Add(TypeInfo, Result);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;

  // Apply to all existing contexts
  TMonitor.Enter(FOwnedContexts);
  try
    for var i := 0 to FOwnedContexts.Count - 1 do
    begin
      var ctx: IJSContext := IJSContext(FOwnedContexts[i]);
      InternalRegisterType(ctx, Result, ClassName);
      
      if not FRegisteredObjectsByClassID.ContainsKey(Result.ClassID) then
        AddRegisteredObjectWithClassID(Result);
      
      if not FRegisteredObjectsByConstructor.ContainsKey(Result.JSConstructor) then
        FRegisteredObjectsByConstructor.Add(Result.JSConstructor, Result);
    end;
  finally
    TMonitor.Exit(FOwnedContexts);
  end;
end;

// Convenience overloads (not part of interface)
function TJSRuntime.RegisterJSObject(const ctx: IJSContext; JSConstructor: JSValueConst; TypeInfo: PTypeInfo): IRegisteredObject;
begin
  Result := CreateRegisteredJSObject(ctx.ctx, JSConstructor, TypeInfo);

  TMonitor.Enter(FRegisteredJSObjects);
  try
    FRegisteredJSObjects.Add(JSConstructor, Result);
  finally
    TMonitor.Exit(FRegisteredJSObjects);
  end;
end;

// Convenience overloads (not part of interface)
function TJSRuntime.TryGetRegisteredObjectFromJSValue(Value: JSValue; out AObject: TRegisteredObjectWithPtr): Boolean;
begin
  AObject.Reg := nil;
  AObject.Ptr := nil;

  var classID := TJSRuntime.GetClassID(Value);
  if classID >= FFirst_ClassID then
  begin
    if not TryGetRegisteredObjectFromClassID(classID, AObject.Reg) then
      raise EArgumentException.Create('Class not registered');

    AObject.Ptr := JS_GetOpaque(Value, classID);
    if AObject.Reg.IsObject then
    begin
      if (TObject(AObject.Ptr) is TObjectReference) then
        AObject.Ptr := (TObject(AObject.Ptr) as TObjectReference).ObjectRef
      else if (TObject(AObject.Ptr) is TRecordReference) then
        AObject.Ptr := (TObject(AObject.Ptr) as TRecordReference).Value.GetReferenceToRawData;
    end;

    Exit(True);
  end;
  Exit(False);
end;

function TJSRuntime.TryGetRegisteredObjectFromClassID(ClassID: JSClassID; out RegisteredObject: IRegisteredObject): Boolean;
begin
  TMonitor.Enter(FRegisteredObjectsByClassID);
  try
    Result := FRegisteredObjectsByClassID.TryGetValue(ClassID, RegisteredObject);
  finally
    TMonitor.Exit(FRegisteredObjectsByClassID);
  end;
end;

function TJSRuntime.TryGetRegisteredObjectFromTypeInfo(TypeInfo: PTypeInfo; out RegisteredObject: IRegisteredObject): Boolean;
begin
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    Result := FRegisteredObjectsByType.TryGetValue(TypeInfo, RegisteredObject);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
end;

function TJSRuntime.TryGetRegisteredObjectFromConstructor(JSConstructor: JSValueConst; out RegisteredObject: IRegisteredObject): Boolean;
begin
  TMonitor.Enter(FRegisteredObjectsByConstructor);
  try
    Result := FRegisteredObjectsByConstructor.TryGetValue(JSConstructor, RegisteredObject);
  finally
    TMonitor.Exit(FRegisteredObjectsByConstructor);
  end;
end;

function TJSRuntime.TryGetRegisteredJSObject(JSConstructor: JSValueConst; out RegisteredObject: IRegisteredObject): Boolean;
begin
  TMonitor.Enter(FRegisteredJSObjects);
  try
    Result := FRegisteredJSObjects.TryGetValue(JSConstructor, RegisteredObject);
  finally
    TMonitor.Exit(FRegisteredJSObjects);
  end;
end;

function TJSRuntime.TryGetRegisteredInterface(const IID: TGuid; out RegisteredObject: IRegisteredObject): Boolean;
begin
  TMonitor.Enter(FRegisteredInterfaces);
  try
    Result := FRegisteredInterfaces.TryGetValue(IID, RegisteredObject);
  finally
    TMonitor.Exit(FRegisteredInterfaces);
  end;
end;

procedure TJSRuntime.SetCustomObjectFactory(const Factory: TCustomObjectFactory);
begin
  FCustomObjectFactory := Factory;
end;

class function TJSRuntime.WaitForJobs(Ctx: JSContext; APromise: JSValue) : JSValue;
begin
  while JS_IsJobPending(JS_GetRuntime(Ctx)) do
  begin
    var unused: JSContext;
    var r := JS_ExecutePendingJob(JS_GetRuntime(Ctx), @unused);
    if r < 0 then
      raise Exception.Create('Job error');
  end;

  if JS_IsPromise(APromise) then
  begin
    var resolve := JS_PromiseResult(Ctx, APromise);
    TJSRuntime.Check(Ctx, resolve);
    JS_FreeValue(Ctx, APromise);
    Result := resolve;
  end else
    Result := APromise;
end;
{ TJSContext }

procedure TJSContext.BeforeDestruction;
begin
  if _ctx <> nil then
  begin
    // Clear the context opaque data before freeing
    if IsQuickJSLoaded then
      JS_SetContextOpaque(_ctx, nil);
    
    if _runtime <> nil then
      (_runtime as TJSRuntime).UnregisterContext(Self);
    
    if IsQuickJSLoaded then
    begin
      try
        JS_FreeContext(_ctx);
      except
        // Ignore exceptions during cleanup
      end;
    end;
  end;
  
  inherited;
end;

constructor TJSContext.Create(const Runtime: IJSRuntime);
begin
  _runtime := Runtime;
  
  if (_runtime = nil) or (_runtime.rt = nil) or not IsQuickJSLoaded then
  begin
    _ctx := nil;
    Exit;
  end;
  
  _ctx := JS_NewContext(_runtime.rt);
  
  // Store interface pointer in context opaque data - allows retrieval without knowing concrete class
  // This enables PropertyDescriptors to get IJSRuntime through IJSContext.Runtime without circular dependency
  JS_SetContextOpaque(_ctx, Pointer(Self as IJSContext));
  
  // Register this context with the runtime for type registrations
  (_runtime as TJSRuntime).RegisterContext(Self);
  
  Initialize;
end;

function TJSContext.eval_internal(Buf: PAnsiChar; buf_len: Integer; FileName: PAnsiChar; eval_flags: Integer): Integer;
begin
  var res := JS_Eval(_ctx, buf, buf_len, Filename, eval_flags);
  if not TJSRuntime.Check(_ctx, res) then Exit;
  res := TJSRuntime.WaitForJobs(_ctx, res);
  TJSRuntime.Check(_ctx, res);
  JS_FreeValue(_ctx, res);
end;

procedure TJSContext.eval(const Code: string; const CodeContext: string);
begin
  var buf: UTF8String := UTF8Encode(Code);
  var filename: UTF8String := UTF8Encode(CodeContext) + #0;
  eval_internal(PAnsiChar(buf), Length(buf), PAnsiChar(filename), JS_EVAL_TYPE_MODULE);
end;

function TJSContext.eval_with_result(const Code: string; const CodeContext: string): TValue;
begin
//  var buf: AnsiString := 'export function __run__() {return ' + AnsiString(Code) + ';}';
   var buf: UTF8String := UTF8Encode(Code + #13#10 + ';export function __run__() { return typeof resultValue !== "undefined" ? resultValue : "resultValue is undefined"; }');
   var filename: UTF8String := UTF8Encode(CodeContext) + #0;

  // var bytecode := JS_Eval(_ctx, PAnsiChar(buf), Length(buf), PAnsiChar(CodeContext), JS_EVAL_TYPE_MODULE or JS_EVAL_FLAG_COMPILE_ONLY);
  var bytecode := JS_Eval(_ctx, PAnsiChar(buf), Length(buf), PAnsiChar(filename), JS_EVAL_TYPE_MODULE or JS_EVAL_FLAG_COMPILE_ONLY);
  bytecode := TJSRuntime.WaitForJobs(_ctx, bytecode);

  if not TJSRuntime.Check(_ctx, bytecode)
    then Exit;
  JS_EvalFunction(_ctx, bytecode);

  bytecode := TJSRuntime.WaitForJobs(_ctx, bytecode);

  var moduledef := JS_VALUE_GET_PTR(bytecode);
  var namespace := JS_GetModuleNamespace(_ctx, moduledef);

  var func := JS_GetPropertyStr(_ctx, namespace, '__run__');
  var res  := JS_Call(_ctx, func, JS_UNDEFINED, 0, nil);

  if not TJSRuntime.Check(_ctx, res) then Exit;
  res := TJSRuntime.WaitForJobs(_ctx, res);
  TJSRuntime.Check(_ctx, res);

  Result := (_runtime as TJSRuntime).JSValueToTValue(_ctx, res, nil);

  JS_FreeValue(_ctx, res);
end;

//function TJSContext.eval_with_result_old(const Code: string; const CodeContext: string): TValue;
//begin
////  var buf: AnsiString := 'export function __run__() {return ' + AnsiString(Code) + ';}';
//   var buf: AnsiString := AnsiString(Code)  + ' ;export function __run__() { return resultValue; }';
//   var filename: AnsiString := AnsiString(CodeContext) + #0;
//
//  // var bytecode := JS_Eval(_ctx, PAnsiChar(buf), Length(buf), PAnsiChar(CodeContext), JS_EVAL_TYPE_MODULE or JS_EVAL_FLAG_COMPILE_ONLY);
//  var bytecode := JS_Eval(_ctx, PAnsiChar(buf), Length(buf), PAnsiChar(filename), JS_EVAL_TYPE_MODULE or JS_EVAL_FLAG_COMPILE_ONLY);
//
//  if not TJSRuntime.Check(_ctx, bytecode) then Exit;
//  JS_EvalFunction(_ctx, bytecode);
//
//  var moduledef := JS_VALUE_GET_PTR(bytecode);
//  var namespace := JS_GetModuleNamespace(_ctx, moduledef);
//
//  var func := JS_GetPropertyStr(_ctx, namespace, '__run__');
//  var res  := JS_Call(_ctx, func, JS_UNDEFINED, 0, nil);
//
//  if not TJSRuntime.Check(_ctx, res) then Exit;
//  res := TJSRuntime.WaitForJobs(_ctx, res);
//  TJSRuntime.Check(_ctx, res);
//
//  Result := TJSRuntime.Instance.JSValueToTValue(_ctx, res, nil);
//
//  JS_FreeValue(_ctx, res);
//end;

class function TJSContext.logme(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue;
var
  i : Integer;
  str : PAnsiChar;
  jsctx: IJSContext;
begin
  jsctx := TJSRuntime.Context[ctx];
  
  for i := 0 to Pred(argc) do
  begin
     str := JS_ToCString(ctx, argv[i]);
     if not Assigned(str) then
       Exit(JS_EXCEPTION);

     var s: string;
     try
       s := UTF8ToString(str);
     finally
       JS_FreeCString(ctx, str);
     end;

     if jsctx <> nil then
       jsctx.OutputLog(s);
  end;

  Result := JS_UNDEFINED;
end;

//function TJSContext.fetch(ctx : JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue;
//begin
//  var f: IFetch := TFetch.Create(ctx, argc, argv);
//  Result := f.Promise.Value;
//end;

function TJSContext.get_ctx: JSContext;
begin
  Result := _ctx;
end;

function TJSContext.get_Runtime: IJSRuntime;
begin
  Result := _runtime;
end;

function TJSContext.get_LogString: TProc<string>;
begin
  Result := _LogString;
end;

procedure TJSContext.set_LogString(const Value: TProc<string>);
begin
  _LogString := Value;
end;

procedure TJSContext.OutputLog(const Value: string);
begin
  if Assigned(_LogString) then
    _LogString(Value);
end;

procedure TJSContext.RegisterLiveObjectInstance(ObjectName: string; AObject: TObject; OwnsObject: Boolean);
begin
  var runtime := _runtime as TJSRuntime;
  TMonitor.Enter(runtime.FRegisteredObjectsByType);
  try
    var tp := PTypeInfo(AObject.ClassInfo);
    var reg: IRegisteredObject := nil;

    if not runtime.FRegisteredObjectsByType.TryGetValue(tp, reg) then
    begin
      runtime.RegisterObjectType(AObject.ClassName, tp);
      reg := runtime.FRegisteredObjectsByType[tp];
    end;

    var global := JS_GetGlobalObject(_ctx);
    var jsval := JS_NewObjectClass(_ctx, reg.ClassID);

    var ptr: Pointer;
    if OwnsObject then
      ptr := Pointer(AObject)
    else
      ptr := TObjectReference.Create(TObject(AObject));

    JS_SetOpaque(jsval, ptr);
    var s: UTF8String := UTF8Encode(ObjectName);
    JS_SetPropertyStr(_ctx, global, PAnsiChar(s), jsval);
    JS_FreeValue(_ctx, global);
  finally
    TMonitor.Exit(runtime.FRegisteredObjectsByType);
  end;
end;

procedure TJSContext.RegisterLiveInterfaceInstance(ObjectName: string; TypeInfo: PTypeInfo; const AInterface: IInterface);
begin
  var runtime := _runtime as TJSRuntime;
  TMonitor.Enter(runtime.FRegisteredObjectsByType);
  try
    var reg: IRegisteredObject := nil;

    if not runtime.FRegisteredObjectsByType.TryGetValue(TypeInfo, reg) then
    begin
      runtime.RegisterObjectType(string(TypeInfo.Name), TypeInfo);
      reg := runtime.FRegisteredObjectsByType[TypeInfo];
    end;

    var global := JS_GetGlobalObject(_ctx);
    var jsval := JS_NewObjectClass(_ctx, reg.ClassID);

    // Bump ref count required
    AInterface._AddRef;
    JS_SetOpaque(jsval, Pointer(AInterface));
    var s: UTF8String := UTF8Encode(ObjectName);
    JS_SetPropertyStr(_ctx, global, PAnsiChar(s), jsval);
    JS_FreeValue(_ctx, global);
  finally
    TMonitor.Exit(runtime.FRegisteredObjectsByType);
  end;
end;

procedure TJSContext.Initialize;
var
  global : JSValue;

const
  // Keep only the explicitly allowed built-in helper(s) on globalThis.
  // SECURITY: Do NOT expose QuickJS std/os here (filesystem/process access).
  bjson_helper : PAnsiChar =
    'import * as bjson from ''qjs:bjson'';'#10+
    'globalThis.bjson = bjson;'#10;

  console_log : PAnsiChar = 'console.log=log;'#10;

  add_fetch: PAnsiChar =
    'globalThis.fetch = function(url, options = {}) {'#10+
    '  return new Promise((resolve,reject) => {'#10+
    '    const xhr = new XMLHttpRequest();'#10+
    '    const method = options.method || ''GET'';'#10+
    ''#10+
    '    // Set headers if provided'#10+
    '    if (options.headers) {'#10+
    '      for (const [key, value] of Object.entries(options.headers)) {'#10+
    '        xhr.setRequestHeader(key, value);'#10+
    '      }'#10+
    '    }'#10+
    ''#10+
    '    xhr.open(method, url, false /* run synchronous*/);'#10+
    ''#10+
    '    if(options.body != null)'#10+
    '      xhr.send(options.body);'#10+
    '    else'#10+
    '      xhr.send();'#10+
    '    '#10+
    '    if(xhr.readyState == 4) {'#10+
    '      const response = {'#10+
    '        ok: xhr.status >= 200 && xhr.status < 300,'#10+
    '        status: xhr.status,'#10+
    '        statusText: xhr.statusText,'#10+
    '        url: xhr.responseURL,'#10+
    '        text: () => Promise.resolve(xhr.responseText),'#10+
    '        json: () => Promise.resolve(JSON.parse(xhr.responseText)),'#10+
    '        blob: () => Promise.resolve(new Blob([xhr.response])),'#10+
    '      };'#10+
    '      resolve(response);'#10+
    '    } else'#10+
    '      reject(''XMLHttpRequest.readyState invalid: '' + xhr.readyState);'#10+
    '  });'#10+
    '};'#10
//    'import { DateTime, Duration, Interval, Settings } from ''./luxon.js'';'#10+
//    + 'globalThis.DateTime = DateTime;'#10+
//    'globalThis.Duration = Duration;'#10+
//    'globalThis.Interval = Interval;'#10+
//    'globalThis.Settings = Settings;'#10
;

begin
  if (_ctx = nil) or not IsQuickJSLoaded then
    Exit;

  // ES6 Module loader.
  JS_SetModuleLoaderFunc(_runtime.rt, nil, @js_module_loader, nil);

  JS_SetHostPromiseRejectionTracker(_runtime.rt, @js_std_promise_rejection_tracker, nil);

  js_std_add_helpers(_ctx, 0, nil);

  // SECURITY: Do not register std/os modules (they enable filesystem/process access).
  js_init_module_bjson(_ctx, 'qjs:bjson');

  eval_internal(bjson_helper, Length(bjson_helper), 'initialize', JS_EVAL_TYPE_MODULE);

  global := JS_GetGlobalObject(_ctx);

  // Define a function in the global context.
  JS_SetPropertyStr(_ctx, global, 'log', JS_NewCFunction(_ctx, @TJSContext.logme, 'log', 1));
  JS_SetPropertyStr(_ctx, global, 'alert', JS_NewCFunction(_ctx, @TJSContext.logme, 'alert', 1));

  eval_internal(console_log, Length(console_log), 'initialize', JS_EVAL_TYPE_MODULE);
  eval_internal(add_fetch, Length(add_fetch), 'initialize', JS_EVAL_TYPE_MODULE);

  JS_FreeValue(_ctx, global);
  
  // js_std_loop should NOT be called here during initialization
  // WaitForJobs during script execution already handles pending jobs
  // Calling js_std_loop here can cause crashes with unhandled promise rejections
  // js_std_loop(_ctx);
end;

{ TJSIndexedPropertyAccessor }
constructor TJSIndexedPropertyAccessor.Create(const Runtime: TJSRuntime; ctx: JSContext; this_obj: JSValue; prop: IPropertyDescriptor);
begin
  _Runtime := Runtime;
  _ctx := ctx;
  _this_obj := this_obj;
  _prop := prop;
end;

destructor TJSIndexedPropertyAccessor.Destroy;
begin
  inherited;
  
  if (_ctx <> nil) and IsQuickJSLoaded then
    JS_FreeValue(_ctx, _this_obj);
end;


procedure _InitRttiPool;
begin
 _RttiContext := TRttiContext.Create;
 _RttiContext.FindType('');
end;


initialization
  _InitRttiPool;


finalization
  _RttiContext.Free();

end.


