unit QuickJS.Register.impl;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  quickjs,
  QuickJS.Register.intf;

type
  TProc_0 = TProc;
  TProc_1 = TProc<Pointer>;
  TProc_Double = TProc<Double>;

//  // Used to pass function parameters.
//  // We can pass a maximum of 20 parameters to a function.
//  JSValueConstArray = array[0..19] of JSValueConst;
//  PJSValueConstArray = ^JSValueConstArray;

  TJSRuntime = class(TInterfacedObject, IJSRuntime)
  protected
    class var _ActiveContexts: TDictionary<JSContext, Pointer {Unsafe IJSContext pointer}>;
    var _rt: JSRuntime;

    function  get_rt: JSRuntime;
    function  get_LogString: TProc<string>;
    procedure set_LogString(const Value: TProc<string>);
    procedure OutputLog(const Value: string);

    class procedure RegisterContext(Ctx: JSContext; const Context: IJSContext);
    class procedure UnRegisterContext(Ctx: JSContext);

  public
    class constructor Create;
    class destructor Destroy;
    constructor Create;
    procedure  BeforeDestruction; override;

    class function  Check(ctx: JSContext; Val: JSValue) : Boolean; overload;
    class procedure Check(FuncResult: Integer); overload;

    function CreateContext: IJSContext;
  end;

  TJSContext = class(TInterfacedObject, IJSContext)
  protected
    _ctx: JSContext;
    _runtime: IJSRuntime;

    function  get_ctx: JSContext;
    function  get_Runtime: IJSRuntime;

    function  eval_buf(Buf: PAnsiChar; buf_len: Integer; filename: PAnsiChar; eval_flags: Integer): Integer;
    class function logme(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
    procedure Initialize;

  public
    constructor Create(const Runtime: IJSRuntime);
    procedure  BeforeDestruction; override;
  end;

  TJSRegister = class
  protected
    class var FInstance: TJSRegister;
    class procedure set_Instance(Value: TJSRegister); static;

    class var JS_DATE_CONSTRUCTOR: JSValue;

  protected
    class var FAutoRegister: Boolean;
    class var FExotics: JSClassExoticMethods;
    class var FRegisteredObjectsByClassID: TDictionary<JSClassID, IRegisteredObject>;
    class var FRegisteredObjectsByType: TDictionary<PTypeInfo, IRegisteredObject>;

    class procedure AddRegisteredObjectWithClassID(const RegisteredObject: IRegisteredObject);
    class procedure InternalRegisterType(const Reg: IRegisteredObject; ctx: JSContext; ClassName: string); virtual;

    function CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor) : IRegisteredObject; virtual;

    class function GenericInvokeCallBack(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericGetItem(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericSetItem(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericClassIterator(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue): JSValue; cdecl; static;
    class function  GenericIteratorNext(ctx : JSContext; this_val : JSValueConst;
      argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
    class function GenericMethodCallData(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericPropertyGetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericPropertySetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function ExtendablePropertyGetter(ctx: JSContext; obj: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function ExtendablePropertySetter(ctx: JSContext; obj: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;

    class function  CConstructor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr; magic : Integer): JSValue; cdecl; static;
    class procedure CFinalize(rt : JSRuntime; this_val : JSValue); cdecl; static;

    class function get_own_property(ctx: JSContext; desc: PJSPropertyDescriptor; obj: JSValueConst; prop: JSAtom) : Integer; cdecl; static;
    class function get_own_property_names (ctx: JSContext; ptab: PPJSPropertyEnum; plen: pUInt32; obj: JSValueConst) : Integer;cdecl; static;
    class function delete_property(ctx: JSContext; obj: JSValueConst; prop: JSAtom) : Integer;cdecl; static;
    class function define_own_property(ctx: JSContext; obj: JSValueConst; prop: JSAtom;
      val:JSValueConst; getter:JSValueConst;
      setter:JSValueConst; flags:Integer):Integer; cdecl; static;

  public
    class constructor Create;
    class destructor  Destroy;

    class function  CreateCallback_0(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_0;
    class function  CreateCallback_Double(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_Double;

    class function  JS_NewDate(ctx: JSContext; epoch_ms: Double) : JSValue;

    class function  GetClassID(Value: JSValueConst) : JSClassID;
    class function  GetClassName(Value: PTypeInfo) : string;
    class function  GetObjectFromJSValue(Value: JSValueConst; PointerIsAnObject: Boolean) : Pointer;

    class function  RegisterObject(ctx: JSContext; ClassName: string; TypeInfo: PTypeInfo) : IRegisteredObject; overload;
    class function  RegisterObject(ctx: JSContext; ClassName: string; TypeInfo: PTypeInfo; AConstructor: TObjectConstuctor) : IRegisteredObject; overload;

    class procedure RegisterLiveObject(ctx: JSContext; ObjectName: string; AObject: TObject; OwnsObject: Boolean); overload;
    class procedure RegisterLiveObject(ctx: JSContext; ObjectName: string; TypeInfo: PTypeInfo; const AInterface: IInterface); overload;
    class function  TryGetRegisteredObjectFromClassID(ClassID: JSClassID; out RegisteredObject: IRegisteredObject) : Boolean;
    class function  TryGetRegisteredObjectFromTypeInfo(TypeInfo: PTypeInfo; out RegisteredObject: IRegisteredObject) : Boolean;

    class property AutoRegister: Boolean read FAutoRegister write FAutoRegister;
    class property Instance: TJSRegister read FInstance write set_Instance;
  end;

  JSConverter = class
  protected
    class var FInstance: JSConverter;
    class procedure set_Instance(Value: JSConverter); static;

  public
    class constructor Create;
    class destructor  Destroy;

    class function GetDefaultValue(const Param: TRttiParameter) : TValue;
    function JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo) : TValue; virtual;
    function TValueToJSValue(ctx: JSContext; const Value: TValue) : JSValue; virtual;
    class function TestParamsAreCompatible(ctx: JSContext; const Param: TRttiParameter; Value: JSValue) : Boolean;

    class property Instance: JSConverter read FInstance write set_Instance;
  end;

  TJSIterator = class abstract
  public
    function MoveNext: Boolean; virtual; abstract;
    function Current: TValue; virtual; abstract;
  end;

  TJSObjectIterator = class(TJSIterator)
  protected
    _instance: TObject;
    _move_next: PObjectMember;
    _get_current: PObjectMember;

  public
    destructor Destroy; override;
    class function CreateIterator(const Obj: TObject) : TJSIterator;

    function MoveNext: Boolean; override;
    function Current: TValue; override;
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

  TPropertyDescriptor = class(TInterfacedObject, IPropertyDescriptor)
  protected
    FTypeInfo: PTypeInfo;

    function  get_MemberType: TMemberType; virtual;
    function  get_TypeInfo: PTypeInfo; virtual;
    function  get_PropertyType: PTypeInfo; virtual;
    function IsInterface: Boolean; virtual;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; virtual;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); virtual;
  public
    constructor Create(AInfo: PTypeInfo); virtual;
  end;

  TRttiStandardPropertyDescriptor = class(TPropertyDescriptor)
  protected
    FProp: TRttiMember;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const RttiProp: TRttiMember);
  end;

  TRttiMethodPropertyDescriptor = class(TPropertyDescriptor, IMethodsPropertyDescriptor)
  protected
    FIsInterface: Boolean;
    FMethods: TArray<TRttiMethod>; // Multiple methods can have the same name

    function  get_MemberType: TMemberType; override;
    function  Methods: TArray<TRttiMethod>;
    function  Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue; virtual;

    function  SelectMethodMatchArguments(ctx: JSContext; argc: Integer; argv: PJSValueConst): TRttiMethod;
  public
    constructor Create(AInfo: PTypeInfo; const RttiMethods: TArray<TRttiMethod>; IsInterface: Boolean);
  end;

  TRttiInterfacePropertyDescriptor = class(TPropertyDescriptor)
  protected
    FGetter: TRttiMethod;
    FSetter: TRttiMethod;

    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo; const RttiGetter: TRttiMethod; const RttiSetter: TRttiMethod);
  end;

  TRttiArrayIndexDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
    function  get_PropertyType: PTypeInfo; override;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue); override;

  public
    constructor Create(AInfo: PTypeInfo);
  end;

  TRttiIteratorDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue; override;
  end;

  TRttiIteratorNextDescriptor = class(TPropertyDescriptor)
  protected
    function  get_MemberType: TMemberType; override;
  end;

  TRegisteredObject = class(TInterfacedObject, IRegisteredObject)
  protected
    class var FOnGetMemberByName: TOnGetMemberByName;
    class var FOnGetMemberNames: TOnGetMemberNames;

  protected
    FClassID: JSClassID;
    FTypeInfo: PTypeInfo;
    FConstructor: TObjectConstuctor;
    FRttiDescriptorCache: TDictionary<string, IPropertyDescriptor>;
    FExtensionProperties: TDictionary<string, string>;
    FObjectSupportsExtension: TObjectSupportsExtension;

  protected
    function  get_ClassID: JSClassID;
    procedure set_ClassID(const Value: JSClassID);
    function  get_IsInterface: Boolean;
    function  get_IsIterator: Boolean;
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
    constructor Create(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor);
    destructor  Destroy; override;

    property IsInterface: Boolean read get_IsInterface;

    class property OnGetMemberByName: TOnGetMemberByName read FOnGetMemberByName write FOnGetMemberByName;
    class property OnGetMemberNames: TOnGetMemberNames read FOnGetMemberNames write FOnGetMemberNames;
  end;

var
  OutputLogString: TProc<string>;
  // To keep the RTTI Pool alive and avoid continuously creating/destroying it
  // See also https://stackoverflow.com/questions/27368556/trtticontext-multi-thread-issue
  _RttiContext: TRttiContext;

implementation

uses
  System.Classes;

class function TJSRegister.CreateCallback_0(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_0;
begin
  Result := procedure begin
    JS_Call(ctx, JSValue, JS_UNDEFINED, 0 {argc}, nil {PJSValueConstArr(argv)});
  end;
end;

class function TJSRegister.CreateCallback_Double(ctx: JSContext; JSValue: JSValueConst; TypeInfo: PTypeInfo) : TProc_Double;
begin
  Result := procedure(D: Double) begin
    var argc := 1;
    var argv: array of JSValueConst;
    SetLength(argv, 1);
    argv[0] := JS_NewFloat64(ctx, D);
    TJSRuntime.Check(ctx, JS_Call(ctx, JSValue, JS_UNDEFINED, argc, PJSValueConstArr(argv)));
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

class procedure TJSRegister.set_Instance(Value: TJSRegister);
begin
  FreeAndNil(FInstance);
  FInstance := Value;
end;

class function TJSRegister.GenericInvokeCallBack(ctx: JSContext; this_val: JSValueConst;
  argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue;
begin
  Result := JS_NewInt64(ctx, 500);
end;

class function TJSRegister.GenericMethodCallData(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, PJSValueConstArr(func_data)[1]));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var m_descr: IMethodsPropertyDescriptor := nil;

  if Supports(descr, IMethodsPropertyDescriptor, m_descr) then
  begin
    var ptr: Pointer;
    if not JS_IsUndefined(this_val) then
      ptr := TJSRegister.GetObjectFromJSValue(this_val, descr.TypeInfo.Kind <> tkInterface) else
      // Get self
      ptr := TJSRegister.GetObjectFromJSValue(PJSValueConstArr(func_data)[0], descr.TypeInfo.Kind <> tkInterface);

    Result := m_descr.Call(ctx, ptr, argc, argv);
  end;
end;

class function TJSRegister.GenericGetItem(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRegister.GetObjectFromJSValue(this_val, False {Ptr is an IInterface} );
  var vt := descr.GetValue(ptr, [magic]);
  Result := JSConverter.Instance.TValueToJSValue(ctx, vt);
end;

class function TJSRegister.GenericSetItem(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRegister.GetObjectFromJSValue(this_val, False {Ptr is an IInterface} );

  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');

  var v := JSConverter.Instance.JSValueToTValue(ctx, PJSValueConstArr(argv)[0], descr.PropertyType);

  descr.SetValue(ptr, [magic], v);
  Result := JS_UNDEFINED;
end;

class function TJSRegister.GenericPropertyGetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRegister.GetObjectFromJSValue(this_val, not descr.IsInterface {Ptr is an IInterface} );
  var vt := descr.GetValue(ptr, []);
  Result := JSConverter.Instance.TValueToJSValue(ctx, vt);
end;

class function TJSRegister.GenericPropertySetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRegister.GetObjectFromJSValue(this_val, not descr.IsInterface {Ptr is an IInterface} );

  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');
  var v := JSConverter.Instance.JSValueToTValue(ctx, PJSValueConstArr(argv)[0], descr.PropertyType);

  descr.SetValue(ptr, [], v);
  Result := JS_UNDEFINED;
end;

class function TJSRegister.ExtendablePropertyGetter(ctx : JSContext; obj: JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data: PJSValue ): JSValue;

begin
  Result := JS_UNDEFINED;

  var reg: IRegisteredObject;
  if FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var ext: IJSExtendableObject;
    var ptr := GetObjectFromJSValue(obj, False {Is object type?});
    var vt: TValue;
    TValue.Make(@ptr, reg.GetTypeInfo, vt);
    if Supports(vt.AsInterface, IJSExtendableObject, ext) then
    begin
      var prop_name := JS_ToCString(ctx, func_data^);
      if Assigned(prop_name) then
      begin
        Result := ext.GetValue(string(prop_name));
        JS_FreeCString(ctx, prop_name);
      end;
    end;
  end;
end;

class function TJSRegister.ExtendablePropertySetter(ctx : JSContext; obj: JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  Result := JS_UNDEFINED;

  if argc <> 1 then
    raise Exception.Create('Invalid number of arguments');

  var reg: IRegisteredObject;
  if FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var ext: IJSExtendableObject;
    var ptr := GetObjectFromJSValue(obj, False {Is object type?});
    var vt: TValue;
    TValue.Make(@ptr, reg.GetTypeInfo, vt);
    if Supports(vt.AsInterface, IJSExtendableObject, ext) then
    begin
      var prop_name := JS_ToCString(ctx, func_data^);
      if Assigned(prop_name) then
      begin
        ext.SetValue(string(prop_name), JS_DupValue(ctx, PJSValueConstArr(argv)[0]));
        JS_FreeCString(ctx, prop_name);
      end;
    end;
  end;
end;

class function TJSRegister.get_own_property(ctx: JSContext; desc: PJSPropertyDescriptor; obj: JSValueConst; prop: JSAtom) : Integer;
var
  rtti_descriptor: IPropertyDescriptor;

  procedure SetRttiArrayIndexProperty(Index: Integer);
  begin
    var data: JSValue := JS_NewInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @GenericGetItem, 0 {length}, Index {magic=index}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @GenericSetItem, 0 {length}, Index {magic=index}, 1 {data_len}, @data {PJSValueConst});

    JS_FreeValue(ctx, data);
  end;

  procedure SetRttiPropertyDesriptorCallBack;
  begin
    var data: JSValue := JS_NewInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @GenericPropertyGetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @GenericPropertySetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});

    JS_FreeValue(ctx, data);
  end;

  procedure SetRttiMethodCallBack;
  begin
    var data: array of JSValueConst;
    SetLength(data, 2);
    data[0] := JS_DupValue(ctx, obj); // Add Self pointer
    data[1] := JS_NewInt64(ctx, Int64(Pointer(rtti_descriptor))); // Method to call

    desc^.flags := JS_PROP_HAS_VALUE or JS_PROP_ENUMERABLE;
    desc^.value := JS_NewCFunctionData(ctx, @GenericMethodCallData, 0 {length}, 999 {magic}, 2, PJSValueConst(data) {PJSValueConst});
    desc^.getter := JS_UNDEFINED;
    desc^.setter := JS_UNDEFINED;

    JS_FreeValue(ctx, data[1]);
  end;

  procedure SetIteratorProperty;
  begin
    var data: JSValue := JS_NewInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_HAS_GET or JS_PROP_ENUMERABLE;
    desc^.value := JS_NewCFunctionData(ctx, @GenericClassIterator, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.getter := JS_UNDEFINED;
    desc^.setter := JS_UNDEFINED;

    JS_FreeValue(ctx, data);
  end;

  procedure SetIteratorNextProperty;
  begin
    var data: JSValue := JS_NewInt64(ctx, Int64(Pointer(rtti_descriptor)));

    desc^.flags := JS_PROP_HAS_GET or JS_PROP_ENUMERABLE;
    desc^.value := JS_NewCFunctionData(ctx, @GenericIteratorNext, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.getter := JS_UNDEFINED;
    desc^.setter := JS_UNDEFINED;

    JS_FreeValue(ctx, data);
  end;

  procedure SetExtendableObjectGetterSetter(const PropertyName: string);
  begin
    // data holds the name of the property
    var s := AnsiString(PropertyName);
    var data: JSValue := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));

    desc^.flags := JS_PROP_GETSET or JS_PROP_HAS_GET or JS_PROP_HAS_SET or JS_PROP_ENUMERABLE;
    desc^.value := JS_UNDEFINED;
    desc^.getter := JS_NewCFunctionData(ctx, @ExtendablePropertyGetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});
    desc^.setter := JS_NewCFunctionData(ctx, @ExtendablePropertySetter, 0 {length}, 999 {magic}, 1 {data_len}, @data {PJSValueConst});

    JS_FreeValue(ctx, data);
  end;

  function TestObjectSupportsExtension(const reg: IRegisteredObject; const PropertyName: string): Boolean;
  begin
    if reg.ObjectSupportsExtension = TObjectSupportsExtension.Unknown then
    begin
      var isObject := reg.GetTypeInfo.Kind = tkClass;
      var ptr := GetObjectFromJSValue(obj, isObject);
      if ptr <> nil then
      begin
        var ext: IJSExtendableObject;
        if isObject then
        begin
          if not Supports(TObject(ptr), IJSExtendableObject, ext) then
            Exit;
        end
        else // Interface
        begin
          var v: TValue;
          TValue.Make(@ptr, reg.GetTypeInfo, v);
          if not Supports(v.AsInterface, IJSExtendableObject, ext) then
            Exit;
        end;

        if ext.define_own_property(PropertyName) then
          reg.ObjectSupportsExtension := TObjectSupportsExtension.Supported else
          reg.ObjectSupportsExtension := TObjectSupportsExtension.NotSupported;
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

  var ansistr := JS_AtomToCString(ctx, prop);
  var member_name: string := ansistr;
  JS_FreeCString(ctx, ansistr);

  {$IFDEF DEBUG}
  var obj_type: string;
  var r: IRegisteredObject;
  if FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), r) then
    obj_type := r.GetTypeInfo.Name;
  var ptr := TJSRegister.GetObjectFromJSValue(obj, r.GetTypeInfo.Kind <> tkInterface);
  {$ENDIF}

  var reg: IRegisteredObject;

  if FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var item_index: Integer;
    if (member_name[1] in ['0'..'9']) then
    begin
      item_index := StrToInt(member_name);
      member_name := 'Array.get_Item';
    end;

    if not reg.TryGetRttiDescriptor(member_name, rtti_descriptor) then
    begin
      rtti_descriptor := reg.GetMemberByName(member_name, [TMemberType.Methods, TMemberType.Property]);
      if rtti_descriptor = nil then
        Exit;
      reg.AddRttiDescriptor(member_name, rtti_descriptor);
    end;

    if rtti_descriptor.MemberType = TMemberType.Iterator then
      SetIteratorProperty
    else if rtti_descriptor.MemberType = TMemberType.IteratorNext then
      SetIteratorNextProperty
    else if rtti_descriptor.MemberType = TMemberType.Methods then
      SetRttiMethodCallBack
    else if rtti_descriptor.MemberType = TMemberType.ArrayIndexer then
      SetRttiArrayIndexProperty(item_index)
    else
      SetRttiPropertyDesriptorCallBack;

    Exit(1);
  end;
end;

// Called to retrieve the properties of a object
class function TJSRegister.get_own_property_names (ctx: JSContext; ptab: PPJSPropertyEnum; plen: pUInt32; obj: JSValueConst):Integer;
begin
  Result := 0; // OK

  var reg: IRegisteredObject;
  if FRegisteredObjectsByClassID.TryGetValue(GetClassID(obj), reg) then
  begin
    var names := reg.GetMemberNames([TMemberType.Methods, TMemberType.Property]);

    var arr: PJSPropertyEnum := js_malloc(ctx, Length(names) * SizeOf(JSPropertyEnum));
    var p := arr;
    var name: string;

    for name in names do
    begin
      p^.is_enumerable := False; // Not used?
      p^.atom := JS_NewAtom(ctx, PAnsiChar(AnsiString(name)));
      inc(p);
    end;

    plen^ := Length(names);
    ptab^ := arr;
  end;
end;

class function TJSRegister.delete_property(ctx: JSContext; obj:JSValueConst; prop:JSAtom):Integer;
begin
  Assert(False, 'Not implemented');
  Result := 0;
end;

class function TJSRegister.JS_NewDate(ctx: JSContext; epoch_ms: Double) : JSValue;
begin
  if JS_IsNull(JS_DATE_CONSTRUCTOR) then
  begin
    var global := JS_NULL;

    try
      global := JS_GetGlobalObject(ctx);
      JS_DATE_CONSTRUCTOR := JS_GetPropertyStr(ctx, global, 'Date');
      if not JS_IsConstructor(ctx, JS_DATE_CONSTRUCTOR) then
        raise Exception.Create('Date constructor not found');
    finally
      JS_FreeValue(ctx, global);
    end;
  end;

  var argc := 1;
  var argv: array of JSValueConst;
  SetLength(argv, 1);
  argv[0] := JS_NewFloat64(ctx, epoch_ms);
  Result := JS_CallConstructor(ctx, JS_DATE_CONSTRUCTOR, argc, PJSValueConst(argv));
  JS_FreeValue(ctx, argv[0]);
end;

class function TJSRegister.GetClassID(Value: JSValueConst): JSClassID;
begin
  var js_obj := JS_VALUE_GET_OBJ(Value);
  Result := JSClassID((PByte(js_obj)+6)^);
end;

class function TJSRegister.GetClassName(Value: PTypeInfo) : string;
begin
  Result  := string(Value.Name);
  if Result[1] = 'T' then
    Result := Result.Substring(1);
end;

class function TJSRegister.GetObjectFromJSValue(Value: JSValueConst; PointerIsAnObject: Boolean) : Pointer;
begin
  Result := JS_GetOpaque(Value, GetClassID(Value));
  if PointerIsAnObject and (TObject(Result) is TObjectReference) then
    Result := (TObject(Result) as TObjectReference).ObjectRef;
end;

class function TJSRegister.define_own_property(ctx: JSContext; obj:JSValueConst; prop:JSAtom;
  val:JSValueConst; getter:JSValueConst;
  setter:JSValueConst; flags:Integer): Integer;
begin
  // method called when a property does not exist on a object
  // However, this situation is already handled in get_own_property..
  Result := 0;
end;

class function TJSRegister.CConstructor(ctx: JSContext; new_target: JSValueConst; argc: Integer; argv: PJSValueConstArr; magic : Integer): JSValue;
var
  reg: IRegisteredObject;

begin
  if FRegisteredObjectsByClassID.TryGetValue(magic, reg) then
  begin
    Result := JS_NewObjectClass(ctx, reg.ClassID);
    var ptr := reg.CreateInstance(ctx, argc, argv);
    // Do not wrap object inside TObjectReference
    // Finalize will free instance when no longer in use
    JS_SetOpaque(Result, ptr);
  end else
    Result := JS_UNDEFINED;
end;

class procedure TJSRegister.CFinalize(rt: JSRuntime; this_val: JSValue); cdecl;
begin
  var cid := GetClassID(this_val);
  var ptr := JS_GetOpaque(this_val, cid);

  if ptr <> nil then
  begin
    var reg: IRegisteredObject;
    if FRegisteredObjectsByClassID.TryGetValue(cid, reg) then
      reg.Finalize(ptr);
  end;
end;

class function TJSRegister.GenericClassIterator(ctx: JSContext; this_val: JSValueConst;
  argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue): JSValue; cdecl;
begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IPropertyDescriptor := IPropertyDescriptor(Pointer(prtti));
  var ptr := TJSRegister.GetObjectFromJSValue(this_val, False {Ptr is an IInterface} );
  var iter := descr.GetValue(ptr, []);
  if not iter.IsEmpty then
  begin
    var reg_iter := FRegisteredObjectsByType[TypeInfo(TJSIterator)];
    Result := JS_NewObjectClass(ctx, reg_iter.ClassID);
    // Do not wrap object inside TObjectReference
    // Finalize will free instance when no longer in use
    JS_SetOpaque(Result, iter.AsObject);
  end;
end;

class function TJSRegister.GenericIteratorNext(ctx : JSContext; this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
begin
  var cid := TJSRegister.GetClassID(this_val);
  var iter := TJSIterator(JS_GetOpaque(this_val, cid));

  Result := JS_NewObject(ctx);

  if iter.MoveNext then
  begin
    JS_SetPropertyStr(ctx, Result, 'value', JSConverter.Instance.TValueToJSValue(ctx, iter.Current));
    JS_SetPropertyStr(ctx, Result, 'done', JS_FALSE);
  end else
    JS_SetPropertyStr(ctx, Result, 'done', JS_TRUE);
end;

{ TJSRegister }

class constructor TJSRegister.Create;
begin
  FInstance := TJSRegister.Create;

  JS_DATE_CONSTRUCTOR := JS_Null;

  FAutoRegister := True;

  FRegisteredObjectsByClassID := TDictionary<JSClassID, IRegisteredObject>.Create;
  FRegisteredObjectsByType := TDictionary<PTypeInfo, IRegisteredObject>.Create;

  FExotics.get_own_property := get_own_property;
  FExotics.get_own_property_names := get_own_property_names;
  FExotics.delete_property := delete_property;
  FExotics.define_own_property := define_own_property;
  FExotics.has_property := nil; // has_property;
  FExotics.get_property := nil; // get_property;
  FExotics.set_property := nil; // set_property;
end;

class destructor TJSRegister.Destroy;
begin
  FreeAndNil(FInstance);
  FRegisteredObjectsByClassID.Free;
  FRegisteredObjectsByType.Free;
end;

class procedure TJSRegister.InternalRegisterType(const Reg: IRegisteredObject; ctx: JSContext; ClassName: string);
var
  obj,global : JSValue;
  JClass : JSClassDef;
  classID: JSClassID;
  classProto: JSValue;

begin
  var s: AnsiString := ClassName;
  JClass.class_name := PAnsiChar(s);
  JClass.finalizer := @CFinalize;
  JClass.gc_mark := nil;
  JClass.call := nil;
  JClass.exotic := @FExotics;

  // Create New Class id.
  classID := 0;
  JS_NewClassID(@classID);

  // Create the Class Name and other stuff.
  JS_NewClass(JS_GetRuntime(ctx), classID, @JClass);

  // New Object act as Prototype for the Class.
  classProto := JS_NewObject(ctx);

  // Set the Prototype to the Class.
  JS_SetClassProto(ctx, classID, classProto);

  // Set the Class native constructor.
  obj := JS_NewCFunction2(ctx, @CConstructor, JClass.class_name, 1, JS_CFUNC_constructor_magic, classID);

  // Add the Class to Global Object so we can use it.
  global := JS_GetGlobalObject(ctx);
  JS_SetPropertyStr(ctx, global, JClass.class_name, obj);
  JS_FreeValue(ctx,global);

  Reg.ClassID := classID;
end;

function TJSRegister.CreateRegisteredObject(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor) : IRegisteredObject;
begin
  Result := TRegisteredObject.Create(ATypeInfo, AConstructor);
end;

class procedure TJSRegister.AddRegisteredObjectWithClassID(const RegisteredObject: IRegisteredObject);
begin
  FRegisteredObjectsByClassID.Add(RegisteredObject.ClassID, RegisteredObject);
end;

class function TJSRegister.RegisterObject(ctx: JSContext; ClassName: string; TypeInfo: PTypeInfo) : IRegisteredObject;
begin
  Result := TJSRegister.Instance.CreateRegisteredObject(TypeInfo, nil);
  InternalRegisterType(Result, ctx, ClassName);
  AddRegisteredObjectWithClassID(Result);

  TMonitor.Enter(FRegisteredObjectsByType);
  try
    FRegisteredObjectsByType.Add(TypeInfo, Result);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
end;

class function TJSRegister.RegisterObject(ctx: JSContext; ClassName: string; TypeInfo: PTypeInfo; AConstructor: TObjectConstuctor) : IRegisteredObject;
begin
  Result := TJSRegister.Instance.CreateRegisteredObject(TypeInfo, AConstructor);
  InternalRegisterType(Result, ctx, ClassName);
  AddRegisteredObjectWithClassID(Result);
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    FRegisteredObjectsByType.Add(TypeInfo, Result);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
end;

class procedure TJSRegister.RegisterLiveObject(ctx: JSContext; ObjectName: string; AObject: TObject; OwnsObject: Boolean);
begin
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    var tp := PTypeInfo(AObject.ClassInfo);
    var reg: IRegisteredObject := nil;

    if not FRegisteredObjectsByType.TryGetValue(tp, reg) then
    begin
      TJSRegister.RegisterObject(ctx, ClassName, tp);
      reg := FRegisteredObjectsByType[tp];
    end;

    var global := JS_GetGlobalObject(ctx);
    var jsval := JS_NewObjectClass(ctx, reg.ClassID);

    var ptr: Pointer;
    if OwnsObject then
      ptr := Pointer(AObject) else
      ptr := TObjectReference.Create(TObject(AObject));

    JS_SetOpaque(jsval, ptr);
    var s: AnsiString := ObjectName;
    JS_SetPropertyStr(ctx, global, PAnsiChar(s), jsval);
    JS_FreeValue(ctx, global);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
end;

class procedure TJSRegister.RegisterLiveObject(ctx: JSContext; ObjectName: string; TypeInfo: PTypeInfo; const AInterface: IInterface);
begin
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    var reg: IRegisteredObject := nil;

    if not FRegisteredObjectsByType.TryGetValue(TypeInfo, reg) then
    begin
      TJSRegister.RegisterObject(ctx, ClassName, TypeInfo);
      reg := FRegisteredObjectsByType[TypeInfo];
    end;

    var global := JS_GetGlobalObject(ctx);
    var jsval := JS_NewObjectClass(ctx, reg.ClassID);

    // Bump ref count required
    AInterface._AddRef;
    JS_SetOpaque(jsval, Pointer(AInterface));
    var s: AnsiString := ObjectName;
    JS_SetPropertyStr(ctx, global, PAnsiChar(s), jsval);
    JS_FreeValue(ctx, global);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
end;

class function TJSRegister.TryGetRegisteredObjectFromClassID(ClassID: JSClassID; out RegisteredObject: IRegisteredObject) : Boolean;
begin
  Result := FRegisteredObjectsByClassID.TryGetValue(ClassID, RegisteredObject);
end;

class function TJSRegister.TryGetRegisteredObjectFromTypeInfo(TypeInfo: PTypeInfo; out RegisteredObject: IRegisteredObject) : Boolean;
begin
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    Result := FRegisteredObjectsByType.TryGetValue(TypeInfo, RegisteredObject);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
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
  if IsInterface then
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

constructor TRegisteredObject.Create(ATypeInfo: PTypeInfo; AConstructor: TObjectConstuctor);
begin
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
      raise Exception.Create('Constructor returned nil');

    if IsInterface then
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
  arr: array of TValue;
begin
  Result := CallConstructor;

  if Result <> nil then
  begin
    if not IsInterface then
    begin
      var ii: IInterface;
      if Supports(TObject(Result), IInterface, ii) then
        ii._AddRef;
    end;
  end
  else
  begin
    var rttiType := TRttiContext.Create.GetType(FTypeInfo);
    var rtti_method: TRttiMethod := nil;

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

    if argc > 0 then
    begin
      var params := rtti_method.GetParameters;
      SetLength(arr, argc);
      for var i := 0 to argc - 1 do
      begin
        if High(params) < i then
          raise Exception.Create('Too many parameters in call to constructor');

        arr[i] := JSConverter.Instance.JSValueToTValue(ctx, PJSValueConstArr(argv)[i], params[i].ParamType.Handle);
      end;
    end;

    Result := rtti_method.Invoke(PTypeInfo(FTypeInfo)^.TypeData.ClassType, arr).AsObject;

    var ii: IInterface;
    if Supports(TObject(Result), IInterface, ii) then
      ii._AddRef;
  end;
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

  if (AName = 'Array.get_Item') then
  begin
    if get_ObjectSupportsIndexing then
      Result := GetArrayIndexer;
    Exit;
  end;

  var rttictx := _RttiContext;
  var rttiType := rttictx.GetType(FTypeInfo);

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
    if FTypeInfo.Kind = tkInterface then
    begin
      var getter := rttiType.GetMethod('get_' + AName);
      var setter := rttiType.GetMethod('set_' + AName);

      if (getter <> nil) or (setter <> nil) then
        Result := TRttiInterfacePropertyDescriptor.Create(FTypeInfo, getter, setter);
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

function TRegisteredObject.get_ObjectSupportsEnumeration: Boolean;
begin
  var tp := _RttiContext.GetType(FTypeInfo);
  Result := tp.GetMethod('GetEnumerator') <> nil;
end;

function TRegisteredObject.get_IsIterator : Boolean;
begin
  Result := FTypeInfo.TypeData.ClassType.InheritsFrom(TJSIterator);
end;

function TRegisteredObject.get_ObjectSupportsExtension: TObjectSupportsExtension;
begin
  Result := FObjectSupportsExtension;
end;

function TRegisteredObject.get_ObjectSupportsIndexing: Boolean;
begin
  Result := Pos('List', FTypeInfo.Name) <> 0;
end;

procedure TRegisteredObject.set_ObjectSupportsExtension(const Value: TObjectSupportsExtension);
begin
  FObjectSupportsExtension := Value;
end;

function TRegisteredObject.TryGetRttiDescriptor(const PropName: string; out RttiMember: IPropertyDescriptor): Boolean;
begin
  Result := FRttiDescriptorCache.TryGetValue(PropName, RttiMember);
end;

{ JSConverter }
class constructor JSConverter.Create;
begin
  FInstance := JSConverter.Create;
end;

class destructor JSConverter.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function JSConverter.GetDefaultValue(const Param: TRttiParameter) : TValue;
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

function JSConverter.JSValueToTValue(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue;
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
    tkEnumeration:
      if Target = System.TypeInfo(Boolean) then
        Result := TValue.From<Boolean>(JS_ToBool(ctx, Value) <> 0);

    tkFloat:
    begin
      if JS_IsNumber(Value) then
      begin
        var v: Integer;
        TJSRuntime.Check(JS_ToInt32(ctx, @v, Value));
        Result := TValue.From<Double>(Double(v));
      end
      else if JS_IsBigFloat(Value) then
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
        Result := TValue.From<string>(string(str));
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
      if JS_IsObject(Value) then
      begin
        var ptr := TJSRegister.GetObjectFromJSValue(Value, True {Is object type?});
        if ptr <> nil then
          TValue.Make(@ptr, Target, Result);
      end
      else
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
    end;
//    tkRecord:
    tkInterface:
    begin
      if JS_IsFunction(ctx, Value) and string(Target.Name).StartsWith('TProc') then
      begin
        if Target.Name = 'TProc' then
        begin
          var p: TProc_0 := nil;
          // Parse RttiType.Name = 'TProc<System.Integer,System.string>' to get parameters to sent
          p := TJSRegister.CreateCallback_0(ctx, JS_DupValue(ctx, Value), Target);
          TValue.Make(@p, Target, Result);
        end
        else
        begin
          var p: TProc_Double := nil;
          // Parse RttiType.Name = 'TProc<System.Integer,System.string>' to get parameters to sent
          p := TJSRegister.CreateCallback_Double(ctx, JS_DupValue(ctx, Value), Target);
          TValue.Make(@p, Target, Result);
        end;
      end

      else if JS_IsObject(Value) then
      begin
        var ptr := TJSRegister.GetObjectFromJSValue(Value, False {Is NOT object type?});
        if ptr <> nil then
          TValue.Make(@ptr, Target, Result);
      end;
    end;

    tkInt64:
    begin
      var v: Int64;
      TJSRuntime.Check(JS_ToInt64(ctx, @v, Value));
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

class procedure JSConverter.set_Instance(Value: JSConverter);
begin
  FreeAndNil(FInstance);
  FInstance := Value;
end;

function JSConverter.TValueToJSValue(ctx: JSContext; const Value: TValue): JSValue;

  function GetRegisteredObjectFromTypeInfo(PInfo: PTypeInfo) : IRegisteredObject;
  begin
    TMonitor.Enter(TJSRegister.FRegisteredObjectsByType);
    try
      if not TJSRegister.FRegisteredObjectsByType.TryGetValue(PInfo, Result) then
      begin
        if not TJSRegister.AutoRegister then Exit(nil); // Result = undefined

        TJSRegister.RegisterObject(ctx, TJSRegister.GetClassName(PInfo), PInfo);
        TJSRegister.FRegisteredObjectsByType.TryGetValue(PInfo, Result);
      end;
    finally
      TMonitor.Exit(TJSRegister.FRegisteredObjectsByType);
    end;
  end;

begin
  Result := JS_NULL;

  case Value.Kind of
    // tkUnknown:
    tkInteger:
      Result := JS_NewInt32(ctx, Value.AsInteger);
//    tkChar:
//    tkEnumeration:
//    tkFloat:
    tkString, tkUString:
    begin
      var s := AnsiString(Value.AsString);
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
    begin
      var v: TValue;
      if Value.TryAsType<TValue>(v) then
        Result := TValueToJSValue(ctx, v);
    end;
    tkInterface:
    begin
      // Property holds a reference to callback procedure
      if string(Value.TypeInfo.Name).StartsWith('TProc') then
      begin
        Result := JS_NewCFunctionData(ctx, @TJSRegister.GenericInvokeCallBack, 0, 999, 0, nil);
      end
      else
      begin
        var reg := GetRegisteredObjectFromTypeInfo(Value.TypeInfo);
        Result := JS_NewObjectClass(ctx, reg.ClassID);
        var ptr: Pointer;
        // No need to call _AddRef, Supports will bump reference count
        Supports(Value.AsInterface, Value.TypeData.Guid, ptr);
        JS_SetOpaque(Result, ptr);
      end;
    end;

    tkInt64:
      Result := JS_NewInt64(ctx, Value.AsInt64);

//    tkDynArray:
    // tkClassRef:
//    tkPointer:
//    tkProcedure:
//    tkMRecord:
  end;
end;

class function JSConverter.TestParamsAreCompatible(ctx: JSContext; const Param: TRttiParameter; Value: JSValue) : Boolean;
begin
  Result := False;

  case Param.ParamType.TypeKind of
    // tkUnknown:
    tkInteger:
      Result := JS_IsNumber(Value) or JS_IsBigInt(Value);

//    tkChar:
    tkEnumeration:
      if Param.ParamType.Handle = System.TypeInfo(Boolean) then
        Result := JS_IsBool(Value) else
        Result := False;

//    tkFloat:
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

//    tkRecord:
    tkInterface:
      // test for method
      Result := JS_IsFunction(ctx, Value);

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

{ TPropertyDescriptor }
constructor TPropertyDescriptor.Create(AInfo: PTypeInfo);
begin
  FTypeInfo := AInfo;
end;

function TPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  raise ENotImplemented.Create('GetValue not implemented');
end;

function TPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.None;
end;

function TPropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  raise ENotImplemented.Create('PropertyType not implemented');
end;

function TPropertyDescriptor.get_TypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TPropertyDescriptor.IsInterface: Boolean;
begin
  Result := (FTypeInfo <> nil) and (FTypeInfo.Kind = tkInterface);
end;

procedure TPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  raise ENotImplemented.Create('SetValue not implemented');
end;

{ TRttiStandardPropertyDescriptor }

constructor TRttiStandardPropertyDescriptor.Create(AInfo: PTypeInfo; const RttiProp: TRttiMember);
begin
  inherited Create(AInfo);
  FProp := RttiProp;
end;

function TRttiStandardPropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  if FProp is TRttiIndexedProperty then
    Result := (FProp as TRttiIndexedProperty).GetValue(Ptr, Index) else
    Result := (FProp as TRttiProperty).GetValue(Ptr);
end;

function TRttiStandardPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

function TRttiStandardPropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := FProp.Handle;
end;

procedure TRttiStandardPropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  if FProp is TRttiIndexedProperty then
    (FProp as TRttiIndexedProperty).SetValue(Ptr, Index, Value) else
    (FProp as TRttiProperty).SetValue(Ptr, Value);
end;

{ TRttiMethodPropertyDescriptor }

function TRttiMethodPropertyDescriptor.Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
begin
  var method := SelectMethodMatchArguments(ctx, argc, argv);

  {$IFDEF DEBUG}
  var s := Method.Name;
  {$ENDIF}

  try
    var params := Method.GetParameters;
    var arr: array of TValue;

    if Length(params) > 0 then
    begin
      SetLength(arr, Length(params));
      for var i := 0 to High(params) do
      begin
        if i < argc then
          arr[i] := JSConverter.Instance.JSValueToTValue(ctx, PJSValueConstArr(argv)[i], params[i].ParamType.Handle) else
          arr[i] := JSConverter.GetDefaultValue(params[i]);
      end;
    end;

    var v: TValue;
    var vt: TValue;
    TValue.Make(@ptr, FTypeInfo, vt);
    v := Method.Invoke(vt, arr);
    Result := JSConverter.Instance.TValueToJSValue(ctx, v);
  except
    on E: Exception do
    begin
      Result := JS_EXCEPTION;
      IJSContext(TJSRuntime._ActiveContexts[ctx]).Runtime.OutputLog(
        string.Format('Calling ''%s'' on type ''%s'' raised exception: %s', [Method.name, FTypeInfo.Name, E.Message]));
    end;
  end;
end;

function TRttiMethodPropertyDescriptor.SelectMethodMatchArguments(ctx: JSContext; argc: Integer; argv: PJSValueConst): TRttiMethod;
begin
  if Length(Methods) = 0 then Exit(nil);
  if Length(Methods) = 1 then Exit(FMethods[0]);

  var firstmatch: TRttiMethod := nil;

  for var m in FMethods do
  begin
    var params := m.GetParameters;
    var match := True;
    for var i := 0 to argc - 1 do
    begin
      // Skip method when there are more arguments passed from JS than method accepts
      if (i = Length(params)) or not JSConverter.TestParamsAreCompatible(ctx, params[i], PJSValueConstArr(argv)[i]) then
      begin
        match := False;
        break;
      end;
    end;

    // Excact match?
    if match and (Length(params) = argc) then
      Exit(m);

    if firstmatch = nil then
      firstmatch := m;
  end;

  Exit(firstmatch);
end;

constructor TRttiMethodPropertyDescriptor.Create(AInfo: PTypeInfo; const RttiMethods: TArray<TRttiMethod>; IsInterface: Boolean);
begin
  inherited Create(AInfo);
  FMethods := RttiMethods;
  FIsInterface := IsInterface;
end;

function TRttiMethodPropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Methods;
end;

function TRttiMethodPropertyDescriptor.Methods: TArray<TRttiMethod>;
begin
  Result := FMethods;
end;

{ TRttiInterfacePropertyDescriptor }

constructor TRttiInterfacePropertyDescriptor.Create(AInfo: PTypeInfo; const RttiGetter, RttiSetter: TRttiMethod);
begin
  inherited Create(AInfo);
  FGetter := RttiGetter;
  FSetter := RttiSetter;
end;

function TRttiInterfacePropertyDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  if FGetter <> nil then
  begin
    var vt := TValue.From<IInterface>(IInterface(ptr));
    Result := FGetter.Invoke(vt, Index);
  end else
    raise Exception.Create('Property cannot be read');
end;

function TRttiInterfacePropertyDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Property;
end;

function TRttiInterfacePropertyDescriptor.get_PropertyType: PTypeInfo;
begin
  Result := FGetter.ReturnType.Handle;
end;

procedure TRttiInterfacePropertyDescriptor.SetValue(const Ptr: Pointer; const Index: array of TValue; const Value: TValue);
begin
  if FSetter <> nil then
  begin
    var vt := TValue.From<IInterface>(IInterface(ptr));

    var args: array of TValue;
    SetLength(args, 1);
    args[0] := Value;

    FSetter.Invoke(vt, args);
  end else
    raise Exception.Create('Property cannot be set');
end;

{ TJSRuntime }

procedure TJSRuntime.BeforeDestruction;
begin
  inherited;

  try
    js_std_free_handlers(_rt);
    JS_FreeRuntime(_rt);
  except

  end;
end;

class procedure TJSRuntime.Check(FuncResult: Integer);
begin
  if FuncResult <> 0 then
    raise Exception.Create('Error in conversion');
end;

class function TJSRuntime.Check(ctx: JSContext; Val: JSValue): Boolean;
begin
  Result := True;

  if JS_IsException(Val) then
  begin
    var exp := JS_GetException(ctx);

    var str := JS_ToCString(ctx, exp);
    var s: string := str;

    var rt := IJSContext(_ActiveContexts[ctx]).Runtime;
    rt.OutputLog(s);

    JS_FreeCString(ctx, str);

    if JS_IsError(ctx, exp) then
    begin
//      var name := JS_GetPropertyStr(_ctx, exp, 'name');
//      str := JS_ToCString(_ctx, name);
//      s := str;
//      _runtime.OutputLog(s);
//      JS_FreeCString(_ctx, str);
//      JS_FreeValue(_ctx, name);

      var stack := JS_GetPropertyStr(ctx, exp, 'stack');
      if not JS_IsUndefined(stack) then
      begin
        str := JS_ToCString(ctx, stack);
        s := str;
        rt.OutputLog(s);
        JS_FreeCString(ctx, str);
      end;
      JS_FreeValue(ctx, stack);
    end;

    JS_FreeValue(ctx, exp);

    Result := False;
  end;
end;

class constructor TJSRuntime.Create;
begin
  _ActiveContexts := TDictionary<JSContext, Pointer {Unsafe IJSContext pointer}>.Create;
end;

class destructor TJSRuntime.Destroy;
begin
  _ActiveContexts.Free;
end;

constructor TJSRuntime.Create;
begin
  _rt := JS_NewRuntime;
end;

function TJSRuntime.CreateContext: IJSContext;
begin
  Result := TJSContext.Create(Self);
end;

function TJSRuntime.get_LogString: TProc<string>;
begin
  Result := OutputLogString;
end;

function TJSRuntime.get_rt: JSRuntime;
begin
  Result := _rt;
end;

procedure TJSRuntime.OutputLog(const Value: string);
begin
  if Assigned(OutputLogString) then
    OutputLogString(Value);
end;

class procedure TJSRuntime.RegisterContext(Ctx: JSContext; const Context: IJSContext);
begin
  TMonitor.Enter(_ActiveContexts);
  try
    _ActiveContexts.Add(Ctx, Pointer(Context));
  finally
    TMonitor.Exit(_ActiveContexts);
  end;
end;

class procedure TJSRuntime.UnRegisterContext(Ctx: JSContext);
begin
  TMonitor.Enter(_ActiveContexts);
  try
    _ActiveContexts.Remove(Ctx);
  finally
    TMonitor.Exit(_ActiveContexts);
  end;
end;

procedure TJSRuntime.set_LogString(const Value: TProc<string>);
begin
  OutputLogString := Value;
end;

{ TJSContext }

procedure TJSContext.BeforeDestruction;
begin
  TJSRuntime.UnRegisterContext(_ctx);
  try
    JS_FreeContext(_ctx);
  except

  end;
  inherited;
end;

constructor TJSContext.Create(const Runtime: IJSRuntime);
begin
  _runtime := Runtime;
  Initialize;
  TJSRuntime.RegisterContext(_ctx, IJSContext(Self));
end;

function TJSContext.eval_buf(Buf: PAnsiChar; buf_len: Integer; filename: PAnsiChar; eval_flags: Integer): Integer;
var
  val: JSValue;

begin
  if TJSRuntime.Check(_ctx, JS_Eval(_ctx, buf, buf_len, filename, eval_flags)) then
    Result := -1 else
    Result := 0;

  JS_FreeValue(_ctx, val);
end;

class function TJSContext.logme(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue;
var
  i : Integer;
  str : PAnsiChar;
begin
  for i := 0 to Pred(argc) do
  begin
     str := JS_ToCString(ctx, argv[i]);
     if not Assigned(str) then
        exit(JS_EXCEPTION);

     var s: string := str;
     if Assigned(OutputLogString) then
      OutputLogString(s) else
      Write(s);

     JS_FreeCString(ctx, str);
  end;

  Result := JS_UNDEFINED;
end;

function TJSContext.get_ctx: JSContext;
begin
  Result := _ctx;
end;

function TJSContext.get_Runtime: IJSRuntime;
begin
  Result := _runtime;
end;

procedure TJSContext.Initialize;
var
  global : JSValue;

const
  std_helper : PAnsiChar =
    'import * as std from ''std'';'#10+
    'import * as os from ''os'';'#10+
    'globalThis.std = std;'#10+
    'globalThis.os = os;'#10;

begin
  _ctx := JS_NewContext(_runtime.rt);

  // ES6 Module loader.
  JS_SetModuleLoaderFunc(_runtime.rt, nil, @js_module_loader, nil);

  js_std_add_helpers(_ctx, 0, nil);
  js_init_module_std(_ctx, 'std');
  js_init_module_os(_ctx, 'os');

  eval_buf(std_helper, Length(std_helper), '<global_helper>', JS_EVAL_TYPE_MODULE);

  global := JS_GetGlobalObject(_ctx);

  // Define a function in the global context.
  JS_SetPropertyStr(_ctx, global, 'log', JS_NewCFunction(_ctx, @logme, 'log', 1));
  JS_SetPropertyStr(_ctx, global, 'alert', JS_NewCFunction(_ctx, @logme, 'alert', 1));

  TJSRegister.RegisterObject(_ctx, 'JSIterator', TypeInfo(TJSIterator));

  JS_FreeValue(_ctx, global);

  js_std_loop(_ctx);
end;

{ JSIterator }
class function TJSObjectIterator.CreateIterator(const Obj: TObject) : TJSIterator;
begin
  Result := nil;

  var tp := _RttiContext.GetType(Obj.ClassType);
  var getEnumerator := tp.GetMethod('GetEnumerator');
  if getEnumerator = nil then Exit;
  var enumarator := getEnumerator.Invoke(Obj, []).AsObject;
  if enumarator = nil then Exit;
  var tp_enum := _RttiContext.GetType(enumarator.ClassType);
  var move_next := tp_enum.GetMethod('MoveNext');
  if move_next = nil then Exit;
  var get_current := tp_enum.GetProperty('Current');
  if get_current = nil then Exit;

  var iter := TJSObjectIterator.Create;
  iter._instance := enumarator;
  iter._move_next := move_next;
  iter._get_current := get_current;

  Result := iter;
end;

function TJSObjectIterator.Current: TValue;
begin
  Result := TRttiProperty(_get_current).GetValue(_instance);
end;

destructor TJSObjectIterator.Destroy;
begin
  inherited;
end;

function TJSObjectIterator.MoveNext: Boolean;
begin
  Result := TRttiMethod(_move_next).Invoke(_instance, []).AsBoolean;
end;

procedure _InitRttiPool;
begin
 _RttiContext := TRttiContext.Create;
 _RttiContext.FindType('');
end;

{ TRttiIteratorDescriptor }
function TRttiIteratorDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  var obj: TObject;

  if FTypeInfo.Kind = tkInterface then
    obj := TObject(IInterface(ptr)) else
    obj := TObject(ptr);

  Result := TJSObjectIterator.CreateIterator(obj);
end;

function TRttiIteratorDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.Iterator;
end;

{ TRttiIteratorNextDescriptor }

function TRttiIteratorNextDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.IteratorNext;
end;

{ TRttiArrayIndexDescriptor }

constructor TRttiArrayIndexDescriptor.Create(AInfo: PTypeInfo);
begin
  inherited Create(AInfo);
end;

function TRttiArrayIndexDescriptor.GetValue(const Ptr: Pointer; const Index: array of TValue): TValue;
begin
  Result := TValue.Empty;
end;

function TRttiArrayIndexDescriptor.get_MemberType: TMemberType;
begin
  Result := TMemberType.ArrayIndexer;
end;

function TRttiArrayIndexDescriptor.get_PropertyType: PTypeInfo;
begin

end;

procedure TRttiArrayIndexDescriptor.SetValue(const Ptr: Pointer;
  const Index: array of TValue; const Value: TValue);
begin
  inherited;

end;

initialization
  _InitRttiPool;

finalization
  _RttiContext.Free();

end.
