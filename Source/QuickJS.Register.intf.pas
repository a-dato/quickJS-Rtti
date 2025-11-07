unit QuickJS.Register.intf;

interface

uses
  System.Variants,
  System.Rtti,
  System.TypInfo, quickjs_ng, System.SysUtils;


type
  TObjectConstuctor = function : Pointer;
  TCustomObjectFactory = function(const ATypeInfo: PTypeInfo; const Args: TArray<TValue>) : Pointer of object;

  TObjectSupportsExtension = (Unknown, Supported, NotSupported);
  
  // Forward declarations
  IJSContext = interface;
  IJSRuntime = interface;

  TMemberType = (None, Methods, &Property, Iterator, IteratorNext, ArrayIndexer, ExtensionProperty, IndexedProperty);
  TMemberTypes = set of TMemberType;

  TInterfaceRef = record
    IID: TGuid;
    ii: IInterface;
  end;

  IJSObject = interface
    ['{8D30FEC0-2EE4-4F09-A8BF-64D6FDA92AF8}']
    function get_Ctx: JSContext;
    function get_Value: JSValueConst;

    function Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue;
    function Call(const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue; overload;
    function Call(const JSFunc: JSValueConst; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue; overload;

    property Ctx: JSContext read get_Ctx;
    property Value: JSValueConst read get_Value;
  end;

  IPropertyDescriptor = interface
    ['{41133217-8E74-455D-953A-4FB4723A4EBE}']
    function  get_MemberType: TMemberType;
    function  get_TypeInfo: PTypeInfo;
    function  get_PropertyType: PTypeInfo;

    function IsInterface: Boolean;

    function  GetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue) : TValue;
    procedure SetValue(const Ptr: Pointer {TObject/IInterface}; const Index: array of TValue; const Value: TValue);

    property MemberType: TMemberType read get_MemberType;
    property TypeInfo: PTypeInfo read get_TypeInfo;
    property PropertyType: PTypeInfo read get_PropertyType;
  end;

  IRegisteredObject = interface
    ['{A4D3F263-C1BE-4550-B460-87CD6F8EEA38}']
    function  get_IsObject: Boolean;
    function  get_IsInterface: Boolean;
    function  get_IsIterator: Boolean;
    function  get_IsIndexedPropertyAccessor: Boolean;

    function  get_ClassID: JSClassID;
    procedure set_ClassID(const Value: JSClassID);
    function  get_JSConstructor: JSValue;
    procedure set_JSConstructor(const Value: JSValue);
    function  get_Kind: TTypeKind;
    function  get_ObjectSupportsEnumeration: Boolean;
    function  get_ObjectSupportsExtension: TObjectSupportsExtension;
    procedure set_ObjectSupportsExtension(const Value: TObjectSupportsExtension);
    function  get_ObjectSupportsIndexing: Boolean;

    function  CreateInstance(ctx: JSContext; argc: Integer; argv: PJSValueConstArr): Pointer;
    procedure Finalize(Ptr: Pointer);
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IPropertyDescriptor;
    function  GetMemberNames(MemberTypes: TMemberTypes) : TArray<string>;
    function  GetIterator: IPropertyDescriptor;
    function  GetIteratorNext: IPropertyDescriptor;
    function  GetTypeInfo: PTypeInfo;

    function  TryGetRttiDescriptor(const PropName: string; out RttiMember: IPropertyDescriptor) : Boolean;
    procedure AddRttiDescriptor(const PropName: string; const RttiMember: IPropertyDescriptor);
    function  TryGetExtensionProperty(const PropName: string; out PropertyName: string) : Boolean;
    procedure AddExtensionProperty(const PropName: string; const PropertyName: string);

    property ClassID: JSClassID read get_ClassID write set_ClassID;
    property IsInterface: Boolean read get_IsInterface;
    property IsIterator: Boolean read get_IsIterator;
    property IsIndexedPropertyAccessor: Boolean read get_IsIndexedPropertyAccessor;
    property IsObject: Boolean read get_IsObject;
    property JSConstructor: JSValue read get_JSConstructor write set_JSConstructor;
    property Kind: TTypeKind read get_Kind;
    property ObjectSupportsEnumeration: Boolean read get_ObjectSupportsEnumeration;
    property ObjectSupportsExtension: TObjectSupportsExtension read get_ObjectSupportsExtension write set_ObjectSupportsExtension;
    property ObjectSupportsIndexing: Boolean read get_ObjectSupportsIndexing;
  end;

  TRegisteredObjectWithPtr = record
    Reg: IRegisteredObject;
    Ptr: Pointer;
  end;

  IJSRuntime = interface
    function  get_rt: JSRuntime;
    function  get_LogString: TProc<string>;
    procedure set_LogString(const Value: TProc<string>);

    function  CreateContext: IJSContext;
    procedure OutputLog(const Value: string);

    property rt: JSRuntime read get_rt;
    property LogString: TProc<string> read get_LogString write set_LogString;
  end;

  IJSContext = interface
    function get_ctx: JSContext;
    function get_Runtime: IJSRuntime;

    procedure eval(const Code: string; const CodeContext: string);
    function  eval_with_result(const Code: string; const CodeContext: string): TValue;

    property ctx: JSContext read get_ctx;
    property runtime: IJSRuntime read get_Runtime;
  end;

  IJSExtendableObject = interface
    ['{05E3C6B4-C097-4767-9D33-4BA0B0A6371D}']
    function  define_own_property(Ctx: JSContext; const Name: string) : Boolean;
    function  GetValue(Ctx: JSContext; const Name: string): JSValue;
    procedure SetValue(Ctx: JSContext; const Name: string; Value: JSValue);
  end;

  TOnGetMemberByName = function(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean) : IPropertyDescriptor of Object;
  TOnGetMemberNames = function(const AObject: IRegisteredObject; MemberTypes: TMemberTypes; var Handled: Boolean) : TArray<string> of Object;

type
  TJSValueToTValueFunc = function(ctx: JSContext; Value: JSValueConst; Target: PTypeInfo): TValue;
  TTValueToJSValueFunc = function(ctx: JSContext; const Value: TValue): JSValue;
  TGetDefaultValueFunc = function(const Param: TRttiParameter): TValue;

  TJSConverterFuncs = record
    JSValueToTValue: TJSValueToTValueFunc;
    TValueToJSValue: TTValueToJSValueFunc;
    GetDefaultValue: TGetDefaultValueFunc;
  end;

var
  JSConverterFuncs: TJSConverterFuncs;
  // Shared RTTI context for performance (initialized in QuickJS.Register.impl)
  _RttiContext: TRttiContext;

implementation

end.

