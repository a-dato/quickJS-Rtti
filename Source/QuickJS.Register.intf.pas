unit QuickJS.Register.intf;

interface

uses
  System.Variants,
  System.Rtti,
  System.TypInfo,
  quickjs, System.SysUtils;

type
  IJSExtendableObject = interface;

  TObjectConstuctor = reference to function: Pointer;

  DefaultValueAttribute = class(TCustomAttribute)
  protected
    FValue: Variant;
  public
    constructor Create(Value: Boolean); overload;
    constructor Create(Value: string); overload;
    property Value: Variant read FValue;
  end;

  TMemberType = (None, Methods, &Property, Iterator, IteratorNext, ArrayIndexer);
  TMemberTypes = set of TMemberType;
  TObjectSupportsExtension = (Unknown, Supported, NotSupported);

  PObjectMember = Pointer;  // Generic pointer to TRttiMember/TRttiMethod/TrriProperty/_PropertyInfo
  PRttiMember = ^TRttiMember;

  IPropertyDescriptor = interface
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

  IMethodsPropertyDescriptor = interface
    ['{3D51ABCB-4C43-482A-8AE4-0749F56CD1CA}']
    function Methods: TArray<TRttiMethod>;
    function Call(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue;
  end;

  IRegisteredObject = interface
    function  get_IsInterface: Boolean;
    function  get_IsIterator: Boolean;

    function  get_ClassID: JSClassID;
    procedure set_ClassID(const Value: JSClassID);
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
    property ObjectSupportsEnumeration: Boolean read get_ObjectSupportsEnumeration;
    property ObjectSupportsExtension: TObjectSupportsExtension read get_ObjectSupportsExtension write set_ObjectSupportsExtension;
    property ObjectSupportsIndexing: Boolean read get_ObjectSupportsIndexing;
  end;

  IJSRuntime = interface
    function  get_rt: JSRuntime;
    function  get_LogString: TProc<string>;
    procedure set_LogString(const Value: TProc<string>);

    procedure OutputLog(const Value: string);

    property rt: JSRuntime read get_rt;
    property LogString: TProc<string> read get_LogString write set_LogString;
  end;

  IJSContext = interface
    function get_ctx: JSContext;
    function get_Runtime: IJSRuntime;

    function eval_buf(Buf: PAnsiChar; buf_len: Integer; filename: PAnsiChar; eval_flags: Integer): Integer;
    property ctx: JSContext read get_ctx;
    property runtime: IJSRuntime read get_Runtime;
  end;

  IJSExtendableObject = interface
    ['{05E3C6B4-C097-4767-9D33-4BA0B0A6371D}']
    function  define_own_property(const Name: string) : Boolean;
    function  GetValue(const Name: string): JSValue;
    procedure SetValue(const Name: string; Value: JSValue);
  end;

  IJSObjectReference = interface
    procedure Invoke(const FuncName: string);
  end;

  TOnGetMemberByName = function(const AObject: IRegisteredObject; const AName: string; MemberTypes: TMemberTypes; var Handled: Boolean) : IPropertyDescriptor of Object;
  TOnGetMemberNames = function(const AObject: IRegisteredObject; MemberTypes: TMemberTypes; var Handled: Boolean) : TArray<string> of Object;

implementation

{ DefaultValueAttribute }

constructor DefaultValueAttribute.Create(Value: Boolean);
begin
  FValue := Value;
end;

constructor DefaultValueAttribute.Create(Value: string);
begin
  FValue := Value;
end;

end.
