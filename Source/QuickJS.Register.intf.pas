unit QuickJS.Register.intf;

interface

uses
  System.Variants,
  System.Rtti,
  System.TypInfo,
  quickjs, System.SysUtils;

type
  IJSExtendableObject = interface;

  TObjectConstuctor = reference to function: TObject;
  TTypedConstuctor<T> = reference to function: T;

  DefaultValueAttribute = class(TCustomAttribute)
  protected
    FValue: Variant;
  public
    constructor Create(Value: Boolean); overload;
    constructor Create(Value: string); overload;
    property Value: Variant read FValue;
  end;

  TMemberType = (None, Methods, Properties, InterfacePropertyGetSet);
  TMemberTypes = set of TMemberType;
  TObjectSupportsExtension = (Unknown, Supported, NotSupported);

  PObjectMember = Pointer;  // Generic pointer to TRttiMember/TRttiMethod/TrriProperty/_PropertyInfo
  PRttiMember = ^TRttiMember;

  IRttiCachedDescriptor = interface
    function  get_Members: TArray<PObjectMember>;
    function  get_MemberType: TMemberType;
    function  get_Getter: TRttiMember;
    function  get_Setter: TRttiMember;
    function  get_TypeInfo: PTypeInfo;

    function IsInterfaceProperty: Boolean;
    function IsRealProperty: Boolean;
    function IsMethod: Boolean;

    property Members: TArray<PObjectMember> read get_Members;
    property MemberType: TMemberType read get_MemberType;
    property Getter: TRttiMember read get_Getter;
    property Setter: TRttiMember read get_Setter;
    property TypeInfo: PTypeInfo read get_TypeInfo;
  end;

  IRegisteredObject = interface
    function  get_IsInterface: Boolean;
    function  get_ClassID: JSClassID;
    procedure set_ClassID(const Value: JSClassID);
    function  get_ObjectSupportsExtension: TObjectSupportsExtension;
    procedure set_ObjectSupportsExtension(const Value: TObjectSupportsExtension);

    function  CreateInstance(ctx: JSContext; argc: Integer; argv: PJSValueConstArr): Pointer;
    procedure Finalize(Ptr: Pointer);
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IRttiCachedDescriptor;
    function  GetTypeInfo: PTypeInfo;

    function  TryGetRttiDescriptor(Atom: JSAtom; out RttiMember: IRttiCachedDescriptor) : Boolean;
    procedure AddRttiDescriptor(Atom: JSAtom; const RttiMember: IRttiCachedDescriptor);
    function  TryGetExtensionProperty(Atom: JSAtom; out PropertyName: string) : Boolean;
    procedure AddExtensionProperty(Atom: JSAtom; const PropertyName: string);

    property ClassID: JSClassID read get_ClassID write set_ClassID;
    property IsInterface: Boolean read get_IsInterface;
    property ObjectSupportsExtension: TObjectSupportsExtension read get_ObjectSupportsExtension write set_ObjectSupportsExtension;
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
