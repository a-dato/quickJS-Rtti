unit QuickJS.Register.dn4d.intf;

interface

uses
  System_,
  System.TypInfo,
  System.SysUtils,
  quickjs_ng,
  QuickJS.Variant, System.Rtti, System.Collections.Generic;

type
  IJSRegisteredObject = interface
    function GetType: &Type;
  end;

  IJSCapturedObject = interface
    function ctx: JSContext;
    function value: JSValueConst;
  end;

  IJSObject = interface(IBaseInterface)
    ['{8D30FEC0-2EE4-4F09-A8BF-64D6FDA92AF8}']
    function get_Ctx: JSContext;
    function get_Value: JSValueConst;

    function Invoke(const Func: AnsiString; const Args: TArray<TValue>; ReturnType: PTypeInfo) : TValue;

    property Ctx: JSContext read get_Ctx;
    property Value: JSValueConst read get_Value;
  end;

  TInterfaceRef = record
    IID: TGuid;
    ii: IInterface;
  end;

  TJSVirtualInterface = class(TVirtualInterface, IJSObject)
  var
    FObj: IJSObject;
    FImplementingInterfaces: List<TInterfaceRef>;

    function  get_JSObject: IJSObject;
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);

  public
    constructor Create(PIID: PTypeInfo; const Obj: IJSObject); reintroduce;
    destructor Destroy; override;
    function   QueryInterface(const IID: TGUID; out Obj): HResult; override;

    property JSObject: IJSObject read get_JSObject implements IJSObject;
  end;

  function JSVariant(const Value: IInterface) : Variant;
  function JSVariantIsNull(const Value: Variant) : Boolean;
  function JSVariantIsUndefined(const Value: Variant) : Boolean;
  function WrapIJSObjectInVirtualInterface(Target: PTypeInfo; const obj_ref: IJSObject) : TValue;

implementation

uses
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  System.Variants;

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

{ TJSVirtualInterface }
constructor TJSVirtualInterface.Create(PIID: PTypeInfo; const Obj: IJSObject);
begin
  inherited Create(PIID, Invoke);
  FImplementingInterfaces := CList<TInterfaceRef>.Create(0);
  FObj := Obj;
end;

destructor TJSVirtualInterface.Destroy;
begin
  inherited;
end;

function TJSVirtualInterface.get_JSObject: IJSObject;
begin
  Result := FObj;
end;

procedure TJSVirtualInterface.Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
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

  Result := FObj.Invoke(name, Copy(Args, 1), rt);
end;

function TJSVirtualInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited;

  if Result <> 0 then
  begin
    var ii_ref := FImplementingInterfaces.Find(function(const item: TInterfaceRef) : Boolean begin
                    Result := IsEqualGUID(IID, item.IID);
                  end);

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

    // Call QuickJS, we expect an object to be returned. This object will be wrapped inside
    // an TJSVirtualInterface object as well and can be cast to the requested type
    var reg: IRegisteredObject;
    if TJSRegister.TryGetRegisteredInterface(IID, reg) then
    begin
      var tp := TValue.From<&Type>(&Type.Create(reg.GetTypeInfo));
      var val := FObj.Invoke('QueryInterface', [tp], reg.GetTypeInfo);
      if not val.IsEmpty then
      begin
        ii_ref.ii := IInterface(val.GetReferenceToRawData^);
        IInterface(Obj) := ii_ref.ii;
        Result := S_OK;
      end;
    end;

    ii_ref.IID := IID;
    FImplementingInterfaces.Add(ii_ref);
  end;
end;

function WrapIJSObjectInVirtualInterface(Target: PTypeInfo; const obj_ref: IJSObject) : TValue;
begin
  var virtual_interface := TJSVirtualInterface.Create(Target, obj_ref);
  var ii: IInterface;
  if Interfaces.Supports(virtual_interface, Target.TypeData.GUID, ii) then
    TValue.Make(@ii, Target, Result);
end;

end.

