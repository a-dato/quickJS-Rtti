unit QuickJS.Variant;

interface

uses System.Variants, System.Rtti;

type
  TJSVariantType = class(TInvokeableVariantType)
  protected
    function  GetInstance(const V: TVarData): TObject;

    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function FixupIdent(const AText: string): string; override;

  public
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  TJSVariantVarData = record
  strict private
    function  GetInstance: IInterface; inline;
    procedure SetInstance(const Value: IInterface); inline;
  public
    procedure Clear; inline;
  public
     VType: TVarType;
     Reserved1, Reserved2, Reserved3: Word;
     property Instance: IInterface read GetInstance write SetInstance;
  public
    case Integer of
    0: (_Interface: Pointer;
        _IsUndefined: Boolean);
    1: (VLargest: TLargestVarData); // to match size as Variant/TVarData type
  end;

  // Reimplement Rtti classes so that GetName can be overriden
  TRttiObject_ = class abstract
  private
    constructor Create(APackage: TRttiPackage; AParent: TRttiObject; var P: PByte); virtual; abstract;
  public
    function GetAttributes: TArray<TCustomAttribute>; virtual; abstract;
  end;

  TRttiNamedObject_ = class(TRttiObject_)
  private
    function GetName: string; virtual; abstract;
  public
    function HasName(const AName: string): Boolean; virtual; abstract;
  end;

  TRttiMember_ = class(TRttiNamedObject_)
  private
    function GetVisibility: Integer; virtual; abstract;
  end;

  TRttiMethod_ = class(TRttiMember_)
  private
    function GetMethodKind: Integer; virtual; abstract;
    function GetCallingConvention: Integer; virtual; abstract;
    function GetReturnType: TRttiType; virtual;
  end;

  TRttiMethodWithName = class(TRttiMethod_)
  private
    _Name: string;
    function GetName: string; override;

  public
    constructor Create(AName: string);
  end;

  function VarJSVariantCreate(const Value: IInterface): Variant;
  function VarJSVariantIsNull(const Value: Variant) : Boolean;
  function VarJSVariantIsUndefined(const Value: Variant) : Boolean;

var
  JSVariantType: TJSVariantType;

implementation

uses
  System.SysUtils, QuickJS.Register.dn4d.intf;

function VarJSVariantCreate(const Value: IInterface): Variant;
begin
  TJSVariantVarData(Result).Clear;
  TJSVariantVarData(Result).Instance := Value;
end;

function VarJSVariantIsNull(const Value: Variant) : Boolean;
begin
  Result :=
    // Checks for regular variants
    (TVarData(Value).VType = varNull) or
    (
       (TJSVariantVarData(Value).VType = JSVariantType.VarType) and
       (TJSVariantVarData(Value)._Interface = nil) and
       not TJSVariantVarData(Value)._IsUndefined
    )
end;

function VarJSVariantIsUndefined(const Value: Variant) : Boolean;
begin
  Result :=
    // Checks for regular variants
    (TVarData(Value).VType = varEmpty) or
    (
      (TJSVariantVarData(Value).VType = JSVariantType.VarType) and
      (TJSVariantVarData(Value)._Interface = nil) and
       TJSVariantVarData(Value)._IsUndefined
    )
end;

{ TJSVariantType }

procedure TJSVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  TJSVariantVarData(V).Clear;
end;

procedure TJSVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    TJSVariantVarData(Dest) := TJSVariantVarData(Source);
    if TJSVariantVarData(Dest)._Interface <> nil then
      IInterface(TJSVariantVarData(Dest)._Interface)._AddRef;
  end;
end;

function TJSVariantType.DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := True;
  TJSVariantVarData(Dest).Clear; // Sets Dest type to TJSVariantType again ==> by default return a 'callable' JSVariant
  TJSVariantVarData(Dest)._IsUndefined := TJSVariantVarData(V)._IsUndefined;

  var vi: TVirtualInterface := GetInstance(V) as TVirtualInterface;
  if (vi <> nil) and Assigned(vi.OnInvoke) then
  begin
    var r: TValue;
    var m := TRttiMethodWithName.Create(Name);
    try
      var args: TArray<TValue>;
      SetLength(args, Length(Arguments) + 1);
      // this pointer
      args[0] := TValue.From<IInterface>(IInterface(TJSVariantVarData(V)._Interface));
      for var i := 0 to High(Arguments) do
        args[i+1] := TValue.FromVariant(Variant(Arguments[i]));

      vi.OnInvoke(TRttiMethod(Pointer(m)), args, r);

      // QuickJS returns an object, wrap it inside interface and return a callable object
      if r.TypeInfo = TypeInfo(JSObjectReference) then
      begin
        var wrapped := WrapIJSObjectInVirtualInterface(TypeInfo(IInterface), r.AsType<JSObjectReference>);
        TJSVariantVarData(Dest).Instance := wrapped.AsInterface;
      end
      // QuickJS returns undefined or nil
      else if r.IsEmpty then
      begin
        // Invoke return an nil pointer to a IInterface when result IS nil
        // Invoke returns 'empty' when result is 'undefined'
        TJSVariantVarData(Dest)._IsUndefined := r.TypeInfo <> TypeInfo(IInterface);
      end
      else
      begin
        var vt := r.AsVariant;
        Dest := TVarData(vt);
        TVarData(vt).VType := varEmpty;
      end;
    finally
      m.Free;
    end;
  end;
end;

function TJSVariantType.DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  Result := False;
end;

function TJSVariantType.FixupIdent(const AText: string): string;
begin
  Result := AText;
end;

function TJSVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result := TObject(IInterface(TJSVariantVarData(V)._Interface));
end;

function TJSVariantType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
begin
  Result := False;
end;

function TJSVariantType.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin

end;

{ TRttiMethodWithName }

constructor TRttiMethodWithName.Create(AName: string);
begin
  _Name := AName;
end;

function TRttiMethodWithName.GetName: string;
begin
  Result := _Name;
end;

{ TRttiMethod_ }

function TRttiMethod_.GetReturnType: TRttiType;
begin
  Result := nil;
end;

{ TJSVariantVarData }

procedure TJSVariantVarData.Clear;
begin
  VType := JSVariantType.VarType;
  _IsUndefined := False;
  if _Interface <> nil then
  begin
    IInterface(_Interface)._Release;
    _Interface := nil;
  end;
end;

function TJSVariantVarData.GetInstance: IInterface;
begin
  Result := IInterface(_Interface);
end;

procedure TJSVariantVarData.SetInstance(const Value: IInterface);
begin
  if _Interface <> nil then
    IInterface(_Interface)._Release;

  _Interface := Pointer(Value);

  if _Interface <> nil then
    IInterface(_Interface)._AddRef;
end;

initialization
  JSVariantType := TJSVariantType.Create;

finalization
  FreeAndNil(JSVariantType);

end.

