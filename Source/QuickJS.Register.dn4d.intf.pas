unit QuickJS.Register.dn4d.intf;

interface

uses
  System_,
  System.TypInfo,
  System.SysUtils,
  
  QuickJS.Variant,
  System.Rtti,
  System.Collections,
  System.Collections.Generic,
  QuickJS.Register.intf, QuickJS.Register.impl, quickjs_ng;

type
  IJSRegisteredObject = interface
    function GetType: &Type;
  end;

  IJSCapturedObject = interface
    function ctx: JSContext;
    function value: JSValueConst;
  end;

  TJSVirtualInterface = class(TVirtualInterface, IJSObject)
  var
    FRuntime: TJSRuntime;
    FTypeInfo: PTypeInfo;
    FObj: IJSObject;
    FImplementingInterfaces: List<TInterfaceRef>;

    function  get_JSObject: IJSObject;
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);

  public
    constructor Create(const Runtime: TJSRuntime; TypeInfo: PTypeInfo; const Obj: IJSObject); reintroduce;
    destructor Destroy; override;
    function   QueryInterface(const IID: TGUID; out Obj): HResult; override;

    property JSObject: IJSObject read get_JSObject implements IJSObject;
  end;

  TJSVirtualListInterface = class;
  
  // Enumerator for TJSVirtualListInterface (non-generic, returns CObject)
  TJSVirtualListEnumerator = class(TBaseInterfacedObject, IEnumerator)
  protected
    FList: TJSVirtualListInterface;
    FIndex: Integer;
    FCount: Integer;
    
    function get_Current: CObject;
    
  public
    constructor Create(AList: TJSVirtualListInterface);
    
    function MoveNext: Boolean;
    procedure Reset;
    
    property Current: CObject read get_Current;
  end;
  
  // Virtual enumerator interface that implements IEnumerator<T> with properly typed Current
  // Uses TVirtualInterface to dynamically implement any IEnumerator<T> interface
  TJSVirtualEnumeratorInterface = class(TVirtualInterface)
  protected
    FList: TJSVirtualListInterface;
    FIndex: Integer;
    FCount: Integer;
    FElementType: PTypeInfo;
    
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    
    function GetCurrentTyped: TValue;
    function MoveNext: Boolean;
    procedure Reset;
    
  public
    constructor Create(AList: TJSVirtualListInterface; EnumeratorTypeInfo: PTypeInfo; ElementType: PTypeInfo); reintroduce;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;
  
  // Virtual list interface that wraps a JavaScript array
  // Uses TVirtualInterface to dynamically implement any IList<T> interface
  TJSVirtualListInterface = class(TVirtualInterface)
  protected
    FRuntime: TJSRuntime;
    FCtx: JSContext;
    FArrayValue: JSValue;
    FElementType: PTypeInfo;
    FTargetTypeInfo: PTypeInfo;
    FEnumeratorTypeInfo: PTypeInfo;
    
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    
    // Helper methods for list operations
    function GetCount: Integer;
    function GetItemAt(Index: Integer): TValue;
    procedure SetItemAt(Index: Integer; const Value: TValue);
    function GetEnumerator: IEnumerator;
    function CreateTypedEnumerator: TValue;
    
  public
    constructor Create(const Runtime: TJSRuntime; TypeInfo: PTypeInfo; ctx: JSContext; ArrayValue: JSValue); reintroduce;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;

  function JSVariant(const Value: IInterface) : Variant;
  function JSVariantIsNull(const Value: Variant) : Boolean;
  function JSVariantIsUndefined(const Value: Variant) : Boolean;
  function WrapIJSObjectInVirtualInterface(const Runtime: TJSRuntime; Target: PTypeInfo; const obj_ref: IJSObject) : TValue;
  function WrapJSArrayInList(const Runtime: TJSRuntime; ctx: JSContext; ArrayValue: JSValue): IList;

implementation

uses
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
constructor TJSVirtualInterface.Create(const Runtime: TJSRuntime; TypeInfo: PTypeInfo; const Obj: IJSObject);
begin
  inherited Create(TypeInfo, Invoke);
  FRuntime := Runtime;
  FTypeInfo := TypeInfo;
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
    name := AnsiString(Method.Name.Substring(4));
  end else
    name := AnsiString(Method.Name);

  var rt: PTypeInfo := nil;
  if Method.ReturnType <> nil then
    rt := Method.ReturnType.Handle;
  Result := FObj.Invoke(name, Copy(Args, 1), rt);

//  if name = 'GetType' then
//    Result := TValue.From<&Type>(&Type.Create(FTypeInfo))
//
//  else
//  begin
//    var rt: PTypeInfo := nil;
//    if Method.ReturnType <> nil then
//      rt := Method.ReturnType.Handle;
//
//    Result := FObj.Invoke(name, Copy(Args, 1), rt);
//  end;
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
    if FRuntime.TryGetRegisteredInterface(IID, reg) then
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

function WrapIJSObjectInVirtualInterface(const Runtime: TJSRuntime; Target: PTypeInfo; const obj_ref: IJSObject) : TValue;
begin
  var virtual_interface := TJSVirtualInterface.Create(Runtime, Target, obj_ref);
  var ii: IInterface;
  if Interfaces.Supports(virtual_interface, Target.TypeData.GUID, ii) then
    TValue.Make(@ii, Target, Result);
end;

function WrapJSArrayInList(const Runtime: TJSRuntime; ctx: JSContext; ArrayValue: JSValue): IList;
begin
  // For non-generic IList, we create a wrapper that implements IList directly
  var wrapper := TJSVirtualListInterface.Create(Runtime, TypeInfo(IList), ctx, ArrayValue);
  var ii: IInterface;
  if Interfaces.Supports(wrapper, IList, ii) then
    Result := IList(ii)
  else
    Result := nil;
end;

{ TJSVirtualListEnumerator }

constructor TJSVirtualListEnumerator.Create(AList: TJSVirtualListInterface);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
  FCount := AList.GetCount;
end;

function TJSVirtualListEnumerator.get_Current: CObject;
begin
  var val := FList.GetItemAt(FIndex);
  if val.IsEmpty then
    Result := nil
  else if val.IsType<IBaseInterface> then
    Result := CObject.Create(val.AsType<IBaseInterface>)
  else if val.IsType<IInterface> then
    Result := CObject.Create(val.AsInterface as IBaseInterface)
  else
    Result := CObject.From<TValue>(val);
end;

function TJSVirtualListEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FCount;
end;

procedure TJSVirtualListEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TJSVirtualEnumeratorInterface }

constructor TJSVirtualEnumeratorInterface.Create(AList: TJSVirtualListInterface; EnumeratorTypeInfo: PTypeInfo; ElementType: PTypeInfo);
begin
  inherited Create(EnumeratorTypeInfo, Invoke);
  FList := AList;
  FIndex := -1;
  FCount := AList.GetCount;
  FElementType := ElementType;
end;

function TJSVirtualEnumeratorInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result = S_OK then
    Exit;
    
  // For any interface request, return self - we'll handle all calls through Invoke
  Result := S_OK;
  Pointer(Obj) := Pointer(Self as IInterface);
  if Pointer(Obj) <> nil then
    IInterface(Obj)._AddRef;
end;

function TJSVirtualEnumeratorInterface.GetCurrentTyped: TValue;
begin
  Result := FList.GetItemAt(FIndex);
end;

function TJSVirtualEnumeratorInterface.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FCount;
end;

procedure TJSVirtualEnumeratorInterface.Reset;
begin
  FIndex := -1;
end;

procedure TJSVirtualEnumeratorInterface.Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  var methodName := Method.Name;
  
  if methodName = 'MoveNext' then
  begin
    Result := TValue.From<Boolean>(MoveNext);
  end
  else if methodName = 'get_Current' then
  begin
    // Return the properly typed value
    Result := GetCurrentTyped;
  end
  else if methodName = 'Reset' then
  begin
    Reset;
    Result := TValue.Empty;
  end
  else if methodName = 'Dispose' then
  begin
    Result := TValue.Empty;
  end
  else
    Result := TValue.Empty;
end;

{ TJSVirtualListInterface }

constructor TJSVirtualListInterface.Create(const Runtime: TJSRuntime; TypeInfo: PTypeInfo; ctx: JSContext; ArrayValue: JSValue);
begin
  inherited Create(TypeInfo, Invoke);
  FRuntime := Runtime;
  FCtx := ctx;
  FArrayValue := JS_DupValue(ctx, ArrayValue);
  FTargetTypeInfo := TypeInfo;
  FEnumeratorTypeInfo := nil;
  FElementType := nil;
  
  // Determine element type from the target interface's get_Item return type
  // and enumerator type from GetEnumerator return type
  var rttiCtx := TRttiContext.Create;
  try
    var rttiType := rttiCtx.GetType(TypeInfo);
    if rttiType is TRttiInterfaceType then
    begin
      for var method in TRttiInterfaceType(rttiType).GetMethods do
      begin
        if (method.Name = 'get_Item') and (method.ReturnType <> nil) then
          FElementType := method.ReturnType.Handle
        else if (method.Name = 'GetEnumerator') and (method.ReturnType <> nil) then
          FEnumeratorTypeInfo := method.ReturnType.Handle;
      end;
    end;
  finally
    rttiCtx.Free;
  end;
end;

destructor TJSVirtualListInterface.Destroy;
begin
  if FCtx <> nil then
    JS_FreeValue(FCtx, FArrayValue);
  inherited;
end;

function TJSVirtualListInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result = S_OK then
    Exit;
    
  // For any interface request, return self - we'll handle all calls through Invoke
  // This allows us to pretend to support any IList<T> interface
  Result := S_OK;
  Pointer(Obj) := Pointer(Self as IInterface);
  if Pointer(Obj) <> nil then
    IInterface(Obj)._AddRef;
end;

function TJSVirtualListInterface.GetCount: Integer;
begin
  var lengthVal := JS_GetPropertyStr(FCtx, FArrayValue, 'length');
  JS_ToInt32(FCtx, @Result, lengthVal);
  JS_FreeValue(FCtx, lengthVal);
end;

function TJSVirtualListInterface.GetItemAt(Index: Integer): TValue;
begin
  var elementVal := JS_GetPropertyUint32(FCtx, FArrayValue, UInt32(Index));
  Result := FRuntime.JSValueToTValue(FCtx, elementVal, FElementType);
  JS_FreeValue(FCtx, elementVal);
end;

procedure TJSVirtualListInterface.SetItemAt(Index: Integer; const Value: TValue);
begin
  var jsVal := FRuntime.TValueToJSValue(FCtx, Value);
  JS_SetPropertyUint32(FCtx, FArrayValue, UInt32(Index), jsVal);
end;

function TJSVirtualListInterface.GetEnumerator: IEnumerator;
begin
  Result := TJSVirtualListEnumerator.Create(Self);
end;

function TJSVirtualListInterface.CreateTypedEnumerator: TValue;
begin
  // If we have a typed enumerator interface (IEnumerator<T>), create a virtual enumerator
  // that returns properly typed values from Current
  if (FEnumeratorTypeInfo <> nil) and (FElementType <> nil) then
  begin
    var virtualEnumerator := TJSVirtualEnumeratorInterface.Create(Self, FEnumeratorTypeInfo, FElementType);
    var ii: IInterface;
    if virtualEnumerator.QueryInterface(FEnumeratorTypeInfo.TypeData.GUID, ii) = S_OK then
    begin
      TValue.Make(@ii, FEnumeratorTypeInfo, Result);
      Exit;
    end;
  end;
  
  // Fall back to non-generic enumerator
  var enumerator := GetEnumerator;
  Result := TValue.From<IEnumerator>(enumerator);
end;

procedure TJSVirtualListInterface.Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  var methodName := Method.Name;
  
  // Handle property getters/setters
  if methodName = 'get_Count' then
  begin
    Result := TValue.From<Integer>(GetCount);
  end
  else if methodName = 'get_Item' then
  begin
    // Args[0] is Self, Args[1] is the index
    var index := Args[1].AsInteger;
    Result := GetItemAt(index);
  end
  else if methodName = 'set_Item' then
  begin
    // Args[0] is Self, Args[1] is the index, Args[2] is the value
    var index := Args[1].AsInteger;
    SetItemAt(index, Args[2]);
    Result := TValue.Empty;
  end
  else if methodName = 'IndexOf' then
  begin
    // Search for item by iterating
    var count := GetCount;
    var searchItem := Args[1];
    Result := TValue.From<Integer>(-1);
    
    for var i := 0 to count - 1 do
    begin
      var item := GetItemAt(i);
      // Simple equality check for interfaces
      if item.IsType<IInterface> and searchItem.IsType<IInterface> then
      begin
        if item.AsInterface = searchItem.AsInterface then
        begin
          Result := TValue.From<Integer>(i);
          Break;
        end;
      end;
    end;
  end
  else if methodName = 'Insert' then
  begin
    var index := Args[1].AsInteger;
    var value := Args[2];
    var jsVal := FRuntime.TValueToJSValue(FCtx, value);
    var indexVal := JS_NewInt32(FCtx, index);
    var deleteCountVal := JS_NewInt32(FCtx, 0);
    
    var spliceFunc := JS_GetPropertyStr(FCtx, FArrayValue, 'splice');
    var argv: array[0..2] of JSValueConst;
    argv[0] := indexVal;
    argv[1] := deleteCountVal;
    argv[2] := jsVal;
    JS_Call(FCtx, spliceFunc, FArrayValue, 3, @argv[0]);
    JS_FreeValue(FCtx, spliceFunc);
    JS_FreeValue(FCtx, indexVal);
    JS_FreeValue(FCtx, deleteCountVal);
    JS_FreeValue(FCtx, jsVal);
    Result := TValue.Empty;
  end
  else if methodName = 'RemoveAt' then
  begin
    var index := Args[1].AsInteger;
    var indexVal := JS_NewInt32(FCtx, index);
    var deleteCountVal := JS_NewInt32(FCtx, 1);
    
    var spliceFunc := JS_GetPropertyStr(FCtx, FArrayValue, 'splice');
    var argv: array[0..1] of JSValueConst;
    argv[0] := indexVal;
    argv[1] := deleteCountVal;
    JS_Call(FCtx, spliceFunc, FArrayValue, 2, @argv[0]);
    JS_FreeValue(FCtx, spliceFunc);
    JS_FreeValue(FCtx, indexVal);
    JS_FreeValue(FCtx, deleteCountVal);
    Result := TValue.Empty;
  end
  else if (methodName = 'RawArray') or (methodName = 'InnerArray') or (methodName = 'ToArray') then
  begin
    // Return nil array - not typically needed for iteration
    Result := TValue.Empty;
  end
  else if methodName = 'GetEnumerator' then
  begin
    // Use typed enumerator to support for..in loops with generic IList<T>
    Result := CreateTypedEnumerator;
  end
  else if methodName = 'get_InnerType' then
  begin
    if FElementType <> nil then
      Result := TValue.From<&Type>(&Type.Create(FElementType))
    else
      Result := TValue.From<&Type>(&Type.Unknown);
  end
  else if methodName = 'get_IsSynchronized' then
    Result := TValue.From<Boolean>(False)
  else if methodName = 'get_SyncRoot' then
    Result := TValue.From<TObject>(nil)
  else if methodName = 'get_IsFixedSize' then
    Result := TValue.From<Boolean>(False)
  else if methodName = 'get_IsReadOnly' then
    Result := TValue.From<Boolean>(False)
  else
    Result := TValue.Empty;
end;

end.

