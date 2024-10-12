unit QuickJS.Register.dn4d.impl;

interface

uses
  System.TypInfo,
  quickjs,
  QuickJS.Register.intf,
  QuickJS.Register.impl,
  System.Collections,
  System.Collections.Generic, System.Rtti;

type
  TJSRegisterTypedObjects = class(TJSRegister)
  protected
    class function GetMemberByName(TypeInfo: PTypeInfo; const AName: string; MemberTypes: TMemberTypes) : IRttiCachedDescriptor;
    class function GetMemberNames(TypeInfo: PTypeInfo; MemberTypes: TMemberTypes) : TArray<string>;

    class function GenericClassIterator(ctx : JSContext; this_val : JSValueConst;
      argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
//    class function GenericIteratorNext(ctx : JSContext; this_val : JSValueConst;
//      argc : Integer; argv : PJSValueConstArr): JSValue; cdecl; static;
    class function GenericInterfacePropertyGetter(ctx: JSContext; this_val: JSValueConst;
      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
    class function GenericInterfacePropertySetter(ctx : JSContext; this_val : JSValueConst;
      argc : Integer; argv : PJSValueConst; magic : Integer;
      func_data : PJSValue ): JSValue; cdecl; static;

  public
    class procedure Initialize(const Context: IJSContext);

//    class procedure RegisterObject<T>(ctx: JSContext; ClassName: string; AConstructor: TTypedConstuctor<T>); overload;
  end;

  TJSIEnumerableIterator = class(TJSIterator)
  protected
    _enumerator: System.Collections.IEnumerator;

  public
    class function CreateIterator(const E: IEnumerable) : TJSIEnumerableIterator;

    function MoveNext: Boolean; override;
    function Current: TValue; override;
  end;
implementation

uses
  System_, System.SysUtils;

{ TJSRegisterTypedObjects }


{ TJSRegisterTypedObjects }

class procedure TJSRegisterTypedObjects.Initialize(const Context: IJSContext);
begin
  GenericClassIteratorPtr := @GenericClassIterator;
  // GenericIteratorNextPtr := @GenericIteratorNext;

  GenericInterfacePropertyGetterPtr := @GenericInterfacePropertyGetter;
  GenericInterfacePropertySetterPtr := @GenericInterfacePropertySetter;

  GetMemberByNameFunc := GetMemberByName;
  GetMemberNamesFunc := GetMemberNames;

  TJSRegister.RegisterObject(Context.ctx, 'JSIEnumerableIterator', TypeInfo(TJSIEnumerableIterator));
end;

class function TJSRegisterTypedObjects.GenericClassIterator(ctx : JSContext; this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
begin
  Result := JS_UNDEFINED;

  var cid := GetClassID(this_val);
  var ptr := JS_GetOpaque(this_val, cid);

  if ptr <> nil then
  begin
    var reg: IRegisteredObject;
    if FRegisteredObjectsByClassID.TryGetValue(cid, reg) then
    begin
      var enumerable: IEnumerable := nil;

      if reg.IsInterface then
        Interfaces.Supports<IEnumerable>(IInterface(ptr), enumerable) else
        Interfaces.Supports<IEnumerable>(TObject(ptr), enumerable);

      if enumerable <> nil then
      begin
        var iter := TJSIEnumerableIterator.CreateIterator(enumerable);
        if iter <> nil then
        begin
          var reg_iter := FRegisteredObjectsByType[TypeInfo(TJSIEnumerableIterator)];
          Result := JS_NewObjectClass(ctx, reg_iter.ClassID);
          // Do not wrap object inside TObjectReference
          // Finalize will free instance when no longer in use
          JS_SetOpaque(Result, iter);
        end else
          Result := TJSRegister.GenericClassIterator(ctx, this_val, argc, argv);
      end;
    end;
  end;
end;

//class function TJSRegisterTypedObjects.GenericIteratorNext(ctx : JSContext; this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
//begin
//  var cid := TJSRegister.GetClassID(this_val);
//  var ptr := JS_GetOpaque(this_val, cid);
//
//  if TObject(ptr) is TJSIterator then
//    Result := TJSRegister.GenericIteratorNext(ctx, this_val, argc, argv)
//
//  else
//  begin
//    var iter := TJSIEnumerableIterator(ptr);
//
//    Result := JS_NewObject(ctx);
//
//    if iter.MoveNext then
//    begin
//      JS_SetPropertyStr(ctx, Result, 'value', JSConverter.TValueToJSValue(ctx, iter.Current));
//      JS_SetPropertyStr(ctx, Result, 'done', JS_FALSE);
//    end else
//      JS_SetPropertyStr(ctx, Result, 'done', JS_TRUE);
//  end;
//end;

class function TJSRegisterTypedObjects.GenericInterfacePropertyGetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  Result := JS_UNDEFINED;

  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IRttiCachedDescriptor := IRttiCachedDescriptor(Pointer(prtti));
  var prop := _PropertyInfo(descr.Getter);
  if prop <> nil then
  begin
    var ptr := GetObjectFromJSValue(this_val, False {Interface});
    var vt: TValue;
    TValue.Make(@ptr, descr.TypeInfo, vt);
    var o: CObject := prop.GetValue(vt.AsInterface, []);

    if o.IsRecord then
    begin
      // CString?
      if o.IsString then
      begin
        var s := AnsiString(o.ToString);
        Result := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));
      end
//      else if o.IsDateTime then
//      begin
//        var s := AnsiString(o.ToString);
//        Result := JS_NewStringLen(ctx, PAnsiChar(s), Length(s));
//      end
      else if o.IsCObject then
      begin
        var v: Variant;
        if o.TryAsType<Variant>(v) then
          Result := JSConverter.TValueToJSValue(ctx, TValue.FromVariant(v));
      end;

    end else
      Result := JSConverter.TValueToJSValue(ctx, o.AsType<TValue>);
  end;
end;

class function TJSRegisterTypedObjects.GenericInterfacePropertySetter(ctx : JSContext; this_val : JSValueConst;
  argc : Integer; argv : PJSValueConst; magic : Integer;
  func_data : PJSValue ): JSValue;

begin
  var prtti: Int64;
  TJSRuntime.Check(JS_ToInt64(ctx, @prtti, func_data^));
  var descr: IRttiCachedDescriptor := IRttiCachedDescriptor(Pointer(prtti));

  var prop := _PropertyInfo(descr.Getter); // Getter is used for properties
  if prop <> nil then
  begin
    if argc <> 1 then
      raise Exception.Create('Invalid number of arguments');

    var ptr := GetObjectFromJSValue(this_val, False {Interface});
    var vt: TValue;
    TValue.Make(@ptr, descr.TypeInfo, vt);
    var v := JSConverter.JSValueToTValue(ctx, PJSValueConstArray(argv)[0], prop.GetType.GetTypeInfo);

    prop.SetValue(vt.AsInterface, v, []);
  end;

  Result := JS_UNDEFINED;
end;

{ TRegisteredTypedObject<T> }

class function TJSRegisterTypedObjects.GetMemberByName(TypeInfo: PTypeInfo; const AName: string; MemberTypes: TMemberTypes): IRttiCachedDescriptor;
begin
  var tp := &Type.Create(TypeInfo);

  var members: TArray<PObjectMember> := nil;
  var membertype: TMemberType := TMemberType.None;

  if TMemberType.Methods in MemberTypes then
  begin
    var method := tp.GetMethod(AName);
    if method <> nil then
    begin
      membertype := TMemberType.Methods;
      SetLength(members, 1);
      members[0] := method;
    end;
    // No support for multiple methods having the same same
    //    if methods <> nil then
    //    begin
    //      membertype := TMemberType.Methods;
    //      SetLength(members, Length(methods));
    //      for var i := 0 to High(methods) do
    //        members[i] := methods[i];
    //    end;
  end;

  if (members = nil) and (TMemberType.Properties in MemberTypes) then
  begin
    SetLength(members, 1);
    members[0] := tp.PropertyByName(AName);
    if (members[0] <> nil) then
    begin
      if TypeInfo.Kind = tkInterface then
        membertype := TMemberType.InterfacePropertyGetSet else
        membertype := TMemberType.Properties;
    end
    else if tp.IsInterfaceType then
    begin
      SetLength(members, 2);
      members[0] := tp.GetMethod('get_' + AName);
      members[1] := tp.GetMethod('set_' + AName);
      if (members[0] <> nil) or (members[1] <> nil) then
        membertype := TMemberType.InterfacePropertyGetSet;
    end
  end;

  Result := TRttiCachedDescriptor.Create(members, membertype, TypeInfo);
end;

class function TJSRegisterTypedObjects.GetMemberNames(TypeInfo: PTypeInfo; MemberTypes: TMemberTypes) : TArray<string>;
begin
  var tp := &Type.Create(TypeInfo);
  var methods := tp.GetMethods;
  var properties := tp.GetProperties;
  var i := 0;

  SetLength(Result, Length(methods) + Length(properties));

  var m: TRttiMethod;
  for m in methods do
  begin
    Result[i] := m.Name;
    inc(i);
  end;

  var p: _PropertyInfo;
  for p in properties do
  begin
    Result[i] := p.Name;
    inc(i);
  end;
end;


{ JSIEnumerableIterator }

class function TJSIEnumerableIterator.CreateIterator(const E: IEnumerable): TJSIEnumerableIterator;
begin
  Result := nil;

  var enum := E.GetEnumerator;
  if enum <> nil then
  begin
    Result := TJSIEnumerableIterator.Create;
    Result._enumerator := enum;
  end;
end;

function TJSIEnumerableIterator.Current: TValue;
begin
  Result := _enumerator.Current.AsType<TValue>;
end;

function TJSIEnumerableIterator.MoveNext: Boolean;
begin
  Result := _enumerator.MoveNext;
end;

end.
