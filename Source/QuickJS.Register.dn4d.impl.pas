unit QuickJS.Register.dn4d.impl;

interface

uses
  System.TypInfo,
  quickjs,
  QuickJS.Register.intf,
  QuickJS.Register.impl;

type
  TJSRegisterTypedObjects = class(TJSRegister)
  protected
//    class function GenericPropertyGetter(ctx: JSContext; this_val: JSValueConst;
//      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;
//
//    class function GenericInterfacePropertyGetter(ctx: JSContext; this_val: JSValueConst;
//      argc: Integer; argv: PJSValueConst; magic: Integer; func_data : PJSValue ): JSValue; cdecl; static;

  public
    class procedure Initialize;

    class procedure RegisterObject<T>(ctx: JSContext; ClassName: string; AConstructor: TTypedConstuctor<T>); overload;
  end;

  TRegisteredTypedObject<T> = class(TRegisteredObject<T>)
  protected
    function  GetMemberByName(const AName: string; MemberTypes: TMemberTypes) : IRttiCachedDescriptor; override;
  end;

implementation

uses
  System_;

{ TJSRegisterTypedObjects }


{ TJSRegisterTypedObjects }

class procedure TJSRegisterTypedObjects.Initialize;
begin
  _JSRegisterInstance := TJSRegisterTypedObjects.Create;

//  GenericPropertyGetterPtr := @GenericPropertyGetter;
//  GenericInterfacePropertyGetterPtr := @GenericInterfacePropertyGetter;
end;

class procedure TJSRegisterTypedObjects.RegisterObject<T>(ctx: JSContext; ClassName: string; AConstructor: TTypedConstuctor<T>);
begin
  var reg: IRegisteredObject := TRegisteredTypedObject<T>.Create(AConstructor);
  InternalRegisterType(reg, ctx, ClassName);
  var p := TypeInfo(T);
  TMonitor.Enter(FRegisteredObjectsByType);
  try
    FRegisteredObjectsByType.Add(p, Reg);
  finally
    TMonitor.Exit(FRegisteredObjectsByType);
  end;
end;

//class function TJSRegisterTypedObjects.GenericInterfacePropertyGetter(
//  ctx: JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConst;
//  magic: Integer; func_data: PJSValue): JSValue;
//begin
//  Result := TJSRegister.GenericInterfacePropertyGetter(ctx, this_val, argc, argv, magic, func_data);
//end;
//
//class function TJSRegisterTypedObjects.GenericPropertyGetter(ctx: JSContext;
//  this_val: JSValueConst; argc: Integer; argv: PJSValueConst; magic: Integer;
//  func_data: PJSValue): JSValue;
//begin
//  Result := TJSRegister.GenericPropertyGetter(ctx, this_val, argc, argv, magic, func_data);
//end;

{ TRegisteredTypedObject<T> }

function TRegisteredTypedObject<T>.GetMemberByName(const AName: string; MemberTypes: TMemberTypes): IRttiCachedDescriptor;
begin
  var tp := &Type.Create(FTypeInfo);

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
    if IsInterface then
    begin
      SetLength(members, 2);
      members[0] := tp.GetMethod('get_' + AName);
      members[1] := tp.GetMethod('set_' + AName);
      if (members[0] <> nil) or (members[1] <> nil) then
        membertype := TMemberType.InterfacePropertyGetSet;
    end
    else
    begin
      SetLength(members, 1);
      members[0] := tp.PropertyByName(AName);
      if (members[0] <> nil) then
        membertype := TMemberType.Properties;
    end;
  end;

  Result := TRttiCachedDescriptor.Create(members, membertype, FTypeInfo);
end;

end.
