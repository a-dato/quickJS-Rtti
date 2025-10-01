unit TestObjectsDefinitionsTest.impl;

interface

uses
  System_,
  QuickJS.Register.ObjectBridgeTypes.intf,
  QuickJS.Register.ObjectBridge.intf,
  System.TypInfo,
  TestObjects.intf;

type
  TestObjectBridgeDefinitions = class
  public
    class procedure RegisterWithObjectBridge(const ObjectBridgeResolver: IObjectBridgeResolver);
    class procedure AddAllInterfacePropertiesAndMethods(
      const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo;
      const ObjectBridgeResolver: IObjectBridgeResolver);
  end;

implementation

uses
  System.SysUtils,
  QuickJS.Register.intf, System.Rtti,
  System.Collections.Generic, System.Collections,
  QuickJS.Register.ObjectBridge.impl, QuickJS.Register.ObjectBridgeTypes.impl,
  quickjs_ng, QuickJS.Register.PropertyDescriptors.impl, QuickJS.Register.impl;

{ TestObjectBridgeDefinitions }

class procedure TestObjectBridgeDefinitions.RegisterWithObjectBridge(const ObjectBridgeResolver: IObjectBridgeResolver);
begin
  // Generic IList<T> properties - matches any interface starting with "ilist<"
  ObjectBridgeResolver.AddPropertyDescriptor(
    TObjectBridgePropertyDescriptor.Create('length',
      // Object checker - check if object is an IList<T>
      function(const AObject: IRegisteredObject): Boolean
      begin
        var interfaceName := LowerCase(string(AObject.GetTypeInfo.Name));
        Result := interfaceName.StartsWith('ilist<');
      end,
      // Property getter - get Count property from IList
      function(const Ptr: Pointer): TValue
      begin
        Result := TValue.Empty;
        if Ptr = nil then Exit;
        
        // Cast to IInterface and try to get Count property
        var intf: IInterface := IInterface(Ptr);
        var list: System.Collections.IList;
        if Interfaces.Supports(intf, System.Collections.IList, list) then
          Result := TValue.From<Integer>(list.Count);
      end));

  // Test extending ITestObject with properties from ITestObject2 and ITestObject3
  // Since we're using TTestObject3 registered as ITestObject, it will support all interfaces
  // This allows access to inherited methods through ObjectBridge
  // Also bridge ITestObject to itself so base interface properties are captured
  AddAllInterfacePropertiesAndMethods(TypeInfo(ITestObject), TypeInfo(ITestObject2), ObjectBridgeResolver);
  AddAllInterfacePropertiesAndMethods(TypeInfo(ITestObject), TypeInfo(ITestObject3), ObjectBridgeResolver);

  // Extension properties for testing
  // testObject.customProperty -> get a custom value
  ObjectBridgeResolver.AddPropertyDescriptor(
    TObjectBridgePropertyDescriptor.Create('customProperty',
      // Object checker - check if object is ITestObject
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := AObject.GetTypeInfo = TypeInfo(ITestObject);
      end,
      // Property getter - return a custom value
      function(const Ptr: Pointer): TValue
      begin
        Result := TValue.From<CString>('This is a custom property from ObjectBridge');
      end));

  // testObject.dynamicCount -> return count of test array
  ObjectBridgeResolver.AddPropertyDescriptor(
    TObjectBridgePropertyDescriptor.Create('dynamicCount',
      // Object checker - check if object is ITestObject
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := AObject.GetTypeInfo = TypeInfo(ITestObject);
      end,
      // Property getter - get count of test array
      function(const Ptr: Pointer): TValue
      begin
        Result := TValue.Empty;
        if Ptr = nil then Exit;
        
        var testObj: ITestObject;
        if Interfaces.Supports(IInterface(Ptr), ITestObject, testObj) then
        begin
          var testArray := testObj.TestArray;
          Result := TValue.From<Integer>(testArray.Count);
        end;
      end));
end;

class procedure TestObjectBridgeDefinitions.AddAllInterfacePropertiesAndMethods(
  const SourceInterface: PTypeInfo; const TargetInterface: PTypeInfo;
  const ObjectBridgeResolver: IObjectBridgeResolver);
var
  rtti: TRttiContext;
  rttiType: TRttiType;
  methods: TArray<TRttiMethod>;
  m: TRttiMethod;
  jsMethodName: CString;
  jsPropertyName: CString;
  properties: TArray<TRttiProperty>;
  prop: TRttiProperty;
  printedPropsHeader: Boolean;
  anyPropsFound: Boolean;

  procedure RegisterPropertyDescriptor(const TargetProp: TRttiProperty; const JsName: CString);
  var
    localProp: TRttiProperty;
    localJsName: CString;
  begin
    localProp := TargetProp;
    localJsName := JsName;

    ObjectBridgeResolver.AddPropertyDescriptor(
      TObjectBridgePropertyDescriptor.Create(localJsName,
        // Object checker - apply to registered objects of the source interface type (by GUID)
        function(const AObject: IRegisteredObject): Boolean
        begin
          Result := (AObject <> nil) and IsEqualGUID(AObject.GetTypeInfo.TypeData.GUID, SourceInterface.TypeData.GUID);
        end,
        // Property getter - get property from target interface
        function(const Ptr: Pointer): TValue
        begin
          Result := TValue.Empty;
          if Ptr = nil then Exit;
          
          // Cast to source interface and query for target interface
          var sourceIntf: IInterface := IInterface(Ptr);
          var targetIntf: IInterface;
          if Interfaces.Supports(sourceIntf, TargetInterface.TypeData.Guid, targetIntf) then
          begin
            var targetValue := TValue.From<IInterface>(targetIntf);
            Result := localProp.GetValue(targetValue.GetReferenceToRawData);
          end;
        end));
  end;

  procedure RegisterMethodDescriptor(const TargetMethod: TRttiMethod; const JsName: CString);
  var
    localMethod: TRttiMethod;
    localJsName: CString;
  begin
    localMethod := TargetMethod;
    localJsName := JsName;

    ObjectBridgeResolver.AddMethodDescriptor(
      TObjectBridgeMethodDescriptor.Create(localJsName,
        // Object checker - apply to registered objects of the source interface type (by GUID)
        function(const AObject: IRegisteredObject): Boolean
        begin
          Result := (AObject <> nil) and IsEqualGUID(AObject.GetTypeInfo.TypeData.GUID, SourceInterface.TypeData.GUID);
        end,
        // Method caller - call method on target interface
        function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
        begin
          Result := JS_UNDEFINED;
          if Ptr = nil then Exit;
          
          // Cast to source interface and query for target interface
          var sourceIntf: IInterface := IInterface(Ptr);
          var targetIntf: IInterface;
          if Interfaces.Supports(sourceIntf, TargetInterface.TypeData.Guid, targetIntf) then
          begin
            // Convert JS arguments to Delphi parameters
            var params := localMethod.GetParameters;
            var args: array of TValue;
            SetLength(args, Length(params));
            
            for var i := 0 to High(params) do
            begin
              if i < argc then
                args[i] := JSConverterFuncs.JSValueToTValue(ctx, PJSValueConstArr(argv)[i], params[i].ParamType.Handle)
              else
                args[i] := JSConverterFuncs.GetDefaultValue(params[i]);
            end;
            
            var targetValue := TValue.From<IInterface>(targetIntf);
            var resultValue := localMethod.Invoke(targetValue, args);
            Result := JSConverterFuncs.TValueToJSValue(ctx, resultValue);
          end;
        end));
  end;
begin
  // Get RTTI for target interface
  rtti := TRttiContext.Create;
  rttiType := rtti.GetType(TargetInterface);
  
  Writeln(Format('Adding properties and methods from %s to %s:', [string(TargetInterface.Name), string(SourceInterface.Name)]));
  
  // Register properties from target interface to be accessible on source interface
  properties := rttiType.GetProperties;
  printedPropsHeader := False;
  anyPropsFound := False;
  if Length(properties) > 0 then
  begin
    Writeln('  Properties:');
    printedPropsHeader := True;
    anyPropsFound := True;
    for prop in properties do
    begin
      // Convert property name to JavaScript-style (lowercase first letter)
      jsPropertyName := CString.Create(prop.Name);
      if jsPropertyName.Length > 0 then
      begin
        jsPropertyName := jsPropertyName.Substring(0, 1).ToLower + jsPropertyName.Substring(1);
      end;
      Writeln(Format('    - %s (JS: %s)', [prop.Name, string(jsPropertyName)]));
      RegisterPropertyDescriptor(prop, jsPropertyName);
    end;
  end;

  // Note: No synthetic GetX -> x properties here to avoid collisions with methods

  if not anyPropsFound then
    Writeln('  Properties: (none)');

  // Register methods as well
  methods := rttiType.GetMethods;
  var validMethods: IList<TRttiMethod> := CList<TRttiMethod>.Create;
  try
    for m in methods do
    begin
      // Only expose real methods, skip getters/setters (get_/set_) and special names
      if m.IsConstructor then
        continue;
      if (m.Name.StartsWith('get_')) or (m.Name.StartsWith('set_')) then
        continue;
      
      validMethods.Add(m);
    end;
    
    if validMethods.Count > 0 then
    begin
      Writeln('  Methods:');
      for m in validMethods do
      begin
        jsMethodName := CString.Create(m.Name);
        if jsMethodName.Length > 0 then
          jsMethodName := jsMethodName.Substring(0, 1).ToLower + jsMethodName.Substring(1);
        
        Writeln(Format('    - %s (JS: %s)', [m.Name, string(jsMethodName)]));
        RegisterMethodDescriptor(m, jsMethodName);
      end;
    end
    else
      Writeln('  Methods: (none)');
  finally

  end;
end;

end.