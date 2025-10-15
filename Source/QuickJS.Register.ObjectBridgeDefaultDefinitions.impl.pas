unit QuickJS.Register.ObjectBridgeDefaultDefinitions.impl;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.AnsiStrings,
  System.Generics.Collections,
  quickjs_ng,
  QuickJS.Register.intf,
  QuickJS.Register.ObjectBridge.intf,
  QuickJS.Register.ObjectBridgeTypes.impl;

type
  TObjectBridgeDefaultDefinitions = class
  public
    // Registers a set of default ObjectBridge descriptors (e.g., forEach)
    class procedure Initialize(const Resolver: IObjectBridgeResolver);
  end;

implementation

uses
  QuickJS.Register.impl, System.Generics.Defaults; // for JSConverter

type
  // RTTI-based enumerator wrapper to avoid DN4D System.Collections dependency
  TRttiEnumeratorHelper = record
    EnumeratorValue: TValue;
    MoveNextMethod: TRttiMethod;
    CurrentMethod: TRttiMethod;
    
    function MoveNext: Boolean;
    function GetCurrent: TValue;
    class function TryGetEnumerator(const Intf: IInterface; out Helper: TRttiEnumeratorHelper): Boolean; static;
  end;

  // RTTI-based list accessor to avoid DN4D System.Collections.IList dependency  
  TRttiListHelper = record
    ListInterface: IInterface;
    ListType: TRttiType;
    ItemProperty: TRttiIndexedProperty;
    CountProperty: TRttiProperty;
    
    function GetCount: Integer;
    function GetItem(Index: Integer): TValue;
    procedure SetItem(Index: Integer; const Value: TValue);
    class function TryGetListHelper(const Intf: IInterface; out Helper: TRttiListHelper): Boolean; static;
  end;

function TRttiEnumeratorHelper.MoveNext: Boolean;
begin
  if MoveNextMethod <> nil then
  begin
    var resultValue := MoveNextMethod.Invoke(EnumeratorValue, []);
    Result := resultValue.AsBoolean;
  end
  else
    Result := False;
end;

function TRttiEnumeratorHelper.GetCurrent: TValue;
begin
  if CurrentMethod <> nil then
    Result := CurrentMethod.Invoke(EnumeratorValue, [])
  else
    Result := TValue.Empty;
end;

class function TRttiEnumeratorHelper.TryGetEnumerator(const Intf: IInterface; out Helper: TRttiEnumeratorHelper): Boolean;
begin
  Result := False;
  if Intf = nil then Exit;
  
  var ctx := TRttiContext.Create;
  
  // Get the actual object implementing the interface
  var obj := Intf as TObject;
  var rttiType := ctx.GetType(obj.ClassType);
  if rttiType = nil then Exit;
  
  // Find GetEnumerator method
  var getEnumMethod := rttiType.GetMethod('GetEnumerator');
  if getEnumMethod = nil then Exit;
  
  // Call GetEnumerator on the object (not the interface TValue)
  Helper.EnumeratorValue := getEnumMethod.Invoke(obj, []);
  if Helper.EnumeratorValue.IsEmpty then Exit;
  
  // Get enumerator type
  var enumType := ctx.GetType(Helper.EnumeratorValue.TypeInfo);
  if enumType = nil then Exit;
  
  // Find MoveNext and Current
  Helper.MoveNextMethod := enumType.GetMethod('MoveNext');
  Helper.CurrentMethod := enumType.GetMethod('get_Current');
  
  Result := (Helper.MoveNextMethod <> nil) and (Helper.CurrentMethod <> nil);
end;

function TRttiListHelper.GetCount: Integer;
begin
  if CountProperty <> nil then
  begin
    var listObj := ListInterface as TObject;
    Result := CountProperty.GetValue(listObj).AsInteger;
  end
  else
    Result := 0;
end;

function TRttiListHelper.GetItem(Index: Integer): TValue;
begin
  if ItemProperty <> nil then
  begin
    var listObj := ListInterface as TObject;
    Result := ItemProperty.GetValue(listObj, [Index]);
  end
  else
    Result := TValue.Empty;
end;

procedure TRttiListHelper.SetItem(Index: Integer; const Value: TValue);
begin
  if ItemProperty <> nil then
  begin
    var listObj := ListInterface as TObject;
    ItemProperty.SetValue(listObj, [Index], Value);
  end;
end;

class function TRttiListHelper.TryGetListHelper(const Intf: IInterface; out Helper: TRttiListHelper): Boolean;
begin
  Result := False;
  if Intf = nil then Exit;
  
  var ctx := TRttiContext.Create;
  var rttiType := ctx.GetType((Intf as TObject).ClassType);
  if rttiType = nil then Exit;
  
  Helper.ListInterface := Intf;
  Helper.ListType := rttiType;
  
  // Find Count property
  Helper.CountProperty := rttiType.GetProperty('Count');
  if Helper.CountProperty = nil then Exit;
  
  // Find indexed Item property (default property)
  var indexedProps := rttiType.GetIndexedProperties;
  for var idxProp in indexedProps do
  begin
    // Look for Item[Integer] or any single-parameter indexer
    if (idxProp.ReadMethod <> nil) and (Length(idxProp.ReadMethod.GetParameters) = 1) then
    begin
      Helper.ItemProperty := idxProp;
      Result := True;
      Exit;
    end;
  end;
end;

class procedure TObjectBridgeDefaultDefinitions.Initialize(const Resolver: IObjectBridgeResolver);
begin
  if Resolver = nil then Exit;

  // IsDelphiObject: boolean property that returns true for all Delphi objects
  // This allows JS code to distinguish Delphi objects from pure JS objects
  // Example usage in JS:
  //   if (someObject.IsDelphiObject) {
  //     console.log("This is a Delphi object");
  //   }
  Resolver.AddPropertyDescriptor(
    TObjectBridgePropertyDescriptor.Create(
      'IsDelphiObject',
      // Object checker: handles any registered object
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := AObject <> nil;
      end,
      // Property getter: always returns true
      function(const Ptr: Pointer): TValue
      begin
        Result := TValue.From<Boolean>(True);
      end,
      // No setter (read-only property)
      nil,
      // Property type info
      TypeInfo(Boolean)
    )
  );

  // forEach: enumerable collections call JS callback for each item (value, index)
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'forEach',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_UNDEFINED;

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            Result := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));

            JS_FreeValue(ctx, call_argv[0]);
            JS_FreeValue(ctx, call_argv[1]);

            inc(index);
          end;
        end;
      end
    )
  );

  // slice: return a JS array subset from an IList (start[, end]) like JS Array.prototype.slice
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'slice',
      // Object checker: supports indexing (lists)
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsIndexing;
      end,
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        listHelper: TRttiListHelper;
      begin
        Result := JS_UNDEFINED;

        // Only handle list-like interfaces for slicing
        if not TRttiListHelper.TryGetListHelper(IInterface(Ptr), listHelper) then Exit;

        var count := listHelper.GetCount;
        var startIdx: Integer := 0;
        var endIdx: Integer := count;

        if argc >= 1 then
        begin
          JS_ToInt32(ctx, @startIdx, PJSValueConstArr(argv)[0]);
          if startIdx < 0 then startIdx := count + startIdx;
        end;

        if argc >= 2 then
        begin
          if JS_IsUndefined(PJSValueConstArr(argv)[1]) then
            endIdx := count
          else
          begin
            JS_ToInt32(ctx, @endIdx, PJSValueConstArr(argv)[1]);
            if endIdx < 0 then endIdx := count + endIdx;
          end;
        end;

        if startIdx < 0 then startIdx := 0;
        if startIdx > count then startIdx := count;
        if endIdx < 0 then endIdx := 0;
        if endIdx > count then endIdx := count;

        var jsArr := JS_NewArray(ctx);

        if endIdx > startIdx then
        begin
          var outIndex := 0;
          for var i := startIdx to endIdx - 1 do
          begin
            var item := listHelper.GetItem(i);
            var v := JSConverter.Instance.TValueToJSValue(ctx, item);
            // Use string index to avoid depending on Uint32 setter availability
            var s := AnsiString(outIndex.ToString);
            JS_SetPropertyStr(ctx, jsArr, PAnsiChar(s), v);
            inc(outIndex);
          end;
        end;

        Result := jsArr;
      end
    )
  );

  // map: transform enumerable collections by calling JS callback for each item, return new JS array
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'map',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_UNDEFINED;

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        var jsArr := JS_NewArray(ctx);
        var outIndex := 0;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            var mappedResult := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));

            // Add mapped result to output array
            var s := AnsiString(outIndex.ToString);
            JS_SetPropertyStr(ctx, jsArr, PAnsiChar(s), mappedResult);

            JS_FreeValue(ctx, call_argv[0]);
            JS_FreeValue(ctx, call_argv[1]);

            inc(index);
            inc(outIndex);
          end;
        end;

        Result := jsArr;
      end
    )
  );

  // sort: sort an IList in-place using optional JS comparison function
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'sort',
      // Object checker: supports indexing (lists)
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsIndexing;
      end,
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        listHelper: TRttiListHelper;
        compareFunc: JSValue;
        hasCompareFunc: Boolean;
        count, i: Integer;
        items: TArray<TValue>;
        cmp: IComparer<TValue>;
      begin
        Result := JS_UNDEFINED;

        if not TRttiListHelper.TryGetListHelper(IInterface(Ptr), listHelper) then Exit;

        hasCompareFunc := (argc >= 1) and JS_IsFunction(ctx, PJSValueConstArr(argv)[0]);
        if hasCompareFunc then
          compareFunc := PJSValueConstArr(argv)[0];

        count := listHelper.GetCount;
        if count <= 1 then
        begin
          Result := JSConverter.Instance.TValueToJSValue(ctx, TValue.From<IInterface>(IInterface(Ptr)));
          Exit;
        end;

        // Copy list items to a dynamic array
        SetLength(items, count);
        for i := 0 to count - 1 do
          items[i] := listHelper.GetItem(i);

        // Build comparer
        cmp := TComparer<TValue>.Construct(
          function(const A, B: TValue): Integer
          var
            // For JS comparator path
            item1, item2, compareResult: JSValue;
            compareInt: Int32;
            call_argv: array[0..1] of JSValueConst;
            // For default comparator path
            s1, s2: string;
          begin
            if hasCompareFunc then
            begin
              item1 := JSConverter.Instance.TValueToJSValue(ctx, A);
              item2 := JSConverter.Instance.TValueToJSValue(ctx, B);
              call_argv[0] := item1;
              call_argv[1] := item2;
              compareResult := JS_Call(ctx, compareFunc, JS_Null, 2, @call_argv[0]);
              JS_ToInt32(ctx, @compareInt, compareResult);

              JS_FreeValue(ctx, item1);
              JS_FreeValue(ctx, item2);
              JS_FreeValue(ctx, compareResult);
              Exit(compareInt); // JS comparator should return <0 / 0 / >0
            end;

            // Default: sort by string length (ascending)
            if not A.IsEmpty then s1 := A.ToString else s1 := '';
            if not B.IsEmpty then s2 := B.ToString else s2 := '';
            Result := Length(s1) - Length(s2);
          end
        );

        // Built-in sort
        TArray.Sort<TValue>(items, cmp);

        // Write back to the list
        for i := 0 to count - 1 do
          listHelper.SetItem(i, items[i]);

        // Return same list reference
        Result := JSConverter.Instance.TValueToJSValue(ctx, TValue.From<IInterface>(IInterface(Ptr)));
      end
    )
  );

  // some: test whether at least one element passes the test implemented by the callback function
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'some',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_NewBool(ctx, False); // Default to false

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            var testResult := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));
            var boolResult := JS_ToBool(ctx, testResult) <> 0;

            JS_FreeValue(ctx, call_argv[0]);
            JS_FreeValue(ctx, call_argv[1]);
            JS_FreeValue(ctx, testResult);

            if boolResult then
            begin
              Result := JS_NewBool(ctx, True);
              Exit;
            end;

            inc(index);
          end;
        end;
      end
    )
  );

  // every: test whether all elements pass the test implemented by the callback function
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'every',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_NewBool(ctx, True); // Default to true

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            var testResult := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));
            var boolResult := JS_ToBool(ctx, testResult) <> 0;

            JS_FreeValue(ctx, call_argv[0]);
            JS_FreeValue(ctx, call_argv[1]);
            JS_FreeValue(ctx, testResult);

            if not boolResult then
            begin
              Result := JS_NewBool(ctx, False);
              Exit;
            end;

            inc(index);
          end;
        end;
      end
    )
  );

  // find: return the first element that passes the test implemented by the callback function
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'find',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_UNDEFINED; // Default to undefined

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            var testResult := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));
            var boolResult := JS_ToBool(ctx, testResult) <> 0;

            JS_FreeValue(ctx, call_argv[1]);
            JS_FreeValue(ctx, testResult);

            if boolResult then
            begin
              Result := target; // Return the found element (don't free target)
              Exit;
            end;

            JS_FreeValue(ctx, call_argv[0]);
            inc(index);
          end;
        end;
      end
    )
  );

  // findIndex: return the index of the first element that passes the test implemented by the callback function
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'findIndex',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_NewInt32(ctx, -1); // Default to -1 (not found)

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            var testResult := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));
            var boolResult := JS_ToBool(ctx, testResult) <> 0;

            JS_FreeValue(ctx, call_argv[0]);
            JS_FreeValue(ctx, call_argv[1]);
            JS_FreeValue(ctx, testResult);

            if boolResult then
            begin
              Result := JS_NewInt32(ctx, index);
              Exit;
            end;

            inc(index);
          end;
        end;
      end
    )
  );

  // filter: create a new JS array with all elements that pass the test implemented by the callback function
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'filter',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_UNDEFINED;

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        var jsArr := JS_NewArray(ctx);
        var outIndex := 0;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            var indexValue := JS_NewInt32(ctx, index);

            var call_argv: array of JSValueConst;
            SetLength(call_argv, 2);
            call_argv[0] := target;
            call_argv[1] := indexValue;

            var testResult := JS_Call(ctx, func, JS_Null, 2, PJSValueConstArr(call_argv));
            var boolResult := JS_ToBool(ctx, testResult) <> 0;

            JS_FreeValue(ctx, call_argv[1]);
            JS_FreeValue(ctx, testResult);

            if boolResult then
            begin
              // Add element to filtered array
              var s := AnsiString(outIndex.ToString);
              JS_SetPropertyStr(ctx, jsArr, PAnsiChar(s), target);
              inc(outIndex);
            end
            else
              JS_FreeValue(ctx, call_argv[0]);

            inc(index);
          end;
        end;

        Result := jsArr;
      end
    )
  );

  // includes: determine whether an array includes a certain value among its entries
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'includes',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_NewBool(ctx, False); // Default to false

        if argc < 1 then Exit;

        var searchValue := PJSValueConstArr(argv)[0];

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            
            // Use string comparison for equality
            var targetStr: PAnsiChar;
            var searchStr: PAnsiChar;
            var targetLen: NativeUInt;
            var searchLen: NativeUInt;
            
            targetStr := JS_ToCStringLen(ctx, @targetLen, target);
            searchStr := JS_ToCStringLen(ctx, @searchLen, searchValue);
            
            var isEqual := (targetLen = searchLen) and (System.AnsiStrings.StrLComp(targetStr, searchStr, targetLen) = 0);
            
            JS_FreeCString(ctx, targetStr);
            JS_FreeCString(ctx, searchStr);
            JS_FreeValue(ctx, target);

            if isEqual then
            begin
              Result := JS_NewBool(ctx, True);
              Exit;
            end;
          end;
        end;
      end
    )
  );

  // indexOf: return the first index at which a given element can be found in the array
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'indexOf',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_NewInt32(ctx, -1); // Default to -1 (not found)

        if argc < 1 then Exit;

        var searchValue := PJSValueConstArr(argv)[0];

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var target := JSConverter.Instance.TValueToJSValue(ctx, current);
            
            // Use string comparison for equality
            var targetStr: PAnsiChar;
            var searchStr: PAnsiChar;
            var targetLen: NativeUInt;
            var searchLen: NativeUInt;
            
            targetStr := JS_ToCStringLen(ctx, @targetLen, target);
            searchStr := JS_ToCStringLen(ctx, @searchLen, searchValue);
            
            var isEqual := (targetLen = searchLen) and (System.AnsiStrings.StrLComp(targetStr, searchStr, targetLen) = 0);
            
            JS_FreeCString(ctx, targetStr);
            JS_FreeCString(ctx, searchStr);
            JS_FreeValue(ctx, target);

            if isEqual then
            begin
              Result := JS_NewInt32(ctx, index);
              Exit;
            end;

            inc(index);
          end;
        end;
      end
    )
  );

  // reduce: execute a reducer function on each element, resulting in a single output value
  Resolver.AddMethodDescriptor(
    TObjectBridgeMethodDescriptor.Create(
      'reduce',
      // Object checker: supports enumeration?
      function(const AObject: IRegisteredObject): Boolean
      begin
        Result := (AObject <> nil) and AObject.ObjectSupportsEnumeration;
      end,
      // Method caller implementation
      function(ctx: JSContext; Ptr: Pointer; argc: Integer; argv: PJSValueConst): JSValue
      var
        enumerator: TRttiEnumeratorHelper;
      begin
        Result := JS_UNDEFINED;

        if argc < 1 then
          raise Exception.Create('Invalid number of arguments - reducer function required');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        var hasInitialValue := argc >= 2;
        var accumulator: JSValue;
        
        if hasInitialValue then
          accumulator := JS_DupValue(ctx, PJSValueConstArr(argv)[1])
        else
          accumulator := JS_UNDEFINED;

        // Use RTTI to get enumerator
        if TRttiEnumeratorHelper.TryGetEnumerator(IInterface(Ptr), enumerator) then
        begin
          var index := 0;
          var isFirstIteration := True;

          while enumerator.MoveNext do
          begin
            var current := enumerator.GetCurrent;
            var currentValue := JSConverter.Instance.TValueToJSValue(ctx, current);
            
            if isFirstIteration and not hasInitialValue then
            begin
              // Use first element as initial accumulator
              accumulator := currentValue;
              isFirstIteration := False;
            end
            else
            begin
              var indexValue := JS_NewInt32(ctx, index);
              var arrayValue := JSConverter.Instance.TValueToJSValue(ctx, TValue.From<IInterface>(IInterface(Ptr)));

              var call_argv: array of JSValueConst;
              SetLength(call_argv, 4);
              call_argv[0] := accumulator;
              call_argv[1] := currentValue;
              call_argv[2] := indexValue;
              call_argv[3] := arrayValue;

              var newAccumulator := JS_Call(ctx, func, JS_Null, 4, PJSValueConstArr(call_argv));
              
              JS_FreeValue(ctx, accumulator);
              JS_FreeValue(ctx, currentValue);
              JS_FreeValue(ctx, indexValue);
              JS_FreeValue(ctx, arrayValue);
              
              accumulator := newAccumulator;
            end;

            inc(index);
          end;
        end;

        Result := accumulator;
      end
    )
  );
end;

end.


