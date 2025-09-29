unit QuickJS.Register.ObjectBridgeDefaultDefinitions.impl;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Collections,
  System.AnsiStrings,
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
  QuickJS.Register.impl, System_; // for JSConverter

class procedure TObjectBridgeDefaultDefinitions.Initialize(const Resolver: IObjectBridgeResolver);
begin
  if Resolver = nil then Exit;

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
        enumerable: IEnumerable;
      begin
        Result := JS_UNDEFINED;

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        l: IEnumerable; // for quick checks if needed
        ilist: System.Collections.IList;
      begin
        Result := JS_UNDEFINED;

        // Only handle IList for slicing
        if not Interfaces.Supports<System.Collections.IList>(IInterface(Ptr), ilist) then Exit;

        var count := ilist.Count;
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
            var v := JSConverter.Instance.TValueToJSValue(ctx, ilist[i].AsType<TValue>);
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
        enumerable: IEnumerable;
      begin
        Result := JS_UNDEFINED;

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        var jsArr := JS_NewArray(ctx);
        var outIndex := 0;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        ilist: System.Collections.IList;
        compareFunc: JSValue;
        hasCompareFunc: Boolean;
      begin
        Result := JS_UNDEFINED;

        // Only handle IList for sorting
        if not Interfaces.Supports<System.Collections.IList>(IInterface(Ptr), ilist) then Exit;

        hasCompareFunc := (argc >= 1) and JS_IsFunction(ctx, PJSValueConstArr(argv)[0]);
        if hasCompareFunc then
          compareFunc := PJSValueConstArr(argv)[0];

        var count := ilist.Count;
        if count <= 1 then
        begin
          Result := JSConverter.Instance.TValueToJSValue(ctx, TValue.From<IInterface>(IInterface(Ptr)));
          Exit;
        end;

        // Simple bubble sort implementation (can be optimized later)
        for var i := 0 to count - 2 do
        begin
          for var j := 0 to count - 2 - i do
          begin
            var shouldSwap := False;
            
            if hasCompareFunc then
            begin
              // Use custom comparison function
              var item1 := JSConverter.Instance.TValueToJSValue(ctx, ilist[j].AsType<TValue>);
              var item2 := JSConverter.Instance.TValueToJSValue(ctx, ilist[j + 1].AsType<TValue>);
              
              var call_argv: array of JSValueConst;
              SetLength(call_argv, 2);
              call_argv[0] := item1;
              call_argv[1] := item2;
              
              var compareResult := JS_Call(ctx, compareFunc, JS_Null, 2, PJSValueConstArr(call_argv));
              var compareInt: Integer;
              JS_ToInt32(ctx, @compareInt, compareResult);
              
              shouldSwap := compareInt > 0;
              
              JS_FreeValue(ctx, item1);
              JS_FreeValue(ctx, item2);
              JS_FreeValue(ctx, compareResult);
            end
            else
            begin
              // Default string comparison
              var val1 := ilist[j];
              var val2 := ilist[j + 1];
              
              // Convert to strings for comparison
              var str1: CString := '';
              var str2: CString := '';
              
              if val1 <> nil then
                str1 := val1.ToString;
              if val2 <> nil then
                str2 := val2.ToString;
                
              shouldSwap := str1.Length > str2.Length;
            end;
            
            if shouldSwap then
            begin
              // Swap elements
              var temp := ilist[j];
              ilist[j] := ilist[j + 1];
              ilist[j + 1] := temp;
            end;
          end;
        end;

        // Return the sorted list (same reference)
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
        enumerable: IEnumerable;
      begin
        Result := JS_NewBool(ctx, False); // Default to false

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        enumerable: IEnumerable;
      begin
        Result := JS_NewBool(ctx, True); // Default to true

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        enumerable: IEnumerable;
      begin
        Result := JS_UNDEFINED; // Default to undefined

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        enumerable: IEnumerable;
      begin
        Result := JS_NewInt32(ctx, -1); // Default to -1 (not found)

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        enumerable: IEnumerable;
      begin
        Result := JS_UNDEFINED;

        if argc <> 1 then
          raise Exception.Create('Invalid number of arguments');

        // Expect a function as first argument
        var func := PJSValueConstArr(argv)[0];
        if not JS_IsFunction(ctx, func) then Exit;

        var jsArr := JS_NewArray(ctx);
        var outIndex := 0;

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
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
        enumerable: IEnumerable;
      begin
        Result := JS_NewBool(ctx, False); // Default to false

        if argc < 1 then Exit;

        var searchValue := PJSValueConstArr(argv)[0];

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
            
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
        enumerable: IEnumerable;
      begin
        Result := JS_NewInt32(ctx, -1); // Default to -1 (not found)

        if argc < 1 then Exit;

        var searchValue := PJSValueConstArr(argv)[0];

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;

          while enum.MoveNext do
          begin
            var target := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
            
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
        enumerable: IEnumerable;
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

        // We treat bridged pointers as interfaces for enumeration
        if Interfaces.Supports<IEnumerable>(IInterface(Ptr), enumerable) then
        begin
          var enum := enumerable.GetEnumerator;
          var index := 0;
          var isFirstIteration := True;

          while enum.MoveNext do
          begin
            var currentValue := JSConverter.Instance.TValueToJSValue(ctx, enum.Current.AsType<TValue>);
            
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


