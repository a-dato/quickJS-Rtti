unit QuickJS.Register.ObjectBridgeDefaultDefinitions.impl;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Collections,
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
end;

end.


