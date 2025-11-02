{
  FreePascal / Delphi bindings for QuickJS Engine.

  Copyright(c) 2019-2020 Coldzer0 <Coldzer0 [at] protonmail.ch>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit quickjs_ng; // sync with version - "2020-04-12" - Dynamic Loading Version

{$IfDef FPC}
  {$MODE Delphi}
  {$PackRecords C}
{$EndIf}

{$IfDef FPC}
  {$IfNDef CPU64}
    {$Define JS_NAN_BOXING}
  {$ENDIF}
{$ELSE}
   {$IfNDef CPUX64}
    {$Define JS_NAN_BOXING}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  dynlibs,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$ENDIF}
  math;

{===============================================================================}
{                              QuickJS Constants                                }
{===============================================================================}
const
  QJS_VERSION = '2020-04-12';

const
  JS_CLASS_BYTECODE_FUNCTION = 13;
  JS_CLASS_ARRAY_BUFFER = 19;
  JS_CLASS_PROMISE = 49;

const
  JS_TAG_FIRST       = -9;
  JS_TAG_BIG_INT     = -9;
  JS_TAG_SYMBOL      = -8;
  JS_TAG_STRING      = -7;
  JS_TAG_MODULE      = -3;
  JS_TAG_FUNCTION_BYTECODE = -2;
  JS_TAG_OBJECT      = -1;

  JS_TAG_INT         = 0;
  JS_TAG_BOOL        = 1;
  JS_TAG_NULL        = 2;
  JS_TAG_UNDEFINED   = 3;
  JS_TAG_UNINITIALIZED = 4;
  JS_TAG_CATCH_OFFSET = 5;
  JS_TAG_EXCEPTION   = 6;
  JS_TAG_SHORT_BIG_INT = 7;
  JS_TAG_FLOAT64     = 8;

  JS_FLOAT64_NAN = NaN;

const
  { flags for object properties }
  JS_PROP_CONFIGURABLE  = (1 shl 0);
  JS_PROP_WRITABLE      = (1 shl 1);
  JS_PROP_ENUMERABLE    = (1 shl 2);
  JS_PROP_C_W_E         = (JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE or JS_PROP_ENUMERABLE);
  JS_PROP_LENGTH        = (1 shl 3); { used internally in Arrays }
  JS_PROP_TMASK         = (3 shl 4); { mask for NORMAL, GETSET, VARREF, AUTOINIT }
  JS_PROP_NORMAL        = (0 shl 4);
  JS_PROP_GETSET        = (1 shl 4);
  JS_PROP_VARREF        = (2 shl 4); { used internally }
  JS_PROP_AUTOINIT      = (3 shl 4); { used internally }

  { flags for JS_DefineProperty }
  JS_PROP_HAS_SHIFT        = 8;
  JS_PROP_HAS_CONFIGURABLE = (1 shl 8);
  JS_PROP_HAS_WRITABLE     = (1 shl 9);
  JS_PROP_HAS_ENUMERABLE   = (1 shl 10);
  JS_PROP_HAS_GET          = (1 shl 11);
  JS_PROP_HAS_SET          = (1 shl 12);
  JS_PROP_HAS_VALUE        = (1 shl 13);

  { throw an exception if false would be returned /
   (JS_DefineProperty/JS_SetProperty) }
  JS_PROP_THROW            = (1 shl 14);
  { throw an exception if false would be returned in strict mode /
     (JS_SetProperty) }
  JS_PROP_THROW_STRICT     = (1 shl 15);
  JS_PROP_NO_ADD           = (1 shl 16); { internal use }
  JS_PROP_NO_EXOTIC        = (1 shl 17); { internal use }

  JS_DEFAULT_STACK_SIZE    = (256 * 1024);

  { JS_Eval() flags }
  JS_EVAL_TYPE_GLOBAL      = (0 shl 0); { global code (default) }
  JS_EVAL_TYPE_MODULE      = (1 shl 0); { module code }
  JS_EVAL_TYPE_DIRECT      = (2 shl 0); { direct call (internal use) }
  JS_EVAL_TYPE_INDIRECT    = (3 shl 0); { indirect call (internal use) }
  JS_EVAL_TYPE_MASK        = (3 shl 0);

  JS_EVAL_FLAG_STRICT      = (1 shl 3); { force 'strict' mode }
  JS_EVAL_FLAG_STRIP       = (1 shl 4); { force 'strip' mode }
  (*
    compile but do not run. The result is an object with a
     JS_TAG_FUNCTION_BYTECODE or JS_TAG_MODULE tag. It can be executed
     with JS_EvalFunction().
  *)
  JS_EVAL_FLAG_COMPILE_ONLY = (1 shl 5); { internal use }

  { don't include the stack frames before this eval in the Error() backtraces }
  JS_EVAL_FLAG_BACKTRACE_BARRIER = (1 shl 6);

  { Object Writer/Reader (currently only used to handle precompiled code)  }
  JS_WRITE_OBJ_BYTECODE     = (1 shl 0); { allow function/module }
  JS_WRITE_OBJ_BSWAP        = (1 shl 1); { byte swapped output }

  JS_READ_OBJ_BYTECODE      = (1 shl 0); { allow function/module  }
  JS_READ_OBJ_ROM_DATA      = (1 shl 1); { avoid duplicating 'buf' data  }

  { C property definition }
  JS_DEF_CFUNC            = 0;
  JS_DEF_CGETSET          = 1;
  JS_DEF_CGETSET_MAGIC    = 2;
  JS_DEF_PROP_STRING      = 3;
  JS_DEF_PROP_INT32       = 4;
  JS_DEF_PROP_INT64       = 5;
  JS_DEF_PROP_DOUBLE      = 6;
  JS_DEF_PROP_UNDEFINED   = 7;
  JS_DEF_OBJECT           = 8;
  JS_DEF_ALIAS            = 9;


  { C function definition }
  { JSCFunctionEnum }
  JS_CFUNC_generic                   = 0;
  JS_CFUNC_generic_magic             = 1;
  JS_CFUNC_constructor               = 2;
  JS_CFUNC_constructor_magic         = 3;
  JS_CFUNC_constructor_or_func       = 4;
  JS_CFUNC_constructor_or_func_magic = 5;
  JS_CFUNC_f_f                       = 6;
  JS_CFUNC_f_f_f                     = 7;
  JS_CFUNC_getter                    = 8;
  JS_CFUNC_setter                    = 9;
  JS_CFUNC_getter_magic              = 10;
  JS_CFUNC_setter_magic              = 11;
  JS_CFUNC_iterator_next             = 12;

  JS_GPN_STRING_MASK  = (1 shl 0);
  JS_GPN_SYMBOL_MASK  = (1 shl 1);
  JS_GPN_PRIVATE_MASK = (1 shl 2);

  { only include the enumerable properties }
  JS_GPN_ENUM_ONLY = (1 shl 4);
  { set theJSPropertyEnum.is_enumerable field }
  JS_GPN_SET_ENUM  = (1 shl 5);

  { C Call Flags }

  JS_CALL_FLAG_CONSTRUCTOR = (1 shl 0);
{===============================================================================}
{===============================================================================}

type
  {$IFNDEF FPC}
    // Delphi Compatible.
    // Anything under XE4.
  {$IF (CompilerVersion <= 25)}
    PUint32 = ^Uint32; // PUint32 not defined in XE4 - Fix by @edwinyzh
  {$IFEND}
    pUInt8  = PByte;
    pInt8   = PShortInt;
    pInt16  = PSmallint;
    PInt32  = PLongint;
  {$ENDIF}
  {$ifdef cpu64}
    size_t  = QWord;
    psize_t = ^size_t;
  {$else}
    size_t  = Cardinal;
    psize_t = ^size_t;
  {$endif}

  JS_BOOL   = Boolean;
  JSRuntime = Pointer;

  PPJSContext = ^_PJSContext; // Pointer to Pointer.
  _PJSContext = ^_JSContext;
  _JSContext  = record end; // Empty record to mimic the JSContext.
  JSContext   = Pointer;

  JSObject  = Pointer;
  JSClass   = Pointer;

  JSModuleDef = Pointer;

  JSString  = Pointer;

  JSClassID  = UInt32;
  PJSClassID = ^JSClassID;

  JSAtom    = UInt32;

  JSCFunctionEnum = Integer;
  JSPromiseStateEnum = integer;

  JSGCObjectHeader = Pointer;

type
  PJSRefCountHeader = ^JSRefCountHeader;
  JSRefCountHeader = record
      ref_count : Integer;
  end;

{$If Defined(JS_NAN_BOXING)}
  JSValue          = UInt64;
  PJSValue         = ^JSValue;
  JSValueConst     = JSValue;
  PJSValueConst    = ^JSValueConst;
  JSValueConstArr  = array[0..(MaxInt div SizeOf(JSValueConst))-1] of JSValueConst;
  PJSValueConstArr = ^JSValueConstArr;
const
  JS_FLOAT64_TAG_ADDEND =  $7ff80000 - JS_TAG_FIRST + 1; // quiet NaN encoding
  JS_NAN                = ($7ff8000000000000 - (JS_FLOAT64_TAG_ADDEND shl 32));
{$Else}
type
  JSValueUnion = record
      case byte of
      0 : (&int32 : int32);
      1 : (float64 : Double);
      2 : (Ptr : Pointer);
  end;

  JSValue = record
      u : JSValueUnion;
      tag : Int64;
  end;
  PJSValue         = ^JSValue;
  JSValueConst     = JSValue;
  PJSValueConst    = ^JSValueConst;
  JSValueConstArr  = array[0..(MaxInt div SizeOf(JSValueConst))-1] of JSValueConst;
  PJSValueConstArr = ^JSValueConstArr;
{$ENDIF}
type
  JSMallocState = record
      malloc_count,
      malloc_size,
      malloc_limit : size_t;
      opaque : Pointer;
  end;
  PJSMallocState = ^JSMallocState;

  //c_malloc = function (s : JSMallocState; size : UInt64) : Pointer;
  //Pc_malloc = ^c_malloc;
  // TODO: Check If funcs need to be Pointers or not. ^^^^^
  JSMallocFunctions = record
     js_malloc  : function (s : PJSMallocState; size : size_t) : Pointer; cdecl;
     js_free    : procedure (s : PJSMallocState; Ptr : Pointer); cdecl;
     js_realloc : function (s : PJSMallocState; Ptr : Pointer ; size : size_t) : Pointer; cdecl;
     js_malloc_usable_size : function (Ptr : Pointer) : size_t; cdecl;
  end;
  PJSMallocFunctions = ^JSMallocFunctions;

  PJSMemoryUsage = ^JSMemoryUsage;
  JSMemoryUsage = record
     malloc_size, malloc_limit, memory_used_size,
     malloc_count,
     memory_used_count,
     atom_count, atom_size,
     str_count, str_size,
     obj_count, obj_size,
     prop_count, prop_size,
     shape_count, shape_size,
     js_func_count, js_func_size, js_func_code_size,
     js_func_pc2line_count, js_func_pc2line_size,
     c_func_count, array_count,
     fast_array_count, fast_array_elements,
     binary_object_count, binary_object_size : Int64;
  end;

{===============================================================================}
{                        Native Functions Callbcaks                             }
{===============================================================================}

  PJSCFunction = ^JSCFunction;
  JSCFunction      = function (ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;

  PJSCFunctionMagic = ^JSCFunctionMagic;
  JSCFunctionMagic = function (ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConst; magic : Integer): JSValue; cdecl;


  PJSCFunctionData = ^JSCFunctionData;
  JSCFunctionData  = function (ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConst; magic : Integer;
    func_data : PJSValue ): JSValue; cdecl;

{===============================================================================}

  PJS_MarkFunc = ^JS_MarkFunc;
  JS_MarkFunc = procedure (rt : JSRuntime; gp : JSGCObjectHeader); cdecl;

  PJSClassFinalizer = ^JSClassFinalizer;
  JSClassFinalizer  = procedure (rt : JSRuntime; val : JSValue); cdecl;

  PJSClassGCMark   = ^JSClassGCMark;
  JSClassGCMark    = procedure (rt : JSRuntime; val : JSValueConst; mark_func: PJS_MarkFunc); cdecl;

  PJSClassCall     = ^JSClassCall;
  JSClassCall      = function (ctx : JSContext;
                              func_obj : JSValueConst;
                              this_val : JSValueConst;
                              argc : Integer; argv : PJSValueConst;
                              flags : Integer) : JSValue; cdecl;

  PJSFreeArrayBufferDataFunc = ^JSFreeArrayBufferDataFunc;
  JSFreeArrayBufferDataFunc  = procedure(rt : JSRuntime; opaque, Ptr : Pointer); cdecl;

  { return != 0 if the JS code needs to be interrupted }
  PJSInterruptHandler = ^JSInterruptHandler;
  JSInterruptHandler  = function (rt : JSRuntime; opaque : Pointer): integer; cdecl;

  { return the module specifier (allocated with js_malloc()) or NULL if exception }
  PJSModuleNormalizeFunc = ^JSModuleNormalizeFunc;
  JSModuleNormalizeFunc  = function (ctx : JSContext;
                              const module_base_name , module_name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
                              opaque : Pointer): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl;


  PJSModuleLoaderFunc = ^JSModuleLoaderFunc;
  JSModuleLoaderFunc  = function (ctx : JSContext; module_name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; opaque : Pointer) : JSModuleDef; cdecl;

  { JS Job support }
  PJSJobFunc = ^JSJobFunc;
  JSJobFunc  = function (ctx : JSContext; argc : Integer; argv : PJSValueConst): JSValue; cdecl;


  { C module definition }
  PJSModuleInitFunc = ^JSModuleInitFunc;
  JSModuleInitFunc  = function (ctx : JSContext; m : JSModuleDef): Integer; cdecl;

  { Promises RejectionTracker CallBack }

  { is_handled = TRUE means that the rejection is handled  }
  PJSHostPromiseRejectionTracker = ^JSHostPromiseRejectionTracker;
  JSHostPromiseRejectionTracker = procedure(ctx : JSContext;
                              promise, reason :JSValueConst;
                              is_handled : JS_BOOL; opaque : Pointer); cdecl;
{===============================================================================}

  { object class support }
  PPJSPropertyEnum = ^PJSPropertyEnum;
  PJSPropertyEnum = ^JSPropertyEnum;
  JSPropertyEnum = record
     is_enumerable : JS_BOOL;
     atom : JSAtom;
  end;

  PJSPropertyDescriptor = ^JSPropertyDescriptor;
  JSPropertyDescriptor = record
     flags : Integer;
     value,
     getter,
     setter : JSValue;
  end;

  PJSClassExoticMethods = ^JSClassExoticMethods;
  JSClassExoticMethods = record
    { Return -1 if exception (can only happen in case of Proxy object),
       FALSE if the property does not exists, TRUE if it exists. If 1 is
       returned, the property descriptor 'desc' is filled if != NULL. }
    get_own_property : function (ctx: JSContext; desc: PJSPropertyDescriptor; obj:JSValueConst; prop:JSAtom):Integer;cdecl;

    { '*ptab' should hold the '*plen' property keys. Return 0 if OK,
       -1 if exception. The 'is_enumerable' field is ignored. }
    get_own_property_names : function (ctx: JSContext; ptab:PPJSPropertyEnum; plen: pUInt32; obj:JSValueConst):Integer;cdecl;

    { return < 0 if exception, or TRUE/FALSE }
    delete_property : function (ctx: JSContext; obj:JSValueConst; prop:JSAtom):Integer;cdecl;

    { return < 0 if exception or TRUE/FALSE }
    define_own_property : function (ctx: JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValueConst; getter:JSValueConst;
                 setter:JSValueConst; flags:Integer):Integer;cdecl;

    { The following methods can be emulated with the previous ones,
       so they are usually not needed }

    { return < 0 if exception or TRUE/FALSE }
    has_property : function (ctx: JSContext; obj:JSValueConst; atom:JSAtom):Integer;cdecl;
    get_property : function (ctx: JSContext; obj:JSValueConst; atom:JSAtom; receiver:JSValueConst):JSValue;cdecl;
    set_property : function (ctx: JSContext; obj:JSValueConst; atom:JSAtom; value:JSValueConst; receiver:JSValueConst;
                   flags:Integer):Integer;cdecl;
    end;

  PJSClassDef = ^JSClassDef;
  JSClassDef = record
    class_name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
    finalizer : PJSClassFinalizer;
    gc_mark : PJSClassGCMark;
    {
      if call != NULL, the object is a function. If (flags &
             JS_CALL_FLAG_CONSTRUCTOR) != 0, the function is called as a
             constructor. In this case, 'this_val' is new.target. A
             constructor call only happens if the object constructor bit is
             set (see JS_SetConstructorBit())
    }
    call : PJSClassCall;
    { XXX: suppress this indirection ? It is here only to save memory
       because only a few classes need these methods }
    exotic : PJSClassExoticMethods;
  end;

  { C function definition }

  constructor_magic_func = function (ctx: JSContext; new_target:JSValueConst; argc:Integer; argv:PJSValueConst;
                              magic:Integer):JSValue; cdecl;
  f_f_func    = function (_para1:double):double cdecl;
  f_f_f_func  = function (_para1:double; _para2:double):double; cdecl;
  Getter_func = function (ctx: JSContext; this_val:JSValueConst):JSValue; cdecl;
  Setter_func = function (ctx: JSContext; this_val:JSValueConst; val:JSValueConst):JSValue;cdecl;
  getter_magic_func  = function (ctx: JSContext; this_val:JSValueConst; magic:Integer):JSValue; cdecl;
  setter_magic_func  = function (ctx: JSContext; this_val:JSValueConst; val:JSValueConst; magic:Integer):JSValue; cdecl;
  iterator_next_func = function (ctx: JSContext; this_val:JSValueConst; argc:Integer; argv:PJSValueConst; pdone:PInteger;
                               magic:Integer):JSValue; cdecl;
  JSCFunctionType = record
    case Integer of
      0 : ( generic : JSCFunction );
      1 : ( generic_magic :  JSCFunctionMagic);
      2 : ( &constructor : JSCFunction );
      3 : ( constructor_magic : constructor_magic_func);
      4 : ( constructor_or_func : JSCFunction );
      5 : ( f_f : f_f_func);
      6 : ( f_f_f : f_f_f_func);
      7 : ( getter : Getter_func);
      8 : ( setter : Setter_func);
      9 : ( getter_magic  : getter_magic_func);
      10 : ( setter_magic : setter_magic_func);
      11 : ( iterator_next : iterator_next_func);
  end;
  PJSCFunctionType = ^JSCFunctionType;

  { C property definition }
  JSCFunctionListEntry = record
    name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
    prop_flags : UInt8;
    def_type : UInt8;
    magic : Int16;
    u : record
    case Integer of
      0 : ( func : record
          length : UInt8; { XXX: should move outside union }
          cproto : UInt8; { XXX: should move outside union }
          cfunc : JSCFunctionType;
        end );
      1 : ( getset : record
          get : JSCFunctionType;
          _set : JSCFunctionType;
        end );
      2 : ( alias : record
          name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
          base : Integer;
        end );
      3 : ( prop_list : record
          tab : ^JSCFunctionListEntry;
          len : Integer;
        end );
      4 : ( str : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf} );
      5 : ( i32 : Int32 );
      6 : ( i64 : Int64 );
      7 : ( f64 : double );
    end;
  end;
  PJSCFunctionListEntry = ^JSCFunctionListEntry;

{===============================================================================}
{                        Dynamic Loading Function Pointer Types                 }
{===============================================================================}

  TJS_NewRuntime = function : JSRuntime; cdecl;
  TJS_SetRuntimeInfo = procedure(rt : JSRuntime; const info : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}); cdecl;
  TJS_SetMemoryLimit = procedure(rt : JSRuntime; limit : size_t); cdecl;
  TJS_SetGCThreshold = procedure(rt : JSRuntime; gc_threshold : size_t); cdecl;
  TJS_SetMaxStackSize = procedure(ctx: JSContext; stack_size:size_t); cdecl;
  TJS_NewRuntime2 = function(const mf : PJSMallocFunctions; opaque : Pointer) : JSRuntime; cdecl;
  TJS_FreeRuntime = procedure(rt : JSRuntime); cdecl;
  TJS_GetRuntimeOpaque = function(rt : JSRuntime) : Pointer; cdecl;
  TJS_SetRuntimeOpaque = procedure(rt : JSRuntime; opaque : Pointer); cdecl;
  TJS_MarkValue = procedure(rt:JSRuntime; val:JSValueConst; mark_func:PJS_MarkFunc); cdecl;
  TJS_RunGC = procedure(rt:JSRuntime); cdecl;
  TJS_IsLiveObject = function(rt:JSRuntime; obj:JSValueConst):JS_BOOL; cdecl;
  TJS_NewContext = function(rt:JSRuntime):JSContext; cdecl;
  TJS_FreeContext = procedure(s: JSContext); cdecl;
  TJS_DupContext = function(ctx : JSContext) : JSContext; cdecl;
  TJS_GetContextOpaque = function(ctx: JSContext):pointer; cdecl;
  TJS_SetContextOpaque = procedure(ctx: JSContext; opaque:pointer); cdecl;
  TJS_GetRuntime = function(ctx: JSContext):JSRuntime; cdecl;
  TJS_SetClassProto = procedure(ctx: JSContext; class_id:JSClassID; obj:JSValue); cdecl;
  TJS_GetClassProto = function(ctx: JSContext; class_id:JSClassID):JSValue; cdecl;
  TJS_NewContextRaw = function(rt: JSRuntime): JSContext; cdecl;
  TJS_AddIntrinsicBaseObjects = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicDate = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicEval = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicStringNormalize = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicRegExpCompiler = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicRegExp = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicJSON = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicProxy = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicMapSet = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicTypedArrays = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicPromise = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicBigInt = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicBigFloat = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicBigDecimal = procedure(ctx: JSContext); cdecl;
  TJS_AddIntrinsicOperators = procedure(ctx: JSContext); cdecl;
  TJS_EnableBignumExt = procedure(ctx: JSContext; enable : JS_BOOL); cdecl;
  Tjs_string_codePointRange = function(ctx: JSContext; this_val:JSValueConst; argc:Integer; argv:PJSValueConst):JSValue; cdecl;
  Tjs_malloc_rt = function(rt: JSRuntime; size:size_t):pointer; cdecl;
  Tjs_free_rt = procedure(rt: JSRuntime; ptr:pointer); cdecl;
  Tjs_realloc_rt = function(rt: JSRuntime; ptr:pointer; size:size_t):pointer; cdecl;
  Tjs_malloc_usable_size_rt = function(rt: JSRuntime; ptr:pointer):size_t; cdecl;
  Tjs_mallocz_rt = function(rt: JSRuntime; size:size_t):pointer; cdecl;
  Tjs_malloc = function(ctx: JSContext; size:size_t):pointer; cdecl;
  Tjs_free = procedure(ctx: JSContext; ptr:pointer); cdecl;
  Tjs_realloc = function(ctx: JSContext; ptr:pointer; size:size_t):pointer; cdecl;
  Tjs_malloc_usable_size = function(ctx: JSContext; ptr:pointer):size_t; cdecl;
  Tjs_realloc2 = function(ctx: JSContext; ptr:pointer; size:size_t; pslack:Psize_t):pointer; cdecl;
  Tjs_mallocz = function(ctx: JSContext; size:size_t):pointer; cdecl;
  Tjs_strdup = function(ctx: JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl;
  Tjs_strndup = function(ctx: JSContext; s:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; n:size_t): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl;
  TJS_ComputeMemoryUsage = procedure(rt: JSRuntime; s:PJSMemoryUsage); cdecl;
  TJS_DumpMemoryUsage = procedure(fp: Pointer; s:PJSMemoryUsage; rt: JSRuntime); cdecl;
  TJS_NewAtomLen = function(ctx: JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; len:size_t):JSAtom; cdecl;
  TJS_NewAtom = function(ctx: JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSAtom; cdecl;
  TJS_NewAtomUInt32 = function(ctx: JSContext; n:UInt32):JSAtom; cdecl;
  TJS_DupAtom = function(ctx: JSContext; v:JSAtom):JSAtom; cdecl;
  TJS_FreeAtom = procedure(ctx: JSContext; v:JSAtom); cdecl;
  TJS_FreeAtomRT = procedure(rt: JSRuntime; v:JSAtom); cdecl;
  TJS_AtomToValue = function(ctx: JSContext; atom:JSAtom):JSValue; cdecl;
  TJS_AtomToString = function(ctx: JSContext; atom:JSAtom):JSValue; cdecl;
  TJS_ValueToAtom = function(ctx: JSContext; val:JSValueConst) : JSAtom; cdecl;
  TJS_NewClassID = function(rt: JSRuntime; pclass_id:PJSClassID):JSClassID; cdecl;
  TJS_NewClass = function(rt: JSRuntime; class_id:JSClassID; class_def: PJSClassDef):Integer; cdecl;
  TJS_IsRegisteredClass = function(rt: JSRuntime; class_id:JSClassID):Integer; cdecl;
  TJS_GetClassID = function(Value: JSValueConst): JSClassID; cdecl;
  TJS_NewBigInt64 = function(ctx : JSContext; v : Int64): JSValue; cdecl;
  TJS_NewBigUint64 = function(ctx : JSContext; v : UInt64): JSValue; cdecl;
  TJS_Throw = function(ctx: JSContext; obj:JSValue):JSValue; cdecl;
  TJS_HasException = function(ctx: JSContext):JS_BOOL; cdecl;
  TJS_GetException = function(ctx: JSContext):JSValue; cdecl;
  TJS_IsError = function(ctx: JSContext; val:JSValueConst):JS_BOOL; cdecl;
  TJS_ResetUncatchableError = procedure(ctx: JSContext); cdecl;
  TJS_NewError = function(ctx: JSContext):JSValue; cdecl;
  TJS_ThrowSyntaxError = function(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl;
  TJS_ThrowTypeError = function(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl;
  TJS_ThrowReferenceError = function(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl;
  TJS_ThrowRangeError = function(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl;
  TJS_ThrowInternalError = function(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl;
  TJS_ThrowOutOfMemory = function(ctx: JSContext): JSValue; cdecl;
  T__JS_FreeValue = procedure(ctx: JSContext; v : JSValue); cdecl;
  T__JS_FreeValueRT = procedure(rt: JSRuntime; v : JSValue); cdecl;
  TJS_ToBool = function(ctx: JSContext; val:JSValueConst):Integer; cdecl;
  TJS_ToInt32 = function(ctx: JSContext; pres:pInt32; val:JSValueConst):Integer; cdecl;
  TJS_ToInt64 = function(ctx: JSContext; pres:PInt64; val:JSValueConst):Integer; cdecl;
  TJS_ToIndex = function(ctx: JSContext; plen:PUInt64; val:JSValueConst):Integer; cdecl;
  TJS_ToFloat64 = function(ctx: JSContext; pres:PDouble; val:JSValueConst):Integer; cdecl;
  TJS_ToBigInt64 = function(ctx: JSContext; pres:PInt64; val:JSValueConst):Integer; cdecl;
  TJS_ToInt64Ext = function(ctx: JSContext; pres:PInt64; val:JSValueConst):Integer; cdecl;
  TJS_NewStringLen = function(ctx:JSContext; str1:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; len1: size_t):JSValue; cdecl;
  TJS_NewAtomString = function(ctx:JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue; cdecl;
  TJS_ToString = function(ctx:JSContext; val:JSValueConst):JSValue; cdecl;
  TJS_ToPropertyKey = function(ctx:JSContext; val:JSValueConst):JSValue; cdecl;
  TJS_ToCStringLen2 = function(ctx:JSContext; plen:psize_t; val1:JSValueConst; cesu8:JS_BOOL): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl;
  TJS_FreeCString = procedure(ctx:JSContext; ptr:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}); cdecl;
  TJS_NewObjectProtoClass = function(ctx:JSContext; proto:JSValueConst; class_id:JSClassID):JSValue; cdecl;
  TJS_NewObjectClass = function(ctx:JSContext; class_id:JSClassID):JSValue; cdecl;
  TJS_NewObjectProto = function(ctx:JSContext; proto:JSValueConst):JSValue; cdecl;
  TJS_NewObject = function(ctx:JSContext):JSValue; cdecl;
  TJS_IsFunction = function(ctx:JSContext; val:JSValueConst):JS_BOOL; cdecl;
  TJS_IsConstructor = function(ctx:JSContext; val:JSValueConst):JS_BOOL; cdecl;
  TJS_SetConstructorBit = function(ctx:JSContext; func_obj : JSValueConst; val:JS_BOOL):JS_BOOL; cdecl;
  TJS_NewArray = function(ctx:JSContext):JSValue; cdecl;
  TJS_IsArray = function(ctx:JSContext; val:JSValueConst):Integer; cdecl;
  TJS_GetPropertyInternal = function(ctx:JSContext; obj:JSValueConst; prop:JSAtom; receiver:JSValueConst; throw_ref_error:JS_BOOL):JSValue; cdecl;
  TJS_GetPropertyStr = function(ctx:JSContext; this_obj:JSValueConst; prop:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue; cdecl;
  TJS_GetPropertyUint32 = function(ctx:JSContext; this_obj:JSValueConst; idx:UInt32):JSValue; cdecl;
  TJS_SetPropertyInternal = function(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValue; flags:Integer):Integer; cdecl;
  TJS_SetPropertyUint32 = function(ctx:JSContext; this_obj:JSValueConst; idx:UInt32; val:JSValue):Integer; cdecl;
  TJS_SetPropertyInt64 = function(ctx:JSContext; this_obj:JSValueConst; idx:Int64; val:JSValue):Integer; cdecl;
  TJS_SetPropertyStr = function(ctx:JSContext; this_obj:JSValueConst; prop:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val:JSValue):Integer; cdecl;
  TJS_HasProperty = function(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom):Integer; cdecl;
  TJS_IsExtensible = function(ctx:JSContext; obj:JSValueConst):Integer; cdecl;
  TJS_PreventExtensions = function(ctx:JSContext; obj:JSValueConst):Integer; cdecl;
  TJS_DeleteProperty = function(ctx:JSContext; obj:JSValueConst; prop:JSAtom; flags:Integer):Integer; cdecl;
  TJS_SetPrototype = function(ctx:JSContext; obj:JSValueConst; proto_val:JSValueConst):Integer; cdecl;
  TJS_GetPrototype = function(ctx:JSContext; val:JSValueConst):JSValueConst; cdecl;
  TJS_GetOwnPropertyNames = function(ctx: JSContext; ptab:PPJSPropertyEnum; plen: pUInt32; obj:JSValueConst; flags : Integer): Integer; cdecl;
  TJS_GetOwnProperty = function(ctx: JSContext; desc : PJSPropertyDescriptor; obj : JSValueConst; prop : JSAtom): Integer; cdecl;
  TJS_ParseJSON = function(ctx:JSContext; buf:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; buf_len:size_t; filename:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue; cdecl;
  TJS_JSONStringify = function(ctx:JSContext; obj, replacer, space0 : JSValueConst):JSValue; cdecl;
  TJS_Call = function(ctx:JSContext; func_obj:JSValueConst; this_obj:JSValueConst; argc:Integer; argv:PJSValueConstArr):JSValue; cdecl;
  TJS_Invoke = function(ctx:JSContext; this_val:JSValueConst; atom:JSAtom; argc:Integer; argv:PJSValueConst):JSValue; cdecl;
  TJS_CallConstructor = function(ctx:JSContext; func_obj:JSValueConst; argc:Integer; argv:PJSValueConst):JSValue; cdecl;
  TJS_CallConstructor2 = function(ctx:JSContext; func_obj:JSValueConst; new_target:JSValueConst; argc:Integer; argv:PJSValueConst):JSValue; cdecl;
  TJS_DetectModule = function(const input:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; input_len : size_t):JS_BOOL; cdecl;
  TJS_Eval = function(ctx:JSContext; input:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; input_len:size_t; filename:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; eval_flags:Integer):JSValue; cdecl;
  TJS_EvalFunction = function(ctx:JSContext; fun_obj : JSValue):JSValue; cdecl;
  TJS_GetGlobalObject = function(ctx:JSContext):JSValue; cdecl;
  TJS_IsInstanceOf = function(ctx:JSContext; val:JSValueConst; obj:JSValueConst):Integer; cdecl;
  TJS_DefineProperty = function(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValueConst; getter:JSValueConst; setter:JSValueConst; flags:Integer):Integer; cdecl;
  TJS_DefinePropertyValue = function(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValue; flags:Integer):Integer; cdecl;
  TJS_DefinePropertyValueUint32 = function(ctx:JSContext; this_obj:JSValueConst; idx:UInt32; val:JSValue; flags:Integer):Integer; cdecl;
  TJS_DefinePropertyValueStr = function(ctx:JSContext; this_obj:JSValueConst; prop:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val:JSValue; flags:Integer):Integer; cdecl;
  TJS_DefinePropertyGetSet = function(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; getter:JSValue; setter:JSValue; flags:Integer):Integer; cdecl;
  TJS_SetOpaque = procedure(obj:JSValue; opaque:pointer); cdecl;
  TJS_GetOpaque = function(obj:JSValueConst; class_id:JSClassID):pointer; cdecl;
  TJS_GetOpaque2 = function(ctx:JSContext; obj:JSValueConst; class_id:JSClassID):pointer; cdecl;
  TJS_NewArrayBuffer = function(ctx:JSContext; buf:pUInt8; len:size_t; free_func:PJSFreeArrayBufferDataFunc; opaque:pointer; is_shared:JS_BOOL):JSValue; cdecl;
  TJS_NewArrayBufferCopy = function(ctx:JSContext; buf:pUInt8; len:size_t):JSValue; cdecl;
  TJS_DetachArrayBuffer = procedure(ctx:JSContext; obj:JSValueConst); cdecl;
  TJS_GetArrayBuffer = function(ctx:JSContext; psize:Psize_t; obj:JSValueConst):pUInt8; cdecl;
  TJS_GetTypedArrayBuffer = function(ctx : JSContext; obj : JSValueConst; pbyte_offset, pbyte_length, pbytes_per_element : psize_t):JSValue; cdecl;
  TJS_NewPromiseCapability = function(ctx:JSContext; resolving_funcs:PJSValue):JSValue; cdecl;
  TJS_PromiseState = function(ctx:JSContext; promise:JSValue):JSPromiseStateEnum; cdecl;
  TJS_PromiseResult = function(ctx:JSContext; promise:JSValue):JSValue; cdecl;
  TJS_IsPromise = function(val: JSValueConst) : Boolean; cdecl;
  TJS_SetHostPromiseRejectionTracker = procedure(rt: JSRuntime; cb : PJSHostPromiseRejectionTracker; opaque : Pointer); cdecl;
  TJS_SetInterruptHandler = procedure(rt:JSRuntime; cb:PJSInterruptHandler; opaque:pointer); cdecl;
  TJS_SetCanBlock = procedure(rt:JSRuntime; can_block:JS_BOOL); cdecl;
  TJS_SetModuleLoaderFunc = procedure(rt:JSRuntime; module_normalize:PJSModuleNormalizeFunc; module_loader:PJSModuleLoaderFunc; opaque:pointer); cdecl;
  TJS_EnqueueJob = function(ctx:JSContext; job_func:PJSJobFunc; argc:Integer; argv:PJSValueConst):Integer; cdecl;
  TJS_IsJobPending = function(rt:JSRuntime):JS_BOOL; cdecl;
  TJS_ExecutePendingJob = function(rt:JSRuntime; pctx: PPJSContext):Integer; cdecl;
  TJS_WriteObject = function(ctx: JSContext; psize:psize_t; obj:JSValueConst; flags:Integer):pUInt8; cdecl;
  TJS_ReadObject = function(ctx: JSContext; buf:pUInt8; buf_len:size_t; flags:Integer):JSValue; cdecl;
  TJS_ResolveModule = function(ctx: JSContext; obj : JSValueConst):Integer; cdecl;
  TJS_GetModuleNamespace = function(ctx: JSContext; ModuleDef: JSModuleDef) : JSValue; cdecl;
  TJS_SetConstructor = procedure(ctx : JSContext; func_obj, proto : JSValueConst); cdecl;
  TJS_NewCFunction2 = function(ctx: JSContext; func:PJSCFunction; name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length:Integer; cproto:JSCFunctionEnum; magic:Integer):JSValue; cdecl;
  TJS_NewCFunctionData = function(ctx: JSContext; func:PJSCFunctionData; length:Integer; magic:Integer; data_len:Integer; data:PJSValueConst):JSValue; cdecl;
  TJS_SetPropertyFunctionList = procedure(ctx: JSContext; obj:JSValueConst; tab:PJSCFunctionListEntry; len:Integer); cdecl;
  TJS_NewCModule = function(ctx: JSContext; name_str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; func:PJSModuleInitFunc): JSModuleDef; cdecl;
  TJS_AddModuleExport = function(ctx: JSContext; m: JSModuleDef; name_str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):Integer; cdecl;
  TJS_AddModuleExportList = function(ctx: JSContext; m: JSModuleDef; tab:PJSCFunctionListEntry; len:Integer):Integer; cdecl;
  TJS_SetModuleExport = function(ctx: JSContext; m: JSModuleDef; export_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val:JSValue):Integer; cdecl;
  TJS_SetModuleExportList = function(ctx: JSContext; m: JSModuleDef; tab:PJSCFunctionListEntry; len:Integer):Integer; cdecl;
  TJS_GetImportMeta = function(ctx: JSContext; m: JSModuleDef) : JSValue; cdecl;
  TJS_GetModuleName = function(ctx: JSContext; m: JSModuleDef) : JSAtom; cdecl;
  Tjs_init_module_std = function(ctx: JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSModuleDef; cdecl;
  Tjs_init_module_os = function(ctx: JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSModuleDef; cdecl;
  Tjs_init_module_bjson = function(ctx: JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSModuleDef; cdecl;
  Tjs_std_add_helpers = procedure(ctx : JSContext; argc : Integer; argv : Pointer); cdecl;
  Tjs_std_loop = procedure(ctx : JSContext); cdecl;
  Tjs_std_init_handlers = procedure(rt:JSRuntime); cdecl;
  Tjs_std_free_handlers = procedure(rt:JSRuntime); cdecl;
  Tjs_std_dump_error = procedure(ctx:JSContext); cdecl;
  Tjs_std_await = function(ctx:JSContext; val:JSValue) : JSValue; cdecl;
  Tjs_load_file = function(ctx:JSContext; pbuf_len: psize_t; filename:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}): Pointer; cdecl;
  Tjs_module_loader = function(ctx:JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; opaque:pointer):JSModuleDef; cdecl;
  Tjs_std_eval_binary = procedure(ctx : JSContext; buf : Pointer; buf_len : size_t; flags : Integer); cdecl;
  Tjs_module_set_import_meta = function(ctx : JSContext; func_val : JSValueConst; use_realpath, is_main : JS_BOOL) : Integer; cdecl;
  Tjs_std_promise_rejection_tracker = procedure(ctx : JSContext; promise, reason : JSValueConst; is_handled : JS_BOOL; opaque : Pointer); cdecl;
  TJS_NewInt64 = function(ctx : JSContext; val : Int64): JSValue; cdecl;
  TJS_FreeValue = procedure(ctx : JSContext; v : JSValue); cdecl;
  TJS_FreeValueRT = procedure(rt : JSRuntime; v : JSValue); cdecl;
  TJS_DupValue = function(ctx : JSContext; v : JSValueConst) : JSValue; cdecl;
  TJS_DupValueRT = function(rt : JSRuntime; v : JSValueConst) : JSValue; cdecl;

{===============================================================================}
{                        Dynamic Loading Variables                              }
{===============================================================================}

const
  {$IFDEF mswindows}
  QJSDLL = {$IfDef WIN64}'quickjs64.dll'{$Else}'quickjs32.dll'{$EndIf};
  {$ELSE}
  QJSDLL = 'libquickjs.so';
  {$endif}

var
  QuickJSHandle: {$IFDEF FPC}TLibHandle{$ELSE}HMODULE{$ENDIF} = 0;
  QuickJSLoaded: Boolean = False;

  JS_NewRuntime: TJS_NewRuntime = nil;
  JS_SetRuntimeInfo: TJS_SetRuntimeInfo = nil;
  JS_SetMemoryLimit: TJS_SetMemoryLimit = nil;
  JS_SetGCThreshold: TJS_SetGCThreshold = nil;
  JS_SetMaxStackSize: TJS_SetMaxStackSize = nil;
  JS_NewRuntime2: TJS_NewRuntime2 = nil;
  JS_FreeRuntime: TJS_FreeRuntime = nil;
  JS_GetRuntimeOpaque: TJS_GetRuntimeOpaque = nil;
  JS_SetRuntimeOpaque: TJS_SetRuntimeOpaque = nil;
  JS_MarkValue: TJS_MarkValue = nil;
  JS_RunGC: TJS_RunGC = nil;
  JS_IsLiveObject: TJS_IsLiveObject = nil;
  JS_NewContext: TJS_NewContext = nil;
  JS_FreeContext: TJS_FreeContext = nil;
  JS_DupContext: TJS_DupContext = nil;
  JS_GetContextOpaque: TJS_GetContextOpaque = nil;
  JS_SetContextOpaque: TJS_SetContextOpaque = nil;
  JS_GetRuntime: TJS_GetRuntime = nil;
  JS_SetClassProto: TJS_SetClassProto = nil;
  JS_GetClassProto: TJS_GetClassProto = nil;
  JS_NewContextRaw: TJS_NewContextRaw = nil;
  JS_AddIntrinsicBaseObjects: TJS_AddIntrinsicBaseObjects = nil;
  JS_AddIntrinsicDate: TJS_AddIntrinsicDate = nil;
  JS_AddIntrinsicEval: TJS_AddIntrinsicEval = nil;
  JS_AddIntrinsicStringNormalize: TJS_AddIntrinsicStringNormalize = nil;
  JS_AddIntrinsicRegExpCompiler: TJS_AddIntrinsicRegExpCompiler = nil;
  JS_AddIntrinsicRegExp: TJS_AddIntrinsicRegExp = nil;
  JS_AddIntrinsicJSON: TJS_AddIntrinsicJSON = nil;
  JS_AddIntrinsicProxy: TJS_AddIntrinsicProxy = nil;
  JS_AddIntrinsicMapSet: TJS_AddIntrinsicMapSet = nil;
  JS_AddIntrinsicTypedArrays: TJS_AddIntrinsicTypedArrays = nil;
  JS_AddIntrinsicPromise: TJS_AddIntrinsicPromise = nil;
  JS_AddIntrinsicBigInt: TJS_AddIntrinsicBigInt = nil;
  JS_AddIntrinsicBigFloat: TJS_AddIntrinsicBigFloat = nil;
  JS_AddIntrinsicBigDecimal: TJS_AddIntrinsicBigDecimal = nil;
  JS_AddIntrinsicOperators: TJS_AddIntrinsicOperators = nil;
  JS_EnableBignumExt: TJS_EnableBignumExt = nil;
  js_string_codePointRange: Tjs_string_codePointRange = nil;
  js_malloc_rt: Tjs_malloc_rt = nil;
  js_free_rt: Tjs_free_rt = nil;
  js_realloc_rt: Tjs_realloc_rt = nil;
  js_malloc_usable_size_rt: Tjs_malloc_usable_size_rt = nil;
  js_mallocz_rt: Tjs_mallocz_rt = nil;
  js_malloc: Tjs_malloc = nil;
  js_free: Tjs_free = nil;
  js_realloc: Tjs_realloc = nil;
  js_malloc_usable_size: Tjs_malloc_usable_size = nil;
  js_realloc2: Tjs_realloc2 = nil;
  js_mallocz: Tjs_mallocz = nil;
  js_strdup: Tjs_strdup = nil;
  js_strndup: Tjs_strndup = nil;
  JS_ComputeMemoryUsage: TJS_ComputeMemoryUsage = nil;
  JS_DumpMemoryUsage: TJS_DumpMemoryUsage = nil;
  JS_NewAtomLen: TJS_NewAtomLen = nil;
  JS_NewAtom: TJS_NewAtom = nil;
  JS_NewAtomUInt32: TJS_NewAtomUInt32 = nil;
  JS_DupAtom: TJS_DupAtom = nil;
  JS_FreeAtom: TJS_FreeAtom = nil;
  JS_FreeAtomRT: TJS_FreeAtomRT = nil;
  JS_AtomToValue: TJS_AtomToValue = nil;
  JS_AtomToString: TJS_AtomToString = nil;
  JS_ValueToAtom: TJS_ValueToAtom = nil;
  JS_NewClassID: TJS_NewClassID = nil;
  JS_NewClass: TJS_NewClass = nil;
  JS_IsRegisteredClass: TJS_IsRegisteredClass = nil;
  JS_GetClassID: TJS_GetClassID = nil;
  JS_NewBigInt64: TJS_NewBigInt64 = nil;
  JS_NewBigUint64: TJS_NewBigUint64 = nil;
  JS_Throw: TJS_Throw = nil;
  JS_HasException: TJS_HasException = nil;
  JS_GetException: TJS_GetException = nil;
  JS_IsError: TJS_IsError = nil;
  JS_ResetUncatchableError: TJS_ResetUncatchableError = nil;
  JS_NewError: TJS_NewError = nil;
  JS_ThrowSyntaxError: TJS_ThrowSyntaxError = nil;
  JS_ThrowTypeError: TJS_ThrowTypeError = nil;
  JS_ThrowReferenceError: TJS_ThrowReferenceError = nil;
  JS_ThrowRangeError: TJS_ThrowRangeError = nil;
  JS_ThrowInternalError: TJS_ThrowInternalError = nil;
  JS_ThrowOutOfMemory: TJS_ThrowOutOfMemory = nil;
  __JS_FreeValue: T__JS_FreeValue = nil;
  __JS_FreeValueRT: T__JS_FreeValueRT = nil;
  JS_ToBool: TJS_ToBool = nil;
  JS_ToInt32: TJS_ToInt32 = nil;
  JS_ToInt64: TJS_ToInt64 = nil;
  JS_ToIndex: TJS_ToIndex = nil;
  JS_ToFloat64: TJS_ToFloat64 = nil;
  JS_ToBigInt64: TJS_ToBigInt64 = nil;
  JS_ToInt64Ext: TJS_ToInt64Ext = nil;
  JS_NewStringLen: TJS_NewStringLen = nil;
  JS_NewAtomString: TJS_NewAtomString = nil;
  JS_ToString: TJS_ToString = nil;
  JS_ToPropertyKey: TJS_ToPropertyKey = nil;
  JS_ToCStringLen2: TJS_ToCStringLen2 = nil;
  JS_FreeCString: TJS_FreeCString = nil;
  JS_NewObjectProtoClass: TJS_NewObjectProtoClass = nil;
  JS_NewObjectClass: TJS_NewObjectClass = nil;
  JS_NewObjectProto: TJS_NewObjectProto = nil;
  JS_NewObject: TJS_NewObject = nil;
  JS_IsFunction: TJS_IsFunction = nil;
  JS_IsConstructor: TJS_IsConstructor = nil;
  JS_SetConstructorBit: TJS_SetConstructorBit = nil;
  JS_NewArray: TJS_NewArray = nil;
  JS_IsArray: TJS_IsArray = nil;
  JS_GetPropertyInternal: TJS_GetPropertyInternal = nil;
  JS_GetPropertyStr: TJS_GetPropertyStr = nil;
  JS_GetPropertyUint32: TJS_GetPropertyUint32 = nil;
  JS_SetPropertyInternal: TJS_SetPropertyInternal = nil;
  JS_SetPropertyUint32: TJS_SetPropertyUint32 = nil;
  JS_SetPropertyInt64: TJS_SetPropertyInt64 = nil;
  JS_SetPropertyStr: TJS_SetPropertyStr = nil;
  JS_HasProperty: TJS_HasProperty = nil;
  JS_IsExtensible: TJS_IsExtensible = nil;
  JS_PreventExtensions: TJS_PreventExtensions = nil;
  JS_DeleteProperty: TJS_DeleteProperty = nil;
  JS_SetPrototype: TJS_SetPrototype = nil;
  JS_GetPrototype: TJS_GetPrototype = nil;
  JS_GetOwnPropertyNames: TJS_GetOwnPropertyNames = nil;
  JS_GetOwnProperty: TJS_GetOwnProperty = nil;
  JS_ParseJSON: TJS_ParseJSON = nil;
  JS_JSONStringify: TJS_JSONStringify = nil;
  JS_Call: TJS_Call = nil;
  JS_Invoke: TJS_Invoke = nil;
  JS_CallConstructor: TJS_CallConstructor = nil;
  JS_CallConstructor2: TJS_CallConstructor2 = nil;
  JS_DetectModule: TJS_DetectModule = nil;
  JS_Eval: TJS_Eval = nil;
  JS_EvalFunction: TJS_EvalFunction = nil;
  JS_GetGlobalObject: TJS_GetGlobalObject = nil;
  JS_IsInstanceOf: TJS_IsInstanceOf = nil;
  JS_DefineProperty: TJS_DefineProperty = nil;
  JS_DefinePropertyValue: TJS_DefinePropertyValue = nil;
  JS_DefinePropertyValueUint32: TJS_DefinePropertyValueUint32 = nil;
  JS_DefinePropertyValueStr: TJS_DefinePropertyValueStr = nil;
  JS_DefinePropertyGetSet: TJS_DefinePropertyGetSet = nil;
  JS_SetOpaque: TJS_SetOpaque = nil;
  JS_GetOpaque: TJS_GetOpaque = nil;
  JS_GetOpaque2: TJS_GetOpaque2 = nil;
  JS_NewArrayBuffer: TJS_NewArrayBuffer = nil;
  JS_NewArrayBufferCopy: TJS_NewArrayBufferCopy = nil;
  JS_DetachArrayBuffer: TJS_DetachArrayBuffer = nil;
  JS_GetArrayBuffer: TJS_GetArrayBuffer = nil;
  JS_GetTypedArrayBuffer: TJS_GetTypedArrayBuffer = nil;
  JS_NewPromiseCapability: TJS_NewPromiseCapability = nil;
  JS_PromiseState: TJS_PromiseState = nil;
  JS_PromiseResult: TJS_PromiseResult = nil;
  JS_IsPromise: TJS_IsPromise = nil;
  JS_SetHostPromiseRejectionTracker: TJS_SetHostPromiseRejectionTracker = nil;
  JS_SetInterruptHandler: TJS_SetInterruptHandler = nil;
  JS_SetCanBlock: TJS_SetCanBlock = nil;
  JS_SetModuleLoaderFunc: TJS_SetModuleLoaderFunc = nil;
  JS_EnqueueJob: TJS_EnqueueJob = nil;
  JS_IsJobPending: TJS_IsJobPending = nil;
  JS_ExecutePendingJob: TJS_ExecutePendingJob = nil;
  JS_WriteObject: TJS_WriteObject = nil;
  JS_ReadObject: TJS_ReadObject = nil;
  JS_ResolveModule: TJS_ResolveModule = nil;
  JS_GetModuleNamespace: TJS_GetModuleNamespace = nil;
  JS_SetConstructor: TJS_SetConstructor = nil;
  JS_NewCFunction2: TJS_NewCFunction2 = nil;
  JS_NewCFunctionData: TJS_NewCFunctionData = nil;
  JS_SetPropertyFunctionList: TJS_SetPropertyFunctionList = nil;
  JS_NewCModule: TJS_NewCModule = nil;
  JS_AddModuleExport: TJS_AddModuleExport = nil;
  JS_AddModuleExportList: TJS_AddModuleExportList = nil;
  JS_SetModuleExport: TJS_SetModuleExport = nil;
  JS_SetModuleExportList: TJS_SetModuleExportList = nil;
  JS_GetImportMeta: TJS_GetImportMeta = nil;
  JS_GetModuleName: TJS_GetModuleName = nil;
  js_init_module_std: Tjs_init_module_std = nil;
  js_init_module_os: Tjs_init_module_os = nil;
  js_init_module_bjson: Tjs_init_module_bjson = nil;
  js_std_add_helpers: Tjs_std_add_helpers = nil;
  js_std_loop: Tjs_std_loop = nil;
  js_std_init_handlers: Tjs_std_init_handlers = nil;
  js_std_free_handlers: Tjs_std_free_handlers = nil;
  js_std_dump_error: Tjs_std_dump_error = nil;
  js_std_await: Tjs_std_await = nil;
  js_load_file: Tjs_load_file = nil;
  js_module_loader: Tjs_module_loader = nil;
  js_std_eval_binary: Tjs_std_eval_binary = nil;
  js_module_set_import_meta: Tjs_module_set_import_meta = nil;
  js_std_promise_rejection_tracker: Tjs_std_promise_rejection_tracker = nil;
  JS_NewInt64: TJS_NewInt64 = nil;
  JS_FreeValue: TJS_FreeValue = nil;
  JS_FreeValueRT: TJS_FreeValueRT = nil;
  JS_DupValue: TJS_DupValue = nil;
  JS_DupValueRT: TJS_DupValueRT = nil;

{ Dynamic Loading Functions }
function LoadQuickJS(const DLLPath: string = ''): Boolean;
function UnloadQuickJS: Boolean;
function IsQuickJSLoaded: Boolean;

{ Note: All QuickJS external APIs are now loaded dynamically through function pointers }


{ internal implementations}

function JS_VALUE_GET_TAG(v : JSValue): Int64;
{ same as JS_VALUE_GET_TAG, but return JS_TAG_FLOAT64 with NaN boxing }
function JS_VALUE_GET_NORM_TAG(v : JSValue): Int64;
function JS_VALUE_IS_NAN(v : JSValue) : JS_BOOL; inline;
function JS_VALUE_GET_INT(v : JSValue): Integer;
function JS_VALUE_GET_BOOL(v : JSValue): Boolean;
function JS_VALUE_GET_FLOAT64(v : JSValue): Double;
function JS_VALUE_GET_PTR(v : JSValue): Pointer;
function JS_MKVAL(tag : Int64; val : Int32): JSValue;
function JS_MKPTR(tag : Int64; ptr : Pointer): JSValue;
function JS_TAG_IS_FLOAT64(tag : Int64): Boolean; inline;
{$IfNDef JS_NAN_BOXING}
function JS_NAN : JSValue;
{$EndIf}
function __JS_NewFloat64({%H-}ctx : JSContext; d : Double): JSValue;

function JS_VALUE_IS_BOTH_INT(v1, v2 : JSValue): Boolean;
function JS_VALUE_IS_BOTH_FLOAT(v1, v2 : JSValue): Boolean;
function JS_VALUE_GET_OBJ(v : JSValue): JSObject;
function JS_VALUE_GET_STRING(v : JSValue): JSString;
function JS_VALUE_HAS_REF_COUNT(v : JSValue): Boolean;

{ special values }

function JS_NULL : JSValue;
function JS_UNDEFINED : JSValue;
function JS_FALSE : JSValue;
function JS_TRUE : JSValue;
function JS_EXCEPTION : JSValue;
function JS_UNINITIALIZED : JSValue;

{ value handling }

function JS_NewBool({%H-}ctx : JSContext; val : JS_BOOL): JSValue; inline;
function JS_NewInt32( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
function JS_NewCatchOffset( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
function JS_NewFloat64(ctx : JSContext; d : Double): JSValue;
function JS_IsBigInt(v : JSValueConst): JS_BOOL; inline;
function JS_IsBool(v : JSValueConst): JS_BOOL; inline;
function JS_IsNull(v : JSValueConst): JS_BOOL; inline;
function JS_IsUndefined(v : JSValueConst): JS_BOOL; inline;
function JS_IsException(v : JSValueConst): JS_BOOL; inline;
function JS_IsUninitialized(v : JSValueConst): JS_BOOL; inline;
function JS_IsString(v : JSValueConst): JS_BOOL; inline;
function JS_IsNumber(v : JSValueConst): JS_BOOL; inline;
function JS_IsSymbol(v : JSValueConst): JS_BOOL; inline;
function JS_IsObject(v : JSValueConst): JS_BOOL; inline;


function JS_ToUint32(ctx : JSContext; pres : pUInt32; val : JSValueConst): Integer; inline;
function JS_ToCStringLen(ctx : JSContext; plen : psize_t; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
function JS_ToCString(ctx : JSContext; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
function JS_GetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom): JSValue; inline;
function JS_SetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom; val : JSValue): Integer; inline;

{ C function definition }

function JS_NewCFunction(ctx : JSContext; func : PJSCFunction; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer): JSValue; inline;
function JS_NewCFunctionMagic(ctx : JSContext; func : PJSCFunctionMagic; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum; magic : Integer): JSValue; inline;


{ C property definition }

function JS_CFUNC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; func : JSCFunction) : JSCFunctionListEntry;
function JS_CFUNC_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; func : JSCFunctionMagic; magic : Int16) : JSCFunctionListEntry;
function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; cproto : JSCFunctionEnum ; func : f_f_func) : JSCFunctionListEntry; overload;
function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; cproto : JSCFunctionEnum ; func : f_f_f_func) : JSCFunctionListEntry; overload;
function JS_ITERATOR_NEXT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; iterator_next : iterator_next_func; magic : Int16) : JSCFunctionListEntry;
function JS_CGETSET_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; fgetter : Getter_func; fsetter : Setter_func) : JSCFunctionListEntry;
function JS_CGETSET_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; fgetter_magic : getter_magic_func; fsetter_magic : setter_magic_func; magic : Int16) : JSCFunctionListEntry;
function JS_PROP_STRING_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_INT32_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : Int32; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_INT64_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : Int64; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_DOUBLE_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : Double; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_UNDEFINED_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_OBJECT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; tab : PJSCFunctionListEntry;  length : Integer; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_ALIAS_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}) : JSCFunctionListEntry;
function JS_ALIAS_BASE_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; base : Integer) : JSCFunctionListEntry;


var
  OldFPUMask : TFPUExceptionMask;

implementation

{===============================================================================}
{                        Dynamic Loading Implementation                          }
{===============================================================================}

function GetProcAddr(Handle: {$IFDEF FPC}TLibHandle{$ELSE}HMODULE{$ENDIF}; const ProcName: AnsiString): Pointer;
begin
  {$IFDEF FPC}
  Result := GetProcedureAddress(Handle, ProcName);
  {$ELSE}
  Result := GetProcAddress(Handle, PAnsiChar(ProcName));
  {$ENDIF}
end;

function IsQuickJSLoaded: Boolean;
begin
  Result := QuickJSLoaded;
end;

function LoadQuickJS(const DLLPath: string = ''): Boolean;
var
  libPath: string;
begin
  Result := False;
  
  if QuickJSLoaded then
  begin
    Result := True;
    Exit;
  end;

  if DLLPath = '' then
    libPath := QJSDLL
  else
    libPath := DLLPath;

  QuickJSHandle := LoadLibrary(PChar(libPath));

  if QuickJSHandle = 0 then
    Exit;

  // Load all function pointers
  @JS_NewRuntime := GetProcAddr(QuickJSHandle, 'JS_NewRuntime');
  @JS_SetRuntimeInfo := GetProcAddr(QuickJSHandle, 'JS_SetRuntimeInfo');
  @JS_SetMemoryLimit := GetProcAddr(QuickJSHandle, 'JS_SetMemoryLimit');
  @JS_SetGCThreshold := GetProcAddr(QuickJSHandle, 'JS_SetGCThreshold');
  @JS_SetMaxStackSize := GetProcAddr(QuickJSHandle, 'JS_SetMaxStackSize');
  @JS_NewRuntime2 := GetProcAddr(QuickJSHandle, 'JS_NewRuntime2');
  @JS_FreeRuntime := GetProcAddr(QuickJSHandle, 'JS_FreeRuntime');
  @JS_GetRuntimeOpaque := GetProcAddr(QuickJSHandle, 'JS_GetRuntimeOpaque');
  @JS_SetRuntimeOpaque := GetProcAddr(QuickJSHandle, 'JS_SetRuntimeOpaque');
  @JS_MarkValue := GetProcAddr(QuickJSHandle, 'JS_MarkValue');
  @JS_RunGC := GetProcAddr(QuickJSHandle, 'JS_RunGC');
  @JS_IsLiveObject := GetProcAddr(QuickJSHandle, 'JS_IsLiveObject');
  @JS_NewContext := GetProcAddr(QuickJSHandle, 'JS_NewContext');
  @JS_FreeContext := GetProcAddr(QuickJSHandle, 'JS_FreeContext');
  @JS_DupContext := GetProcAddr(QuickJSHandle, 'JS_DupContext');
  @JS_GetContextOpaque := GetProcAddr(QuickJSHandle, 'JS_GetContextOpaque');
  @JS_SetContextOpaque := GetProcAddr(QuickJSHandle, 'JS_SetContextOpaque');
  @JS_GetRuntime := GetProcAddr(QuickJSHandle, 'JS_GetRuntime');
  @JS_SetClassProto := GetProcAddr(QuickJSHandle, 'JS_SetClassProto');
  @JS_GetClassProto := GetProcAddr(QuickJSHandle, 'JS_GetClassProto');
  @JS_NewContextRaw := GetProcAddr(QuickJSHandle, 'JS_NewContextRaw');
  @JS_AddIntrinsicBaseObjects := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicBaseObjects');
  @JS_AddIntrinsicDate := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicDate');
  @JS_AddIntrinsicEval := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicEval');
  @JS_AddIntrinsicStringNormalize := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicStringNormalize');
  @JS_AddIntrinsicRegExpCompiler := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicRegExpCompiler');
  @JS_AddIntrinsicRegExp := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicRegExp');
  @JS_AddIntrinsicJSON := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicJSON');
  @JS_AddIntrinsicProxy := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicProxy');
  @JS_AddIntrinsicMapSet := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicMapSet');
  @JS_AddIntrinsicTypedArrays := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicTypedArrays');
  @JS_AddIntrinsicPromise := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicPromise');
  @JS_AddIntrinsicBigInt := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicBigInt');
  @JS_AddIntrinsicBigFloat := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicBigFloat');
  @JS_AddIntrinsicBigDecimal := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicBigDecimal');
  @JS_AddIntrinsicOperators := GetProcAddr(QuickJSHandle, 'JS_AddIntrinsicOperators');
  @JS_EnableBignumExt := GetProcAddr(QuickJSHandle, 'JS_EnableBignumExt');
  @js_string_codePointRange := GetProcAddr(QuickJSHandle, 'js_string_codePointRange');
  @js_malloc_rt := GetProcAddr(QuickJSHandle, 'js_malloc_rt');
  @js_free_rt := GetProcAddr(QuickJSHandle, 'js_free_rt');
  @js_realloc_rt := GetProcAddr(QuickJSHandle, 'js_realloc_rt');
  @js_malloc_usable_size_rt := GetProcAddr(QuickJSHandle, 'js_malloc_usable_size_rt');
  @js_mallocz_rt := GetProcAddr(QuickJSHandle, 'js_mallocz_rt');
  @js_malloc := GetProcAddr(QuickJSHandle, 'js_malloc');
  @js_free := GetProcAddr(QuickJSHandle, 'js_free');
  @js_realloc := GetProcAddr(QuickJSHandle, 'js_realloc');
  @js_malloc_usable_size := GetProcAddr(QuickJSHandle, 'js_malloc_usable_size');
  @js_realloc2 := GetProcAddr(QuickJSHandle, 'js_realloc2');
  @js_mallocz := GetProcAddr(QuickJSHandle, 'js_mallocz');
  @js_strdup := GetProcAddr(QuickJSHandle, 'js_strdup');
  @js_strndup := GetProcAddr(QuickJSHandle, 'js_strndup');
  @JS_ComputeMemoryUsage := GetProcAddr(QuickJSHandle, 'JS_ComputeMemoryUsage');
  @JS_DumpMemoryUsage := GetProcAddr(QuickJSHandle, 'JS_DumpMemoryUsage');
  @JS_NewAtomLen := GetProcAddr(QuickJSHandle, 'JS_NewAtomLen');
  @JS_NewAtom := GetProcAddr(QuickJSHandle, 'JS_NewAtom');
  @JS_NewAtomUInt32 := GetProcAddr(QuickJSHandle, 'JS_NewAtomUInt32');
  @JS_DupAtom := GetProcAddr(QuickJSHandle, 'JS_DupAtom');
  @JS_FreeAtom := GetProcAddr(QuickJSHandle, 'JS_FreeAtom');
  @JS_FreeAtomRT := GetProcAddr(QuickJSHandle, 'JS_FreeAtomRT');
  @JS_AtomToValue := GetProcAddr(QuickJSHandle, 'JS_AtomToValue');
  @JS_AtomToString := GetProcAddr(QuickJSHandle, 'JS_AtomToString');
  @JS_ValueToAtom := GetProcAddr(QuickJSHandle, 'JS_ValueToAtom');
  @JS_NewClassID := GetProcAddr(QuickJSHandle, 'JS_NewClassID');
  @JS_NewClass := GetProcAddr(QuickJSHandle, 'JS_NewClass');
  @JS_IsRegisteredClass := GetProcAddr(QuickJSHandle, 'JS_IsRegisteredClass');
  @JS_GetClassID := GetProcAddr(QuickJSHandle, 'JS_GetClassID');
  @JS_NewBigInt64 := GetProcAddr(QuickJSHandle, 'JS_NewBigInt64');
  @JS_NewBigUint64 := GetProcAddr(QuickJSHandle, 'JS_NewBigUint64');
  @JS_Throw := GetProcAddr(QuickJSHandle, 'JS_Throw');
  @JS_HasException := GetProcAddr(QuickJSHandle, 'JS_HasException');
  @JS_GetException := GetProcAddr(QuickJSHandle, 'JS_GetException');
  @JS_IsError := GetProcAddr(QuickJSHandle, 'JS_IsError');
  @JS_ResetUncatchableError := GetProcAddr(QuickJSHandle, 'JS_ResetUncatchableError');
  @JS_NewError := GetProcAddr(QuickJSHandle, 'JS_NewError');
  @JS_ThrowSyntaxError := GetProcAddr(QuickJSHandle, 'JS_ThrowSyntaxError');
  @JS_ThrowTypeError := GetProcAddr(QuickJSHandle, 'JS_ThrowTypeError');
  @JS_ThrowReferenceError := GetProcAddr(QuickJSHandle, 'JS_ThrowReferenceError');
  @JS_ThrowRangeError := GetProcAddr(QuickJSHandle, 'JS_ThrowRangeError');
  @JS_ThrowInternalError := GetProcAddr(QuickJSHandle, 'JS_ThrowInternalError');
  @JS_ThrowOutOfMemory := GetProcAddr(QuickJSHandle, 'JS_ThrowOutOfMemory');
  @__JS_FreeValue := GetProcAddr(QuickJSHandle, '__JS_FreeValue');
  @__JS_FreeValueRT := GetProcAddr(QuickJSHandle, '__JS_FreeValueRT');
  @JS_ToBool := GetProcAddr(QuickJSHandle, 'JS_ToBool');
  @JS_ToInt32 := GetProcAddr(QuickJSHandle, 'JS_ToInt32');
  @JS_ToInt64 := GetProcAddr(QuickJSHandle, 'JS_ToInt64');
  @JS_ToIndex := GetProcAddr(QuickJSHandle, 'JS_ToIndex');
  @JS_ToFloat64 := GetProcAddr(QuickJSHandle, 'JS_ToFloat64');
  @JS_ToBigInt64 := GetProcAddr(QuickJSHandle, 'JS_ToBigInt64');
  @JS_ToInt64Ext := GetProcAddr(QuickJSHandle, 'JS_ToInt64Ext');
  @JS_NewStringLen := GetProcAddr(QuickJSHandle, 'JS_NewStringLen');
  @JS_NewAtomString := GetProcAddr(QuickJSHandle, 'JS_NewAtomString');
  @JS_ToString := GetProcAddr(QuickJSHandle, 'JS_ToString');
  @JS_ToPropertyKey := GetProcAddr(QuickJSHandle, 'JS_ToPropertyKey');
  @JS_ToCStringLen2 := GetProcAddr(QuickJSHandle, 'JS_ToCStringLen2');
  @JS_FreeCString := GetProcAddr(QuickJSHandle, 'JS_FreeCString');
  @JS_NewObjectProtoClass := GetProcAddr(QuickJSHandle, 'JS_NewObjectProtoClass');
  @JS_NewObjectClass := GetProcAddr(QuickJSHandle, 'JS_NewObjectClass');
  @JS_NewObjectProto := GetProcAddr(QuickJSHandle, 'JS_NewObjectProto');
  @JS_NewObject := GetProcAddr(QuickJSHandle, 'JS_NewObject');
  @JS_IsFunction := GetProcAddr(QuickJSHandle, 'JS_IsFunction');
  @JS_IsConstructor := GetProcAddr(QuickJSHandle, 'JS_IsConstructor');
  @JS_SetConstructorBit := GetProcAddr(QuickJSHandle, 'JS_SetConstructorBit');
  @JS_NewArray := GetProcAddr(QuickJSHandle, 'JS_NewArray');
  @JS_IsArray := GetProcAddr(QuickJSHandle, 'JS_IsArray');
  @JS_GetPropertyInternal := GetProcAddr(QuickJSHandle, 'JS_GetPropertyInternal');
  @JS_GetPropertyStr := GetProcAddr(QuickJSHandle, 'JS_GetPropertyStr');
  @JS_GetPropertyUint32 := GetProcAddr(QuickJSHandle, 'JS_GetPropertyUint32');
  @JS_SetPropertyInternal := GetProcAddr(QuickJSHandle, 'JS_SetPropertyInternal');
  @JS_SetPropertyUint32 := GetProcAddr(QuickJSHandle, 'JS_SetPropertyUint32');
  @JS_SetPropertyInt64 := GetProcAddr(QuickJSHandle, 'JS_SetPropertyInt64');
  @JS_SetPropertyStr := GetProcAddr(QuickJSHandle, 'JS_SetPropertyStr');
  @JS_HasProperty := GetProcAddr(QuickJSHandle, 'JS_HasProperty');
  @JS_IsExtensible := GetProcAddr(QuickJSHandle, 'JS_IsExtensible');
  @JS_PreventExtensions := GetProcAddr(QuickJSHandle, 'JS_PreventExtensions');
  @JS_DeleteProperty := GetProcAddr(QuickJSHandle, 'JS_DeleteProperty');
  @JS_SetPrototype := GetProcAddr(QuickJSHandle, 'JS_SetPrototype');
  @JS_GetPrototype := GetProcAddr(QuickJSHandle, 'JS_GetPrototype');
  @JS_GetOwnPropertyNames := GetProcAddr(QuickJSHandle, 'JS_GetOwnPropertyNames');
  @JS_GetOwnProperty := GetProcAddr(QuickJSHandle, 'JS_GetOwnProperty');
  @JS_ParseJSON := GetProcAddr(QuickJSHandle, 'JS_ParseJSON');
  @JS_JSONStringify := GetProcAddr(QuickJSHandle, 'JS_JSONStringify');
  @JS_Call := GetProcAddr(QuickJSHandle, 'JS_Call');
  @JS_Invoke := GetProcAddr(QuickJSHandle, 'JS_Invoke');
  @JS_CallConstructor := GetProcAddr(QuickJSHandle, 'JS_CallConstructor');
  @JS_CallConstructor2 := GetProcAddr(QuickJSHandle, 'JS_CallConstructor2');
  @JS_DetectModule := GetProcAddr(QuickJSHandle, 'JS_DetectModule');
  @JS_Eval := GetProcAddr(QuickJSHandle, 'JS_Eval');
  @JS_EvalFunction := GetProcAddr(QuickJSHandle, 'JS_EvalFunction');
  @JS_GetGlobalObject := GetProcAddr(QuickJSHandle, 'JS_GetGlobalObject');
  @JS_IsInstanceOf := GetProcAddr(QuickJSHandle, 'JS_IsInstanceOf');
  @JS_DefineProperty := GetProcAddr(QuickJSHandle, 'JS_DefineProperty');
  @JS_DefinePropertyValue := GetProcAddr(QuickJSHandle, 'JS_DefinePropertyValue');
  @JS_DefinePropertyValueUint32 := GetProcAddr(QuickJSHandle, 'JS_DefinePropertyValueUint32');
  @JS_DefinePropertyValueStr := GetProcAddr(QuickJSHandle, 'JS_DefinePropertyValueStr');
  @JS_DefinePropertyGetSet := GetProcAddr(QuickJSHandle, 'JS_DefinePropertyGetSet');
  @JS_SetOpaque := GetProcAddr(QuickJSHandle, 'JS_SetOpaque');
  @JS_GetOpaque := GetProcAddr(QuickJSHandle, 'JS_GetOpaque');
  @JS_GetOpaque2 := GetProcAddr(QuickJSHandle, 'JS_GetOpaque2');
  @JS_NewArrayBuffer := GetProcAddr(QuickJSHandle, 'JS_NewArrayBuffer');
  @JS_NewArrayBufferCopy := GetProcAddr(QuickJSHandle, 'JS_NewArrayBufferCopy');
  @JS_DetachArrayBuffer := GetProcAddr(QuickJSHandle, 'JS_DetachArrayBuffer');
  @JS_GetArrayBuffer := GetProcAddr(QuickJSHandle, 'JS_GetArrayBuffer');
  @JS_GetTypedArrayBuffer := GetProcAddr(QuickJSHandle, 'JS_GetTypedArrayBuffer');
  @JS_NewPromiseCapability := GetProcAddr(QuickJSHandle, 'JS_NewPromiseCapability');
  @JS_PromiseState := GetProcAddr(QuickJSHandle, 'JS_PromiseState');
  @JS_PromiseResult := GetProcAddr(QuickJSHandle, 'JS_PromiseResult');
  @JS_IsPromise := GetProcAddr(QuickJSHandle, 'JS_IsPromise');
  @JS_SetHostPromiseRejectionTracker := GetProcAddr(QuickJSHandle, 'JS_SetHostPromiseRejectionTracker');
  @JS_SetInterruptHandler := GetProcAddr(QuickJSHandle, 'JS_SetInterruptHandler');
  @JS_SetCanBlock := GetProcAddr(QuickJSHandle, 'JS_SetCanBlock');
  @JS_SetModuleLoaderFunc := GetProcAddr(QuickJSHandle, 'JS_SetModuleLoaderFunc');
  @JS_EnqueueJob := GetProcAddr(QuickJSHandle, 'JS_EnqueueJob');
  @JS_IsJobPending := GetProcAddr(QuickJSHandle, 'JS_IsJobPending');
  @JS_ExecutePendingJob := GetProcAddr(QuickJSHandle, 'JS_ExecutePendingJob');
  @JS_WriteObject := GetProcAddr(QuickJSHandle, 'JS_WriteObject');
  @JS_ReadObject := GetProcAddr(QuickJSHandle, 'JS_ReadObject');
  @JS_ResolveModule := GetProcAddr(QuickJSHandle, 'JS_ResolveModule');
  @JS_GetModuleNamespace := GetProcAddr(QuickJSHandle, 'JS_GetModuleNamespace');
  @JS_SetConstructor := GetProcAddr(QuickJSHandle, 'JS_SetConstructor');
  @JS_NewCFunction2 := GetProcAddr(QuickJSHandle, 'JS_NewCFunction2');
  @JS_NewCFunctionData := GetProcAddr(QuickJSHandle, 'JS_NewCFunctionData');
  @JS_SetPropertyFunctionList := GetProcAddr(QuickJSHandle, 'JS_SetPropertyFunctionList');
  @JS_NewCModule := GetProcAddr(QuickJSHandle, 'JS_NewCModule');
  @JS_AddModuleExport := GetProcAddr(QuickJSHandle, 'JS_AddModuleExport');
  @JS_AddModuleExportList := GetProcAddr(QuickJSHandle, 'JS_AddModuleExportList');
  @JS_SetModuleExport := GetProcAddr(QuickJSHandle, 'JS_SetModuleExport');
  @JS_SetModuleExportList := GetProcAddr(QuickJSHandle, 'JS_SetModuleExportList');
  @JS_GetImportMeta := GetProcAddr(QuickJSHandle, 'JS_GetImportMeta');
  @JS_GetModuleName := GetProcAddr(QuickJSHandle, 'JS_GetModuleName');
  @js_init_module_std := GetProcAddr(QuickJSHandle, 'js_init_module_std');
  @js_init_module_os := GetProcAddr(QuickJSHandle, 'js_init_module_os');
  @js_init_module_bjson := GetProcAddr(QuickJSHandle, 'js_init_module_bjson');
  @js_std_add_helpers := GetProcAddr(QuickJSHandle, 'js_std_add_helpers');
  @js_std_loop := GetProcAddr(QuickJSHandle, 'js_std_loop');
  @js_std_init_handlers := GetProcAddr(QuickJSHandle, 'js_std_init_handlers');
  @js_std_free_handlers := GetProcAddr(QuickJSHandle, 'js_std_free_handlers');
  @js_std_dump_error := GetProcAddr(QuickJSHandle, 'js_std_dump_error');
  @js_std_await := GetProcAddr(QuickJSHandle, 'js_std_await');
  @js_load_file := GetProcAddr(QuickJSHandle, 'js_load_file');
  @js_module_loader := GetProcAddr(QuickJSHandle, 'js_module_loader');
  @js_std_eval_binary := GetProcAddr(QuickJSHandle, 'js_std_eval_binary');
  @js_module_set_import_meta := GetProcAddr(QuickJSHandle, 'js_module_set_import_meta');
  @js_std_promise_rejection_tracker := GetProcAddr(QuickJSHandle, 'js_std_promise_rejection_tracker');
  @JS_NewInt64 := GetProcAddr(QuickJSHandle, 'JS_NewInt64');
  @JS_FreeValue := GetProcAddr(QuickJSHandle, 'JS_FreeValue');
  @JS_FreeValueRT := GetProcAddr(QuickJSHandle, 'JS_FreeValueRT');
  @JS_DupValue := GetProcAddr(QuickJSHandle, 'JS_DupValue');
  @JS_DupValueRT := GetProcAddr(QuickJSHandle, 'JS_DupValueRT');

  QuickJSLoaded := True;
  Result := True;
end;

function UnloadQuickJS: Boolean;
begin
  Result := False;
  
  if not QuickJSLoaded then
  begin
    Result := True;
    Exit;
  end;

  if QuickJSHandle <> 0 then
  begin
    {$IFDEF FPC}
    FreeLibrary(QuickJSHandle);
    {$ELSE}
    FreeLibrary(QuickJSHandle);
    {$ENDIF}
    QuickJSHandle := 0;
  end;

  // Clear all function pointers
  JS_NewRuntime := nil;
  JS_SetRuntimeInfo := nil;
  JS_SetMemoryLimit := nil;
  JS_SetGCThreshold := nil;
  JS_SetMaxStackSize := nil;
  JS_NewRuntime2 := nil;
  JS_FreeRuntime := nil;
  JS_GetRuntimeOpaque := nil;
  JS_SetRuntimeOpaque := nil;
  JS_MarkValue := nil;
  JS_RunGC := nil;
  JS_IsLiveObject := nil;
  JS_NewContext := nil;
  JS_FreeContext := nil;
  JS_DupContext := nil;
  JS_GetContextOpaque := nil;
  JS_SetContextOpaque := nil;
  JS_GetRuntime := nil;
  JS_SetClassProto := nil;
  JS_GetClassProto := nil;
  JS_NewContextRaw := nil;
  JS_AddIntrinsicBaseObjects := nil;
  JS_AddIntrinsicDate := nil;
  JS_AddIntrinsicEval := nil;
  JS_AddIntrinsicStringNormalize := nil;
  JS_AddIntrinsicRegExpCompiler := nil;
  JS_AddIntrinsicRegExp := nil;
  JS_AddIntrinsicJSON := nil;
  JS_AddIntrinsicProxy := nil;
  JS_AddIntrinsicMapSet := nil;
  JS_AddIntrinsicTypedArrays := nil;
  JS_AddIntrinsicPromise := nil;
  JS_AddIntrinsicBigInt := nil;
  JS_AddIntrinsicBigFloat := nil;
  JS_AddIntrinsicBigDecimal := nil;
  JS_AddIntrinsicOperators := nil;
  JS_EnableBignumExt := nil;
  js_string_codePointRange := nil;
  js_malloc_rt := nil;
  js_free_rt := nil;
  js_realloc_rt := nil;
  js_malloc_usable_size_rt := nil;
  js_mallocz_rt := nil;
  js_malloc := nil;
  js_free := nil;
  js_realloc := nil;
  js_malloc_usable_size := nil;
  js_realloc2 := nil;
  js_mallocz := nil;
  js_strdup := nil;
  js_strndup := nil;
  JS_ComputeMemoryUsage := nil;
  JS_DumpMemoryUsage := nil;
  JS_NewAtomLen := nil;
  JS_NewAtom := nil;
  JS_NewAtomUInt32 := nil;
  JS_DupAtom := nil;
  JS_FreeAtom := nil;
  JS_FreeAtomRT := nil;
  JS_AtomToValue := nil;
  JS_AtomToString := nil;
  JS_ValueToAtom := nil;
  JS_NewClassID := nil;
  JS_NewClass := nil;
  JS_IsRegisteredClass := nil;
  JS_GetClassID := nil;
  JS_NewBigInt64 := nil;
  JS_NewBigUint64 := nil;
  JS_Throw := nil;
  JS_HasException := nil;
  JS_GetException := nil;
  JS_IsError := nil;
  JS_ResetUncatchableError := nil;
  JS_NewError := nil;
  JS_ThrowSyntaxError := nil;
  JS_ThrowTypeError := nil;
  JS_ThrowReferenceError := nil;
  JS_ThrowRangeError := nil;
  JS_ThrowInternalError := nil;
  JS_ThrowOutOfMemory := nil;
  __JS_FreeValue := nil;
  __JS_FreeValueRT := nil;
  JS_ToBool := nil;
  JS_ToInt32 := nil;
  JS_ToInt64 := nil;
  JS_ToIndex := nil;
  JS_ToFloat64 := nil;
  JS_ToBigInt64 := nil;
  JS_ToInt64Ext := nil;
  JS_NewStringLen := nil;
  JS_NewAtomString := nil;
  JS_ToString := nil;
  JS_ToPropertyKey := nil;
  JS_ToCStringLen2 := nil;
  JS_FreeCString := nil;
  JS_NewObjectProtoClass := nil;
  JS_NewObjectClass := nil;
  JS_NewObjectProto := nil;
  JS_NewObject := nil;
  JS_IsFunction := nil;
  JS_IsConstructor := nil;
  JS_SetConstructorBit := nil;
  JS_NewArray := nil;
  JS_IsArray := nil;
  JS_GetPropertyInternal := nil;
  JS_GetPropertyStr := nil;
  JS_GetPropertyUint32 := nil;
  JS_SetPropertyInternal := nil;
  JS_SetPropertyUint32 := nil;
  JS_SetPropertyInt64 := nil;
  JS_SetPropertyStr := nil;
  JS_HasProperty := nil;
  JS_IsExtensible := nil;
  JS_PreventExtensions := nil;
  JS_DeleteProperty := nil;
  JS_SetPrototype := nil;
  JS_GetPrototype := nil;
  JS_GetOwnPropertyNames := nil;
  JS_GetOwnProperty := nil;
  JS_ParseJSON := nil;
  JS_JSONStringify := nil;
  JS_Call := nil;
  JS_Invoke := nil;
  JS_CallConstructor := nil;
  JS_CallConstructor2 := nil;
  JS_DetectModule := nil;
  JS_Eval := nil;
  JS_EvalFunction := nil;
  JS_GetGlobalObject := nil;
  JS_IsInstanceOf := nil;
  JS_DefineProperty := nil;
  JS_DefinePropertyValue := nil;
  JS_DefinePropertyValueUint32 := nil;
  JS_DefinePropertyValueStr := nil;
  JS_DefinePropertyGetSet := nil;
  JS_SetOpaque := nil;
  JS_GetOpaque := nil;
  JS_GetOpaque2 := nil;
  JS_NewArrayBuffer := nil;
  JS_NewArrayBufferCopy := nil;
  JS_DetachArrayBuffer := nil;
  JS_GetArrayBuffer := nil;
  JS_GetTypedArrayBuffer := nil;
  JS_NewPromiseCapability := nil;
  JS_PromiseState := nil;
  JS_PromiseResult := nil;
  JS_IsPromise := nil;
  JS_SetHostPromiseRejectionTracker := nil;
  JS_SetInterruptHandler := nil;
  JS_SetCanBlock := nil;
  JS_SetModuleLoaderFunc := nil;
  JS_EnqueueJob := nil;
  JS_IsJobPending := nil;
  JS_ExecutePendingJob := nil;
  JS_WriteObject := nil;
  JS_ReadObject := nil;
  JS_ResolveModule := nil;
  JS_GetModuleNamespace := nil;
  JS_SetConstructor := nil;
  JS_NewCFunction2 := nil;
  JS_NewCFunctionData := nil;
  JS_SetPropertyFunctionList := nil;
  JS_NewCModule := nil;
  JS_AddModuleExport := nil;
  JS_AddModuleExportList := nil;
  JS_SetModuleExport := nil;
  JS_SetModuleExportList := nil;
  JS_GetImportMeta := nil;
  JS_GetModuleName := nil;
  js_init_module_std := nil;
  js_init_module_os := nil;
  js_init_module_bjson := nil;
  js_std_add_helpers := nil;
  js_std_loop := nil;
  js_std_init_handlers := nil;
  js_std_free_handlers := nil;
  js_std_dump_error := nil;
  js_std_await := nil;
  js_load_file := nil;
  js_module_loader := nil;
  js_std_eval_binary := nil;
  js_module_set_import_meta := nil;
  js_std_promise_rejection_tracker := nil;
  JS_NewInt64 := nil;
  JS_FreeValue := nil;
  JS_FreeValueRT := nil;
  JS_DupValue := nil;
  JS_DupValueRT := nil;

  QuickJSLoaded := False;
  Result := True;
end;

{$If Defined(JS_NAN_BOXING)}

function JS_VALUE_GET_TAG(v : JSValue): Int64;
begin
  Result := Integer(v shr 32);
end;

function JS_VALUE_GET_INT(v : JSValue): Integer;
begin
  Result := Integer(v);
end;

function JS_VALUE_GET_BOOL(v : JSValue): Boolean;
begin
  Result := Boolean(v);
end;

function JS_VALUE_GET_PTR(v : JSValue): Pointer;
begin
  Result := {%H-}Pointer(v); // TODO: check if this works the right way.
end;

function JS_MKVAL(tag : Int64; val : Int32): JSValue;
begin
  Result := tag shl 32 or val;
end;

function JS_MKPTR(tag : Int64; ptr : Pointer): JSValue;
begin
  Result := JSValue((tag shl 32) or UIntPtr(ptr));
end;

function JS_VALUE_GET_FLOAT64(v : JSValue): Double;
type
  rec = record
    case Byte of
      0 : (v : JSValue);
      1 : (d : Double);
  end;
var
  u : rec;
begin
  u.v := v;
  u.v {$IfDef FPC}+={$Else} := u.v +{$EndIf} UInt64(JS_FLOAT64_TAG_ADDEND shl 32);
  Result := u.d;
end;

function __JS_NewFloat64({%H-}ctx : JSContext; d : Double): JSValue;
type
  rec = record
    case Byte of
      0 : (d : Double);
      1 : (u64 : UInt64);
  end;
var
  u : rec;
  v : JSValue;
begin
  u.d := d;
  { normalize NaN }
  if ((u.u64 and $7fffffffffffffff) > $7ff0000000000000) then
    v := UInt64(JS_NAN)
  else
    v := u.u64 - UInt64(JS_FLOAT64_TAG_ADDEND shl 32);
  Result := v;
end;

function JS_TAG_IS_FLOAT64(tag : Int64): Boolean; inline;
begin
  Result := Boolean( UInt64((tag) - JS_TAG_FIRST) >= (JS_TAG_FLOAT64 - JS_TAG_FIRST) );
end;

{ same as JS_VALUE_GET_TAG, but return JS_TAG_FLOAT64 with NaN boxing }
function JS_VALUE_GET_NORM_TAG(v : JSValue): Int64;
var
  tag : UInt32;
begin
  tag := JS_VALUE_GET_TAG(v);
  if JS_TAG_IS_FLOAT64(tag) then
      Result := JS_TAG_FLOAT64
  else
      Result := tag;
end;

function JS_VALUE_IS_NAN(v : JSValue) : JS_BOOL; inline;
begin
  Result := (JS_VALUE_GET_TAG(v) = (JS_NAN shr 32));
end;

{$else}

function JS_VALUE_GET_TAG(v : JSValue): Int64;
begin
  Result := v.tag;
end;
{ same as JS_VALUE_GET_TAG, but return JS_TAG_FLOAT64 with NaN boxing }
function JS_VALUE_GET_NORM_TAG(v : JSValue): Int64;
begin
  Result := JS_VALUE_GET_TAG(v);
end;

function JS_VALUE_GET_INT(v : JSValue): Integer;
begin
  Result := v.u.&int32;
end;

function JS_VALUE_GET_BOOL(v : JSValue): Boolean;
begin
  Result := Boolean(v.u.&int32);
end;

function JS_VALUE_GET_FLOAT64(v : JSValue): Double;
begin
  Result := v.u.float64;
end;

function JS_VALUE_GET_PTR(v : JSValue): Pointer;
begin
  Result := v.u.Ptr;
end;

function JS_MKVAL(tag : Int64; val : Int32): JSValue;
begin
  Result.u.&int32 := val;
  Result.tag := tag;
end;

function JS_MKPTR(tag : Int64; ptr : Pointer): JSValue;
begin
  Result.u.Ptr := ptr;
  Result.tag := tag;
end;

function JS_TAG_IS_FLOAT64(tag : Int64): Boolean; inline;
begin
  Result := UInt64(tag) = JS_TAG_FLOAT64;
end;

function JS_NAN : JSValue;
begin
  Result.u.float64 := JS_FLOAT64_NAN;
  Result.tag := JS_TAG_FLOAT64;
end;

function __JS_NewFloat64({%H-}ctx : JSContext; d : Double): JSValue;
begin
  Result.u.float64 := d;
  Result.tag := JS_TAG_FLOAT64;
end;

function JS_VALUE_IS_NAN(v : JSValue) : JS_BOOL; inline;
type
  UnionRec = record
    case Byte of
      0 : (d : Double);
      1 : (u64 : UInt64);
  end;
var
  u : UnionRec;
begin
  if (v.tag <> JS_TAG_FLOAT64) then
    Exit(False);
  u.d := v.u.float64;
  Result := (u.u64 and $7fffffffffffffff) > $7ff0000000000000;
end;

{$ENDIF}

function JS_VALUE_IS_BOTH_INT(v1, v2 : JSValue): Boolean;
begin
  Result := ((JS_VALUE_GET_TAG(v1) or JS_VALUE_GET_TAG(v2)) = JS_TAG_INT);
end;

function JS_VALUE_IS_BOTH_FLOAT(v1, v2 : JSValue): Boolean;
begin
  Result := (JS_TAG_IS_FLOAT64(JS_VALUE_GET_TAG(v1)) and JS_TAG_IS_FLOAT64(JS_VALUE_GET_TAG(v2)))
end;

function JS_VALUE_GET_OBJ(v : JSValue): JSObject;
begin
  Result := JS_VALUE_GET_PTR(v);
end;

function JS_VALUE_GET_STRING(v : JSValue): JSString;
begin
  Result := JS_VALUE_GET_PTR(v);
end;

function JS_VALUE_HAS_REF_COUNT(v : JSValue): Boolean;
begin
  Result := UInt64(JS_VALUE_GET_TAG(v)) >= UInt64(JS_TAG_FIRST);
end;

{ special values }

function JS_NULL : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_NULL, 0);
end;

function JS_UNDEFINED : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_UNDEFINED, 0);
end;

function JS_FALSE : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_BOOL, 0);
end;

function JS_TRUE : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_BOOL, 1);
end;

function JS_EXCEPTION : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_EXCEPTION, 0);
end;

function JS_UNINITIALIZED : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_UNINITIALIZED, 0);
end;

{ value handling }

function JS_NewBool({%H-}ctx : JSContext; val : JS_BOOL): JSValue;
begin
  Result := JS_MKVAL(JS_TAG_BOOL, Int32(val));
end;

function JS_NewInt32( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
begin
  Result := JS_MKVAL(JS_TAG_INT, val);
end;

function JS_NewCatchOffset( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
begin
  Result := JS_MKVAL(JS_TAG_CATCH_OFFSET, val);
end;

function JS_NewUint32(ctx : JSContext; val : UInt32): JSValue;
begin
  if val <= $7fffffff then
    Result := JS_NewInt32(ctx, val)
  else
    Result := __JS_NewFloat64(ctx, val);
end;

function JS_NewFloat64(ctx : JSContext; d : Double): JSValue;
type
  rec = record
    case Byte of
      0 : (d : Double);
      1 : (u : UInt64);
  end;
var
  u,t : rec;
  val : Int32;
begin
  u.d := d;
  val := Int32(Round(d));
  t.d := val;
  { -0 cannot be represented as integer, so we compare the bit representation }
  if u.u = t.u then
    Result := JS_MKVAL(JS_TAG_INT, val)
  else
    Result := __JS_NewFloat64(ctx, d);
end;

function JS_IsBigInt(v : JSValueConst): Boolean; inline;
begin
  // Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_BIG_INT);
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_SHORT_BIG_INT);
end;

function JS_IsBool(v : JSValueConst): JS_BOOL; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_BOOL);
end;

function JS_IsNull(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_NULL);
end;

function JS_IsUndefined(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_UNDEFINED);
end;

function JS_IsException(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_EXCEPTION);
end;

function JS_IsUninitialized(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_UNINITIALIZED);
end;

function JS_IsString(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_STRING);
end;

function JS_IsNumber(v: JSValueConst): JS_BOOL; inline;
var
  tag : Integer;
begin
  tag := JS_VALUE_GET_TAG(v);
  Result := (tag = JS_TAG_INT) or JS_TAG_IS_FLOAT64(tag);
end;

function JS_IsSymbol(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_SYMBOL);
end;

function JS_IsObject(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_OBJECT);
end;

function JS_ToUint32(ctx : JSContext; pres : pUInt32; val : JSValueConst): Integer; inline;
begin
  Result := JS_ToInt32(ctx, pInt32(pres), val);
end;

function JS_ToCStringLen(ctx : JSContext; plen : psize_t; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
begin
  Result := JS_ToCStringLen2(ctx, plen, val, False);
end;

function JS_ToCString(ctx : JSContext; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
begin
  Result := JS_ToCStringLen2(ctx, nil, val, False);
end;

function JS_GetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom): JSValue; inline;
begin
  Result := JS_GetPropertyInternal(ctx, this_obj, prop, this_obj, False);
end;

function JS_SetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom; val : JSValue): Integer; inline;
begin
  Result := JS_SetPropertyInternal(ctx, this_obj, prop, val, JS_PROP_THROW);
end;

{ C function definition }

function JS_NewCFunction(ctx : JSContext; func : PJSCFunction; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer): JSValue; inline;
begin
  Result := JS_NewCFunction2(ctx, func, name, length, JS_CFUNC_generic, 0);
end;

function JS_NewCFunctionMagic(ctx : JSContext; func : PJSCFunctionMagic; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum; magic : Integer): JSValue; inline;
begin
  Result := JS_NewCFunction2(ctx, PJSCFunction(func), name, length, cproto, magic);;
end;

{ C property definition }

function JS_CFUNC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           func : JSCFunction) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := 0;
  Result.u.func.length := length;
  Result.u.func.cproto := JS_CFUNC_generic;
  Result.u.func.cfunc.generic := func;
end;

function JS_CFUNC_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           func : JSCFunctionMagic; magic : Int16) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := magic;
  Result.u.func.length := length;
  Result.u.func.cproto := JS_CFUNC_generic_magic;
  Result.u.func.cfunc.generic_magic := func;
end;

function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum ; func : f_f_func) : JSCFunctionListEntry; overload;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := 0;
  Result.u.func.length := length;
  Result.u.func.cproto := cproto;
  Result.u.func.cfunc.f_f := func;
end;

function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum ; func : f_f_f_func) : JSCFunctionListEntry; overload;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := 0;
  Result.u.func.length := length;
  Result.u.func.cproto := cproto;
  Result.u.func.cfunc.f_f_f := func;
end;

function JS_ITERATOR_NEXT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           iterator_next : iterator_next_func; magic : Int16) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := magic;
  Result.u.func.length := length;
  Result.u.func.cproto := JS_CFUNC_iterator_next;
  Result.u.func.cfunc.iterator_next := iterator_next;
end;

function JS_CGETSET_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           fgetter : Getter_func; fsetter : Setter_func ) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CGETSET;
  Result.magic := 0;
  Result.u.getset.get.getter  := fgetter;
  Result.u.getset._set.setter := fsetter;
end;

function JS_CGETSET_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           fgetter_magic : getter_magic_func;
           fsetter_magic : setter_magic_func; magic : Int16) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CGETSET_MAGIC;
  Result.magic := magic;
  Result.u.getset.get.getter_magic := fgetter_magic;
  Result.u.getset._set.setter_magic := fsetter_magic;
end;

function JS_PROP_STRING_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_STRING;
  Result.magic := 0;
  Result.u.str := val;
end;

function JS_PROP_INT32_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : Int32; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_INT32;
  Result.magic := 0;
  Result.u.i32 := val;
end;

function JS_PROP_INT64_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : Int64; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_INT64;
  Result.magic := 0;
  Result.u.i64 := val;
end;

function JS_PROP_DOUBLE_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : Double; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_DOUBLE;
  Result.magic := 0;
  Result.u.f64 := val;
end;

function JS_PROP_UNDEFINED_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_UNDEFINED;
  Result.magic := 0;
  Result.u.i32 := 0;
end;

function JS_OBJECT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; tab : PJSCFunctionListEntry;
           length : Integer; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_OBJECT;
  Result.magic := 0;
  Result.u.prop_list.tab := {$IfDef FPC}tab{$Else}Pointer(tab){$EndIf};
  Result.u.prop_list.len := length;
end;

function JS_ALIAS_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_ALIAS;
  Result.magic := 0;
  Result.u.alias.name := from;
  Result.u.alias.base := -1;
end;

function JS_ALIAS_BASE_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           base : Integer) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_ALIAS;
  Result.magic := 0;
  Result.u.alias.name := from;
  Result.u.alias.base := base;
end;

{ bignum stuff :D }

function c_udivti3(num,den:uint64):uint64; cdecl; // public alias: {$ifdef darwin}'___udivti3'{$else} '__udivdi3'{$endif};
begin
 result:=num div den;
end;


initialization
  // fix the Invalid floating point operation .
  OldFPUMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

finalization
   SetExceptionMask(OldFPUMask);

end.


