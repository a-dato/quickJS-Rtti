unit App.Frames.FMX.impl;

interface

uses
  System_,
  System.SysUtils,
  App.Component.intf,
  App.Windows.intf,
  System.Collections.Generic
  {$IFDEF FRAMEWORK_VCL}
  , Vcl.Forms
  {$ELSE}
  , FMX.Forms
  {$ENDIF}
  ;

type
  TFMXWindowFrame = class(TFrame, IWindowFrame, IComponent)
  protected
    // IBaseInterface
    function AsType(const Value: &Type) : CObject; virtual;
    function TryAsType(const Value: &Type) : CObject; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function  getRefCount: Integer;
    function  GetObject: TObject; virtual;
    function  GetType: &Type; virtual;
    procedure Dispose; virtual;
    function  ToString: CString; reintroduce; overload; virtual;
    function  Equals(const other: CObject): Boolean; virtual;

    // IWindowFrame
    function  WaitFor(CompletionProc: TProc = nil) : Boolean;
    function  get_Content: TObject;
    procedure set_Content(const Value: TObject);

    // IComponent
    function  get_Parent: IComponent;
    function  get_Children: List<IComponent>;
    function  get_Name: CString;
    procedure set_Name(const Value: CString);
  end;

implementation

{$R *.fmx}

uses
  {$IFDEF FRAMEWORK_VCL}
  Vcl.Controls, Vcl.Forms
  {$ELSE}
  FMX.Controls, FMX.Types, App.Component.FMX.impl
  {$ENDIF}
  , App.Environment.impl, System.Rtti;

// Wraps each control directly contained by Container in a TFmxComponent
function GetChildComponents(const Parent: IComponent; Container: TObject) : List<IComponent>;
begin
  {$IFNDEF FRAMEWORK_VCL}
  if Container is TFmxObject then
    Exit(TFmxComponent.CreateChildren(Parent, Container as TFmxObject));
  {$ENDIF}
  Result := CList<IComponent>.Create;
end;

{ TFMXWindowFrame }

function TFMXWindowFrame.AsType(const Value: &Type): CObject;
begin
  Result := TryAsType(Value);
  if Result = nil then
    raise InvalidCastException.Create('cast to: ''' +  Value.Name + ''' failed');
end;

procedure TFMXWindowFrame.Dispose;
begin

end;

function TFMXWindowFrame.Equals(const other: CObject): Boolean;
begin
  Result := (other <> nil) and (GetHashCode = other.GetHashCode);
end;

function TFMXWindowFrame.GetObject: TObject;
begin
  Result := Self;
end;

function TFMXWindowFrame.getRefCount: Integer;
begin
  // Lifetime is managed by the frame's owner, not by reference counting
  Result := -1;
end;

function TFMXWindowFrame.GetType: &Type;
begin
  Result := &Type.Create(Self.ClassInfo);
end;

function TFMXWindowFrame.get_Children: List<IComponent>;
begin
  Result := GetChildComponents(Self, Self);
end;

function TFMXWindowFrame.WaitFor(CompletionProc: TProc) : Boolean;
begin
  // Ready as soon as it is created
  if Assigned(CompletionProc) then
  begin
    CompletionProc();
    Result := False;
  end else
    Result := True;
end;

function TFMXWindowFrame.get_Content: TObject;
begin
  // The frame is its own content
  Result := Self;
end;

function TFMXWindowFrame.get_Name: CString;
begin
  Result := Name;
end;

function TFMXWindowFrame.get_Parent: IComponent;
begin
  if not Supports(Parent, IComponent, Result) then
    Result := nil;
end;

function TFMXWindowFrame.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

procedure TFMXWindowFrame.set_Content(const Value: TObject);
begin
  if Value <> Self then
    raise CException.Create('The content of a frame cannot be changed');
end;

procedure TFMXWindowFrame.set_Name(const Value: CString);
begin
  Name := Value.ToString;
end;

function TFMXWindowFrame.ToString: CString;
begin
  Result := ClassName;
end;

function TFMXWindowFrame.TryAsType(const Value: &Type): CObject;
begin
  Result := nil;

  var ii: IInterface;
  if Value.IsInterfaceType and (QueryInterface(Value.Guid, ii) = S_OK) then
  begin
    var v: TValue;
    TValue.Make(@ii, Value.GetTypeInfo, v);
    Result := v;
  end;
end;

function TFMXWindowFrame._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TFMXWindowFrame._Release: Integer;
begin
  Result := inherited _Release;
end;

end.
