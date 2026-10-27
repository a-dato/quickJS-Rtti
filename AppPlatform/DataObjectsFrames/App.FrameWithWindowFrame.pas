unit App.FrameWithWindowFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System_, System.Collections.Generic, App.Component.intf, App.Windows.intf;

type
  TFmxFrameWithWindowFrameSupport = class(TFrame, IWindowFrame, IComponent)
  protected
    // IBaseInterface
    function  AsType(const Value: &Type) : CObject; virtual;
    function  TryAsType(const Value: &Type) : CObject; virtual;
    function  QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  getRefCount: Integer;
    function  GetObject: TObject; virtual;
    function  GetType: &Type; virtual;
    procedure Dispose; virtual;
    function  ToString: CString; reintroduce; overload; virtual;
    function  Equals(const other: CObject): Boolean; reintroduce; overload; virtual;

    // IComponent
    function  get_Parent: IComponent;
    function  get_Children: List<IComponent>; virtual;
    function  get_Name: CString;
    procedure set_Name(const Value: CString);

    // IWindowFrame
    function  WaitFor(CompletionProc: TProc = nil) : Boolean; virtual;
    function  get_Content: TObject;
    procedure set_Content(const Value: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  System.Rtti, App.Component.FMX.impl;

{$R *.fmx}

{ TFmxFrameWithWindowFrameSupport }

function TFmxFrameWithWindowFrameSupport.AsType(const Value: &Type): CObject;
begin
  Result := TryAsType(Value);
  if Result = nil then
    raise InvalidCastException.Create('cast to: ''' +  Value.Name + ''' failed');
end;

function TFmxFrameWithWindowFrameSupport.TryAsType(const Value: &Type): CObject;
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

function TFmxFrameWithWindowFrameSupport.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TFmxFrameWithWindowFrameSupport._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TFmxFrameWithWindowFrameSupport._Release: Integer;
begin
  Result := inherited _Release;
end;

function TFmxFrameWithWindowFrameSupport.getRefCount: Integer;
begin
  // Lifetime is managed by the frame's owner, not by reference counting
  Result := -1;
end;

function TFmxFrameWithWindowFrameSupport.GetObject: TObject;
begin
  Result := Self;
end;

function TFmxFrameWithWindowFrameSupport.GetType: &Type;
begin
  Result := &Type.Create(Self.ClassInfo);
end;

procedure TFmxFrameWithWindowFrameSupport.Dispose;
begin

end;

function TFmxFrameWithWindowFrameSupport.ToString: CString;
begin
  Result := ClassName;
end;

function TFmxFrameWithWindowFrameSupport.Equals(const other: CObject): Boolean;
begin
  Result := (other <> nil) and (GetHashCode = other.GetHashCode);
end;

function TFmxFrameWithWindowFrameSupport.get_Parent: IComponent;
begin
  if not Supports(Parent, IComponent, Result) then
    Result := nil;
end;

function TFmxFrameWithWindowFrameSupport.get_Children: List<IComponent>;
begin
  Result := TFmxComponent.CreateChildren(Self, Self);
end;

function TFmxFrameWithWindowFrameSupport.get_Name: CString;
begin
  Result := Name;
end;

procedure TFmxFrameWithWindowFrameSupport.set_Name(const Value: CString);
begin
  Name := Value.ToString;
end;

function TFmxFrameWithWindowFrameSupport.WaitFor(CompletionProc: TProc) : Boolean;
begin
  // Ready as soon as it is created, descendants that need preparation override this
  if Assigned(CompletionProc) then
  begin
    CompletionProc();
    Result := False;
  end else
    Result := True;
end;

function TFmxFrameWithWindowFrameSupport.get_Content: TObject;
begin
  // The frame is its own content
  Result := Self;
end;

procedure TFmxFrameWithWindowFrameSupport.set_Content(const Value: TObject);
begin
  if Value <> Self then
    raise CException.Create('The content of a frame cannot be changed');
end;

end.
