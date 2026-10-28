{$I Adato.inc}

unit App.UIStateDelegate;

interface

uses
  {$IFDEF DELPHI}
  System.SysUtils,
  {$ENDIF}
  System_;

type
  TApplyState = (ApplySucceeded, CancelSucceeded, ApplyException, CanceledByExternal);

  ApplyStateChangedProc = procedure(const ApplyState: TApplyState) of object;

  {$IFDEF DELPHI}
  IGlobalApplyStateDelegate = interface(IDelegate)
    ['{AAF2BB44-4C58-4887-8A1E-9FCDB845FEEA}']
    procedure Add(Value: ApplyStateChangedProc);
    function  Contains(Value: ApplyStateChangedProc) : Boolean;
    procedure Remove(value: ApplyStateChangedProc);
    procedure Invoke(const ApplyState: TApplyState);
  end;

  TGlobalApplyStateDelegate = class(Delegate, IGlobalApplyStateDelegate)
  protected
    procedure Add(Value: ApplyStateChangedProc);
    function  Contains(Value: ApplyStateChangedProc): Boolean;
    procedure Remove(value: ApplyStateChangedProc);
    procedure Invoke(const ApplyState: TApplyState);
  end;
  {$ELSE}
  TGlobalApplyStateDelegate = public delegate(const ApplyState: TApplyState; const AObjectType: ObjectType);
  IGlobalApplyStateDelegate = TGlobalApplyStateDelegate;
  {$ENDIF}

var
  GlobalApplyStateDelegate: IGlobalApplyStateDelegate;

implementation

{ TGlobalApplyStateDelegate }

{$IFDEF DELPHI}
procedure TGlobalApplyStateDelegate.Add(Value: ApplyStateChangedProc);
begin
  inherited Add(TMethod(Value));
end;

function TGlobalApplyStateDelegate.Contains(Value: ApplyStateChangedProc): Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure TGlobalApplyStateDelegate.Invoke(const ApplyState: TApplyState);
begin
  var cnt := 0;
  while cnt < _events.Count do
  begin
    ApplyStateChangedProc(_events[cnt]^)(ApplyState);
    inc(cnt);
  end;
end;

procedure TGlobalApplyStateDelegate.Remove(value: ApplyStateChangedProc);
begin
  inherited Remove(TMethod(Value));
end;
{$ENDIF}

{$IFDEF DELPHI}
initialization
  GlobalApplyStateDelegate := TGlobalApplyStateDelegate.Create;

finalization
  GlobalApplyStateDelegate := nil;
{$ENDIF}

end.

