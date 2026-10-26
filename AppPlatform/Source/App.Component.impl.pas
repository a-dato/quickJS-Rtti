unit App.Component.impl;

interface

uses
  System_,
  App.Component.intf, System.Collections.Generic;

type
  {$M+}
  TComponent = class(TBaseInterfacedObject, IComponent)
  protected
    function  get_Parent: IComponent; virtual;
    function  get_Children: List<IComponent>; virtual;

    function  get_Name: CString; virtual;
    procedure set_Name(const Value: CString); virtual;

  public
    constructor Create(const Parent: IComponent);
  end;

implementation

{ TComponent }

constructor TComponent.Create(const Parent: IComponent);
begin
  // _parent := Parent;
end;

function TComponent.get_Children: List<IComponent>;
begin

end;

function TComponent.get_Name: CString;
begin

end;

function TComponent.get_Parent: IComponent;
begin
//
end;

procedure TComponent.set_Name(const Value: CString);
begin

end;

end.

