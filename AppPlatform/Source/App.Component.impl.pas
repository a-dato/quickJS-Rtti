unit App.Component.impl;

interface

uses
  System_,
  App.Component.intf;

type
  {$M+}
  TComponent = class(TBaseInterfacedObject, IComponent)
  protected
    _owner: IComponent;

    function get_Owner: IComponent;

  public
    constructor Create(const Owner: IComponent);
  end;

implementation

{ TComponent }

constructor TComponent.Create(const Owner: IComponent);
begin
  _owner := Owner;
end;

function TComponent.get_Owner: IComponent;
begin
  Result := _owner;
end;

end.

