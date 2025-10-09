unit App.Component.intf;

interface

uses
  System_;

type
  {$M+}
  IComponent = interface(IBaseInterface)
    ['{E30797E5-B9BF-4FE6-BD33-293FDFB5EE89}']
    function get_Owner: IComponent;

    property Owner: IComponent read get_Owner;
  end;

implementation

end.

