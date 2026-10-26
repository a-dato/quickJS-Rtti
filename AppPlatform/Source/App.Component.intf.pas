unit App.Component.intf;

interface

uses
  System_, System.Collections.Generic;

type
  {$M+}
  IComponent = interface(IBaseInterface)
    ['{E30797E5-B9BF-4FE6-BD33-293FDFB5EE89}']
    function  get_Parent: IComponent;
    function  get_Children: List<IComponent>;

    function  get_Name: CString;
    procedure set_Name(const Value: CString);

    property Children: List<IComponent> read get_Children;
    property Parent: IComponent read get_Parent;
    property Name: CString read get_Name write set_Name;
  end;

implementation

end.

