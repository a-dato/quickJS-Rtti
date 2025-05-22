unit Customer.intf;

interface

uses
  System_,
  App.Windows.intf, App.Objects.intf;

type
  {$M+}
  ICustomer = interface(IBaseInterface)
    ['{42B8B40D-2951-402E-A28C-73B0DD8B04D5}']
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function get_Name: string;
    procedure set_Name(const Value: string);

    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
  end;

implementation

end.
