unit Project.intf;

interface

uses
  System_, ADato.AvailabilityProfile.intf;

type
  {$M+}
  IProject = interface(IBaseInterface)
    ['{42B8B40D-2951-402E-A28C-73B0DD8B04D5}']
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Name: string;
    procedure set_Name(const Value: string);
    function  get_ChildProject: IProject;
    function  get_TimeInterval: TimeInterval;
    procedure set_TimeInterval(const Value: TimeInterval);

    property ID: CObject read get_ID write set_ID;
    property Name: string read get_Name write set_Name;
    property TimeInterval: TimeInterval read get_TimeInterval write set_TimeInterval;

    property ChildProject: IProject read get_ChildProject;
  end;

implementation

end.
