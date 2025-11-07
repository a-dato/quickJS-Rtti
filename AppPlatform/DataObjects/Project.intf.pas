unit Project.intf;

interface

uses
  System_;

type
  {$M+}
  IProject = interface(IBaseInterface)
    ['{42B8B40D-2951-402E-A28C-73B0DD8B04D5}']
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Description: string;
    procedure set_Description(const Value: string);
    function  get_ChildProject: IProject;

    function  Equals(const Other: IProject): Boolean;

    property ID: CObject read get_ID write set_ID;
    property Description: string read get_Description write set_Description;

    property Child: IProject read get_ChildProject;
  end;

implementation

end.

