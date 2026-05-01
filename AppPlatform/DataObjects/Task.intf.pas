unit Task.intf;

interface

uses
  System_;

type
  {$M+}
  ITask = interface(IBaseInterface)
    ['{C0B904FE-DABD-441F-AACF-7356EE31CD45}']
    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Description: string;
    procedure set_Description(const Value: string);

    function  Equals(const Other: ITask): Boolean;

    property ID: CObject read get_ID write set_ID;
    property Description: string read get_Description write set_Description;
  end;

implementation

end.

