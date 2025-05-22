unit App.Content.intf;

interface

uses
  System_;

type
  {$M+}
  IContentBuilder = interface(IBaseInterface)
    function Build(const AOwner: CObject): CObject;
  end;

  {$M+}
  IContentBinder = interface(IBaseInterface)
    procedure Bind(const AContent: CObject; const AType: &Type; const Data: CObject);
  end;

  {$M+}
  IContentProvider = interface(IBaseInterface)
    function get_Data: CObject;

    property Data: CObject read get_Data;
  end;

implementation

end.
