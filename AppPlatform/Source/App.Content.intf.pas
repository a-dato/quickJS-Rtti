unit App.Content.intf;

interface

uses
  System_, App.Storage.intf, System.Collections;

type
  {$M+}
  IContentBuilder = interface(IBaseInterface)
    function Build(const AOwner: CObject): CObject;
  end;

  {$M+}
  IContentBinder = interface(IBaseInterface)
    procedure Bind(const AContent: CObject; const AType: &Type; const Storage: IAppStorage);
  end;

  {$M+}
  IContentProvider = interface(IBaseInterface)
    function Data(const Filter: CObject): IList;
  end;

implementation

end.

