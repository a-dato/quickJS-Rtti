unit App.Content.intf;

interface

uses
  System_,
  System.Collections,
  App.Storage.intf;

type
  {$M+}
  IContentBinder = interface(IBaseInterface)
    procedure Bind(const AType: &Type; const Control: TObject; const Data: CObject);
  end;

  {$M+}
  IContentProvider = interface(IBaseInterface)
    function CreateStorage: IStorage;
    function Data(const Filter: CObject): IList;
  end;

implementation

end.

