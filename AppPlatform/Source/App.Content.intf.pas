unit App.Content.intf;

interface

uses
  System_,
  System.Collections,
  App.Storage.intf,
  App.Windows.intf;

type
  {$M+}
  IContentBinder = interface(IBaseInterface)
    procedure Bind(const AType: &Type; const Window: IWindow; const Data: CObject);
  end;

  {$M+}
  IContentProvider = interface(IBaseInterface)
    function CreateStorage: IStorage;
    function Data(const Filter: CObject): IList;
  end;

implementation

end.

