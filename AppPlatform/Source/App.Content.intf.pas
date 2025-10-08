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
    procedure Bind(const AContent: CObject; const AType: &Type; const Storage: IStorage);
  end;

  {$M+}
  IContentProvider = interface(IBaseInterface)
    function CreateInstance : CObject; overload;
    function CreateInstance(const Param1: CObject) : CObject; overload;
    function CreateInstance(const Param1: CObject; const Param2: CObject) : CObject; overload;
    function CreateInstance(const Param1: CObject; const Param2: CObject; const Param3: CObject) : CObject; overload;
    function CreateInstance(const Param1: CObject; const Param2: CObject; const Param3: CObject; const Param4: CObject) : CObject; overload;

    function CreateStorage: IStorage;
    function Data(const Filter: CObject): IList;
  end;

implementation

end.

