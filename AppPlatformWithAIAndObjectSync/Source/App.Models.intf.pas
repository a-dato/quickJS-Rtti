unit App.Models.intf;

interface

uses
  System_,
  ADato.ObjectModel.List.intf;

type
  IAppModels = interface(IBaseInterface)

    function CreateOrGet(const AType: &Type) : IObjectListModel;
  end;

implementation

end.

