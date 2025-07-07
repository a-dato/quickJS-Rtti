unit App.Models.impl;

interface

uses
  System_,
  System.Collections.Generic,
  ADato.ObjectModel.List.intf,
  App.Intf,
  App.Models.intf;

type
  AppModels = class(TBaseInterfacedObject, IAppModels)
  protected
    _app: IAppObject;
    _models: Dictionary<&Type, IObjectListModel>;

    function CreateOrGet(const AType: &Type) : IObjectListModel;

  public
    constructor Create(const App: IAppObject);
  end;

implementation

{ AppModels }

constructor AppModels.Create(const App: IAppObject);
begin
  _app := App;
  _models := CDictionary<&Type, IObjectListModel>.Create;
end;

function AppModels.CreateOrGet(const AType: &Type): IObjectListModel;
begin
  if not _models.TryGetValue(AType, Result) then
  begin
    var ot := _app.Config.ObjectType(AType);
    if ot = nil then
      raise CException.Create('Unknown type');
    // Result := ot.CreateModel;
    _models[AType] := Result;
  end;
end;

end.


