unit Project.impl;

interface

uses
  System_,
  Project.intf,
  App.Objects.intf,
  App.Objects.impl,
  App.Content.intf,
  ADato.ObjectModel.List.intf;

type
  TProject = class(TBaseInterfacedObject, IProject)
  protected
    class var _objectType: IObjectType;
    class function get_Type: &Type; static;
    class function get_ObjectType: IObjectType; static;

  protected
    _ID: CObject;
    _Name: string;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Name: string;
    procedure set_Name(const Value: string);

  public
    class property &Type: &Type read get_Type;
    class property ObjectType: IObjectType read get_ObjectType;
  end;

  ProjectType = class(ObjectType)
  protected
    function GetType: &Type; override;
  end;

  ProjectProvider = class(TBaseInterfacedObject, IContentProvider)
  protected
    function get_Data: CObject;
  end;

implementation

uses
  ADato.ObjectModel.List.Tracking.impl, System.Collections.Generic,
  System.SysUtils, System.Collections;


{ TProject }
class function TProject.get_Type: &Type;
begin
  Result := System_.&Type.From<ProjectType>;
end;

class function TProject.get_ObjectType: IObjectType;
begin
  if _objectType = nil then
    _objectType := ProjectType.Create;

  Result := _objectType;
end;

function TProject.get_ID: CObject;
begin
  Result := _ID;
end;

function TProject.get_Name: string;
begin
  Result := _Name;
end;

procedure TProject.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TProject.set_Name(const Value: string);
begin
  _Name := Value;
end;

{ ProjectProvider }

function ProjectProvider.get_Data: CObject;
begin
  var model: IObjectListModel := TObjectListModelWithChangeTracking<IProject>.Create(function: IProject begin Result := TProject.Create; end);

  var l: List<IProject> := CList<IProject>.Create;

  for var i := 0 to 9 do
  begin
    var p: IProject := TProject.Create;
    p.ID := i;
    p.Name := 'Project ' + i.ToString;

    l.Add(p);
  end;

  model.Context := l as IList;

  Result := model;
end;

{ CustomerType }

function ProjectType.GetType: &Type;
begin
  Result := &Type.From<IProject>;
end;

end.
