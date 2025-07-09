unit Project.impl;

interface

uses
  System_,
  Project.intf,
  App.TypeDescriptor.intf,
  App.TypeDescriptor.impl,
  App.Content.intf,
  ADato.ObjectModel.List.intf, System.Collections.Generic, System.Collections,
  ADato.AvailabilityProfile.intf;

type
  TProject = class(TBaseInterfacedObject, IProject, IExtendableObject)
  protected
    class var _typeDescriptor: ITypeDescriptor;
    class function get_Type: &Type; static;
    class function get_TypeDescriptor: ITypeDescriptor; static;

  protected
    _ID: CObject;
    _Name: string;
    _Child: IProject;
    _TimeInterval: TimeInterval;

    _PropertyValue: Dictionary<_PropertyInfo, CObject>;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Name: string;
    procedure set_Name(const Value: string);
    function  get_ChildProject: IProject;
    function  get_TimeInterval: TimeInterval;
    procedure set_TimeInterval(const Value: TimeInterval);

    // IExtendableObject
    function  get_PropertyValue(const AProperty: _PropertyInfo): CObject;
    procedure set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);

  public
    constructor Create;

    class property &Type: &Type read get_Type;
    class property TypeDescriptor: ITypeDescriptor read get_TypeDescriptor;
  end;

  ProjectType = class(TTypeDescriptor)
  protected
    function GetType: &Type; override;
  end;

  ProjectProvider = class(TBaseInterfacedObject, IContentProvider)
  protected
    _data: List<IProject>;

    function Data(const Filter: CObject): CObject;
  end;

implementation

uses
  ADato.ObjectModel.List.Tracking.impl,
  System.SysUtils, System.Rtti;


{ TProject }
function TProject.get_TimeInterval: TimeInterval;
begin
  Result := _TimeInterval;
end;

class function TProject.get_Type: &Type;
begin
  // Result := System_.&Type.From<ProjectType>;
  Result := System_.&Type.From<IProject>;
end;

class function TProject.get_TypeDescriptor: ITypeDescriptor;
begin
  if _typeDescriptor = nil then
    _typeDescriptor := ProjectType.Create;

  Result := _typeDescriptor;
end;

function TProject.get_PropertyValue(const AProperty: _PropertyInfo): CObject;
begin
  _PropertyValue.TryGetValue(AProperty, Result);
end;

constructor TProject.Create;
begin
  _PropertyValue := CDictionary<_PropertyInfo, CObject>.Create;
  _TimeInterval := TimeInterval.Create(CDateTime.Now, CDateTime.Now.AddDays(10));
end;

function TProject.get_ID: CObject;
begin
  Result := _ID;
end;

function TProject.get_Name: string;
begin
  Result := _Name;
end;

function TProject.get_ChildProject: IProject;
begin
  if _Child = nil then
  begin
    _Child := TProject.Create;
    _Child.ID := Int64(_ID) + 100;
    _Child.Name := 'Child of ' + _Name;
  end;

  Result := _Child;
end;

procedure TProject.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TProject.set_Name(const Value: string);
begin
  _Name := Value;
end;

procedure TProject.set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);
begin
  _PropertyValue[AProperty] := Value;
end;

procedure TProject.set_TimeInterval(const Value: TimeInterval);
begin
  _TimeInterval := Value;
end;

{ ProjectProvider }

function ProjectProvider.Data(const Filter: CObject): CObject;
begin
  if _data = nil then
  begin
    _data := CList<IProject>.Create;

    for var i := 0 to 9 do
    begin
      var p: IProject := TProject.Create;
      p.ID := i;
      p.Name := 'Project ' + i.ToString;

      _data.Add(p);
    end;
  end;

  Result := CObject.From<List<IProject>>(_data);
end;

{ CustomerType }

function ProjectType.GetType: &Type;
begin
  Result := &Type.From<IProject>;
end;

end.

