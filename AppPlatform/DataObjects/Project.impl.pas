unit Project.impl;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic,
  Project.intf,
  App.TypeDescriptor.intf,
  App.TypeDescriptor.impl,
  App.Content.intf,
  App.Storage.intf,
  ADato.ObjectModel.List.intf,
  App.Content.impl,
  Task.intf,
  BulkUpdate.intf,
  ProjectLoader.intf;

type
  TProject = class(TBaseInterfacedObject,
    IProject,
    IExtendableObject,
    IBulkUpdate,
    IProjectLoader)
//    , IJSExtendableObject)
  protected
    class var _typeDescriptor: ITypeDescriptor;
    class function get_Type: &Type; static;
    class function get_TypeDescriptor: ITypeDescriptor; static;

  protected
    _ID: CObject;
    _Description: string;
    _Child: IProject;
    _Holidays: List<THoliday>;
    _tasks: List<ITask>;

    _PropertyValue: Dictionary<_PropertyInfo, CObject>;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Description: string;
    procedure set_Description(const Value: string);
    function  get_ChildProject: IProject;
    function  get_Holidays: List<THoliday>;
    function  get_Tasks: List<ITask>;

    // IExtendableObject
    function  get_PropertyValue(const AProperty: _PropertyInfo): CObject;
    procedure set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);

  public
    constructor Create;

    function  GetHashCode: Integer; override;
    function  Equals(const Other: IProject): Boolean; overload; virtual;
    function  Equals(const other: CObject): Boolean; overload; override;

    // IBulkUpdate
    procedure BeginBulkUpdate;
    procedure EndBulkUpdate;

    // IProjectLoader
    function  IsLoaded: Boolean;
    procedure ForceOpen;
    procedure ForceClose;

    class property &Type: &Type read get_Type;
    class property TypeDescriptor: ITypeDescriptor read get_TypeDescriptor;
  end;

  ProjectType = class(TTypeDescriptor)
  protected
    function GetType: &Type; override;
  end;

  ProjectProvider = class(TContentProvider)
  protected
    function CreateInstance: CObject; override;
    function Data(const Filter: CObject): IList; override;
  end;

implementation

uses
  ADato.ObjectModel.List.Tracking.impl,
  System.SysUtils, System.Rtti, App.intf, App.Storage.impl, Task.impl;


{ TProject }
function TProject.GetHashCode: Integer;
begin
  Result := _ID.GetHashCode;
end;

function TProject.Equals(const Other: IProject): Boolean;
begin
  Result := CObject.Equals(_ID, Other.ID)
end;

procedure TProject.EndBulkUpdate;
begin

end;

function TProject.Equals(const other: CObject): Boolean;
begin
  Result := Equals(Other.AsType<IProject>);
end;

procedure TProject.ForceClose;
begin
  ;
end;

procedure TProject.ForceOpen;
begin
  ;
end;

function TProject.get_Tasks: List<ITask>;
begin
  if _tasks = nil then
    _tasks := CList<ITask>.Create;

  Result := _tasks;
end;

class function TProject.get_Type: &Type;
begin
  // Result := System_.&Type.From<ProjectType>;
  Result := System_.&Type.From<IProject>;
end;

class function TProject.get_TypeDescriptor: ITypeDescriptor;
begin
  if _typeDescriptor = nil then
  begin
    _typeDescriptor := ProjectType.Create(&Type.From<IProject>, 'Project', 'Projects');
    _typeDescriptor.AddSupportedInterface(&Type.From<IBulkUpdate>);
    _typeDescriptor.AddSupportedInterface(&Type.From<IProjectLoader>);
  end;

  Result := _typeDescriptor;
end;

function TProject.IsLoaded: Boolean;
begin

end;

function TProject.get_PropertyValue(const AProperty: _PropertyInfo): CObject;
begin
  _PropertyValue.TryGetValue(AProperty, Result);

  if (Result = nil) and AProperty.GetType.IsOfType<IList> then
  begin
    Result := CObject.From<IList>(CList<CObject>.Create);
    _PropertyValue[AProperty] := Result;
  end;
end;

procedure TProject.BeginBulkUpdate;
begin

end;

constructor TProject.Create;
begin
  _PropertyValue := CDictionary<_PropertyInfo, CObject>.Create;
end;

function TProject.get_ID: CObject;
begin
  Result := _ID;
end;

function TProject.get_Description: string;
begin
  Result := _Description;
end;

function TProject.get_ChildProject: IProject;
begin
  if _Child = nil then
  begin
    _Child := TProject.Create;
    _Child.ID := Int64(_ID) + 100;
    _Child.Description := 'Child of ' + _Description;
  end;

  Result := _Child;
end;

function TProject.get_Holidays: List<THoliday>;
begin
  if _Holidays = nil then
    _Holidays := CList<THoliday>.Create;
  Result := _Holidays;
end;

procedure TProject.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TProject.set_Description(const Value: string);
begin
  _Description := Value;
end;

procedure TProject.set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);
begin
  _PropertyValue[AProperty] := Value;
end;

function ProjectProvider.CreateInstance: CObject;
begin
  Result := _app.Factory.CreateInstance(_descriptor.GetType);
end;

function ProjectProvider.Data(const Filter: CObject): IList;
begin
  var l: List<IProject> := CList<IProject>.Create;

  for var i := 0 to 9 do
  begin
    var p: IProject := TProject.Create;
    p.ID := i;
    p.Description := 'Project ' + i.ToString;

    var h: THoliday;
    h.Start := CDateTime.Now.AddDays(i).Date;
    h.Stop := CDateTime.Now.AddDays(i + 2).Date;
    h.IceCreams := CList<TIceCream>.Create;
    var ic: TIceCream;
    ic.ID := i;
    ic.Name := 'Raket ' + i.ToString;
    h.IceCreams.Add(ic);

    p.Holidays.Add(h);

    l.Add(p);
  end;

  Result := l as IList;
end;

function ProjectType.GetType: &Type;
begin
  Result := &Type.From<IProject>;
end;

end.

