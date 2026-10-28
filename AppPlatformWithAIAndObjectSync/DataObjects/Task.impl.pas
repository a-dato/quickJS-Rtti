unit Task.impl;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic,
  Task.intf,
  App.TypeDescriptor.intf,
  App.TypeDescriptor.impl,
  App.Content.intf,
  App.Storage.intf,
  ADato.ObjectModel.List.intf,
  App.Content.impl;

type
  TTask = class(TBaseInterfacedObject, ITask{, IExtendableObject})
  protected
    //class var _typeDescriptor: ITypeDescriptor;
    class function get_Type: &Type; static;
//    class function get_TypeDescriptor: ITypeDescriptor; static;

  protected
    _ID: CObject;
    _Description: string;

    _PropertyValue: Dictionary<_PropertyInfo, CObject>;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Description: string;
    procedure set_Description(const Value: string);

    // IExtendableObject
//    function  get_PropertyValue(const AProperty: _PropertyInfo): CObject;
//    procedure set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);

  public
    constructor Create;

    function  GetHashCode: Integer; override;
    function  Equals(const Other: ITask): Boolean; overload; virtual;
    function  Equals(const other: CObject): Boolean; overload; override;

    class property &Type: &Type read get_Type;
//    class property TypeDescriptor: ITypeDescriptor read get_TypeDescriptor;
  end;

//  TaskType = class(TTypeDescriptor)
//  protected
//    function GetType: &Type; override;
//  end;
//
//  TaskProvider = class(TContentProvider)
//  protected
//    function CreateInstance: CObject; override;
//    function Data(const Filter: CObject): IList; override;
//  end;

implementation

uses
  ADato.ObjectModel.List.Tracking.impl,
  System.SysUtils, System.Rtti, App.intf, App.Storage.impl;


{ TTask }

function TTask.GetHashCode: Integer;
begin
  Result := _ID.GetHashCode;
end;

function TTask.Equals(const Other: ITask): Boolean;
begin
  Result := CObject.Equals(_ID, Other.ID)
end;

function TTask.Equals(const other: CObject): Boolean;
begin
  Result := Equals(Other.AsType<ITask>);
end;

class function TTask.get_Type: &Type;
begin
  Result := System_.&Type.From<ITask>;
end;

//class function TTask.get_TypeDescriptor: ITypeDescriptor;
//begin
//  if _typeDescriptor = nil then
//    _typeDescriptor := TaskType.Create(&Type.From<ITask>, 'Task', 'Tasks');
//
//  Result := _typeDescriptor;
//end;

//function TTask.get_PropertyValue(const AProperty: _PropertyInfo): CObject;
//begin
//  _PropertyValue.TryGetValue(AProperty, Result);
//
//  if (Result = nil) and AProperty.GetType.IsOfType<IList> then
//  begin
//    Result := CObject.From<IList>(CList<CObject>.Create);
//    _PropertyValue[AProperty] := Result;
//  end;
//end;

constructor TTask.Create;
begin
  _PropertyValue := CDictionary<_PropertyInfo, CObject>.Create;
end;

function TTask.get_ID: CObject;
begin
  Result := _ID;
end;

function TTask.get_Description: string;
begin
  Result := _Description;
end;

procedure TTask.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TTask.set_Description(const Value: string);
begin
  _Description := Value;
end;

//procedure TTask.set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);
//begin
//  _PropertyValue[AProperty] := Value;
//end;

//function TaskProvider.CreateInstance: CObject;
//begin
//  Result := _app.Factory.CreateInstance(_descriptor.GetType);
//end;
//
//function TaskProvider.Data(const Filter: CObject): IList;
//begin
//  var l: List<ITask> := CList<ITask>.Create;
//
//  for var i := 0 to 9 do
//  begin
//    var t: ITask := TTask.Create;
//    t.ID := i;
//    t.Description := 'Task ' + i.ToString;
//
//    l.Add(t);
//  end;
//
//  Result := l as IList;
//end;
//
//function TaskType.GetType: &Type;
//begin
//  Result := &Type.From<ITask>;
//end;

end.

