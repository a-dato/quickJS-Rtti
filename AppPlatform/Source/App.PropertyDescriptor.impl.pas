unit App.PropertyDescriptor.impl;

interface

uses
  System_,
  System.Collections.Generic,
  App.PropertyDescriptor.intf;

type
  TPicklist = class(TBaseInterfacedObject, IPicklist)
  protected
    function Items(const Filter: CString) : CObject; overload; // List, array or else...
    function Items(const Pred: Predicate<CObject>) : CObject; overload; // List, array or else...
    function Format(const Item: CObject) : CString;
  end;

  TPropertyWithDescriptor = class(CPropertyWrapper, IPropertyDescriptor, INotifyPropertyChanged, ICustomProperty)
  protected
    _PropertyDescriptor: IPropertyDescriptor;
    _Marshaller: IMarshaller;
    _Notifiers: List<INotify>;

    function  get_CustomProperty: ICustomProperty;

    function  GetValue(const obj: CObject; const index: array of CObject): CObject; override;
    procedure SetValue(const obj: CObject; const Value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false); override;

    // INotifyPropertyChanged
    procedure Add(const Value: INotify);
    procedure Remove(const Value: INotify);

  public
    constructor Create(const AProperty: _PropertyInfo; const ADescriptor: IPropertyDescriptor);

    property CustomProperty: ICustomProperty read get_CustomProperty implements ICustomProperty;
    property PropertyDescriptor: IPropertyDescriptor read _PropertyDescriptor implements IPropertyDescriptor;
  end;

  TCollectionPropertyWithDescriptor = class(CPropertyWrapper, IPropertyDescriptor, ICollectionPropertyDescriptor)
  protected
    _PropertyDescriptor: IPropertyDescriptor;

  public
    constructor Create(const AProperty: _PropertyInfo; const ADescriptor: IPropertyDescriptor);

    property PropertyDescriptor: IPropertyDescriptor read _PropertyDescriptor implements IPropertyDescriptor;
  end;

implementation

uses
  System.ClassHelpers;

{ TObjectsConfig }


{ TPicklist }

function TPicklist.Format(const Item: CObject): CString;
begin

end;

function TPicklist.Items(const Filter: CString): CObject;
begin

end;

function TPicklist.Items(const Pred: Predicate<CObject>): CObject;
begin

end;

{ TPropertyWithDescriptor }
procedure TPropertyWithDescriptor.Add(const Value: INotify);
begin
  if Value <> nil then
  begin
    if _Notifiers = nil then
      _Notifiers := CList<INotify>.Create(1);

    _Notifiers.Add(Value);
  end;
end;

constructor TPropertyWithDescriptor.Create(const AProperty: _PropertyInfo; const ADescriptor: IPropertyDescriptor);
begin
  inherited Create(AProperty);
  _PropertyDescriptor := ADescriptor;
  _marshaller := _PropertyDescriptor.Marshaller;
  Add(_PropertyDescriptor.Notify);
end;

function TPropertyWithDescriptor.GetValue(const obj: CObject; const index: array of CObject): CObject;
begin
  Result := inherited;
  if _marshaller <> nil then
    Result := _marshaller.Unmarshal(obj, Result);
end;

function TPropertyWithDescriptor.get_CustomProperty: ICustomProperty;
begin
  Interfaces.Supports<ICustomProperty>(_property, Result);
end;

procedure TPropertyWithDescriptor.Remove(const Value: INotify);
begin
  _Notifiers.Remove(Value);
end;

procedure TPropertyWithDescriptor.SetValue(const obj, Value: CObject; const index: array of CObject; ExecuteTriggers: Boolean);
begin
  var val: CObject := Value;

  if _Notifiers <> nil then
    for var notify in _Notifiers do
      val := notify.OnChanging(obj, val);

  if (_marshaller <> nil) and (Value.IsInterface or Value.IsObject) then
    val := _marshaller.Marshal(obj, Value) else
    val := Value;

  inherited SetValue(obj, val, index, ExecuteTriggers);

  if _Notifiers <> nil then
    for var notify in _Notifiers do
      notify.OnChanged(obj, val);
end;

{ TCollectionPropertyWithDescriptor }

constructor TCollectionPropertyWithDescriptor.Create(const AProperty: _PropertyInfo; const ADescriptor: IPropertyDescriptor);
begin
  inherited Create(AProperty);
  _PropertyDescriptor := ADescriptor;
end;

end.

