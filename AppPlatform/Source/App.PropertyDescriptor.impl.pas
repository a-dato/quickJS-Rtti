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

  TPropertyDescriptor = class(TBaseInterfacedObject, IPropertyDescriptor)
  protected
    _Picklist: IPicklist;
    _EditorType: EditorType;
    _Visible: Boolean;

    function  get_EqualityComparer: IEqualityComparer;
    function  get_Formatter: IFormatter;
    procedure set_Formatter(const Value: IFormatter);
    function  get_IsCollectionProperty: Boolean; virtual;
    function  get_Marshaller: IMarshaller;
    procedure set_Marshaller(const Value: IMarshaller);
    function  get_Notify: INotify;
    procedure set_Notify(const Value: INotify);
    function  get_Picklist: IPicklist;
    procedure set_Picklist(const Value: IPicklist);
    function  get_EditorType: EditorType;
    procedure set_EditorType(const Value: EditorType);
    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);
  end;

  TPropertyWithDescriptor = class(CPropertyWrapper, IPropertyDescriptor, INotifyPropertyChanged)
  protected
    _PropertyDescriptor: IPropertyDescriptor;
    _Marshaller: IMarshaller;
    _Notifiers: List<INotify>;

    function  GetValue(const obj: CObject; const index: array of CObject): CObject; override;
    procedure SetValue(const obj: CObject; const Value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false); override;

    // INotifyPropertyChanged
    procedure Add(const Value: INotify);
    procedure Remove(const Value: INotify);

  public
    constructor Create(const AProperty: _PropertyInfo; const ADescriptor: IPropertyDescriptor);

    property PropertyDescriptor: IPropertyDescriptor read _PropertyDescriptor implements IPropertyDescriptor;
  end;

  TCollectionPropertyDescriptor = class(TPropertyDescriptor, ICollectionPropertyDescriptor)
  protected
    function  get_IsCollectionProperty: Boolean; override;

  end;

implementation

uses
  System.ClassHelpers;

{ TObjectsConfig }

{ TPropertyDescriptor }

function TPropertyDescriptor.get_EditorType: EditorType;
begin
  Result := _EditorType;
end;

function TPropertyDescriptor.get_EqualityComparer: IEqualityComparer;
begin

end;

function TPropertyDescriptor.get_Formatter: IFormatter;
begin

end;

function TPropertyDescriptor.get_IsCollectionProperty: Boolean;
begin
  Result := False;
end;

function TPropertyDescriptor.get_Picklist: IPicklist;
begin
  Result := _Picklist;
end;

function TPropertyDescriptor.get_Visible: Boolean;
begin
  Result := _Visible;
end;

procedure TPropertyDescriptor.set_EditorType(const Value: EditorType);
begin
  _EditorType := Value;
end;

procedure TPropertyDescriptor.set_Formatter(const Value: IFormatter);
begin

end;

function TPropertyDescriptor.get_Marshaller: IMarshaller;
begin

end;

function TPropertyDescriptor.get_Notify: INotify;
begin

end;

procedure TPropertyDescriptor.set_Marshaller(const Value: IMarshaller);
begin

end;

procedure TPropertyDescriptor.set_Notify(const Value: INotify);
begin

end;

procedure TPropertyDescriptor.set_Picklist(const Value: IPicklist);
begin
  _PickList := Value;
end;

procedure TPropertyDescriptor.set_Visible(const Value: Boolean);
begin
  _Visible := Value;
end;

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

  if _marshaller <> nil then
    val := _marshaller.Marshal(obj, Value) else
    val := Value;

  inherited SetValue(obj, val, index, ExecuteTriggers);

  if _Notifiers <> nil then
    for var notify in _Notifiers do
      notify.OnChanged(obj, val);
end;


{ TCollectionPropertyDescriptor }
function TCollectionPropertyDescriptor.get_IsCollectionProperty: Boolean;
begin
  Result := True;
end;

end.

