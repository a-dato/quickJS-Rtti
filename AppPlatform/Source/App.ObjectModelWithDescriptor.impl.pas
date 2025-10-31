unit App.ObjectModelWithDescriptor.impl;

interface

uses
  System_,
  ADato.ObjectModel.List.Tracking.impl,
  App.ObjectModelWithDescriptor.intf, App.PropertyDescriptor.intf,
  System.Collections, System.Collections.ListInterface.impl;

type
  TMarshalledList = class(TVirtualListBase)
  protected
    _marshaller: IMarshaller;
    _source: IList;

    function  get_Item_object(Index: Integer): CObject; override;
    procedure set_Item_object(Index: Integer; const Value: CObject); override;

    function  get_Count: Integer; override;

  public
    constructor Create(const ASource: IList; const AMarshaller: IMarshaller);
  end;

  TObjectModelWithDescriptor<T> = class(TObjectListModelWithChangeTracking<T>, IObjectModelWithDescriptor)
  protected
    _PropertyDescriptor: IPropertyDescriptor;

    procedure set_Context(const Value: IList); override;
    function get_PropertyDescriptor: IPropertyDescriptor;

  public
    constructor Create(const AType: &Type; const ADescriptor: IPropertyDescriptor);

  end;

implementation

{ TObjectModelWithDescriptor<T> }

constructor TObjectModelWithDescriptor<T>.Create(const AType: &Type; const ADescriptor: IPropertyDescriptor);
begin
  inherited Create(AType);
  _PropertyDescriptor := ADescriptor;
end;

function TObjectModelWithDescriptor<T>.get_PropertyDescriptor: IPropertyDescriptor;
begin
  Result := _PropertyDescriptor;
end;

procedure TObjectModelWithDescriptor<T>.set_Context(const Value: IList);
begin
  if (_PropertyDescriptor <> nil) then
  begin
    var m := _PropertyDescriptor.Marshaller;
    if m <> nil then
    begin
      var l := TMarshalledList.Create(Value, m);
      inherited set_Context(l);
      Exit;
    end;
  end;

  inherited;
end;

{ TMarshalledList }

constructor TMarshalledList.Create(const ASource: IList; const AMarshaller: IMarshaller);
begin
  inherited Create;
  _source := ASource;
  _marshaller := AMarshaller;
end;

function TMarshalledList.get_Count: Integer;
begin
  Result := _source.Count;
end;

function TMarshalledList.get_Item_object(Index: Integer): CObject;
begin
  Result := _marshaller.Unmarshal(_source, _source[Index]);
end;

procedure TMarshalledList.set_Item_object(Index: Integer; const Value: CObject);
begin
  _source[Index] := _marshaller.Marshal(_source, Value);
end;

end.

