unit Customer.impl;

interface

uses
  System_,
  Customer.intf,
  App.Objects.intf,
  App.Objects.impl,
  App.Content.intf,
  ADato.ObjectModel.List.intf;

type
  TCustomer = class(TBaseInterfacedObject, ICustomer)
  protected
    _ID: CObject;
    _Name: string;

  protected
    class var _CustomerType: IObjectType;
    class function get_CustomerType: IObjectType; static;

    function  get_ID: CObject;
    procedure set_ID(const Value: CObject);
    function  get_Name: string;
    procedure set_Name(const Value: string);

  public
    class property CustomerType: IObjectType read get_CustomerType;
  end;

  CustomerType = class(ObjectType)
    function GetType: &Type; override;
  end;

  TCustomerProvider = class(TBaseInterfacedObject, IContentProvider)
  protected
    function get_Data: CObject;
  end;

implementation

uses
  ADato.ObjectModel.List.Tracking.impl, System.Collections.Generic,
  System.SysUtils, System.Collections;

class function TCustomer.get_CustomerType: IObjectType;
begin
  if _CustomerType = nil then
    _CustomerType := Customer.impl.CustomerType.Create;

  Result := _CustomerType;
end;

{ TCustomer }

function TCustomer.get_ID: CObject;
begin
  Result := _ID;
end;

function TCustomer.get_Name: string;
begin
  Result := _Name;
end;

procedure TCustomer.set_ID(const Value: CObject);
begin
  _ID := Value;
end;

procedure TCustomer.set_Name(const Value: string);
begin
  _Name := Value;
end;

{ TCustomerProvider }

function TCustomerProvider.get_Data: CObject;
begin
  var model: IObjectListModel := TObjectListModelWithChangeTracking<ICustomer>.Create(function: ICustomer begin Result := TCustomer.Create; end);

  var l: List<ICustomer> := CList<ICustomer>.Create;

  for var i := 0 to 9 do
  begin
    var c: ICustomer := TCustomer.Create;
    c.ID := i;
    c.Name := 'Customer ' + i.ToString;

    l.Add(c);
  end;

  model.Context := l as IList;

  Result := model;
end;

{ CustomerType }

function CustomerType.GetType: &Type;
begin
  Result := &Type.From<ICustomer>;
end;

end.
