unit SyncTest.OrderModel;

// The Delphi side "business logic": order lines, as a plain interface with
// properties (the same shape as IProject in BindSimpleTest), and the list model
// that holds them.
//
//   Quantity    - editable
//   UnitPrice   - editable
//   Subtotal    = Quantity x UnitPrice                         (read only)
//   Discount    - editable ONLY when Subtotal > 100, 0..Subtotal;
//                 reset to 0 when Subtotal drops to 100 or below
//   Total       = Subtotal - Discount                          (read only)
//
// What can be edited is told by the object itself through IPropertyAccessibility
// (ADato.PropertyAccessibility.Intf). The framework's control bindings use it
// to enable/disable controls; the webview bindings use it the same way.
//
// TOrderListModel: the change tracking list model, limited to 2..10 items.
//
// The model knows nothing about UIs. UIs (Delphi controls and webview
// elements) are attached with IObjectModelContext.Bind, see the main form.

interface

uses
  System.SysUtils, System_, ADato.ObjectModel.intf, ADato.PropertyAccessibility.Intf,
  ADato.ObjectModel.List.Tracking.impl, ADato.ObjectModel.TrackInterfaces;

type
  IOrder = interface(IBaseInterface)
    ['{6D1E4C2A-8B3F-4A57-9C0D-2E7F1B5A9C34}']
    function  get_Name: string;
    function  get_Quantity: Double;
    procedure set_Quantity(const Value: Double);
    function  get_UnitPrice: Double;
    procedure set_UnitPrice(const Value: Double);
    function  get_Subtotal: Double;
    function  get_Discount: Double;
    procedure set_Discount(const Value: Double);
    function  get_Total: Double;

    property Name: string read get_Name;
    property Quantity: Double read get_Quantity write set_Quantity;
    property UnitPrice: Double read get_UnitPrice write set_UnitPrice;
    property Subtotal: Double read get_Subtotal;
    property Discount: Double read get_Discount write set_Discount;
    property Total: Double read get_Total;
  end;

  TOrder = class(TBaseInterfacedObject, IOrder, IPropertyAccessibility)
  private
    FName: string;
    FQuantity, FUnitPrice, FSubtotal, FDiscount, FTotal: Double;
    procedure Recalculate;
  protected
    function  get_Name: string;
    function  get_Quantity: Double;
    procedure set_Quantity(const Value: Double);
    function  get_UnitPrice: Double;
    procedure set_UnitPrice(const Value: Double);
    function  get_Subtotal: Double;
    function  get_Discount: Double;
    procedure set_Discount(const Value: Double);
    function  get_Total: Double;

    // IPropertyAccessibility: what can be edited, decided by the object
    function  CanEditObject: Boolean;
    function  PropertyState(const PropName: CString): TEditableState;
    procedure ClearEditablity(const PropName: CString);
  public
    const
      DiscountThreshold = 100;   // Discount allowed when Subtotal > this
    constructor Create(const AName: string; const AQuantity, AUnitPrice: Double; const ADiscount: Double = 0);

    // Which properties a property depends on. Used to Link bindings, so they
    // refresh (value AND editability) when one of those changes.
    class function InputsOf(const PropName: string): TArray<string>; static;
  end;

  // The change tracking list model, with a size limit. CanAdd/CanRemove are
  // what the DataControl, the Delphi buttons and the webview all ask, so the
  // limit holds everywhere. IEditableModel and IAddToList are listed again so
  // these methods replace the base class ones (interface re-implementation).
  TOrderListModel = class(TObjectListModelWithChangeTracking<IOrder>, IEditableModel, IAddToList)
  protected
    function  CanAdd: Boolean;
    function  CanRemove: Boolean;
    function  AddNew(Index: Integer = -1; AddBefore: Boolean = False): Boolean;
    procedure Remove; overload;
  public
    const
      MinCount = 2;
      MaxCount = 10;
  end;

  // Binds a UI binding to a property of the order, and links it to the
  // properties it depends on. Used for Delphi controls AND webview elements.
  procedure BindOrderProperty(const Context: IObjectModelContext; const PropName: string; const Binding: IPropertyBinding);

implementation

uses
  System.Math, SyncTest.Trace;

function Money(const Value: Double): string;
begin
  Result := FormatFloat('0.00', Value, TFormatSettings.Invariant);
end;

procedure BindOrderProperty(const Context: IObjectModelContext; const PropName: string; const Binding: IPropertyBinding);
begin
  Context.Bind(PropName, Binding);
  for var input in TOrder.InputsOf(PropName) do
    Context.Link(input, Binding);
end;

{ TOrder }

constructor TOrder.Create(const AName: string; const AQuantity, AUnitPrice: Double; const ADiscount: Double);
begin
  inherited Create;
  FName := AName;
  FQuantity := AQuantity;
  FUnitPrice := AUnitPrice;
  FDiscount := ADiscount;
  Trace.Enter('ORDER', Format('new ' + AName + ': Quantity %s, UnitPrice %s, Discount %s, calculate the rest',
    [FQuantity.ToString, Money(FUnitPrice), Money(FDiscount)]));
  try
    Recalculate;
  finally
    Trace.Leave;
  end;
end;

class function TOrder.InputsOf(const PropName: string): TArray<string>;
begin
  if (PropName = 'Subtotal') or (PropName = 'Discount') then
    Result := ['Quantity', 'UnitPrice']
  else if PropName = 'Total' then
    Result := ['Quantity', 'UnitPrice', 'Discount']
  else
    Result := [];
end;

function TOrder.get_Name: string;      begin Result := FName; end;
function TOrder.get_Quantity: Double;  begin Result := FQuantity; end;
function TOrder.get_UnitPrice: Double; begin Result := FUnitPrice; end;
function TOrder.get_Subtotal: Double;  begin Result := FSubtotal; end;
function TOrder.get_Discount: Double;  begin Result := FDiscount; end;
function TOrder.get_Total: Double;     begin Result := FTotal; end;

function TOrder.CanEditObject: Boolean;
begin
  Result := True;
end;

function TOrder.PropertyState(const PropName: CString): TEditableState;
begin
  var name: string := PropName;
  if (name = 'Quantity') or (name = 'UnitPrice') then
    Result := TEditableState.Editable
  else if name = 'Discount' then
  begin
    if FSubtotal > DiscountThreshold then
      Result := TEditableState.Editable
    else
      Result := TEditableState.ReadOnly;
  end
  else
    Result := TEditableState.ReadOnly;   // Name, Subtotal, Total
end;

procedure TOrder.ClearEditablity(const PropName: CString);
begin
  // nothing cached
end;

procedure TOrder.set_Quantity(const Value: Double);
begin
  if (Value < 0) or (Frac(Value) <> 0) then
    raise EArgumentException.Create('Quantity must be a whole number, 0 or more');
  if Value = FQuantity then Exit;
  Trace.Line('ORDER', Format('Quantity %s -> %s', [FQuantity.ToString, Value.ToString]));
  FQuantity := Value;
  Recalculate;
end;

procedure TOrder.set_UnitPrice(const Value: Double);
begin
  if Value < 0 then
    raise EArgumentException.Create('Unit price cannot be negative');
  var v := RoundTo(Value, -2);
  if v = FUnitPrice then Exit;
  Trace.Line('ORDER', Format('UnitPrice %s -> %s', [Money(FUnitPrice), Money(v)]));
  FUnitPrice := v;
  Recalculate;
end;

procedure TOrder.set_Discount(const Value: Double);
begin
  var v := RoundTo(Value, -2);
  if v = FDiscount then Exit;
  if (v <> 0) and (FSubtotal <= DiscountThreshold) then
    raise EArgumentException.CreateFmt('A discount is only allowed when Subtotal is above %d (it is %s)',
      [DiscountThreshold, Money(FSubtotal)]);
  if (v < 0) or (v > FSubtotal) then
    raise EArgumentException.CreateFmt('Discount must be between 0 and the Subtotal (%s)', [Money(FSubtotal)]);
  Trace.Line('ORDER', Format('Discount %s -> %s', [Money(FDiscount), Money(v)]));
  FDiscount := v;
  Recalculate;
end;

procedure TOrder.Recalculate;

  procedure Rule(const Name: string; var Field: Double; const NewValue: Double; const Why: string);
  begin
    if Field = NewValue then
      Trace.Line('RULE', Format('%s = %s -> %s (unchanged)', [Name, Why, Money(NewValue)]))
    else
      Trace.Line('RULE', Format('%s = %s -> %s (was %s)', [Name, Why, Money(NewValue), Money(Field)]));
    Field := NewValue;
  end;

begin
  Rule('Subtotal', FSubtotal, RoundTo(FQuantity * FUnitPrice, -2),
    Format('Quantity x UnitPrice = %s x %s', [FQuantity.ToString, Money(FUnitPrice)]));

  if FSubtotal <= DiscountThreshold then
    Rule('Discount', FDiscount, 0,
      Format('0 and read only, because Subtotal %s <= %d', [Money(FSubtotal), DiscountThreshold]))
  else if FDiscount > FSubtotal then
    Rule('Discount', FDiscount, FSubtotal,
      Format('at most the Subtotal %s', [Money(FSubtotal)]))
  else
    Trace.Line('RULE', Format('Discount editable, because Subtotal %s > %d (stays %s)',
      [Money(FSubtotal), DiscountThreshold, Money(FDiscount)]));

  Rule('Total', FTotal, RoundTo(FSubtotal - FDiscount, -2),
    Format('Subtotal - Discount = %s - %s', [Money(FSubtotal), Money(FDiscount)]));
end;

{ TOrderListModel }

function TOrderListModel.CanAdd: Boolean;
begin
  Result := inherited CanAdd and (get_Context.Count < MaxCount);
end;

function TOrderListModel.CanRemove: Boolean;
begin
  Result := inherited CanRemove and (get_Context.Count > MinCount);
end;

function TOrderListModel.AddNew(Index: Integer; AddBefore: Boolean): Boolean;
begin
  if not CanAdd then
  begin
    Trace.Line('LIST', Format('AddNew refused: the list holds at most %d items', [MaxCount]));
    Exit(False);
  end;
  Result := inherited AddNew(Index, AddBefore);
end;

procedure TOrderListModel.Remove;
begin
  if not CanRemove then
  begin
    Trace.Line('LIST', Format('Remove refused: the list holds at least %d items', [MinCount]));
    Exit;
  end;
  inherited Remove;
end;

end.
