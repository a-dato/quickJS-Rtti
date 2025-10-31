unit ObjectDesigner;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  App.intf, System_,
  App.PropertyDescriptor.intf, FMX.ScrollControl.Impl,
  FMX.ScrollControl.WithRows.Impl, FMX.ScrollControl.WithCells.Impl,
  FMX.ScrollControl.WithEditableCells.Impl, FMX.ScrollControl.DataControl.Impl,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ScrollControl.Events;

type
  TObjectDesignerForm = class(TForm)
    Layout1: TLayout;
    ObjectTypes: TDataControl;
    ObjectProperties: TDataControl;
    Button1: TButton;
    Button2: TButton;
    lyTypes: TLayout;
    Label1: TLabel;
    Splitter1: TSplitter;
    lyProperties: TLayout;
    Label2: TLabel;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ObjectTypesCellChanged(const Sender: TObject; e: DCCellChangedEventArgs);
    procedure ObjectTypesCellFormatting(const Sender: TObject; e: DCCellFormattingEventArgs);
  private
    _app: IAppObject;

  protected
    _selected: &Type;

    function GetDescriptor(const AType: &Type) : IPropertyDescriptor;
    procedure LoadPropertyEditor;
  public
    procedure Load(const App: IAppObject);

    property Selected: &Type read _selected;
  end;

implementation

uses
  System.Collections, System.ClassHelpers,
  ADato.ObjectModel.List.intf,
  ADato.ObjectModel.List.Tracking.impl,
  System.Collections.Generic,
  ADato.Extensions.intf,
  App.TypeDescriptor.intf,
  App.TypeDescriptor.impl;

{$R *.fmx}

function TObjectDesignerForm.GetDescriptor(const AType: &Type) : IPropertyDescriptor;
begin
  var ot := _app.Config.TypeDescriptor(AType);
  if ot <> nil then
    Result := ot.PropertyDescriptor[AType.Name];
end;

procedure TObjectDesignerForm.Button1Click(Sender: TObject);
begin
  var customerType := _app.Config.TypeByName('Customer');
  _app.Config.AddProperty(_selected, 'Customer', 'Customer', customerType, GetDescriptor(customerType));

  var addressType := _app.Config.TypeByName('Address');
  _app.Config.AddProperty(_selected, 'Customer.Address', 'Customer address', addressType, GetDescriptor(addressType));

  LoadPropertyEditor;
end;

procedure TObjectDesignerForm.Button2Click(Sender: TObject);
begin
  var sfAccountType := _app.Config.TypeByName('SFAccount');
  _app.Config.AddProperty(_selected, 'SFAccount', 'Salesforce', sfAccountType, GetDescriptor(sfAccountType));
end;

procedure TObjectDesignerForm.Button3Click(Sender: TObject);
begin
  var customerType := _app.Config.TypeByName('Customer');

  // IList<customerType>

  // _app.Config.AddProperty(_selected, 'Customers', 'Customers', &Type.From<IList>, GetDescriptor(customerType));

  // TCollectionPropertyDescriptor
  _app.Config.AddProperty(_selected, 'Customers', 'Customers', &Type.From<IList>, GetDescriptor(customerType));
end;

{ TObjectDesignerForm }

procedure TObjectDesignerForm.Load(const App: IAppObject);
begin
  _app := App;

  var model: IObjectListModel := TObjectListModelWithChangeTracking<&Type>.Create(nil);
  model.Context := _app.Config.Types as IList;

  ObjectTypes.Model := model;
end;

procedure TObjectDesignerForm.ObjectTypesCellChanged(const Sender: TObject; e: DCCellChangedEventArgs);
begin
  if e.NewCell = nil then
  begin
    ObjectProperties.Model := nil;
    Exit;
  end
  else if CObject.Equals(e.NewCell.Row.DataItem, _selected) then
    Exit;

  _selected := e.NewCell.Row.DataItem.AsType<&Type>;

  LoadPropertyEditor;
end;

procedure TObjectDesignerForm.LoadPropertyEditor;
begin
  var propertymodel: IObjectListModel := TObjectListModelWithChangeTracking<_PropertyInfo>.Create(nil);

  //var objectType := _app.Config.ObjectType[_selected];

  var props: List<_PropertyInfo> := CList<_PropertyInfo>.Create();

  for var prop in _selected.GetProperties do
    props.Add(prop);

  propertymodel.Context := props as IList;

  ObjectProperties.Model := propertymodel;
end;

procedure TObjectDesignerForm.ObjectTypesCellFormatting(const Sender: TObject;
    e: DCCellFormattingEventArgs);
begin
  if CObject.Equals(e.Cell.Column.Tag, 'name') then
    e.Value := e.Cell.Row.DataItem.AsType<&Type>.Name;
end;

end.

