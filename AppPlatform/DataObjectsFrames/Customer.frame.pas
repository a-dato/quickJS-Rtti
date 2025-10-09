unit Customer.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  App.EditorManager.intf, System.Actions, FMX.ActnList, FMX.Edit,
  FMX.TabControl, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ScrollControl.Impl, FMX.ScrollControl.WithRows.Impl,
  FMX.ScrollControl.WithCells.Impl, FMX.ScrollControl.WithEditableCells.Impl,
  FMX.ScrollControl.DataControl.Impl;

type
  TCustomerFrame = class(TFrame)
    Layout1: TLayout;
    Label1: TLabel;
    Splitter1: TSplitter;
    TabPages: TTabControl;
    tbCustomfields: TTabItem;
    tbGeneral: TTabItem;
    Label2: TLabel;
    Layout2: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    acOk: TAction;
    Label3: TLabel;

    Customer_Model: TDataControl;
    Customer_Age_1: TEdit;
    Customer_Name_1: TLabel;
    Customer_Name_2: TEdit;
    lyEditors: TLayout;

    procedure acOkExecute(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  App.EditorManager.impl;

{$R *.fmx}

procedure TCustomerFrame.acOkExecute(Sender: TObject);
begin
  // Close;
end;

constructor TCustomerFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

end.

