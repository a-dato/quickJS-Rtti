unit Project.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.ListBox, FMX.Edit, FMX.TabControl,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ScrollControl.Impl,
  FMX.ScrollControl.WithRows.Impl, FMX.ScrollControl.WithCells.Impl,
  FMX.ScrollControl.WithEditableCells.Impl, FMX.ScrollControl.DataControl.Impl,
  FMX.ScrollControl.Events, JSGeneral.frame, Customer.frame;

type
  TProjectFrame = class(TFrame)
    IProject_Model: TDataControl;
    Layout1: TLayout;
    Label1: TLabel;
    IProject_Name_1: TLabel;
    Splitter1: TSplitter;
    TabPages: TTabControl;
    Customers: TTabItem;
    tbGeneral: TTabItem;
    Label2: TLabel;
    IProject_Name_2: TEdit;
    Layout2: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    acOk: TAction;
    Label3: TLabel;
    IProject_Age_1: TEdit;
    Label4: TLabel;
    IProject_Customer_0: TComboBox;
    Label5: TLabel;
    IProject_Customer_Address_0: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    IProject_SFAccount_0: TComboBox;
    Button2: TButton;
    IProject_Customers_Model: TCustomerFrame;
    procedure acOkExecute(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  App.EditorManager.impl, System_;

{$R *.fmx}

procedure TProjectFrame.acOkExecute(Sender: TObject);
begin
  // Close;
end;

constructor TProjectFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TProjectFrame.Button2Click(Sender: TObject);
begin
  for var s in IProject_Model.SelectedItems do
    ShowMessage(s.ToString);
end;

end.

