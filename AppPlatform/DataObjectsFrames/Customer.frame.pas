unit Customer.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.DataControl.ScrollableControl,
  FMX.DataControl.ScrollableRowControl, FMX.DataControl.Static,
  FMX.DataControl.Editable, FMX.DataControl.Impl, FMX.Edit, FMX.TabControl,
  System.Actions, FMX.ActnList;

type
  TCustomerFrame = class(TFrame)
    ICustomer_Model: TDataControl;
    Layout1: TLayout;
    Label1: TLabel;
    ICustomer_Name_1: TLabel;
    Splitter1: TSplitter;
    TabPages: TTabControl;
    tbCustomfields: TTabItem;
    tbGeneral: TTabItem;
    Label2: TLabel;
    ICustomer_Name_2: TEdit;
    Layout2: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    acOk: TAction;
    Label3: TLabel;
    ICustomer_Age_1: TEdit;
    procedure acOkExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TCustomerFrame.acOkExecute(Sender: TObject);
begin
  // Close;
end;

end.
