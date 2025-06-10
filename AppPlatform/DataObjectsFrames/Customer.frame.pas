unit Customer.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.DataControl.ScrollableControl,
  FMX.DataControl.ScrollableRowControl, FMX.DataControl.Static,
  FMX.DataControl.Editable, FMX.DataControl.Impl, FMX.Edit, FMX.TabControl,
  System.Actions, FMX.ActnList, LynxX.Controls.EditorPanel,
  App.EditorManager.intf;

type
  TCustomerFrame = class(TFrame, IEditorManager)
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
    Customer_Name_0: TADatoEditorPanel;
    Customer_Name_1: TLabel;
    Customer_Name_2: TEdit;
    lyEditors: TLayout;

    procedure acOkExecute(Sender: TObject);
  private
    _EditorManager: IEditorManager;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    property EditorManager: IEditorManager read _EditorManager implements IEditorManager;
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
  _EditorManager := TEditorManager.Create(lyEditors);
end;

end.
