unit ProjectSelection.frame;

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
  TProjectSelectionFrame = class(TFrame)
    IProject_Model: TDataControl;
    Layout1: TLayout;
    Label1: TLabel;
    IProject_Name_1: TLabel;
    Layout2: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    acOk: TAction;
    Button2: TButton;
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

procedure TProjectSelectionFrame.acOkExecute(Sender: TObject);
begin
  // Close;
end;

constructor TProjectSelectionFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TProjectSelectionFrame.Button2Click(Sender: TObject);
begin
  for var s in IProject_Model.SelectedItems do
    ShowMessage(s.ToString);
end;

end.

