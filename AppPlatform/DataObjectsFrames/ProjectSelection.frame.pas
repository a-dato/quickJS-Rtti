unit ProjectSelection.frame;

interface

uses
  System_,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Collections.Generic,
  System.Actions,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ActnList, FMX.ListBox, FMX.Edit, FMX.TabControl,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ScrollControl.Impl,
  FMX.ScrollControl.WithRows.Impl, FMX.ScrollControl.WithCells.Impl,
  FMX.ScrollControl.WithEditableCells.Impl, FMX.ScrollControl.DataControl.Impl,
  FMX.ScrollControl.Events;

type
  TProjectSelectionFrame = class(TFrame)
    IProject_Model: TDataControl;
    Layout1: TLayout;
    Label1: TLabel;
    IProject_Description_1: TLabel;
    Layout2: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    acOk: TAction;
    procedure acOkExecute(Sender: TObject);
  private

  protected
    function  get_Selected: List<CObject>;
    procedure set_Selected(const Items: List<CObject>);
  public

    property Selected: List<CObject> read get_Selected write set_Selected;
  end;

implementation

uses
  App.EditorManager.impl, System.Collections;

{$R *.fmx}

procedure TProjectSelectionFrame.acOkExecute(Sender: TObject);
begin
  // Close;
end;

function TProjectSelectionFrame.get_Selected: List<CObject>;
begin
  Result := IProject_Model.SelectedItems;
end;

procedure TProjectSelectionFrame.set_Selected(const Items: List<CObject>);
begin
  IProject_Model.AssignSelection(Items as IList);
end;

end.

