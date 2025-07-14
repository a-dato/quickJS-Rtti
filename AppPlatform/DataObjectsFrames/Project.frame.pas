unit Project.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.ListBox, FMX.Edit, FMX.TabControl,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ScrollControl.Impl,
  FMX.ScrollControl.WithRows.Impl, FMX.ScrollControl.WithCells.Impl,
  FMX.ScrollControl.WithEditableCells.Impl, FMX.ScrollControl.DataControl.Impl,
  FMX.ScrollControl.Events,
  app.EditorManager.intf;

type
  TProjectFrame = class(TFrame, IEditorManager)
    IProject_Model: TDataControl;
    Layout1: TLayout;
    Label1: TLabel;
    IProject_Name_1: TLabel;
    Splitter1: TSplitter;
    TabPages: TTabControl;
    tbCustomfields: TTabItem;
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
    procedure acOkExecute(Sender: TObject);
    procedure IProject_ModelCellFormatting(const Sender: TObject; e:
        DCCellFormattingEventArgs);
  private
    _EditorManager: IEditorManager;
  public
    constructor Create(AOwner: TComponent); override;

    property EditorManager: IEditorManager read _EditorManager implements IEditorManager;
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
  // _EditorManager := TEditorManager.Create(lyEditors);
end;

procedure TProjectFrame.IProject_ModelCellFormatting(const Sender: TObject; e: DCCellFormattingEventArgs);
begin
//  var js_obj: JSObjectReference;
//
//  if e.Value.TryGetValue<JSObjectReference>(js_obj) then
//  begin
//    e.Value := js_obj.Invoke<CString>('Name');
//    e.FormattingApplied := True;
//  end;
end;

end.

