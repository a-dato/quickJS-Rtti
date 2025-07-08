unit App.KeyNavigator.impl;

interface

uses
  System_,
  System.UITypes,
  FMX.Controls,
  FMX.Layouts,
  System.Classes,
  App.KeyNavigator.intf,
  App.EditorManager.intf;

type
  TKeyNavigator = class(TInterfacedObject, IKeyNavigator, ISelectorKeyNavigator)
  private
    [unsafe] _editorManager: IEditorManager; // avoids circulair lock

  public
    // IKeyNavigator
    function  TryHandleKeyNavigation(var Key: Word; Shift: TShiftState; const Current: TControl; ChangeFocus: Boolean = True): Boolean;
    function  GetControl: TControl;
    function  InnerNavigationKeys: TArray<Word>;
    function  GetScrollControl: TCustomScrollBox;
    function  NavigatorType: TNavigatorType;
    function  SetFocusIfValid(const ForceControl: TControl = nil): Boolean;

  public
    constructor Create(const EditorManager: IEditorManager); reintroduce;

    procedure SelectFirstAvailable;
  end;

implementation

uses
  System.Generics.Collections,
  FMX.ControlCalculations,
  FMX.Types,
  FMX.Text, App.EditorPanel.intf;

{ TKeyNavigator }

constructor TKeyNavigator.Create(const EditorManager: IEditorManager);
begin
  inherited Create;

  _editorManager := EditorManager;
end;

function TKeyNavigator.GetControl: TControl;
begin
  Result := _editorManager.ParentControl;
end;

function TKeyNavigator.GetScrollControl: TCustomScrollBox;
begin
  var parentCtrl := _editorManager.ParentControl;
  while (parentCtrl <> nil) and not (parentCtrl is TCustomScrollBox) do
    parentCtrl := parentCtrl.ParentControl;

  if parentCtrl <> nil then
    Result := parentCtrl as TCustomScrollBox else
    Result := nil;
end;

function TKeyNavigator.InnerNavigationKeys: TArray<Word>;
begin
  Result := [vkTab, vkUp, vkDown, vkHome, vkEnd, vkPrior, vkNext];
end;

function TKeyNavigator.NavigatorType: TNavigatorType;
begin
  Result := TNavigatorType.EditorNavigator;
end;

procedure TKeyNavigator.SelectFirstAvailable;
begin
  for var child in _editorManager.EditorPanels do
  begin
    if child.Panel.Visible and child.Panel.Enabled then
    begin
      child.Panel.Editor.SetFocus;
      Break;
    end;
  end;
end;

function TKeyNavigator.SetFocusIfValid(const ForceControl: TControl = nil): Boolean;
begin
  Result := ForceControl <> nil;
  if ForceControl <> nil then
  begin
    ForceControl.SetFocus;

    var ta: ITextActions;
    if Interfaces.Supports<ITextActions>(ForceControl, ta) then
      TThread.ForceQueue(nil, procedure
      begin
        ta.SelectAll;
        ta.GoToTextEnd;
      end);
  end;
end;

function TKeyNavigator.TryHandleKeyNavigation(var Key: Word; Shift: TShiftState; const Current: TControl; ChangeFocus: Boolean = True): Boolean;

  function FindFocusable(Control: TControl): TControl;
  begin
    if (Control <> nil) and Control.CanFocus then
      Exit(Control);

    for var I := 0 to Control.ControlsCount - 1 do
    begin
      var subControl := FindFocusable(Control.Controls[I]);
      if subControl <> nil then
        Exit(subControl);
    end;

    Result := nil;
  end;

begin
  Result := False;
  if not TArray.Contains<Word>(InnerNavigationKeys, Key) or not ChangeFocus then
    Exit(False);

  var useKey := Key;
  if Key = vkTab then
  begin
    if ssShift in Shift then
      useKey := vkUp else
      useKey := vkDown;
  end;

  {$IFDEF NOT_WORKING}
  var ctrl: TControl := nil;

  if (useKey in [vkUp, vkDown]) then
  begin
    ctrl := Current;
    while (ctrl <> nil) and not (ctrl is IEditorPanel) do
      ctrl := ctrl.ParentControl;
  end;

  var hasPanel := False;
  for var pnl in _editorManager.EditorPanels do
    if pnl.Panel = ctrl then
    begin
      hasPanel := True;
      Break;
    end;

  // check if the current selected is part of this EditorManager
  if (ctrl <> nil) and not hasPanel then
    ctrl := nil;

  var newCtrl: IEditorPanel := nil;

  var panelCount := _editorManager.EditorPanels.Count;

  if (useKey in [vkHome, vkEnd]) and (panelCount > 0) then
  begin
    if useKey = vkHome then
      newCtrl := _editorManager.EditorPanels[0].Panel
    else if useKey = vkEnd then
      newCtrl := _editorManager.EditorPanels[panelCount - 1].Panel;
  end
  else begin
    for var child in _editorManager.EditorPanels do
    begin
      if (child.Panel = ctrl) or not child.Panel.Visible or not child.Panel.Enabled or not child.Panel.Editor.Visible or not child.Panel.Editor.Enabled then
        Continue;

      if (useKey in [vkUp, vkPrior]) and ((ctrl = nil) or (child.Panel.Position.Y < ctrl.Position.Y)) then
      begin
        if (newCtrl = nil) or ((useKey = vkPrior) = (child.Panel.Position.Y < newCtrl.Position.Y)) then
          newCtrl := child.Panel;
      end
      else if (useKey in [vkDown, vkNext]) and ((ctrl = nil) or (child.Panel.Position.Y > ctrl.Position.Y)) then
      begin
        if (newCtrl = nil) or ((useKey = vkNext) = (child.Panel.Position.Y > newCtrl.Position.Y)) then
          newCtrl := child.Panel;
      end;
    end;
  end;

  if newCtrl <> nil then
  begin
    if Current = newCtrl.Editor then
      Exit(False);

    // TODO: Instead of checking for datetime think it would be nice to know if editor is more complex control
    // and we're able to tab through subcontrols without checking for specific editor.
    if (newCtrl.Editor is TADatoDateTimeEdit) then
    begin
      var editor := newCtrl.Editor as TADatoDateTimeEdit;
      var tablist := editor.TabListControls;
      var tablistItem: IControl;

      if useKey = vkUp then
      begin
        for var I := High(tablist) downto Low(tablist) do
        begin
          if tablist[i].Visible then
          begin
            tablistItem := tablist[i];
            Break;
          end;
        end;
      end
      else
      begin
        for var I := Low(tablist) to High(tablist) do
        begin
          if tablist[i].Visible then
          begin
            tablistItem := tablist[i];
            Break;
          end;
        end;
      end;

      if tablistItem <> nil then
        tablistItem.SetFocus;

    end else
    begin
      var focusableControl := FindFocusable(newCtrl.Editor);
      if focusableControl <> nil then
        focusableControl.SetFocus;
    end;

    ScrollControlInView(newCtrl, GetScrollControl);

    Exit(True);
  end;
  {$ENDIF}
end;

end.

