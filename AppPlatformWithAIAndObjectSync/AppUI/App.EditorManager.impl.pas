unit App.EditorManager.impl;

interface

uses
  System_,
  System.Collections.Generic,
  System.Collections,
  System.SysUtils,

  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  // LynxX.Controls.EditorPanel,
  // LynxX.Controls.Rectangle,

  ADato.ObjectModel.intf,
  ADato.ObjectModel.List.intf,
  System.Classes,
  App.KeyNavigator.intf,
  App.EditorPanel.intf,
  App.UIStateDelegate,
  App.EditorManager.intf;

type
  TEditorManager = class(TInterfacedObject, IEditorManager)
  protected
    _parent: TControl;
    _editorPanels: List<TEditorPanelWithPropertyBinding>;
    _keyNavigator: IKeyNavigator;
    _beforeRealigning: TBeforeRealign;
    _seperators: Dictionary<IEditorPanel, TControl>;

    procedure set_checkPanelVisibilityOnPropertyChanged(const Value: CString);
    function  get_ParentControl: TControl;
    procedure set_ParentControl(const Value: TControl);
    function  get_EditorPanels: List<TEditorPanelWithPropertyBinding>;
    function  get_KeyNavigator: IKeyNavigator;
    procedure set_BeforeRealigning(const Value: TBeforeRealign);

    function GetUsableMultiEditProperties: List<CString>; virtual;

    function FindAdditionalControl(const PanelAbove: IEditorPanel): TControl;
    function FindSeperatorControl(const PanelBelow: IEditorPanel): TControl;

  protected
    [weak] _omContext: IObjectModelContext;
    [weak] _omList: IObjectListModel;

    _realignIndex: Integer;
    _realigning: Boolean;
    _checkPanelVisibilityOnPropertyChanged: CString;

    _additionalControls: Dictionary<IEditorPanel, TControl>;

    procedure DoRealign; virtual;
    procedure OnPanelRealigned;

    procedure BeginRealign;
    procedure EndRealign;

    procedure CreateBindingsForType; virtual;
    procedure OnContextChanged(const Sender: IObjectModelContext; const Context: CObject); virtual;
    procedure OnPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
    procedure ApplyStateChanged(const ApplyState: TApplyState);

    procedure InternalLink(const ABinding: IPropertyBinding; const Links: StringArray);
    procedure OnMultiSelectChanged(const Sender: IObjectListModel; const Context: IList);

  public
    constructor Create(const Parent: TControl); reintroduce;
    destructor Destroy; override;

    procedure AddTitleBinding(const PropertyName: CString; const EditorPanel: IEditorPanel; const Links: StringArray = []);
    function  AddEditorBinding(const PropertyName: CString; const EditorPanel: IEditorPanel; const Links: StringArray = []): IPropertyBinding; overload; virtual;
    function  AddEditorBinding(const AProperty: _PropertyInfo; const EditorPanel: IEditorPanel; const Links: StringArray = []): IPropertyBinding; overload; virtual;
    function  AddControlBinding(const PropertyName: CString; const Control: TControl; Position: TControlPosition; const Links: StringArray = []): IPropertyBinding;
    procedure AddControl(const Control: TControl; Position: TControlPosition; const EditorPanel: IEditorPanel);
    procedure AddControlToParent(const Control: TControl; const EditorPanelAbove: IEditorPanel);
    procedure AddEditor(const EditorPanel: IEditorPanel; const Binding: IPropertyBinding = nil);

    procedure ShowSeperationLine(const EditorPanelBelow: IEditorPanel; MakeVisible: Boolean);

    procedure Bind(const ObjectModelContext: IObjectModelContext); overload;
    procedure Bind(const ObjectListModel: IObjectListModel); overload;
    procedure Unbind; virtual;

    procedure ForceRealign(Immidiate: Boolean = True);

    procedure CheckPanelVisiblity(const Context: CObject); virtual;

    function PanelByTitle(const Title: CString): TEditorPanelWithPropertyBinding;

    property CheckPanelVisibilityOnPropertyChanged: CString write set_checkPanelVisibilityOnPropertyChanged;
    property ParentControl: TControl read get_ParentControl write set_ParentControl;
    property EditorPanels: List<TEditorPanelWithPropertyBinding> read get_EditorPanels;
    property BeforeRealigning: TBeforeRealign write set_beforeRealigning;
  end;

  TEditorManagersGlue = class(TBaseInterfacedObject, IEditorManagersGlue)
  private
    _editorManagers: TEditorManagerArr;

    procedure OnEditorManagerRealigned(ParentControl: TControl);

    procedure set_EditorManagers(const Value: TEditorManagerArr);
  public
    destructor Destroy; override;

    procedure Glue(ParentControl: TControl = nil);

    property EditorManagers: TEditorManagerArr write set_EditorManagers;
  end;

const
  PADDING_TOPBOTTOM = 15.0;
  PADDING_LEFTRIGHT = 20.0;
  MARGIN_BETWEEN = 5;

implementation

uses
  ADato.ObjectModel.Binders, System.Types, App.KeyNavigator.impl;
//  ADato.InterfaceWithID.Intf,
//  ADato.PropertyAccessibility.Intf,
//  System.UITypes, FMX.Text, FMX.ControlCalculations,
//  System.Generics.Collections,
//  FMX.StdCtrls, FMX.Memo,
//  ADato.Presenter.CustomProperties.intf, FMX.Objects, System.Math;

{ TEditorManager }

constructor TEditorManager.Create(const Parent: TControl);
begin
  inherited Create;

  _editorPanels := CList<TEditorPanelWithPropertyBinding>.Create;

  set_ParentControl(Parent);
  _checkPanelVisibilityOnPropertyChanged := nil;
end;

procedure TEditorManager.CreateBindingsForType;
begin
  // nothing to do here
end;

procedure TEditorManager.ShowSeperationLine(const EditorPanelBelow: IEditorPanel; MakeVisible: Boolean);
begin
  var seperatorLine := FindSeperatorControl(EditorPanelBelow);
  if (seperatorLine = nil) and MakeVisible then
  begin
    if _seperators = nil then
      _seperators := CDictionary<IEditorPanel, TControl>.Create;

//    var line := TADatoLine.Create(nil);
//    line.Align := TAlignLayout.Top;
//    line.Height := 1;
//    line.Margins.Top := 10;
//    line.Margins.Bottom := 10;
//    line.LineType := TLineType.Top;
//    _parent.AddObject(line);
//
//    _seperators.Add(EditorPanelBelow, line);
//
//    seperatorLine := line;
  end;

  if (seperatorLine <> nil) then
    seperatorLine.Visible := MakeVisible;
end;

destructor TEditorManager.Destroy;
begin
  Unbind;

  inherited;
end;

procedure TEditorManager.InternalLink(const ABinding: IPropertyBinding; const Links: StringArray);
begin
  for var link in Links do
    _omContext.Link(link, ABinding);
end;

procedure TEditorManager.AddControl(const Control: TControl; Position: TControlPosition; const EditorPanel: IEditorPanel);
begin
  // most of the time it is the latest added panel
  for var pnlInfoIx := _editorPanels.Count - 1 downto 0 do
  begin
    var pnlInfo := _editorPanels[pnlInfoIx];
    if pnlInfo.Panel = EditorPanel then
    begin
      _editorPanels[pnlInfoIx] := pnlInfo.AddControl(Control, Position);

      if Control.Parent <> _parent then
      begin
        Control.Align := TAlignLayout.Top;
        _parent.AddObject(Control);
      end;

      OnPanelRealigned;
      Exit;
    end;
  end;
end;

function TEditorManager.AddControlBinding(const PropertyName: CString; const Control: TControl; Position: TControlPosition; const Links: StringArray): IPropertyBinding;
begin
  Assert(_omContext <> nil);
  Assert(Control <> nil);

  Result := TPropertyBinding.CreateBindingByControl(Control);
  _omContext.Bind(PropertyName, Result);

  InternalLink(Result, Links);

  // most of the time it is the latest added panel
  for var pnlInfoIx := _editorPanels.Count - 1 downto 0 do
  begin
    var pnlInfo := _editorPanels[pnlInfoIx];
    if CString.Equals(pnlInfo.Binding.PropertyInfo.Name, PropertyName) then
    begin
      _editorPanels[pnlInfoIx] := pnlInfo.AddControl(Control, Position);
      OnPanelRealigned;
      Exit;
    end;
  end;
end;

procedure TEditorManager.AddEditor(const EditorPanel: IEditorPanel; const Binding: IPropertyBinding = nil);
begin
  Assert(EditorPanel <> nil);

  var alignable: IAlignableObject;
  if Interfaces.Supports<IAlignableObject>(EditorPanel, alignable) then
    alignable.Margins.Rect := TRectF.Empty;
  EditorPanel.OnRealigned := OnPanelRealigned;
  EditorPanel.Order := _editorPanels.Count;

  _editorPanels.Add(TEditorPanelWithPropertyBinding.Create(EditorPanel, Binding));

  // set Height of parent panel
  OnPanelRealigned;
end;

function TEditorManager.AddEditorBinding(const PropertyName: CString; const EditorPanel: IEditorPanel; const Links: StringArray = []): IPropertyBinding;
begin
  Assert(_omContext <> nil);
  Assert(EditorPanel <> nil);

  Result := TPropertyBinding.CreateBindingByControl(EditorPanel.Editor);
  _omContext.Bind(PropertyName, Result);

  InternalLink(Result, Links);
  AddEditor(EditorPanel, Result);
end;

function TEditorManager.AddEditorBinding(const AProperty: _PropertyInfo; const EditorPanel: IEditorPanel; const Links: StringArray = []): IPropertyBinding;
begin
  Assert(_omContext <> nil);
  Assert(EditorPanel <> nil);

  Result := TPropertyBinding.CreateBindingByControl(EditorPanel.Editor);
  _omContext.Bind(AProperty, Result);

  InternalLink(Result, Links);
  AddEditor(EditorPanel, Result);
end;

procedure TEditorManager.AddControlToParent(const Control: TControl; const EditorPanelAbove: IEditorPanel);
begin
  if (EditorPanelAbove = nil) or (Control = nil) then
    Exit;

  if _additionalControls = nil then
    _additionalControls := CDictionary<IEditorPanel, TControl>.Create;

  _parent.AddObject(Control);
  _parent.Align := TAlignLayout.Top;
  _additionalControls.Add(EditorPanelAbove, Control);
end;

procedure TEditorManager.AddTitleBinding(const PropertyName: CString; const EditorPanel: IEditorPanel; const Links: StringArray = []);
begin
  Assert(_omContext <> nil);

  var binding := TPropertyBinding.CreateBindingByControl(EditorPanel.TitleControl);
  _omContext.Bind(PropertyName, binding);

  InternalLink(binding, Links);
end;

procedure TEditorManager.ApplyStateChanged(const ApplyState: TApplyState);
begin
  if Applystate in [TApplyState.CancelSucceeded, TApplyState.ApplyException, TApplyState.CanceledByExternal] then
    CheckPanelVisiblity(_omContext.Context);
end;

function TEditorManager.FindAdditionalControl(const PanelAbove: IEditorPanel): TControl;
begin
  if (_additionalControls = nil) or not _additionalControls.TryGetValue(PanelAbove, Result) then
    Result := nil;
end;

function TEditorManager.FindSeperatorControl(const PanelBelow: IEditorPanel): TControl;
begin
  if (_seperators = nil) or not _seperators.TryGetValue(PanelBelow, Result) then
    Result := nil;
end;

procedure TEditorManager.DoRealign;
begin
  Assert(not _realigning);

  if Assigned(_beforeRealigning) then
    _beforeRealigning(_parent);

  BeginRealign;
  try
    var pos := _parent.Padding.Top  - 1 {-1 is to make sure a item is not positioned at same position as control below, so they get mixed up in order};

    for var ix := 0 to _editorPanels.Count - 1 do
    begin
      var pnl := _editorPanels[ix];

      var line := FindSeperatorControl(pnl.Panel);
      if (line <> nil) and line.Visible then
      begin
        line.Position.Y := pos;
        pos := pos + line.Margins.Top + line.Height + line.Margins.Bottom;
      end;

      var ctrl: IControl;
      var alignable: IAlignableObject;
      if Interfaces.Supports<IControl>(pnl.Panel, ctrl) and Interfaces.Supports<IAlignableObject>(pnl.Panel, alignable) then
      begin
        if ctrl.Visible then
        begin
          if (ix > 0) and (line = nil) then
            alignable.Margins.Top := MARGIN_BETWEEN else
            alignable.Margins.Top := 0;

          pos := pos + alignable.Margins.Top {can be 0};
        end;

        for var ctrlInfo in pnl.AdditionalControls do
          if (ctrlInfo.Position = TControlPosition.Above) and ctrlInfo.Control.Visible then
          begin
            ctrlInfo.Control.Position.Y := pos;
            pos := pos + ctrlInfo.Control.Height + ctrlInfo.Control.Margins.Top + ctrlInfo.Control.Margins.Bottom;
          end;

        if ctrl.Visible then
        begin
          alignable.SetBounds(alignable.Left, pos, alignable.Width, alignable.Height);
          pos := pos + alignable.Height;
        end;
      end;

      for var ctrlInfo in pnl.AdditionalControls do
        if (ctrlInfo.Position = TControlPosition.Below) and ctrlInfo.Control.Visible then
        begin
          ctrlInfo.Control.Position.Y := pos;
          ctrlInfo.Control.Margins.Left := pnl.Panel.Editor.Position.X;
          ctrlInfo.Control.Width := pnl.Panel.Editor.Width;

          pos := pos + ctrlInfo.Control.Height + ctrlInfo.Control.Margins.Top + ctrlInfo.Control.Margins.Bottom;
        end;

      var additional := FindAdditionalControl(pnl.Panel);
      if (additional <> nil) and additional.Visible then
      begin
        additional.Position.Y := pos;
        pos := pos + additional.Height + additional.Margins.Top + additional.Margins.Bottom;
      end;
    end;

    _parent.Height := pos + _parent.Padding.Bottom + 1;
  finally
    EndRealign;
  end;
end;

procedure TEditorManager.ForceRealign(Immidiate: Boolean = True);
begin
  if Immidiate then
    DoRealign else
    OnPanelRealigned;
end;

procedure TEditorManager.OnPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
begin
  var doCheckPanelVisibility: Boolean := False;

//  var propEditabilityCache: IPropertyAccessibility;
//  if Context.TryAsType<IPropertyAccessibility>(propEditabilityCache) then
//  begin
//    var prop := AProperty as IObjectModelProperty;
//    for var b in prop.Bindings do
//      if prop.IsLink(b) then
//      begin
//        propEditabilityCache.ClearEditablity(b.PropertyInfo.Name);
//        doCheckPanelVisibility := True;
//      end;
//  end;

  var prop := _checkPanelVisibilityOnPropertyChanged;
  if not CString.IsNullOrEmpty(prop) and CString.Equals(prop, AProperty.Name) then
    doCheckPanelVisibility := True;

  if doCheckPanelVisibility then
    CheckPanelVisiblity(Context);
end;

function TEditorManager.PanelByTitle(const Title: CString): TEditorPanelWithPropertyBinding;
begin
  for var pnlPair in _editorPanels do
    if CString.Equals(Title, pnlPair.Binding.PropertyInfo.Name) then
      Exit(pnlPair);

  Result := TEditorPanelWithPropertyBinding.Create(nil, nil);
end;

function TEditorManager.get_EditorPanels: List<TEditorPanelWithPropertyBinding>;
begin
  Result := _editorPanels;
end;

function TEditorManager.get_KeyNavigator: IKeyNavigator;
begin
  if _keyNavigator = nil then
    _keyNavigator := TKeyNavigator.Create(Self);

  Result := _keyNavigator;
end;

function TEditorManager.get_ParentControl: TControl;
begin
  Result := _parent;
end;

procedure TEditorManager.set_checkPanelVisibilityOnPropertyChanged( const Value: CString);
begin
  _checkPanelVisibilityOnPropertyChanged := Value;
end;

procedure TEditorManager.set_BeforeRealigning(const Value: TBeforeRealign);
begin
  _BeforeRealigning := Value;
end;

procedure TEditorManager.set_ParentControl(const Value: TControl);
begin
  if _parent <> Value then
  begin
    _parent := Value;
    _parent.Padding.Top := PADDING_TOPBOTTOM;
    _parent.Padding.Bottom := PADDING_TOPBOTTOM;
    _parent.Padding.Left := PADDING_LEFTRIGHT;
    _parent.Padding.Right := PADDING_LEFTRIGHT;
  end;
end;

procedure TEditorManager.OnContextChanged(const Sender: IObjectModelContext; const Context: CObject);
begin
  CheckPanelVisiblity(Context);
end;

procedure TEditorManager.OnMultiSelectChanged(const Sender: IObjectListModel; const Context: IList);
begin
  CheckPanelVisiblity(Sender.ObjectContext);
end;

function TEditorManager.GetUsableMultiEditProperties: List<CString>;
begin
  // all properties are the same in this multiselect, for there is one item max
  Result := nil;
end;

procedure TEditorManager.CheckPanelVisiblity(const Context: CObject);
//var
//  presenter: IPropertyAccessibility;
//  editability: TEditableState;
begin
  {$IFDEF NOT_WORKING}
  if Context <> nil then
    Context.TryAsType<IPropertyAccessibility>(presenter);

  var multiEditableProperties := GetUsableMultiEditProperties;

  for var pair in _editorPanels do
  begin
    // Editor without binding
    if (pair.Binding = nil) then
      continue;

    var controlBinding := pair.Binding as IControlBinding;

    // multiselect is on, and the property is not editable for all multi selected
    if (multiEditableProperties <> nil) and not multiEditableProperties.Contains(pair.Binding.PropertyInfo.Name) then
      editability := TEditableState.Invisible
    else
    begin
      if presenter <> nil then
        editability := presenter.CanEditProperty(pair.Binding.PropertyInfo.Name) else
        editability := TEditableState.ReadOnly;

      // check if multiselect is on. In that case override every property visibility that is not multi editable
      if (_omList <> nil) and _omList.HasMultiSelection and ((editability <> TEditableState.MultiEditable) or not pair.Binding.PropertyInfo.CanWrite) then
        editability := TEditableState.Invisible;
    end;

    var visibilityCheck := editability.IsVisible;
    if pair.Panel.Visible <> visibilityCheck then
    begin
      pair.Panel.Visible := visibilityCheck;
      controlBinding.UpdateControlVisibility(visibilityCheck);
      OnPanelRealigned(pair.Panel);
    end;

    var editabilityCheck := editability.IsEditable or (pair.Panel.EditorType = TEditorType.Text);
    pair.Panel.TitleControl.Enabled := editabilityCheck;
    pair.Panel.Editor.Enabled := editabilityCheck;

    if controlBinding.Control.Enabled <> editabilityCheck then
      controlBinding.UpdateControlEditability(editabilityCheck);

    // pair.Key.Controls are the additional controls of the TEditPanel (so not Title nor EditCtrl)
    if pair.Panel.InnerControls.Count > 0 then
      for var i := 0 to pair.Panel.InnerControls.Count - 1 do
      begin
        var ctrl := pair.Panel.InnerControls.Items[i].Control;
        ctrl.Enabled := editabilityCheck;
      end;
  end;
  {$ENDIF}
end;

procedure TEditorManager.OnPanelRealigned;
begin
//  if _realigning then
//    Exit;

  inc(_realignIndex);
  var ix := _realignIndex;

  TThread.ForceQueue(nil, procedure
  begin
    if ix <>_realignIndex then
      Exit;

    DoRealign;
  end);
end;

procedure TEditorManager.BeginRealign;
begin
  Assert(not _realigning);

  _realigning := True;
  inc(_realignIndex);

  // inc(IEditorPanel._RealignIndex);
end;

procedure TEditorManager.Bind(const ObjectListModel: IObjectListModel);
begin
  _omList := ObjectListModel;
  _omList.MultiSelect.Delegate.Add(OnMultiSelectChanged);

  Bind(_omList.ObjectModelContext);
end;

procedure TEditorManager.EndRealign;
begin
  Assert(_realigning);

  // dec(IEditorPanel._RealignIndex);

  _realigning := False;
end;

procedure TEditorManager.Bind(const ObjectModelContext: IObjectModelContext);
begin
  Assert(ObjectModelContext <> nil);

  Assert(_omContext = nil);

  _omContext := ObjectModelContext;
  _omContext.OnContextChanged.Add(OnContextChanged);
  _omContext.OnPropertyChanged.Add(OnPropertyChanged);

  GlobalApplyStateDelegate.Add(ApplyStateChanged);

  CreateBindingsForType;

  if _omContext.Context <> nil then
    OnContextChanged(_omContext, _omContext.Context);
end;

procedure TEditorManager.Unbind;
begin
  inc(_realignIndex);

  if _additionalControls <> nil then
    _additionalControls.Clear;

  if _seperators <> nil then
  begin
    for var line in _seperators.Values do
      line.Free;

    _seperators.Clear;
  end;

  for var ix := 0 to _editorPanels.Count - 1 do
  begin
    var entry := _editorPanels[ix];
    entry.Panel.OnRealigned := nil;

    // if editor is assigned including a binding
    // check if omc already cleared by double property binding
    if (entry.Binding <> nil) then
      _omContext.Unbind(entry.Binding);
  end;

//  for var panel in _editorPanels.Keys do
//    panel.beforeRealigning := nil;

  _editorPanels.Clear;

  GlobalApplyStateDelegate.Remove(ApplyStateChanged);

  if _omContext <> nil then
  begin
    _omContext.OnPropertyChanged.Remove(OnPropertyChanged);
    _omContext.OnContextChanged.Remove(OnContextChanged);
    _omContext := nil;
  end;

  if _omList <> nil then
  begin
    _omList.MultiSelect.Delegate.Remove(OnMultiSelectChanged);
    _omList := nil;
  end;
end;

{ TEditorManagersGlue }

destructor TEditorManagersGlue.Destroy;
begin
  set_EditorManagers(nil);

  inherited;
end;

procedure TEditorManagersGlue.Glue(ParentControl: TControl = nil);
begin
  var startIx := -1;
  var stopIx: Integer;
  for var ix := 0 to High(_editorManagers) do
  begin
    var mgr := _editorManagers[ix];
    if mgr.ParentControl.Visible then
    begin
      if startIx = -1 then
        startIx := ix;

      stopIx := ix;
    end;
  end;

  {$IFDEF NOT_WORKING}
  for var ix := startIx to stopIx do
  begin
    var mgr := _editorManagers[ix];

    var sides: TSides := [TSide.Left, TSide.Right];
    var corners: TCorners := [];

    if ix = startIx then
    begin
      sides := sides + [TSide.Top];
      corners := corners + [TCorner.TopLeft, TCorner.TopRight];
    end;

    if ix = stopIx then
    begin
      sides := sides + [TSide.Bottom];
      corners := corners + [TCorner.BottomLeft, TCorner.BottomRight];
    end;

    if mgr.ParentControl is TRectangle then
    begin
      var rect := mgr.ParentControl as TRectangle;

      rect.Sides := sides;
      rect.Corners := corners;
    end;

    if mgr.EditorPanels.Count > 0 then
      mgr.ShowSeperationLine(mgr.EditorPanels[0].Panel, ix > startIx);

    mgr.ParentControl.Padding.Top := IfThen(ix = startIx, PADDING_TOPBOTTOM, 0);
    mgr.ParentControl.Padding.Bottom := IfThen(ix = stopIx, PADDING_TOPBOTTOM, 0);
    mgr.ParentControl.Margins.Top := IfThen(ix = startIx, mgr.ParentControl.Margins.Top, 0);
    mgr.ParentControl.Margins.Bottom := IfThen(ix = stopIx, mgr.ParentControl.Margins.Bottom, 0);

    if (ParentControl = nil) or (ParentControl <> mgr.ParentControl) then
    begin
      mgr.BeforeRealigning := nil;
      try
        mgr.ForceRealign(True);
      finally
        mgr.BeforeRealigning := OnEditorManagerRealigned;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TEditorManagersGlue.OnEditorManagerRealigned(ParentControl: TControl);
begin
  Glue(ParentControl);
end;

procedure TEditorManagersGlue.set_EditorManagers(const Value: TEditorManagerArr);
begin
  for var mgr in _editorManagers do
    mgr.BeforeRealigning := nil;

  _editorManagers := Value;

  for var mgr in _editorManagers do
    mgr.BeforeRealigning := OnEditorManagerRealigned;
end;

end.

