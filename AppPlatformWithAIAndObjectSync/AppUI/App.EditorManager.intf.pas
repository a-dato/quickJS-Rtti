unit App.EditorManager.intf;

interface

uses
  System_,
  System.Collections.Generic,
  System.Collections,
  System.SysUtils,
  System.Classes,

  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  // LynxX.Controls.EditorPanel,
  // LynxX.Controls.Rectangle,

  ADato.ObjectModel.intf,
  ADato.ObjectModel.List.intf,
  App.KeyNavigator.intf,
  App.EditorPanel.intf,
  App.UIStateDelegate;

type
  TControlPosition = (Above, Below);

  TAdditionalControl = record
  public
    Control: TControl;
    Position: TControlPosition;

    constructor Create(const AControl: TControl; const APosition: TControlPosition);
  end;

  TEditorPanelWithPropertyBinding = record
  public
    // binding can be double cleared, for double bindings can exist for same property of same ObjectModelContext.. Therefor must be WEAK
    [weak] Binding: IPropertyBinding;

    Panel: IEditorPanel;
    OrgHeight: Single;
    AdditionalControls: List<TAdditionalControl>;

    constructor Create(const APanel: IEditorPanel; const ABinding: IPropertyBinding);

    function AddControl(const Control: TControl; const ControlPosition: TControlPosition): TEditorPanelWithPropertyBinding;

    function IsEmpty: Boolean;
    function Order: Integer;
  end;

  TBeforeRealign = reference to procedure(ParentControl: TControl);

  IEditorManager = interface
    ['{340C7304-D20A-4548-B736-8B5E9A22E944}']
    function  get_ParentControl: TControl;
    procedure set_ParentControl(const Value: TControl);
    function  get_EditorPanels: List<TEditorPanelWithPropertyBinding>;
    procedure set_checkPanelVisibilityOnPropertyChanged(const Value: CString);
    function  get_KeyNavigator: IKeyNavigator;
    procedure set_BeforeRealigning(const Value: TBeforeRealign);

    procedure AddTitleBinding(const PropertyName: CString; const EditorPanel: IEditorPanel; const Links: StringArray = []);
    function  AddEditorBinding(const PropertyName: CString; const EditorPanel: IEditorPanel; const Links: StringArray = []): IPropertyBinding; overload;
    function  AddEditorBinding(const AProperty: _PropertyInfo; const EditorPanel: IEditorPanel; const Links: StringArray = []): IPropertyBinding; overload;
    function  AddControlBinding(const PropertyName: CString; const Control: TControl; Position: TControlPosition; const Links: StringArray = []): IPropertyBinding;
    procedure AddControl(const Control: TControl; Position: TControlPosition; const EditorPanel: IEditorPanel);
    procedure AddControlToParent(const Control: TControl; const EditorPanelAbove: IEditorPanel);
    procedure AddEditor(const EditorPanel: IEditorPanel; const Binding: IPropertyBinding = nil);

    procedure ShowSeperationLine(const EditorPanelBelow: IEditorPanel; MakeVisible: Boolean);

    procedure Bind(const ObjectModelContext: IObjectModelContext); overload;
    procedure Bind(const ObjectListModel: IObjectListModel); overload;
    procedure Unbind;

    procedure ForceRealign(Immidiate: Boolean = True);
    procedure CheckPanelVisiblity(const Context: CObject);

    function PanelByTitle(const Title: CString): TEditorPanelWithPropertyBinding;

    property CheckPanelVisibilityOnPropertyChanged: CString write set_checkPanelVisibilityOnPropertyChanged;
    property ParentControl: TControl read get_ParentControl write set_ParentControl;
    property EditorPanels: List<TEditorPanelWithPropertyBinding> read get_EditorPanels;
    property KeyNavigator: IKeyNavigator read get_KeyNavigator;

    property BeforeRealigning: TBeforeRealign write set_BeforeRealigning;
  end;

  TEditorManagerArr = array of IEditorManager;

  IEditorManagersGlue = interface(IBaseInterface)
    ['{9AEED7CB-E1B7-48DA-8826-06AFDCC593D7}']
    procedure set_EditorManagers(const Value: TEditorManagerArr);

    procedure Glue(ParentControl: TControl = nil);

    property EditorManagers: TEditorManagerArr write set_EditorManagers;
  end;

const
  PADDING_TOPBOTTOM = 15.0;
  PADDING_LEFTRIGHT = 20.0;
  MARGIN_BETWEEN = 5;

implementation

{ TEditorPanelWithPropertyBinding }

function TEditorPanelWithPropertyBinding.AddControl(const Control: TControl; const ControlPosition: TControlPosition): TEditorPanelWithPropertyBinding;
begin
  Result.Panel := Panel;
  Result.Binding := Binding;
  Result.OrgHeight := OrgHeight;

  var l := AdditionalControls;
  l.Add(TAdditionalControl.Create(Control, ControlPosition));

  Result.AdditionalControls := l;
end;

constructor TEditorPanelWithPropertyBinding.Create(const APanel: IEditorPanel; const ABinding: IPropertyBinding);
begin
  Panel := APanel;
  Binding := ABinding;

  var alignable: IAlignableObject;
  if Interfaces.Supports<IAlignableObject>(APanel, alignable) then
    OrgHeight := alignable.Height else
    OrgHeight := 0;

  AdditionalControls := CList<TAdditionalControl>.Create;
end;

function TEditorPanelWithPropertyBinding.IsEmpty: Boolean;
begin
  Result := Panel = nil;
end;

function TEditorPanelWithPropertyBinding.Order: Integer;
begin
  Result := Panel.Order;
end;

{ TAdditionalControl }

constructor TAdditionalControl.Create(const AControl: TControl; const APosition: TControlPosition);
begin
  Control := AControl;
  Position := APosition;
end;

end.

