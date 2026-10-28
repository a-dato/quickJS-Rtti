unit AppMain;

// AppPlatform Studio: the main window.
//
//   +---------------------------------------------------------------+
//   | AppPlatform Studio        [Build an app with AI]  AI server ● |
//   +-----------+---------------------------------------------------+
//   | Types     | Customer                                          |
//   |  IProject | Storage 'Customers' · 100 objects · 4 properties  |
//   |  Customer | [Objects] [Properties]                            |
//   |  Address  |  grid of the objects        | selected object     |
//   |           |                             |  property editors   |
//   +-----------+---------------------------------------------------+
//   | [Log] [Script]                                                |
//   +---------------------------------------------------------------+
//
// The inspector reads and writes the selected object through the shared object
// model (AppPlatform.AppSync.ModelFor), like the live objects of app pages: an
// edit here reaches every open app page showing the object, and an edit in a
// page shows up here. "Open in app" opens an app with the selected object as its
// param (page.param('project') in the app's host block).
//
// Everything is generated from the platform's type metadata (Studio.Model), so
// a type registered from JavaScript, or a property added at runtime, shows up
// like a Delphi one. The window refreshes when it gets focus again, so changes
// made by AI-built apps (Lynx Apps) show up when you come back.

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.TabControl, FMX.Grid, FMX.Grid.Style, FMX.Memo, FMX.Memo.Types,
  FMX.Edit, FMX.Objects, FMX.ScrollBox, FMX.Controls.Presentation,
  System_,
  System.Generics.Collections,
  ADato.ObjectModel.intf,
  App.Storage.intf,
  AppPlatform.AppSync,
  Studio.Model;

type
  TStudioForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    _started: Boolean;
    _types: TArray<&Type>;
    _selectedType: &Type;
    _properties: TProperties;
    _storage: IStorage;
    _lastServerCheck: TDateTime;
    _aiOpened: Boolean;
    _logFile: string;

    // Top bar
    _serverStatus: TLabel;
    // Left: the types
    _typeList: TListBox;
    // Center: the selected type
    _typeTitle: TLabel;
    _typeInfo: TLabel;
    _pages: TTabControl;
    _objectsGrid: TStringGrid;
    _addObject: TButton;
    _addObjectWhy: TLabel;
    _noStorage: TLabel;
    _inspector: TVertScrollBox;
    _inspectorTitle: TLabel;
    _openInApp: TComboBox;
    _openInAppIds: TArray<string>;
    // The selected object on the shared model (see the header)
    _sync: IObjectModelContext;
    _syncBindings: TDictionary<string, TCallbackBinding>;
    _syncRefs: TList<IPropertyBinding>;
    _syncPending: Boolean;
    _gridPending: Boolean;
    _inspectorBuilds: Integer;
    // Test driver (APPPLATFORM_TEST_DRIVER, see StartTestDriver)
    _driverFile: string;
    _driverDone: Integer;
    _driverTimer: TTimer;
    _propertiesGrid: TStringGrid;
    _newPropName: TEdit;
    _newPropLabel: TEdit;
    _newPropType: TComboBox;
    _addPropertyTitle: TLabel;
    _addPropertyHint: TLabel;
    _scriptInfo: TLabel;
    _scriptMemo: TMemo;
    _scriptSave: TButton;
    _scriptDelete: TButton;
    // Bottom: log and script console
    _bottom: TTabControl;
    _bottomSplitter: TSplitter;
    _log: TMemo;
    _script: TMemo;
    _tokensPage: TTabItem;
    _tokenList: TListBox;
    _tokenName: TEdit;
    _tokenValue: TEdit;

    procedure BuildTopBar;
    procedure BuildTypeList;
    procedure BuildTypeView;
    procedure BuildObjectsPage(const Page: TTabItem);
    procedure BuildPropertiesPage(const Page: TTabItem);
    procedure BuildScriptPage(const Page: TTabItem);
    procedure BuildBottom;
    procedure BuildTokensPage(const Page: TTabItem);
    procedure ShowTokens;

    procedure LoadTypes;
    procedure ShowType(const AType: &Type);
    procedure ShowObjects;
    procedure ShowProperties;
    procedure ShowInspector;
    procedure BindInspector(const Item: CObject);
    procedure UnbindInspector;
    procedure InspectedObjectChanged;
    // Re-reads the values in the object grid (an app page changed an object)
    procedure RefreshGridValues;
    // Sets Prop of Item through the shared model when Item is the inspected object
    procedure WriteValue(const Prop: _PropertyInfo; const Item, Value: CObject);
    procedure FillOpenInApp;
    procedure OpenInAppChange(Sender: TObject);
    procedure AddObjectClick(Sender: TObject);
    // A new object of the selected type in its storage; returns an error or ''
    function  AddObject: string;
    // Whether the selected type's provider makes new objects; Why: if not, why not
    function  CanAddObject(out Why: string) : Boolean;
    procedure StartTestDriver(const CommandFile: string);
    procedure DriverTick(Sender: TObject);
    procedure DriverCommand(const Line: string);
    procedure DriverLog(const Text: string);
    procedure ShowScript;
    procedure ShowAddProperty;
    procedure AddPropertyToScript(const TypeName, PropName, PropType, PropLabel: string);
    procedure SelectType(const Name: string);
    procedure AddPropertyEditor(const Item: CObject; const Prop: _PropertyInfo);
    procedure AddListEditor(const Box: TLayout; const Item: CObject; const Prop: _PropertyInfo; const ItemType: &Type; const Value: CObject);
    procedure ListCheckChange(Sender: TObject);
    function  AddValueEditors(const Box: TLayout; const Item: CObject; const Prop: _PropertyInfo; const PropType: &Type; const Value: CObject) : Boolean;
    procedure RefreshAll;
    procedure RefreshSelectedRow;
    procedure CheckServer;

    procedure LogLine(const Text: string);
    procedure LogTypeSummary;

    procedure TypeListChange(Sender: TObject);
    procedure ObjectsGridSelChanged(Sender: TObject);
    procedure AddPropertyClick(Sender: TObject);
    procedure OpenAIClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure ToggleConsoleClick(Sender: TObject);
    procedure RunScriptClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure EditorComboChange(Sender: TObject);
    procedure EditorEditExit(Sender: TObject);
    procedure EditorEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure NewTypeClick(Sender: TObject);
    procedure SaveScriptClick(Sender: TObject);
    procedure DeleteTypeClick(Sender: TObject);
    procedure ScriptMemoKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure TokensClick(Sender: TObject);
    procedure SaveStateClick(Sender: TObject);
    procedure ForgetStateClick(Sender: TObject);
    procedure TokenListChange(Sender: TObject);
    procedure SaveTokenClick(Sender: TObject);
    procedure RemoveTokenClick(Sender: TObject);
  end;

var
  StudioForm: TStudioForm;

implementation

uses
  System.Math,
  System.StrUtils,
  System.IOUtils,
  System.Threading,
  System.Net.HttpClient,
  System.JSON,
  System.RegularExpressions,
  System.Collections,
  App.intf,
  App.PropertyDescriptor.intf,
  App.TypeDescriptor.intf,
  FMX.DialogService,
  Studio.Platform,
  App.TypeScripts,
  App.Tokens,
  AppPlatform.LynxApps.Host,
  AppPlatform.LynxApps.Window,
  AppPlatform.AppSession,
  App.ObjectCreator,
  ADato.ObjectModel.impl,
  Winapi.Windows,
  SyncSelfTest;

{$R *.fmx}

const
  SIDEBAR_COLOR = $FFF3F5F9;
  ACCENT_COLOR = $FF2F6FED;
  MUTED_COLOR = $FF5B6B82;
  ONLINE_COLOR = $FF1E9E55;
  OFFLINE_COLOR = $FFC23737;

  SCRIPT_EXAMPLE =
    '// Runs on the live app (F9). ''app'' is the same object AI-built apps use.'#13#10 +
    '// console.log writes to the Log tab.'#13#10 +
    #13#10 +
    'const projects = app.Storage[''Projects''];'#13#10 +
    'console.log(`${projects.Count} projects, first: ${projects[0].Description}`);'#13#10 +
    #13#10 +
    '// Change data, then look at the Objects page:'#13#10 +
    '// projects[0].Description = ''Renamed from a script'';'#13#10;

type
  // Ties a property editor to the object and property it edits.
  TEditorLink = class(TComponent)
  public
    Item: CObject;
    Prop: _PropertyInfo;
    // The values to choose from (picklist or storage), nil for free input
    Choices: IList;
    // Set for a field of a value type (Address.Street): Prop holds the value,
    // Sub is the field edited on it
    Sub: _PropertyInfo;
    Original: string;
  end;

function ResultError(const Json: string; out Output: string) : string; forward;

function Link(Sender: TObject) : TEditorLink;
begin
  Result := (Sender as TFmxObject).TagObject as TEditorLink;
end;

procedure StyleText(const Lbl: TLabel; const Size: Single; const Bold: Boolean; const Color: TAlphaColor = TAlphaColors.Null);
begin
  Lbl.StyledSettings := Lbl.StyledSettings - [TStyledSetting.Size, TStyledSetting.Style];
  Lbl.TextSettings.Font.Size := Size;
  if Bold then
    Lbl.TextSettings.Font.Style := [TFontStyle.fsBold];
  if Color <> TAlphaColors.Null then
  begin
    Lbl.StyledSettings := Lbl.StyledSettings - [TStyledSetting.FontColor];
    Lbl.TextSettings.FontColor := Color;
  end;
end;

function NewLabel(const Parent: TFmxObject; const Text: string; const Align: TAlignLayout; const Height: Single = 20) : TLabel;
begin
  Result := TLabel.Create(Parent);
  Result.Parent := Parent;
  Result.Align := Align;
  Result.Height := Height;
  Result.Text := Text;
  if Align = TAlignLayout.Top then
    Result.Position.Y := 100000;   // below the Top-aligned controls created before it
  Result.WordWrap := True;
  Result.AutoSize := False;
end;

function NewButton(const Parent: TFmxObject; const Text: string; const Width: Single; const OnClick: TNotifyEvent) : TButton;
begin
  Result := TButton.Create(Parent);
  Result.Parent := Parent;
  Result.Align := TAlignLayout.Right;
  Result.Width := Width;
  Result.Margins.Rect := TRectF.Create(4, 6, 4, 6);
  Result.Text := Text;
  Result.OnClick := OnClick;
end;

function NewPanel(const Parent: TFmxObject; const Align: TAlignLayout; const Size: Single; const Color: TAlphaColor) : TRectangle;
begin
  Result := TRectangle.Create(Parent);
  Result.Parent := Parent;
  Result.Align := Align;
  if Align in [TAlignLayout.Left, TAlignLayout.Right] then
    Result.Width := Size else
    Result.Height := Size;
  Result.Fill.Color := Color;
  Result.Stroke.Kind := TBrushKind.None;
end;

// A labelled field in a row of fields
function NewField(const Row: TLayout; const Caption: string; const Width: Single) : TLayout;
begin
  Result := TLayout.Create(Row);
  Result.Parent := Row;
  Result.Align := TAlignLayout.Left;
  Result.Position.X := 100000;   // after the fields created before it
  Result.Width := Width;
  Result.Margins.Right := 8;
  var lbl := NewLabel(Result, Caption, TAlignLayout.Top, 18);
  StyleText(lbl, 11, False, MUTED_COLOR);
end;

function NewGrid(const Parent: TFmxObject) : TStringGrid;
begin
  Result := TStringGrid.Create(Parent);
  Result.Parent := Parent;
  Result.Align := TAlignLayout.Client;
  Result.Options := Result.Options - [TGridOption.Editing] + [TGridOption.RowSelect, TGridOption.AlwaysShowSelection];
end;

procedure SetColumns(const Grid: TStringGrid; const Headers: TArray<string>; const Width: Single = 140);
begin
  Grid.ClearColumns;
  for var header in Headers do
  begin
    var col := TStringColumn.Create(Grid);
    col.Header := header;
    col.Width := Width;
    Grid.AddObject(col);
  end;
end;

{ TStudioForm: layout }

procedure TStudioForm.FormCreate(Sender: TObject);
begin
  Caption := 'AppPlatform Studio';
  OnKeyDown := FormKeyDown;

  BuildTopBar;
  BuildBottom;
  BuildTypeList;
  BuildTypeView;

  _logFile := ChangeFileExt(ParamStr(0), '.log');
  TFile.WriteAllText(_logFile, '');
  TPlatform.OnLog := procedure(Text: string) begin LogLine(Text); end;
  // A type script was saved or deleted, here or by the AI (Lynx Apps)
  TTypeScripts.OnChanged := procedure(Name: string) begin
    TThread.ForceQueue(nil, procedure begin
      LogLine('Type ' + Name + ' changed');
      RefreshAll;
    end);
  end;
  // An app page or a host block changed an object: the grid shows it too
  OnObjectsEdited := RefreshGridValues;
  try
    TPlatform.Start;
  except
    on E: Exception do
      LogLine('Startup failed: ' + E.Message);
  end;

  _started := True;
  LoadTypes;
  LogTypeSummary;

  // Headless self-test of live app sessions (SyncSelfTest.pas): runs before the
  // window is shown, then exits
  var selfTest := GetEnvironmentVariable('APPPLATFORM_SYNC_SELFTEST');
  if selfTest <> '' then
  begin
    RunSyncSelfTest(selfTest);
    // Idle like the running app does: queued work must settle, not rebuild the
    // inspector forever
    var builds := _inspectorBuilds;
    for var i := 1 to 50 do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Sleep(5);
    end;
    TFile.AppendAllText(ChangeFileExt(ParamStr(0), '.selftest.log'),
      Format('inspector rebuilds while idle: %d', [_inspectorBuilds - builds]) + sLineBreak, TEncoding.UTF8);
    ExitProcess(0);
  end;

  CheckServer;

  var driver := GetEnvironmentVariable('APPPLATFORM_TEST_DRIVER');
  if driver <> '' then
    StartTestDriver(driver);
end;

// End-to-end test driver: with APPPLATFORM_TEST_DRIVER=<command file> the
// window opens offscreen (no focus taken, nothing to see) and runs the lines a
// test script appends to the file, one per line; results go to <exe>.driver.log:
//   open <url>                 an app window (offscreen too)
//   select <Type> <row>        the type and the object (0-based row)
//   edit <Property> <text>     as typing in the inspector and leaving the field
//   show <Property>            the grid cell and the inspector field
//   add                        + Add object
//   savestate                  Save state
//   exit
procedure TStudioForm.StartTestDriver(const CommandFile: string);
begin
  _driverFile := CommandFile;
  TFile.WriteAllText(ChangeFileExt(ParamStr(0), '.driver.log'), '', TEncoding.UTF8);
  Position := TFormPosition.Designed;
  Left := -32000;
  Top := -32000;
  _driverTimer := TTimer.Create(Self);
  _driverTimer.Interval := 200;
  _driverTimer.OnTimer := DriverTick;
  DriverLog('ready');
end;

procedure TStudioForm.DriverLog(const Text: string);
begin
  TFile.AppendAllText(ChangeFileExt(ParamStr(0), '.driver.log'),
    FormatDateTime('hh:nn:ss.zzz', Now) + '  ' + Text + sLineBreak, TEncoding.UTF8);
end;

procedure TStudioForm.DriverTick(Sender: TObject);
begin
  if not TFile.Exists(_driverFile) then
    Exit;
  var lines: TArray<string>;
  try
    lines := TFile.ReadAllLines(_driverFile, TEncoding.UTF8);
  except
    Exit;   // being written
  end;
  while _driverDone < Length(lines) do
  begin
    var line := lines[_driverDone].Trim;
    Inc(_driverDone);
    if line <> '' then
    try
      DriverCommand(line);
    except
      on E: Exception do
        DriverLog('error in "' + line + '": ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure TStudioForm.DriverCommand(const Line: string);

  function FindProp(const Name: string) : _PropertyInfo;
  begin
    for var prop in _properties do
      if SameText(prop.Name, Name) then
        Exit(prop);
    raise Exception.Create('no property ' + Name);
  end;

begin
  // command, first argument, the rest of the line as is
  var words: TArray<string> := [];
  var rest := Line;
  for var i := 1 to 2 do
  begin
    var at := rest.IndexOf(' ');
    if at < 0 then
      Break;
    words := words + [rest.Substring(0, at)];
    rest := rest.Substring(at + 1);
  end;
  words := words + [rest];
  var command := words[0].ToLower;
  DriverLog('> ' + Line);

  if command = 'open' then
    TAppPlatformLynxAppsWindow.Open(words[1], 720, 520)
  else if command = 'select' then
  begin
    SelectType(words[1]);
    _objectsGrid.Selected := StrToInt(words[2]);
    ShowInspector;
    DriverLog('selected ' + _inspectorTitle.Text);
  end
  else if command = 'edit' then
  begin
    // What EditorEditExit does with the field's text
    var prop := FindProp(words[1]);
    var row := _objectsGrid.Selected;
    var text := '';
    if Length(words) > 2 then
      text := words[2];
    WriteValue(prop, _storage[row], text);
    RefreshSelectedRow;
    DriverLog('edited');
  end
  else if command = 'savestate' then
  begin
    SaveStateClick(nil);
    DriverLog('saved state');
  end
  else if command = 'add' then
  begin
    var why: string;
    if not CanAddObject(why) then
      DriverLog('add refused: ' + why + ' (button enabled: ' + BoolToStr(_addObject.Enabled, True) + ')')
    else
    begin
      AddObjectClick(nil);
      DriverLog(Format('added: %s, %d objects', [_inspectorTitle.Text, _storage.Count]));
    end;
  end
  else if command = 'show' then
  begin
    var prop := FindProp(words[1]);
    var row := _objectsGrid.Selected;
    var cell := '';
    for var c := 0 to High(_properties) do
      if _properties[c] = prop then
        cell := _objectsGrid.Cells[c, row];
    var field := '(no field)';
    for var i := 0 to _inspector.Content.ChildrenCount - 1 do
      for var child in _inspector.Content.Children[i].Children do
        if (child is TEdit) and (TEdit(child).TagObject is TEditorLink) and (TEditorLink(TEdit(child).TagObject).Prop = prop) then
          field := TEdit(child).Text;
    DriverLog(Format('show %s: grid "%s", inspector "%s", rebuilds %d', [string(prop.Name), cell, field, _inspectorBuilds]));
  end
  else if command = 'exit' then
  begin
    DriverLog('exit');
    _driverTimer.Enabled := False;
    Application.Terminate;
  end
  else
    DriverLog('unknown command');
end;

procedure TStudioForm.BuildTopBar;
begin
  var bar := NewPanel(Self, TAlignLayout.Top, 48, TAlphaColors.White);
  bar.Padding.Rect := TRectF.Create(12, 0, 8, 0);

  var title := NewLabel(bar, 'AppPlatform Studio', TAlignLayout.Left, 48);
  title.Width := 220;
  StyleText(title, 16, True);

  NewButton(bar, 'Console', 80, ToggleConsoleClick);
  NewButton(bar, 'Tokens', 80, TokensClick);
  var forget := NewButton(bar, 'Forget state', 100, ForgetStateClick);
  forget.Hint := 'Removes the saved state: the next start loads every type from its provider again.';
  forget.ShowHint := True;
  var save := NewButton(bar, 'Save state', 100, SaveStateClick);
  save.Hint := 'Saves all objects and property values to the state folder next to the exe; they are loaded at the next start.';
  save.ShowHint := True;
  NewButton(bar, 'Refresh', 80, RefreshClick);

  _serverStatus := NewLabel(bar, 'AI server: checking...', TAlignLayout.Right, 48);
  _serverStatus.Width := 260;
  _serverStatus.TextSettings.HorzAlign := TTextAlign.Trailing;
  _serverStatus.Margins.Right := 8;
  StyleText(_serverStatus, 12, False, MUTED_COLOR);

  var ai := NewButton(bar, 'Build an app with AI', 170, OpenAIClick);
  ai.Hint := 'Opens Lynx Apps from ' + LYNX_APPS_BASE_URL + '. The apps it builds read and change the objects shown here.';
  ai.ShowHint := True;

  var line := NewPanel(Self, TAlignLayout.Top, 1, $FFDFE4EE);
  line.Position.Y := 48;
end;

procedure TStudioForm.BuildTypeList;
begin
  var side := NewPanel(Self, TAlignLayout.Left, 230, SIDEBAR_COLOR);
  side.Padding.Rect := TRectF.Create(8, 8, 8, 8);

  var heading := NewLabel(side, 'TYPES', TAlignLayout.Top, 24);
  StyleText(heading, 11, True, MUTED_COLOR);

  var hint := NewLabel(side, 'Delphi interfaces, and JavaScript types defined by a script in js\types.', TAlignLayout.Top, 36);
  StyleText(hint, 11, False, MUTED_COLOR);

  var newType := TButton.Create(side);
  newType.Parent := side;
  newType.Align := TAlignLayout.Bottom;
  newType.Height := 30;
  newType.Margins.Top := 8;
  newType.Text := '+ New type';
  newType.OnClick := NewTypeClick;

  _typeList := TListBox.Create(side);
  _typeList.Parent := side;
  _typeList.Align := TAlignLayout.Client;
  _typeList.ItemHeight := 44;
  _typeList.OnChange := TypeListChange;
end;

procedure TStudioForm.BuildTypeView;
begin
  var view := TLayout.Create(Self);
  view.Parent := Self;
  view.Align := TAlignLayout.Client;
  view.Padding.Rect := TRectF.Create(16, 10, 12, 8);

  _typeTitle := NewLabel(view, 'Select a type', TAlignLayout.Top, 30);
  StyleText(_typeTitle, 20, True);

  _typeInfo := NewLabel(view, '', TAlignLayout.Top, 22);
  StyleText(_typeInfo, 12, False, MUTED_COLOR);
  _typeInfo.Margins.Bottom := 6;

  _pages := TTabControl.Create(view);
  _pages.Parent := view;
  _pages.Align := TAlignLayout.Client;

  var objects := _pages.Add;
  objects.Text := 'Objects';
  BuildObjectsPage(objects);

  var props := _pages.Add;
  props.Text := 'Properties';
  BuildPropertiesPage(props);

  var script := _pages.Add;
  script.Text := 'Script';
  BuildScriptPage(script);

  _pages.ActiveTab := objects;
end;

procedure TStudioForm.BuildObjectsPage(const Page: TTabItem);
begin
  var side := NewPanel(Page, TAlignLayout.Right, 320, SIDEBAR_COLOR);
  side.Padding.Rect := TRectF.Create(10, 8, 10, 8);

  _inspectorTitle := NewLabel(side, 'Selected object', TAlignLayout.Top, 26);
  StyleText(_inspectorTitle, 13, True);

  var hint := NewLabel(side, 'Edits are written to the object right away (Enter or leaving the field), and open app windows showing it follow.', TAlignLayout.Top, 34);
  StyleText(hint, 11, False, MUTED_COLOR);

  _openInApp := TComboBox.Create(side);
  _openInApp.Parent := side;
  _openInApp.Align := TAlignLayout.Top;
  _openInApp.Position.Y := 100000;
  _openInApp.Margins.Bottom := 6;
  _openInApp.Hint := 'Opens an AI-built app with this object; its host block gets it as page.param(...).';
  _openInApp.ShowHint := True;
  _openInApp.OnChange := OpenInAppChange;

  _inspector := TVertScrollBox.Create(side);
  _inspector.Parent := side;
  _inspector.Align := TAlignLayout.Client;

  _noStorage := NewLabel(Page, '', TAlignLayout.Top, 80);
  _noStorage.Margins.Rect := TRectF.Create(12, 12, 12, 0);
  _noStorage.Text := 'This type has no storage, so it has no objects of its own.'#13#10 +
    'It is used as the type of a property of another type (for example Customer.Address).';
  StyleText(_noStorage, 13, False, MUTED_COLOR);

  var bar := TLayout.Create(Page);
  bar.Parent := Page;
  bar.Align := TAlignLayout.Top;
  bar.Height := 38;
  _addObject := NewButton(bar, '+ Add object', 120, AddObjectClick);
  _addObject.Align := TAlignLayout.Left;
  _addObject.Margins.Rect := TRectF.Create(0, 4, 8, 6);
  _addObject.Hint := 'Adds a new object to this type''s storage, made by its provider. Open app windows that list the storage show it right away.';
  _addObject.ShowHint := True;
  // Why a type's objects cannot be added (its provider reads them from elsewhere)
  _addObjectWhy := NewLabel(bar, '', TAlignLayout.Client, 38);
  StyleText(_addObjectWhy, 11, False, MUTED_COLOR);

  _objectsGrid := NewGrid(Page);
  _objectsGrid.OnSelChanged := ObjectsGridSelChanged;
end;

procedure TStudioForm.BuildPropertiesPage(const Page: TTabItem);
begin
  var add := NewPanel(Page, TAlignLayout.Bottom, 150, SIDEBAR_COLOR);
  add.Padding.Rect := TRectF.Create(10, 8, 10, 8);

  _addPropertyTitle := NewLabel(add, 'Add a property', TAlignLayout.Top, 24);
  StyleText(_addPropertyTitle, 13, True);

  _addPropertyHint := NewLabel(add, '', TAlignLayout.Top, 36);
  StyleText(_addPropertyHint, 11, False, MUTED_COLOR);

  var row := TLayout.Create(add);
  row.Parent := add;
  row.Align := TAlignLayout.Top;
  row.Height := 46;
  row.Margins.Top := 6;
  row.Position.Y := 100000;

  _newPropName := TEdit.Create(Self);
  _newPropName.Parent := NewField(row, 'Name', 160);
  _newPropName.Align := TAlignLayout.Top;
  _newPropName.TextPrompt := 'e.g. Customer';

  _newPropLabel := TEdit.Create(Self);
  _newPropLabel.Parent := NewField(row, 'Label (optional)', 160);
  _newPropLabel.Align := TAlignLayout.Top;

  _newPropType := TComboBox.Create(Self);
  _newPropType.Parent := NewField(row, 'Type', 180);
  _newPropType.Align := TAlignLayout.Top;

  var btn := TButton.Create(row);
  btn.Parent := row;
  btn.Align := TAlignLayout.Left;
  btn.Position.X := 100000;
  btn.Width := 110;
  btn.Margins.Rect := TRectF.Create(0, 18, 0, 0);
  btn.Text := 'Add property';
  btn.OnClick := AddPropertyClick;

  _propertiesGrid := NewGrid(Page);
  SetColumns(_propertiesGrid, ['Name', 'Label', 'Type', 'Writable'], 170);
end;

procedure TStudioForm.BuildScriptPage(const Page: TTabItem);
begin
  var bar := TLayout.Create(Page);
  bar.Parent := Page;
  bar.Align := TAlignLayout.Top;
  bar.Height := 40;
  bar.Padding.Rect := TRectF.Create(0, 4, 0, 4);

  _scriptSave := NewButton(bar, 'Save (Ctrl+S)', 110, SaveScriptClick);
  _scriptSave.Align := TAlignLayout.Left;
  _scriptDelete := NewButton(bar, 'Delete type', 100, DeleteTypeClick);
  _scriptDelete.Align := TAlignLayout.Left;

  _scriptInfo := NewLabel(bar, '', TAlignLayout.Client, 32);
  _scriptInfo.Margins.Left := 8;
  StyleText(_scriptInfo, 11, False, MUTED_COLOR);

  _scriptMemo := TMemo.Create(Page);
  _scriptMemo.Parent := Page;
  _scriptMemo.Align := TAlignLayout.Client;
  _scriptMemo.StyledSettings := _scriptMemo.StyledSettings - [TStyledSetting.Family];
  _scriptMemo.TextSettings.Font.Family := 'Consolas';
  _scriptMemo.OnKeyDown := ScriptMemoKeyDown;
end;

procedure TStudioForm.BuildBottom;
begin
  _bottom := TTabControl.Create(Self);
  _bottom.Parent := Self;
  _bottom.Align := TAlignLayout.Bottom;
  _bottom.Height := 200;

  _bottomSplitter := TSplitter.Create(Self);
  _bottomSplitter.Parent := Self;
  _bottomSplitter.Align := TAlignLayout.Bottom;
  _bottomSplitter.Height := 5;
  _bottomSplitter.Position.Y := _bottom.Position.Y - 5;

  var logPage := _bottom.Add;
  logPage.Text := 'Log';
  _log := TMemo.Create(logPage);
  _log.Parent := logPage;
  _log.Align := TAlignLayout.Client;
  _log.ReadOnly := True;

  var scriptPage := _bottom.Add;
  scriptPage.Text := 'Console';

  var bar := TLayout.Create(scriptPage);
  bar.Parent := scriptPage;
  bar.Align := TAlignLayout.Top;
  bar.Height := 34;
  NewButton(bar, 'Run (F9)', 90, RunScriptClick).Align := TAlignLayout.Left;
  var hint := NewLabel(bar, 'JavaScript on the live app; try here what you want an AI-built app to do.', TAlignLayout.Client, 34);
  hint.Margins.Left := 8;
  StyleText(hint, 11, False, MUTED_COLOR);

  _script := TMemo.Create(scriptPage);
  _script.Parent := scriptPage;
  _script.Align := TAlignLayout.Client;
  _script.StyledSettings := _script.StyledSettings - [TStyledSetting.Family];
  _script.TextSettings.Font.Family := 'Consolas';
  _script.Text := SCRIPT_EXAMPLE;

  _tokensPage := _bottom.Add;
  _tokensPage.Text := 'Tokens';
  BuildTokensPage(_tokensPage);

  _bottom.ActiveTab := logPage;
end;

// API keys by name, in tokens.json next to the exe (App.Tokens)
procedure TStudioForm.BuildTokensPage(const Page: TTabItem);
begin
  var hint := NewLabel(Page, 'Saved in ' + TLocalTokens.FileName + ' (plain text, this machine only). ' +
    'Type scripts and AI-built apps use them as tokens[''Name'']; the AI only sees the names.', TAlignLayout.Top, 34);
  hint.Margins.Rect := TRectF.Create(8, 4, 8, 0);
  StyleText(hint, 11, False, MUTED_COLOR);

  _tokenList := TListBox.Create(Page);
  _tokenList.Parent := Page;
  _tokenList.Align := TAlignLayout.Left;
  _tokenList.Width := 240;
  _tokenList.Margins.Rect := TRectF.Create(8, 4, 8, 8);
  _tokenList.OnChange := TokenListChange;

  var row := TLayout.Create(Page);
  row.Parent := Page;
  row.Align := TAlignLayout.Top;
  row.Height := 56;
  row.Position.Y := 100000;
  row.Margins.Top := 4;

  _tokenName := TEdit.Create(Self);
  _tokenName.Parent := NewField(row, 'Name (e.g. TrelloKey)', 180);
  _tokenName.Align := TAlignLayout.Top;
  _tokenName.Position.Y := 100000;

  _tokenValue := TEdit.Create(Self);
  _tokenValue.Parent := NewField(row, 'Value', 260);
  _tokenValue.Align := TAlignLayout.Top;
  _tokenValue.Position.Y := 100000;
  _tokenValue.Password := True;
  _tokenValue.TextPrompt := 'paste the key or token';

  var buttons := NewField(row, '', 190);
  var save := NewButton(buttons, 'Save', 90, SaveTokenClick);
  save.Align := TAlignLayout.Left;
  save.Margins.Rect := TRectF.Create(0, 0, 4, 0);
  var remove := NewButton(buttons, 'Remove', 90, RemoveTokenClick);
  remove.Align := TAlignLayout.Left;
  remove.Position.X := 100000;
  remove.Margins.Rect := TRectF.Create(0, 0, 4, 0);

  ShowTokens;
end;

procedure TStudioForm.ShowTokens;
begin
  _tokenList.Clear;
  for var name in TLocalTokens.Names do
  begin
    var value: string;
    TLocalTokens.TryGetValue(name, value);
    // The last characters tell keys apart without showing them
    _tokenList.Items.Add(name + '   ••••' + value.Substring(Max(0, value.Length - 4)));
  end;
end;

{ TStudioForm: content }

procedure TStudioForm.LoadTypes;
begin
  var selectedName := '';
  if _selectedType <> nil then
    selectedName := TStudioModel.TypeName(_selectedType);

  _types := [];
  for var t in _app.Config.Types do
    _types := _types + [t];

  _typeList.OnChange := nil;
  _typeList.BeginUpdate;
  try
    _typeList.Clear;

    for var i := 0 to High(_types) do
    begin
      var name := TStudioModel.TypeName(_types[i]);
      var storage := TStudioModel.StorageOf(_types[i]);

      var item := TListBoxItem.Create(_typeList);
      item.StyleLookup := 'listboxitembottomdetail';
      item.Text := name;
      var origin := 'Delphi';
      if (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(name) then
        origin := name + '.js';
      if storage <> nil then
        item.ItemData.Detail := Format('%s · %d objects · %s', [storage.Name, storage.Count, origin]) else
        item.ItemData.Detail := 'value type · ' + origin;
      _typeList.AddObject(item);


      if name = selectedName then
        _typeList.ItemIndex := i;
    end;
  finally
    _typeList.EndUpdate;
    _typeList.OnChange := TypeListChange;
  end;

  if (_typeList.ItemIndex < 0) and (Length(_types) > 0) then
    _typeList.ItemIndex := 0;

  if _typeList.ItemIndex >= 0 then
    ShowType(_types[_typeList.ItemIndex]);
end;

procedure TStudioForm.ShowType(const AType: &Type);
begin
  _selectedType := AType;
  _properties := TStudioModel.PropertiesOf(AType);
  _storage := TStudioModel.StorageOf(AType);

  var name := TStudioModel.TypeName(AType);
  _typeTitle.Text := name;
  _addPropertyTitle.Text := 'Add a property to ' + name;

  if _storage <> nil then
    _typeInfo.Text := Format('Storage ''%s'' · %d objects · %d properties', [_storage.Name, _storage.Count, Length(_properties)]) else
    _typeInfo.Text := Format('No storage · %d properties', [Length(_properties)]);

  ShowObjects;
  ShowProperties;
  ShowAddProperty;
  ShowScript;
end;

// A script type gets new properties in its script (saved, like edit_type);
// a Delphi type only as a runtime extension (lost when the app closes).
procedure TStudioForm.ShowAddProperty;
begin
  var name := TStudioModel.TypeName(_selectedType);
  var scripted := (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(name);

  _newPropType.Clear;
  if scripted then
  begin
    _addPropertyTitle.Text := 'Add a property to ' + name + '.js';
    _addPropertyHint.Text := name + ' is defined by its script: the property is added to the script''s ' +
      'PropertyDescriptor and the script is saved and run again (see the Script tab).';
    for var scalar in ['string', 'number', 'integer', 'boolean', 'date'] do
      _newPropType.Items.Add(scalar);
    for var t in _types do
      _newPropType.Items.Add(TStudioModel.TypeName(t));
  end
  else
  begin
    _addPropertyTitle.Text := 'Extend ' + name + ' at runtime';
    _addPropertyHint.Text := name + ' is compiled Delphi. A property added here exists until AppPlatform closes; ' +
      'it is not saved. To keep one, a type script adds it: Extends: { ' + name + ': { ... } } in its descriptor.';
    _newPropType.Items.Add('Text');
    for var t in _types do
      _newPropType.Items.Add(TStudioModel.TypeName(t));
  end;
  _newPropType.ItemIndex := 0;
end;

procedure TStudioForm.AddPropertyToScript(const TypeName, PropName, PropType, PropLabel: string);
begin
  var source := TypeScripts.Get(TypeName);
  // registerType form: this.PropertyDescriptor = { Name: { Type, Label } };
  // the older defineType form: properties: { Name: { type, label } }
  var descriptorForm := True;
  var match := TRegEx.Match(source, 'PropertyDescriptor\s*=\s*\{');
  if not match.Success then
  begin
    descriptorForm := False;
    match := TRegEx.Match(source, 'properties\s*:\s*\{');
  end;
  if not match.Success then
  begin
    LogLine('Add property: no "this.PropertyDescriptor = {" in ' + TypeName + '.js; add it on the Script tab.');
    Exit;
  end;

  var line := '    ' + PropName + ': { ' + IfThen(descriptorForm, 'Type', 'type') + ': ''' + PropType + '''';
  if PropLabel <> '' then
    line := line + ', ' + IfThen(descriptorForm, 'Label', 'label') + ': ''' + PropLabel.Replace('''', '\''') + '''';
  line := line + ' },';

  // The '}' that closes properties (0-based; braces inside quotes don't count)
  var open := match.Index + match.Length - 2;
  var close := -1;
  var depth := 0;
  var quote := #0;
  for var i := open to source.Length - 1 do
  begin
    var c := source.Chars[i];
    if quote <> #0 then
    begin
      if c = '\' then
        Continue;
      if (c = quote) and ((i = 0) or (source.Chars[i - 1] <> '\')) then
        quote := #0;
    end
    else if CharInSet(c, ['''', '"', '`']) then
      quote := c
    else if c = '{' then
      inc(depth)
    else if c = '}' then
    begin
      dec(depth);
      if depth = 0 then
      begin
        close := i;
        Break;
      end;
    end;
  end;
  if close < 0 then
  begin
    LogLine('Add property: the properties of ' + TypeName + '.js are not closed; fix it on the Script tab.');
    Exit;
  end;

  // Append after the last property, on its own line before the '}'
  var before := source.Substring(0, close).TrimRight;
  if not (before.EndsWith(',') or before.EndsWith('{')) then
    before := before + ',';
  var closeLine := source.LastIndexOf(#10, close - 1) + 1;
  var indent := source.Substring(closeLine, close - closeLine);
  if indent.Trim <> '' then
    indent := '  ';
  // One level deeper than the closing brace
  line := indent + line.TrimLeft.PadLeft(line.TrimLeft.Length + 2);
  source := before + sLineBreak + line + sLineBreak + indent + source.Substring(close);

  var output: string;
  var error := ResultError(TypeScripts.Save(TypeName, source), output);
  if error <> '' then
    LogLine('Add property to ' + TypeName + '.js failed (the script is unchanged):' + sLineBreak + error)
  else
  begin
    LogLine(Format('Added %s to %s.js (%s)', [PropName, TypeName, PropType]));
    if output <> '' then
      LogLine(output);
  end;
end;

procedure TStudioForm.ShowScript;
begin
  var name := TStudioModel.TypeName(_selectedType);
  var scripted := (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(name);

  _scriptSave.Enabled := scripted;
  _scriptDelete.Enabled := scripted;
  _scriptMemo.ReadOnly := not scripted;

  if scripted then
  begin
    _scriptInfo.Text := TPath.Combine(TypeScriptStore.Folder, name + '.js') +
      '. Saving runs the script and redefines the type (its objects start over from the script''s data).';
    _scriptMemo.Text := TypeScriptStore.Get(name);
  end
  else
  begin
    _scriptInfo.Text := name + ' is defined in Delphi, not by a type script.';
    _scriptMemo.Text := '// ' + name + ' is a Delphi interface, registered by the host at startup' + sLineBreak +
      '// (Studio.Platform.pas, DataObjects\Project.intf.pas). It has no script to edit.' + sLineBreak +
      '// A type script can add properties to it, e.g. in TrelloTask.js:' + sLineBreak +
      '//   this.Extends = { ' + name + ': { TrelloTask: { Type: ''TrelloTask'', Label: ''Trello card'' } } };' + sLineBreak +
      '// Properties added on the Properties tab extend it until the app closes.';
  end;
end;

procedure TStudioForm.SelectType(const Name: string);
begin
  for var i := 0 to High(_types) do
    if SameText(TStudioModel.TypeName(_types[i]), Name) then
    begin
      _typeList.ItemIndex := i;
      Exit;
    end;
end;

// Whether objects can be added is the provider's call (App.ObjectCreator): a
// provider that reads its objects from elsewhere (Trello, ...) cannot make new ones
function TStudioForm.CanAddObject(out Why: string) : Boolean;
begin
  Why := '';
  var typeName := TStudioModel.TypeName(_selectedType);
  if _storage = nil then
  begin
    Why := typeName + ' has no storage.';
    Exit(False);
  end;

  if (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(typeName) then
  begin
    Result := TypeScriptStore.CanAdd(typeName);
    if not Result then
      Why := typeName + ' objects come from its provider (' + typeName + '.js), which has no Add(values): ' +
        'they cannot be made here. A provider that owns its objects adds Add(values) to make new ones.';
    Exit;
  end;

  var descriptor := _app.Config.TypeDescriptor(_selectedType);
  Result := (descriptor <> nil) and Supports(descriptor.Provider, IObjectCreator);
  if not Result then
    Why := typeName + ' objects come from its provider, which cannot make new ones.';
end;

function TStudioForm.AddObject: string;
begin
  var why: string;
  if not CanAddObject(why) then
    Exit(why);
  var typeName := TStudioModel.TypeName(_selectedType);

  if (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(typeName) then
  begin
    // The script provider's Add makes the values; the platform creates and stores it
    var output: string;
    Exit(ResultError(TypeScriptStore.AddObject(typeName, '{}'), output));
  end;

  // A Delphi provider makes the object; it goes into the storage
  var creator: IObjectCreator;
  Supports(_app.Config.TypeDescriptor(_selectedType).Provider, IObjectCreator, creator);
  var obj := creator.NewObject(_storage.Data);
  if obj.IsNull then
    Exit('the provider made no ' + typeName);
  _storage.Data.Add(obj);
  Result := '';
end;

procedure TStudioForm.AddObjectClick(Sender: TObject);
begin
  var typeName := TStudioModel.TypeName(_selectedType);
  var error := '';
  try
    error := AddObject;
  except
    on E: Exception do error := E.Message;
  end;
  if error <> '' then
  begin
    LogLine('Add object to ' + typeName + ' failed: ' + error);
    Exit;
  end;

  // Live lists of open app pages that list this storage get the new object
  TAppSession.StorageChanged(_storage.Name);
  ShowType(_selectedType);
  if _storage <> nil then
  begin
    _objectsGrid.Selected := _storage.Count - 1;
    ShowInspector;
    LogLine(Format('Added a %s (%s)', [typeName, _inspectorTitle.Text]));
  end;
end;

procedure TStudioForm.ShowObjects;
begin
  var hasObjects := _storage <> nil;
  _noStorage.Visible := not hasObjects;
  _addObject.ParentControl.Visible := hasObjects;
  var why: string;
  _addObject.Enabled := CanAddObject(why);
  _addObjectWhy.Text := why;
  _objectsGrid.Visible := hasObjects;
  _inspector.ParentControl.Visible := hasObjects;

  var row := _objectsGrid.Selected;

  _objectsGrid.BeginUpdate;
  try
    var headers: TArray<string> := [];
    for var prop in _properties do
      headers := headers + [TStudioModel.PropertyLabel(_selectedType, prop)];
    SetColumns(_objectsGrid, headers);

    if not hasObjects then
    begin
      _objectsGrid.RowCount := 0;
      Exit;
    end;

    _objectsGrid.RowCount := _storage.Count;
    for var r := 0 to _storage.Count - 1 do
      for var c := 0 to High(_properties) do
        _objectsGrid.Cells[c, r] := TStudioModel.ValueText(_storage[r], _properties[c]);
  finally
    _objectsGrid.EndUpdate;
  end;

  if _storage.Count > 0 then
    _objectsGrid.Selected := EnsureRange(row, 0, _storage.Count - 1);
  ShowInspector;
end;

procedure TStudioForm.ShowProperties;
begin
  _propertiesGrid.RowCount := Length(_properties);
  for var i := 0 to High(_properties) do
  begin
    var prop := _properties[i];
    _propertiesGrid.Cells[0, i] := prop.Name;
    _propertiesGrid.Cells[1, i] := TStudioModel.PropertyLabel(_selectedType, prop);
    var typeText := TStudioModel.PropertyTypeName(_selectedType, prop);
    var itemType: &Type;
    if TypeScriptStore.ExtensionItemType(_selectedType, prop.Name, itemType) then
      typeText := 'list of ' + TStudioModel.TypeName(itemType);
    var addedBy := TypeScriptStore.ExtendedBy(_selectedType, prop.Name);
    if addedBy <> '' then
      typeText := typeText + '  (added by ' + addedBy + '.js)';
    _propertiesGrid.Cells[2, i] := typeText;
    try
      _propertiesGrid.Cells[3, i] := IfThen(prop.CanWrite, 'yes', 'no');
    except
      on E: Exception do
        _propertiesGrid.Cells[2, i] := '<' + E.Message + '>';
    end;
  end;
end;

// One callback binding per property, on a context of the type's shared model:
// a change made anywhere else (an app page) refreshes the inspector and row
procedure TStudioForm.BindInspector(const Item: CObject);
begin
  _sync := TObjectModelContext.Create(ModelFor(_selectedType));
  _sync.Context := Item;
  _syncBindings := TDictionary<string, TCallbackBinding>.Create;
  _syncRefs := TList<IPropertyBinding>.Create;
  for var prop in _properties do
  try
    var binding := TCallbackBinding.Create(InspectedObjectChanged);
    var ref: IPropertyBinding := binding;
    _sync.Bind(string(prop.Name), ref);
    binding.Armed := True;
    _syncBindings.Add(string(prop.Name), binding);
    _syncRefs.Add(ref);
  except
    // a property the model does not know: edited directly, without sync
  end;
end;

procedure TStudioForm.UnbindInspector;
begin
  if _syncBindings <> nil then
    for var binding in _syncBindings.Values do
      binding.Detach;
  if _sync <> nil then
    _sync.Unbind;
  _sync := nil;
  FreeAndNil(_syncBindings);
  FreeAndNil(_syncRefs);
end;

procedure TStudioForm.RefreshGridValues;
begin
  if _gridPending then
    Exit;
  _gridPending := True;
  TThread.ForceQueue(nil, procedure
    begin
      _gridPending := False;
      if (_storage = nil) or (_objectsGrid.RowCount <> _storage.Count) then
        Exit;
      for var r := 0 to _storage.Count - 1 do
        for var c := 0 to High(_properties) do
          _objectsGrid.Cells[c, r] := TStudioModel.ValueText(_storage[r], _properties[c]);
    end);
end;

procedure TStudioForm.InspectedObjectChanged;
begin
  if _syncPending then
    Exit;
  _syncPending := True;
  TThread.ForceQueue(nil, procedure
    begin
      _syncPending := False;
      RefreshSelectedRow;
      ShowInspector;
    end);
end;

procedure TStudioForm.WriteValue(const Prop: _PropertyInfo; const Item, Value: CObject);
begin
  var binding: TCallbackBinding;
  if (_sync <> nil) and (_syncBindings <> nil) and _syncBindings.TryGetValue(string(Prop.Name), binding)
    and CObject.Equals(_sync.Context, Item) then
    binding.Edit(Value)
  else
    Prop.SetValue(Item, Value, []);
end;

// The saved apps, to open with the selected object
procedure TStudioForm.FillOpenInApp;
begin
  _openInApp.OnChange := nil;
  try
    _openInApp.Clear;
    _openInAppIds := [''];
    _openInApp.Items.Add('Open in app...');
    var root := TJSONObject.ParseJSONValue(LynxAppsHost.LoadAppsJson);
    try
      var apps: TJSONArray := nil;
      if root is TJSONObject then
        apps := TJSONObject(root).GetValue('apps') as TJSONArray;
      if apps <> nil then
        for var app in apps do
          if app is TJSONObject then
          begin
            _openInApp.Items.Add(TJSONObject(app).GetValue<string>('name', '(no name)'));
            _openInAppIds := _openInAppIds + [TJSONObject(app).GetValue<string>('id', '')];
          end;
    finally
      root.Free;
    end;
    _openInApp.ItemIndex := 0;
    _openInApp.Visible := _openInApp.Count > 1;
  finally
    _openInApp.OnChange := OpenInAppChange;
  end;
end;

// app.html?id=<app>&param=<name>:<Type>:<ID>; the param is named after the
// type: project for IProject, supplier for Supplier
procedure TStudioForm.OpenInAppChange(Sender: TObject);
begin
  var index := _openInApp.ItemIndex;
  if (index <= 0) or (index > High(_openInAppIds)) then
    Exit;
  var row := _objectsGrid.Selected;
  if (_storage = nil) or (row < 0) or (row >= _storage.Count) then
    Exit;

  var typeName := TStudioModel.TypeName(_selectedType);
  var id := '';
  for var prop in _properties do
    if SameText(prop.Name, 'ID') then
      id := TStudioModel.ValueText(_storage[row], prop);
  if id = '' then
  begin
    LogLine('Open in app: ' + typeName + ' objects have no ID.');
    Exit;
  end;

  var paramName := typeName;
  if (paramName.Length > 1) and paramName.StartsWith('I') and CharInSet(paramName.Chars[1], ['A'..'Z']) then
    paramName := paramName.Substring(1);
  paramName := paramName.Substring(0, 1).ToLower + paramName.Substring(1);

  var url := LYNX_APPS_BASE_URL + 'app.html?id=' + _openInAppIds[index] +
    '&param=' + paramName + ':' + typeName + ':' + id;
  LogLine(Format('Opening %s with %s (%s %s)', [_openInApp.Items[index], paramName, typeName, id]));
  _aiOpened := True;   // refresh when the Studio gets focus again
  TAppPlatformLynxAppsWindow.Open(url, 720, 520);
  TThread.ForceQueue(nil, procedure begin _openInApp.ItemIndex := 0; end);
end;

procedure TStudioForm.ShowInspector;
begin
  // A focused edit saves its value (OnExit) before it is destroyed
  if (Focused <> nil) and (Focused.GetObject is TEdit) and _inspector.IsChild(Focused.GetObject as TFmxObject) then
  begin
    var edit := Focused.GetObject as TEdit;
    edit.OnExit := nil;
    Focused := nil;
    EditorEditExit(edit);
  end;

  Inc(_inspectorBuilds);
  UnbindInspector;
  _inspector.BeginUpdate;
  try
    _inspector.Content.DeleteChildren;

    var row := _objectsGrid.Selected;
    if (_storage = nil) or (row < 0) or (row >= _storage.Count) then
    begin
      _inspectorTitle.Text := 'Selected object';
      _openInApp.Visible := False;
      Exit;
    end;

    var item := _storage[row];
    _inspectorTitle.Text := Format('%s %d of %d', [TStudioModel.TypeName(_selectedType), row + 1, _storage.Count]);
    FillOpenInApp;
    BindInspector(item);

    for var prop in _properties do
      AddPropertyEditor(item, prop);
  finally
    _inspector.EndUpdate;
    // Editors grow their box while they are built (a checkbox per object):
    // measure the content again, or the part below the first size takes no clicks
    _inspector.InvalidateContentSize;
    _inspector.RealignContent;
  end;
end;

// One editor per property:
//   - the type of the property has a storage -> pick one of its objects
//   - a list                                 -> item count, read-only
//   - writable                               -> text field
//   - otherwise                              -> read-only text
procedure TStudioForm.AddPropertyEditor(const Item: CObject; const Prop: _PropertyInfo);
begin
  var box := TLayout.Create(_inspector);
  box.Parent := _inspector;
  box.Align := TAlignLayout.Top;
  box.Height := 48;
  box.Position.Y := MaxInt;   // keep the order: append at the bottom

  var lbl := NewLabel(box, TStudioModel.PropertyLabel(_selectedType, Prop), TAlignLayout.Top, 18);
  StyleText(lbl, 11, False, MUTED_COLOR);

  var link := TEditorLink.Create(box);
  link.Item := Item;
  link.Prop := Prop;

  var propType: &Type;
  var value: CObject;
  try
    propType := TStudioModel.PropertyTypeOf(_selectedType, Prop);
    value := Prop.GetValue(Item, []);
  except
    on E: Exception do
    begin
      NewLabel(box, '<' + E.Message + '>', TAlignLayout.Top, 24);
      Exit;
    end;
  end;

  // A list a script added (IProject.TrelloTasks): check the objects it holds
  var itemType: &Type;
  if (TypeScriptStore <> nil) and TypeScriptStore.ExtensionItemType(_selectedType, Prop.Name, itemType) then
  begin
    AddListEditor(box, Item, Prop, itemType, value);
    Exit;
  end;

  link.Choices := TStudioModel.ChoicesFor(Prop, propType);
  if (link.Choices <> nil) and Prop.CanWrite then
  begin
    var combo := TComboBox.Create(box);
    combo.Parent := box;
    combo.Align := TAlignLayout.Top;
    combo.Position.Y := 100000;   // below its label
    combo.TagObject := link;
    combo.Items.Add('(none)');
    // Values of JavaScript types are unmarshalled into a new object on every
    // read, so match on the text as well as on identity
    var valueText := TStudioModel.Format(value);
    var selected := 0;
    for var i := 0 to link.Choices.Count - 1 do
    begin
      var choice := link.Choices[i];
      var choiceText := TStudioModel.Format(choice);
      combo.Items.Add(choiceText);
      if (selected = 0) and not value.IsNull and (CObject.Equals(choice, value) or (choiceText = valueText)) then
        selected := i + 1;
    end;
    combo.ItemIndex := selected;
    combo.OnChange := EditorComboChange;
    Exit;
  end;

  // A value type (registered, nothing to choose from): a field per property
  if Prop.CanWrite and TStudioModel.IsRegisteredType(propType) and not TStudioModel.IsList(propType) then
    if AddValueEditors(box, Item, Prop, propType, value) then
      Exit;

  var text := TStudioModel.Format(value);

  // A registered type without fields cannot be typed in: its marshaller
  // expects an object, not text
  if TStudioModel.IsList(propType) or not Prop.CanWrite or TStudioModel.IsRegisteredType(propType) then
  begin
    var ro := NewLabel(box, text, TAlignLayout.Top, 24);
    if text = '' then
      ro.Text := '(empty)';
    Exit;
  end;

  var edit := TEdit.Create(box);
  edit.Parent := box;
  edit.Align := TAlignLayout.Top;
  edit.Position.Y := 100000;   // below its label
  edit.Text := text;
  edit.TagObject := link;
  link.Original := text;
  edit.OnExit := EditorEditExit;
  edit.OnKeyDown := EditorEditKeyDown;
end;

// A checkbox per object of ItemType's storage; checked ones are in the list
procedure TStudioForm.AddListEditor(const Box: TLayout; const Item: CObject; const Prop: _PropertyInfo; const ItemType: &Type; const Value: CObject);
begin
  var link := TEditorLink.Create(Box);
  link.Item := Item;
  link.Prop := Prop;
  link.Choices := TStudioModel.StorageOf(ItemType);

  if (link.Choices = nil) or (link.Choices.Count = 0) then
  begin
    NewLabel(Box, '(no ' + TStudioModel.TypeName(ItemType) + ' objects to choose from)', TAlignLayout.Top, 24);
    Exit;
  end;

  var current: IList := nil;
  if not Value.IsNull then
    Value.TryGetValue<IList>(current);

  for var i := 0 to link.Choices.Count - 1 do
  begin
    var choice := link.Choices[i];
    var choiceText := TStudioModel.Format(choice);
    var checked := False;
    // Objects of a JavaScript type are new objects after its script ran
    // again: match on the text as well as on identity
    if current <> nil then
      for var j := 0 to current.Count - 1 do
        if CObject.Equals(current[j], choice) or (TStudioModel.Format(current[j]) = choiceText) then
          checked := True;

    var check := TCheckBox.Create(Box);
    check.Parent := Box;
    check.Align := TAlignLayout.Top;
    check.Position.Y := 100000;
    check.Height := 24;
    check.Text := choiceText;
    check.IsChecked := checked;
    check.Tag := i;
    check.TagObject := link;
    check.OnChange := ListCheckChange;
    Box.Height := Box.Height + check.Height;
  end;
end;

procedure TStudioForm.ListCheckChange(Sender: TObject);
begin
  var link := Link(Sender);
  var box := (Sender as TCheckBox).ParentControl;
  var list := TStudioModel.NewList;
  for var child in box.Controls do
    if (child is TCheckBox) and TCheckBox(child).IsChecked then
      list.Add(link.Choices[TCheckBox(child).Tag]);
  try
    WriteValue(link.Prop, link.Item, CObject.From<IList>(list));
  except
    on E: Exception do
      LogLine('Set ' + link.Prop.Name + ' failed: ' + E.Message);
  end;
  RefreshSelectedRow;
end;

// The fields of a value (Address: Street, Zip, City) as text fields, indented
// under the property's label. False when the type has no text fields.
function TStudioForm.AddValueEditors(const Box: TLayout; const Item: CObject; const Prop: _PropertyInfo; const PropType: &Type; const Value: CObject) : Boolean;
begin
  var fields := TStudioModel.PropertiesOf(PropType);
  Result := False;

  for var field in fields do
  begin
    var fieldType: &Type;
    try
      fieldType := TStudioModel.PropertyTypeOf(PropType, field);
    except
      Continue;
    end;
    if not field.CanWrite or TStudioModel.IsRegisteredType(fieldType) or TStudioModel.IsList(fieldType) then
      Continue;

    var row := TLayout.Create(Box);
    row.Parent := Box;
    row.Align := TAlignLayout.Top;
    row.Height := 28;
    row.Position.Y := 100000;
    row.Margins.Left := 12;

    var lbl := NewLabel(row, field.Name, TAlignLayout.Left, 28);
    lbl.Width := 60;
    StyleText(lbl, 11, False, MUTED_COLOR);

    var text := '';
    if not Value.IsNull then
      text := TStudioModel.ValueText(Value, field);

    var link := TEditorLink.Create(row);
    link.Item := Item;
    link.Prop := Prop;
    link.Sub := field;
    link.Original := text;

    var edit := TEdit.Create(row);
    edit.Parent := row;
    edit.Align := TAlignLayout.Client;
    edit.Text := text;
    edit.TagObject := link;
    edit.OnExit := EditorEditExit;
    edit.OnKeyDown := EditorEditKeyDown;

    Box.Height := Box.Height + row.Height;
    Result := True;
  end;
end;

procedure TStudioForm.RefreshSelectedRow;
begin
  var row := _objectsGrid.Selected;
  if (_storage = nil) or (row < 0) or (row >= _storage.Count) then
    Exit;

  for var c := 0 to High(_properties) do
    _objectsGrid.Cells[c, row] := TStudioModel.ValueText(_storage[row], _properties[c]);
end;

procedure TStudioForm.RefreshAll;
begin
  if _started then
    LoadTypes;
  ShowTokens;
end;

procedure TStudioForm.CheckServer;
begin
  _lastServerCheck := Now;
  TTask.Run(procedure begin
    var online := False;
    var client := THTTPClient.Create;
    try
      client.ConnectionTimeout := 1500;
      client.ResponseTimeout := 1500;
      try
        online := client.Get(LYNX_APPS_BASE_URL).StatusCode < 500;
      except
        online := False;
      end;
    finally
      client.Free;
    end;

    TThread.Queue(nil, procedure begin
      if online then
      begin
        _serverStatus.Text := 'AI server: online (' + LYNX_APPS_BASE_URL + ')';
        _serverStatus.TextSettings.FontColor := ONLINE_COLOR;
      end
      else
      begin
        _serverStatus.Text := 'AI server: offline - run "node server.js" in IntegrationProject';
        _serverStatus.TextSettings.FontColor := OFFLINE_COLOR;
      end;
    end);
  end);
end;

procedure TStudioForm.LogLine(const Text: string);
begin
  var line := FormatDateTime('hh:nn:ss', Now) + '  ' + Text;
  _log.Lines.Add(line);
  _log.GoToTextEnd;
  // Also next to the exe (AppPlatform.log), for when the window is not at hand
  try
    TFile.AppendAllText(_logFile, line + sLineBreak);
  except
  end;
end;

// What the platform contains after startup: one line per type.
procedure TStudioForm.LogTypeSummary;
begin
  for var t in _types do
  begin
    var storage := TStudioModel.StorageOf(t);
    var names := '';
    for var prop in TStudioModel.PropertiesOf(t) do
      names := names + IfThen(names <> '', ', ') + prop.Name;

    if storage <> nil then
      LogLine(Format('Type %s: storage ''%s'' with %d objects; properties: %s',
        [TStudioModel.TypeName(t), storage.Name, storage.Count, names]))
    else
      LogLine(Format('Type %s: no storage; properties: %s', [TStudioModel.TypeName(t), names]));
  end;
end;

{ TStudioForm: events }

procedure TStudioForm.FormActivate(Sender: TObject);
begin
  // Back from a Lynx Apps window: show what its apps changed. Queued, so a
  // control whose event activated the form is not destroyed inside that event.
  if _aiOpened then
    TThread.ForceQueue(nil, RefreshAll);

  if Now - _lastServerCheck > 10 / SecsPerDay then
    CheckServer;
end;

procedure TStudioForm.TypeListChange(Sender: TObject);
begin
  if _typeList.ItemIndex >= 0 then
    ShowType(_types[_typeList.ItemIndex]);
end;

procedure TStudioForm.ObjectsGridSelChanged(Sender: TObject);
begin
  ShowInspector;
end;

procedure TStudioForm.AddPropertyClick(Sender: TObject);
begin
  if _selectedType = nil then
    Exit;

  var name := _newPropName.Text.Trim;
  if not IsValidIdent(name) then
  begin
    LogLine('Add property: "' + name + '" is not a valid property name.');
    Exit;
  end;

  for var prop in _properties do
    if SameText(prop.Name, name) then
    begin
      LogLine('Add property: ' + TStudioModel.TypeName(_selectedType) + ' already has a property ' + name + '.');
      Exit;
    end;

  if _newPropType.ItemIndex < 0 then
  begin
    LogLine('Add property: choose a type.');
    Exit;
  end;

  var caption := _newPropLabel.Text.Trim;
  if caption = '' then
    caption := name;

  var typeName := TStudioModel.TypeName(_selectedType);
  if (TypeScriptStore <> nil) and TypeScriptStore.IsScripted(typeName) then
  begin
    AddPropertyToScript(typeName, name, _newPropType.Text, _newPropLabel.Text.Trim);
    _newPropName.Text := '';
    _newPropLabel.Text := '';
    Exit;   // the save refreshes the view
  end;

  var propType: &Type;
  var descriptor: IPropertyDescriptor := nil;
  if _newPropType.ItemIndex = 0 then
    propType := &Type.From<string>
  else
  begin
    TStudioModel.FindType(_newPropType.Text, propType);
    var typeDescriptor := _app.Config.TypeDescriptor(propType);
    if typeDescriptor <> nil then
      descriptor := typeDescriptor.PropertyDescriptor[TStudioModel.TypeName(propType)];
  end;

  try
    TypeScriptStore.AddRuntimeProperty(_selectedType, name, caption, propType, descriptor);
    LogLine(Format('Added %s.%s (%s) at runtime; not saved', [TStudioModel.TypeName(_selectedType), name, _newPropType.Text]));
    _newPropName.Text := '';
    _newPropLabel.Text := '';
  except
    on E: Exception do
      LogLine('Add property failed: ' + E.Message);
  end;

  ShowType(_selectedType);
end;

procedure TStudioForm.EditorComboChange(Sender: TObject);
begin
  var link := Link(Sender);
  var index := (Sender as TComboBox).ItemIndex;
  try
    if index <= 0 then
      WriteValue(link.Prop, link.Item, Default(CObject)) else
      WriteValue(link.Prop, link.Item, link.Choices[index - 1]);
  except
    on E: Exception do
      LogLine('Set ' + link.Prop.Name + ' failed: ' + E.Message);
  end;
  RefreshSelectedRow;
end;

procedure TStudioForm.EditorEditExit(Sender: TObject);
begin
  var link := Link(Sender);
  var edit := Sender as TEdit;
  if edit.Text = link.Original then
    Exit;

  try
    if link.Sub <> nil then
    begin
      // A field of a value: change the field, then write the value back (a
      // property added at runtime stores a marshalled copy)
      var value := link.Prop.GetValue(link.Item, []);
      if value.IsNull then
      begin
        link.Prop.SetValue(link.Item, '{}', []);
        value := link.Prop.GetValue(link.Item, []);
      end;
      link.Sub.SetValue(value, edit.Text, []);
      WriteValue(link.Prop, link.Item, value);
    end
    else
      WriteValue(link.Prop, link.Item, edit.Text);
    link.Original := edit.Text;
  except
    on E: Exception do
    begin
      LogLine('Set ' + link.Prop.Name + ' failed: ' + E.Message);
      edit.Text := link.Original;
    end;
  end;
  RefreshSelectedRow;
end;

procedure TStudioForm.EditorEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkReturn then
    EditorEditExit(Sender);
end;

function ResultError(const Json: string; out Output: string) : string;
begin
  Result := '';
  Output := '';
  var value := TJSONObject.ParseJSONValue(Json);
  try
    if not (value is TJSONObject) then
      Exit('Unexpected result: ' + Json);
    var obj := value as TJSONObject;
    obj.TryGetValue<string>('output', Output);
    if not obj.GetValue<Boolean>('ok', False) then
      Result := obj.GetValue<string>('error', 'failed');
  finally
    value.Free;
  end;
end;

procedure TStudioForm.NewTypeClick(Sender: TObject);
begin
  TDialogService.InputQuery('New type', ['Name (e.g. Invoice)'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult <> mrOk then
        Exit;

      var name := AValues[0].Trim;
      var source :=
        'import {Object_, Provider_, TypeDescriptor_, registerType} from ''../platform.js'';' + sLineBreak +
        sLineBreak +
        'export class ' + name + ' extends Object_ {' + sLineBreak +
        '  get Name() { return this._Name; }' + sLineBreak +
        '  set Name(value) { this._Name = value; }' + sLineBreak +
        sLineBreak +
        '  ToString() { return this.Name; }' + sLineBreak +
        '}' + sLineBreak +
        sLineBreak +
        '// Where the objects come from' + sLineBreak +
        'class ' + name + 'Provider extends Provider_ {' + sLineBreak +
        '  Data(filter) {' + sLineBreak +
        '    return [{ Name: ''First ' + name + ''' }];' + sLineBreak +
        '  }' + sLineBreak +
        '}' + sLineBreak +
        sLineBreak +
        '// The type info: storage and property descriptors' + sLineBreak +
        'class ' + name + 'Type extends TypeDescriptor_ {' + sLineBreak +
        '  constructor() {' + sLineBreak +
        '    super();' + sLineBreak +
        '    this.Provider = new ' + name + 'Provider();' + sLineBreak +
        '    this.PropertyDescriptor = {' + sLineBreak +
        '      Name: { Type: ''string'' },' + sLineBreak +
        '    };' + sLineBreak +
        '  }' + sLineBreak +
        sLineBreak +
        '  get StorageName() { return ''' + name + 's''; }' + sLineBreak +
        '  CreateInstance() { return new ' + name + '(); }' + sLineBreak +
        '}' + sLineBreak +
        sLineBreak +
        'registerType(' + name + ', new ' + name + 'Type());' + sLineBreak;

      var output: string;
      var error := ResultError(TypeScripts.Save(name, source), output);
      if error <> '' then
      begin
        LogLine('New type ' + name + ' failed: ' + error);
        Exit;
      end;

      LoadTypes;
      SelectType(name);
      _pages.ActiveTab := _pages.Tabs[2];
    end);
end;

procedure TStudioForm.SaveScriptClick(Sender: TObject);
begin
  if not _scriptSave.Enabled then
    Exit;

  var name := TStudioModel.TypeName(_selectedType);
  var output: string;
  var error := ResultError(TypeScripts.Save(name, _scriptMemo.Text), output);
  if error <> '' then
  begin
    _bottom.Visible := True;
    _bottomSplitter.Visible := True;
    _bottom.ActiveTab := _bottom.Tabs[0];
    LogLine('Save ' + name + '.js failed (the previous definition is kept):' + sLineBreak + error);
    Exit;
  end;

  if output <> '' then
    LogLine(output);
  LogLine('Saved ' + name + '.js');
  // OnChanged refreshes the view
end;

procedure TStudioForm.DeleteTypeClick(Sender: TObject);
begin
  var name := TStudioModel.TypeName(_selectedType);
  TDialogService.MessageDialog('Delete the type ' + name + ' and its script ' + name + '.js?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult <> mrYes then
        Exit;

      var output: string;
      var error := ResultError(TypeScripts.Delete(name), output);
      if error <> '' then
        LogLine('Delete ' + name + ' failed: ' + error) else
        LogLine('Deleted ' + name);
    end);
end;

procedure TStudioForm.ScriptMemoKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key = Ord('S')) or (KeyChar = 's') or (KeyChar = 'S')) then
  begin
    SaveScriptClick(nil);
    Key := 0;
    KeyChar := #0;
  end;
end;

procedure TStudioForm.OpenAIClick(Sender: TObject);
begin
  _aiOpened := True;
  TAppPlatformLynxAppsWindow.OpenManager;
end;

procedure TStudioForm.RefreshClick(Sender: TObject);
begin
  RefreshAll;
  CheckServer;
end;

procedure TStudioForm.SaveStateClick(Sender: TObject);
begin
  var output: string;
  var error := ResultError(TypeScriptStore.SaveState, output);
  if error <> '' then
    LogLine('Save state failed: ' + error) else
    LogLine(output);
end;

procedure TStudioForm.ForgetStateClick(Sender: TObject);
begin
  var output: string;
  var error := ResultError(TypeScriptStore.DiscardState, output);
  if error <> '' then
    LogLine('Forget state failed: ' + error) else
    LogLine(output);
end;

procedure TStudioForm.TokensClick(Sender: TObject);
begin
  _bottom.Visible := True;
  _bottomSplitter.Visible := True;
  _bottom.ActiveTab := _tokensPage;
end;

procedure TStudioForm.TokenListChange(Sender: TObject);
begin
  if _tokenList.ItemIndex < 0 then
    Exit;
  _tokenName.Text := TLocalTokens.Names[_tokenList.ItemIndex];
  _tokenValue.Text := '';
  _tokenValue.TextPrompt := 'type a new value to replace it';
end;

procedure TStudioForm.SaveTokenClick(Sender: TObject);
begin
  var name := _tokenName.Text.Trim;
  if not IsValidIdent(name) then
  begin
    LogLine('Token: "' + name + '" is not a valid name; use letters and digits, e.g. TrelloKey.');
    Exit;
  end;
  if _tokenValue.Text.Trim = '' then
  begin
    LogLine('Token ' + name + ': type a value (Remove deletes a token).');
    Exit;
  end;

  TLocalTokens.SetValue(name, _tokenValue.Text.Trim);
  LogLine('Token ' + name + ' saved');
  _tokenValue.Text := '';
  ShowTokens;
end;

procedure TStudioForm.RemoveTokenClick(Sender: TObject);
begin
  var name := _tokenName.Text.Trim;
  var value: string;
  if not TLocalTokens.TryGetValue(name, value) then
  begin
    LogLine('Token: there is no token ' + name + '.');
    Exit;
  end;
  TLocalTokens.Remove(name);
  LogLine('Token ' + name + ' removed');
  _tokenName.Text := '';
  ShowTokens;
end;

procedure TStudioForm.ToggleConsoleClick(Sender: TObject);
begin
  _bottom.Visible := not _bottom.Visible;
  _bottomSplitter.Visible := _bottom.Visible;
end;

procedure TStudioForm.RunScriptClick(Sender: TObject);
begin
  _bottom.ActiveTab := _bottom.Tabs[0];
  LogLine('Running script...');
  try
    TPlatform.Run(_script.Text, 'script');
  except
    on E: Exception do
      LogLine('Script error: ' + E.Message);
  end;
  RefreshAll;
end;

procedure TStudioForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkF9 then
  begin
    _bottom.Visible := True;
    _bottomSplitter.Visible := True;
    RunScriptClick(nil);
    Key := 0;
  end;
end;

end.
