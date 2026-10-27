unit App.Environment.impl;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic,

  App.intf,
  App.Component.intf,
  App.Environment.intf,
  App.TypeDescriptor.intf,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.Config.intf,
  App.Content.intf,
  App.Storage.intf,

  {$IFDEF FRAMEWORK_VCL}
  VCL.Forms
  {$ELSE}
  FMX.Types,
  FMX.Forms
  {$ENDIF}
  ;

type
  Environment = class(TBaseInterfacedObject, IEnvironment)
  type
    TFormClass = class of TForm;

  protected
    class var _MainWindow: IWindow;

  protected
    _formClass: TFormClass;
    _windows: List<IWindow>;
    _appActions: IAppActions;
    _persistence: IAppPersistence;

    function get_MainWindow: IWindow;
    function get_Windows: List<IWindow>;
    function get_TickCount: Integer;
    function get_Persistence: IAppPersistence;

    function CreateWindow(const AType: &Type; const AOwner: IComponent)  : IWindow;
    function IsOpen(const AObject: CObject) : Boolean;
    function Open(const AObject: CObject) : Boolean;
    function Close(const AObject: CObject) : Boolean;

  public
    constructor Create(const AClass: TFormClass; const AppActions: IAppActions; const Persistence: IAppPersistence = nil);
  end;

  TFrameBinder = class(TBaseInterfacedObject, IContentBinder)
  protected
    _handlers: List<IContextChangedHandler>;

//    {$IFDEF FRAMEWORK_VCL}
//    function  CreateChildrenList(const AControl: TWinControl) : TChildrenList;
//    {$ELSE}
//    function  CreateChildrenList(const AControl: TFmxObject) : TChildrenList;
//    {$ENDIF}

    procedure BindChildren(const AType: &Type; const Children: List<IComponent>; const AModel: IObjectListModel);

    procedure Bind(const AType: &Type; const Window: IWindow; const Data: CObject);

    function WrapProperty(const AProp: _PropertyInfo) : _PropertyInfo; virtual;
  public
  end;

{$IFDEF DEBUG}
var modelLock: IBaseInterface;
{$ENDIF}

implementation

uses
  System.Classes,
  System.SysUtils,
  System.ClassHelpers,
  ADato.ObjectModel.Binders,
  ADato.ObjectModel.List.Tracking.impl,
  ADato.ObjectModel.impl,
  ADato.ObjectModel.intf,
  App.PropertyDescriptor.intf,
  ADato.ObjectModel.List.impl,
  App.ObjectModelWithDescriptor.impl,
  App.Windows.impl
  {$IFDEF FRAMEWORK_VCL}
  {$ELSE}
  , FMX.ScrollControl.DataControl.Impl
  {$ENDIF}
  , FMX.ScrollControl.DataControl.Binders;

{ Environment }

constructor Environment.Create(const AClass: TFormClass; const AppActions: IAppActions; const Persistence: IAppPersistence = nil);
begin
  _formClass := AClass;
  _appActions := AppActions;
  _persistence := Persistence;

  _windows := CList<IWindow>.Create;
end;

function Environment.CreateWindow(const AType: &Type; const AOwner: IComponent) : IWindow;
begin
  var wnd: IWindow := nil;

  TThread.Synchronize(nil, procedure begin
    var f := _formClass.Create(nil);
    wnd := TWindow.Create(AType, f);
    _windows.Add(wnd);
  end);

  Result := wnd;
end;

function Environment.get_MainWindow: IWindow;
begin
  if _MainWindow = nil then
    _MainWindow := TWindow.Create(&Type.From<IWindow>, Application.MainForm);

  Result := _MainWindow;
end;

function Environment.get_Persistence: IAppPersistence;
begin
  Result := _persistence;
end;

function Environment.get_TickCount: Integer;
begin
  Result := System_.Environment.TickCount;
end;

function Environment.get_Windows: List<IWindow>;
begin
  Result := _windows;
end;

// IAppActions
function Environment.Close(const AObject: CObject): Boolean;
begin
  Result := False;
end;

function Environment.IsOpen(const AObject: CObject): Boolean;
begin
  Result := False;
end;

function Environment.Open(const AObject: CObject): Boolean;
begin
  Result := _appActions.Open(AObject);
end;

{ TFrameBinder }
//{$IFDEF FRAMEWORK_VCL}
//function TFrameBinder.CreateChildrenList(const AControl: TWinControl) : TChildrenList;
//begin
//  Result := CList<TControl>.Create(AControl.ControlCount);
//  for var i := 0 to AControl.ControlCount -  1 do
//    Result.Add(AControl.Controls[i]);
//end;
//{$ELSE}
//function TFrameBinder.CreateChildrenList(const AControl: TFmxObject) : TChildrenList;
//begin
//  Result := AControl.Children;
//end;
//{$ENDIF}

procedure TFrameBinder.BindChildren(const AType: &Type; const Children: List<IComponent>; const AModel: IObjectListModel);

  function ConcatNames(var Names: StringArray) : CString;
  begin
    // names: ['IProject', 'Customer', 'Address', 'Zip', '1' <-- Name indexer]

    // Do we have an indexer?
    var d: Integer;
    if Integer.TryParse(Names[High(Names)], d) then
      SetLength(Names, Length(Names) - 1);  // Remove indexer

    Result := '';

    for var i := 1 to High(Names) do
    begin
      if i = 1 then
        Result := Names[i] else
        Result := Result  + '.' + Names[i];
    end;
  end;

  function GetParentModel(const Names: StringArray) : IObjectListModel;
  begin
    var s: IStorage;

    if CString.Equals(AModel.ObjectType.Name, Names[0]) then
      Result := AModel

    else if _app.TryGetStorage(Names[0], s) then
    begin
      Result := TObjectListModelWithChangeTracking<IBaseInterface>.Create(s.DataType);
      Result.Context := s.Data;
    end;
  end;

  function GetModel(const Names: StringArray; out PropertyToBind: _PropertyInfo) : IObjectListModel;
  begin
    PropertyToBind := nil;

    var parentModel := GetParentModel(Names);

    // Loop will be skipped for IProject_Model
    // Go deep for nested properties: IProject_Customers_Model
    for var i := 1 to High(Names) - 1 do
    begin
      var parentProperty := parentModel.ObjectType.PropertyByName(Names[i]);

      if parentProperty = nil then // Property does not exists
        Exit(nil);

      PropertyToBind := parentProperty;

      var handlerExists := False;
      for var handler in _handlers do
      begin
        if handler.ParentModel.Equals(parentModel) and handler.ParentProperty.Equals(parentProperty) then
        begin
          handlerExists := True;
          parentModel := handler.ChildModel;
          break;
        end;
      end;

      // Create a new model?
      var dscr: IPropertyDescriptor;
      if not handlerExists and Interfaces.Supports<IPropertyDescriptor>(parentProperty, dscr) then
      begin
        var t: &Type := dscr.GetType;
        var childModel: IObjectListModel := TObjectModelWithDescriptor<IBaseInterface>.Create(t, dscr);
        _handlers.Add(TContextChangedHandler.Create(parentModel, childModel, parentProperty));
        parentModel := childModel;
      end else
        Exit(nil);
    end;

    Exit(parentModel);
  end;

  function GetPathProperty(const OwnerType: &Type; const Names: StringArray) : _PropertyInfo;
  begin
    var path: TArray<_PropertyInfo>;
    SetLength(path, Length(Names) - 1);
    var property_type := OwnerType;

    for var i := 1 to High(Names) do
    begin
      var sub_property := property_type.PropertyByName(Names[i]);
      if sub_property = nil then
        Exit(nil);
      path[i] := sub_property;
      property_type := sub_property.GetType;
    end;

    Result := TPathProperty.Create(path[0], path);
  end;

begin
  {$IFDEF FRAMEWORK_FMX}
  for var child in Children do
  begin
    // Name looks like
    //  ObjectType_Property                     -> IProject_Name
    //  ObjectType_Property_Index               -> IProject_Name_1
    //  ObjectType_Property_SubProperty_Index   -> IProject_Customer_Address_1
    var names := child.Name.Split(['_']);
    if Length(names) >= 2 then
    begin
      var propertyName := ConcatNames(names); // 'Customer.Address.Zip'

      if names[High(names)] = 'Model' then  // IProject_Model, Customers_Model
      begin
        var propertyToBind: _PropertyInfo;
        var mdl := GetModel(names, {out}propertyToBind);

        if mdl <> nil then
        begin
          if (child.GetObject is TDataControl) then
          begin
            if propertyToBind <> nil then
            begin
              (child as TDataControl).Model := mdl;

              // mdl.ObjectModelContext.Bind(propertyToBind, bind);
            end else
              (child.GetObject as TDataControl).Model := mdl;
          end else
            BindChildren(mdl.ObjectType, child.Children, mdl);
        end;

        continue;
      end;

      // propertyName -> 'Description'
      // propertyName -> 'Customer.Address'
      // propertyName -> 'Holidays.IceCreeam'
      var objectProperty := AType.PropertyByName(propertyName);

      if (objectProperty = nil) and (Length(names) > 2) then
        objectProperty := GetPathProperty(AType, names);

      if objectProperty <> nil then
      begin
        var control_binding: IControlWithBindingSupport;
        var binder: IPropertyBinding;
        if Interfaces.Supports<IControlWithBindingSupport>(child, control_binding) then
          binder := TGenericControlBinding.Create(control_binding)
        else
        begin
          binder := TPropertyBinding.CreateBindingByControl(TFMXObject(child.GetObject));
          // IProject_Holidays -> Binder is a TDataControlBinding
          // KV: 14/07/2026 -> This code should not be necesary
          if binder is TDataControlBinding then
            (binder as TDataControlBinding).PropType := TTreePropertyType.DataList;
        end;

        AModel.ObjectModelContext.Bind(propertyName, binder);
      end;
    end;

    BindChildren(AType, child.Children, AModel);
  end;
  {$ENDIF}
end;

function TFrameBinder.WrapProperty(const AProp: _PropertyInfo): _PropertyInfo;
begin
  if not Interfaces.Supports<IObjectModelProperty>(AProp) then
    Result := TObjectModelPropertyWrapper.Create(AProp) else
    Result := AProp;
end;

procedure TFrameBinder.Bind(const AType: &Type; const Window: IWindow; const Data: CObject);
begin
  {$IFDEF FRAMEWORK_FMX}
  var children := Window.Children;
  if (children <> nil) and (children.Count > 0) then
  begin
    _handlers := CList<IContextChangedHandler>.Create;

    var model: IObjectListModel := nil;
    model := TObjectListModelWithChangeTracking<IBaseInterface>.Create(AType);

    var l: IList;
    var s: IStorage;

    if Data.TryAsType<IStorage>(s) then
      l := s.Data
    else if not Data.TryAsType<IList>(l) then
      raise CException.Create('Data is invalid');

    model.Context := l;

    if l.Count > 0 then
      model.ObjectContext := l[0];

    BindChildren(AType, children, model);

    modelLock := model;
  end;
  {$ENDIF}
end;

end.


