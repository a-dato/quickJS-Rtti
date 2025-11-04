unit App.Environment.impl;

interface

uses
  System_,
  System.Collections.Generic,
  FMX.Forms,
  FMX.Types,

  App.Environment.intf,
  App.TypeDescriptor.intf,
  App.Windows.intf,
  ADato.ObjectModel.List.intf,
  App.intf,
  App.Config.intf,
  App.Content.intf,
  App.Storage.intf;

type
  Environment = class(TBaseInterfacedObject, IEnvironment)
  type
    TFormClass = class of TForm;

  protected
    class var _FormClass: TFormClass;

  protected
    function get_MainForm: IWindow;

    function get_TickCount: Integer;

    function CreateWindow(const AType: &Type; const AOwner: CObject)  : IWindow;

  public
    class constructor Create;
    class property FormClass: TFormClass read _FormClass write _FormClass;
  end;

  TFrameBuilder = class(TBaseInterfacedObject, IContentBuilder)
  protected
    _creator: WindowFrameCreateFunc;

    // IContentBuilder
    function Build(const AOwner: CObject): CObject;

  public
    constructor Create(const ACreator: WindowFrameCreateFunc);

    property Creator: WindowFrameCreateFunc read _creator;
  end;

  TFrameBinder = class(TBaseInterfacedObject, IContentBinder)
  protected
    _handlers: List<IContextChangedHandler>;

    procedure BindChildren(const AType: &Type; const Children: TFmxChildrenList; const AModel: IObjectListModel);
    procedure Bind(const AType: &Type; const Control: TObject; const Storage: IStorage);

    function WrapProperty(const AProp: _PropertyInfo) : _PropertyInfo; virtual;
  public
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.ClassHelpers,
  ADato.ObjectModel.Binders,
  System.Collections,
  ADato.ObjectModel.List.Tracking.impl,
  ADato.ObjectModel.impl,
  ADato.ObjectModel.intf,
  App.PropertyDescriptor.intf
  ,App.Windows.FMX.impl
  ,FMX.Controls
  ,FMX.ScrollControl.DataControl.Impl
  , ADato.ObjectModel.List.impl
  , App.ObjectModelWithDescriptor.impl
  , App.Windows.impl;

{ Environment }

class constructor Environment.Create;
begin

end;

function Environment.CreateWindow(const AType: &Type; const AOwner: CObject) : IWindow;
begin
  var cmp: TComponent := nil;
  if (AOwner <> nil) and not AOwner.TryAsType<TComponent>(cmp) then
    raise ArgumentException.Create('AOwner must be of type TComponent');

  var f := FormClass.Create(cmp);
  Result := TWindow.Create(_app, AType, f);
end;

function Environment.get_MainForm: IWindow;
begin
  Result := nil;
end;

function Environment.get_TickCount: Integer;
begin
  Result := System_.Environment.TickCount;
end;

{ TFrameBuilder }

function TFrameBuilder.Build(const AOwner: CObject): CObject;
begin
  var cmp: TComponent := nil;
  if (AOwner <> nil) and not AOwner.TryAsType<TComponent>(cmp) then
    raise ArgumentException.Create('AOwner must be of type TComponent');
//  Result := _frameClass.Create();
//  (Result as TFrame).Owner
end;

constructor TFrameBuilder.Create(const ACreator: WindowFrameCreateFunc);
begin
  _creator := ACreator;
end;

{ TFrameBinder }

procedure TFrameBinder.BindChildren(const AType: &Type; const Children: TFmxChildrenList; const AModel: IObjectListModel);

  function ConcatNames(var Names: TArray<string>) : string;
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

  function GetModel(const Names: TArray<string>) : IObjectListModel;
  begin
    var parentModel := AModel;

    // Loop will be skipped for IProject_Model
    // Go deep for nested properties: IProject_Customers_Model
    for var i := 1 to High(Names) - 1 do
    begin
      var parentProperty := parentModel.ObjectType.PropertyByName(Names[i]);

      if parentProperty = nil then // Property does not exists
        Exit(nil);

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
        var childModel: IObjectListModel := TObjectModelWithDescriptor<IBaseInterface>.Create(dscr.GetType, dscr);
        _handlers.Add(TContextChangedHandler.Create(parentModel, childModel, parentProperty));
        parentModel := childModel;
      end else
        Exit(nil);
    end;

    Exit(parentModel);
  end;

begin
  for var c in Children do
  begin
    // Name looks like
    //  ObjectType_Property                     -> IProject_Name
    //  ObjectType_Property_Index               -> IProject_Name_1
    //  ObjectType_Property_SubProperty_Index   -> IProject_Customer_Address_1
    var names := string(c.Name).Split(['_']);
    if (Length(names) >= 2) and (names[0] = AType.Name) then
    begin
      var propertyName := ConcatNames(names); // 'Customer.Address.Zip'

      if names[High(names)] = 'Model' then  // IProject_Model, IProject_Customers_Model
      begin
        var mdl := GetModel(names);

        if mdl <> nil then
        begin
          if (c is TDataControl) then
            (c as TDataControl).Model := AModel else
            BindChildren(mdl.ObjectType, c.Children, mdl);
        end;

        continue;
      end;

      var objectProperty := AType.PropertyByName(propertyName);
      if objectProperty <> nil then
      begin
        var bind := TPropertyBinding.CreateBindingByControl(c);
        AModel.ObjectModelContext.Bind(propertyName, bind);
      end;

//      var objectProperty := AType.PropertyByName(propertyName);
//      if objectProperty <> nil then
//      begin
//        var bind := TPropertyBinding.CreateBindingByControl(c);
//        AModel.ObjectModelContext.Bind(WrapProperty(objectProperty), bind);
//      end;
    end;

    if c.ChildrenCount > 0 then
      BindChildren(AType, c.Children, AModel);
  end;
end;

function TFrameBinder.WrapProperty(const AProp: _PropertyInfo): _PropertyInfo;
begin
  if not Interfaces.Supports<IObjectModelProperty>(AProp) then
    Result := TObjectModelPropertyWrapper.Create(AProp) else
    Result := AProp;
end;

procedure TFrameBinder.Bind(const AType: &Type; const Control: TObject; const Storage: IStorage);
begin
  _handlers := CList<IContextChangedHandler>.Create;

  if Control is TControl then
  begin
    var ctrl: TControl := Control as TControl;

    // var objectType := _app.Config.TypeDescriptor(AType);

    var model: IObjectListModel := nil;
    model := TObjectListModelWithChangeTracking<IBaseInterface>.Create(AType);
    model.Context := Storage.Data;

//    if not Storage.Data.TryAsType<IObjectListModel>(model) then
//    begin
//      var list: IList;
//      if Storage.Data.TryAsType<IList>(list) then
//      begin
//        model := TObjectListModelWithChangeTracking<IBaseInterface>.Create(AType);
//        model.Context := list;
//      end;
//    end;

    if model <> nil then
    begin
      // var tp := model.ObjectModel.GetType;
      if ctrl.ChildrenCount > 0 then
      begin
//        var editorManager: IEditorManager;
//        if Interfaces.Supports<IEditorManager>(ctrl, editorManager) then
//          editorManager.Bind(model.ObjectModelContext);
//
//        BindChildren(ctrl.Children, model, editorManager, AType, objectType);
        BindChildren(AType, ctrl.Children, model);
      end;
    end else
      raise CException.Create('Data is invalid');
  end;
end;

end.

