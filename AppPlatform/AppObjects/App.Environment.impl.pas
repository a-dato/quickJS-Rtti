unit App.Environment.impl;

interface

uses
  System_,
  App.Environment.intf,
  App.Objects.intf,
  App.Windows.intf,
  FMX.Forms,
  FMX.Types,
  ADato.ObjectModel.List.intf,
  App.intf,
  App.Content.intf, App.EditorManager.intf;

type
  Environment = class(TBaseInterfacedObject, IEnvironment)
  type
    TFormClass = class of TForm;

  protected
    class var _FormClass: TFormClass;

  protected
    function get_TickCount: Integer;

    function CreateWindowFrame(const AOwner: CObject; const AType: &Type) : IWindowFrame;

  public
    class constructor Create;
    class property FormClass: TFormClass read _FormClass write _FormClass;
  end;

  TFrameBuilder = class(TBaseInterfacedObject, IContentBuilder)
  type
    TFrameClass = class of TFrame;

  protected
    _frameClass: TFrameClass;

    // IContentBuilder
    function Build(const AOwner: CObject): CObject;

  public
    constructor Create(FrameClass: TFrameClass);

    property FrameClass: TFrameClass read _frameClass;
  end;

  TFrameBinder = class(TBaseInterfacedObject, IContentBinder)
  protected
    procedure BindChildren(const Children: TFmxChildrenList; const AModel: IObjectListModel;
      const EditorManager: IEditorManager; const DataType: &Type; const ObjectType: IObjectType);
    procedure Bind(const AContent: CObject; const AType: &Type; const Data: CObject);

    function WrapProperty(const AProp: _PropertyInfo) : _PropertyInfo; virtual;
  public
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.ClassHelpers,
  App.Windows.FMX.impl,
  FMX.Controls,
  ADato.ObjectModel.Binders,
  FMX.DataControl.Impl, System.Collections,
  ADato.ObjectModel.List.Tracking.impl, ADato.ObjectModel.impl,
  App.EditorPanel.intf, ADato.ObjectModel.intf, App.PropertyDescriptor.intf;

{ Environment }

class constructor Environment.Create;
begin

end;

function Environment.CreateWindowFrame(const AOwner: CObject; const AType: &Type) : IWindowFrame;
begin
  var cmp: TComponent := nil;
  if (AOwner <> nil) and not AOwner.TryAsType<TComponent>(cmp) then
    raise ArgumentException.Create('AOwner must be of type TComponent');

  var f := FormClass.Create(cmp);
  Result := WindowFrame.Create(f);
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
  Result := _frameClass.Create(cmp);
end;

constructor TFrameBuilder.Create(FrameClass: TFrameClass);
begin
  _frameClass := FrameClass;
end;

{ TFrameBinder }

procedure TFrameBinder.BindChildren(const Children: TFmxChildrenList; const AModel: IObjectListModel;
  const EditorManager: IEditorManager; const DataType: &Type; const ObjectType: IObjectType);

  function ConcatNames(const Names: TArray<string>) : string;
  begin
    // names: ['IProject', 'Customer', 'Address', 'Zip', '1' <-- Name indexer]
    var n := High(Names);

    // Do we have an indexer?
    var d: Integer;
    if Integer.TryParse(Names[High(Names)], d) then
      dec(n);

    Result := '';

    for var i := 1 to n do
    begin
      if i = 1 then
        Result := Names[i] else
        Result := Result  + '.' + Names[i];
    end;
  end;

begin
  for var c in Children do
  begin
    // Name looks like
    //  ObjectType_Property                     -> IProject_Name
    //  ObjectType_Property_Index               -> IProject_Name_1
    //  ObjectType_Property_SubProperty_Index   -> IProject_Customer_Address_1
    var names := string(c.Name).Split(['_']);
    if (Length(names) >= 2) and (names[0] = DataType.Name) then
    begin
      var propertyName := ConcatNames(names); // 'Customer.Address.Zip'

      if (c is TDataControl) and (propertyName = 'Model') then
      begin
        (c as TDataControl).Model := AModel;
        continue;
      end;

      var objectProperty := DataType.PropertyByName(propertyName);
      if objectProperty <> nil then
      begin
        var bind := TPropertyBinding.CreateBindingByControl(c);
        AModel.ObjectModelContext.Bind(WrapProperty(objectProperty), bind);
      end;
    end;

    if c.ChildrenCount > 0 then
      BindChildren(c.Children, AModel, EditorManager, DataType, ObjectType);
  end;
end;

function TFrameBinder.WrapProperty(const AProp: _PropertyInfo): _PropertyInfo;
begin
  if not Interfaces.Supports<IObjectModelProperty>(AProp) then
    Result := TObjectModelPropertyWrapper.Create(AProp) else
    Result := AProp;
end;

procedure TFrameBinder.Bind(const AContent: CObject; const AType: &Type; const Data: CObject);
begin
  var ctrl: TControl;
  if AContent.TryAsType<TControl>(ctrl) then
  begin
    var objectType := _app.Config.ObjectType[AType];
//    var dataType := objectType.GetType;

    var model: IObjectListModel := nil;
    if not Data.TryAsType<IObjectListModel>(model) then
    begin
      var list: IList;
      if Data.TryAsType<IList>(list) then
      begin
//        model := TObjectListModelWithChangeTracking<IBaseInterface>.Create(dataType);
        model := TObjectListModelWithChangeTracking<IBaseInterface>.Create(AType);
        model.Context := list;
      end;
    end;

    if model <> nil then
    begin
      // var tp := model.ObjectModel.GetType;
      if ctrl.ChildrenCount > 0 then
      begin
        var editorManager: IEditorManager;
        if Interfaces.Supports<IEditorManager>(ctrl, editorManager) then
          editorManager.Bind(model.ObjectModelContext);

        BindChildren(ctrl.Children, model, editorManager, AType, objectType);
        // BindChildren(ctrl.Children, model, editorManager, dataType, objectType);
      end;
    end else
      raise CException.Create('Data is invalid');
  end;
end;

end.
