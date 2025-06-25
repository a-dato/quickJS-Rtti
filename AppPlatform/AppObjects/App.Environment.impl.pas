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
  App.EditorPanel.intf, ADato.ObjectModel.intf, App.PropertyDescriptor.intf,
  App.PathProperty;

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

  procedure ConcatNames(const Names: TArray<string>; out PropertyName: string; out SubProperties: TArray<string>);
  begin
    // names: ['IProject', 'Customer', 'Address', 'Zip', '1' <-- Name indexer]

    var n := High(Names);
    // Do we have an indexer?
    var d: Integer;
    if Integer.TryParse(Names[High(Names)], d) then
      dec(n);

     // ['Customer', 'Address', 'Zip']
    SubProperties := Copy(Names, 1, n);

    PropertyName := '';   // Customer.Address.Zip

    for var i := 1 to n do
    begin
      if PropertyName = '' then
        PropertyName := Names[i] else
        PropertyName := PropertyName + '.' + Names[i];
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
      var propertyName: string;           // 'Customer.Address.Zip'
      var subProperties: TArray<string>;  // ['Customer', 'Address', 'Zip']

      ConcatNames(names, {out}propertyName, {out}subProperties);
      var hasNestedProperty: Boolean := Length(subProperties) > 1;

      if (c is TDataControl) and (propertyName = 'Model') then
      begin
        (c as TDataControl).Model := AModel;
        continue;
      end;

      var objectProperty := DataType.PropertyByName(propertyName);
      if objectProperty <> nil then
      begin
        if hasNestedProperty then
        begin
          var path_prop: _PropertyInfo := TPathProperty.Create(objectProperty);
          var sub_property: ISubProperty := path_prop as ISubProperty;
          var sub_prop_type := DataType;

          for var propName in subProperties do
          begin
            var sub_prop := sub_prop_type.PropertyByName(propName);

            if sub_prop = nil then
              break;

            sub_prop_type := sub_prop.GetType;
            var sub_property_objectType := _app.Config.ObjectType[sub_prop_type];
            if sub_property_objectType <> nil then
            begin
              // Get PropertyDescriptor for 'CustomerType' in CustomerType class
              var descriptor: IPropertyDescriptor := sub_property_objectType.PropertyDescriptor[sub_prop_type.Name];
              if descriptor <> nil then
                sub_prop := TPropertyWithDescriptor.Create(sub_prop, descriptor);
            end;

            sub_property := sub_property.Add(sub_prop);
          end;

          objectProperty := path_prop;
        end
        else
        begin
          // 'Customer' property => type = Customer
          var property_objectType := _app.Config.ObjectType[objectProperty.GetType];
          if property_objectType <> nil then
          begin
            var descriptor: IPropertyDescriptor := property_objectType.PropertyDescriptor[objectProperty.GetType.Name];
            if descriptor <> nil then
              objectProperty := TPropertyWithDescriptor.Create(objectProperty, descriptor);
          end;
        end;

        var bind := TPropertyBinding.CreateBindingByControl(c);
        AModel.ObjectModelContext.Bind(WrapProperty(objectProperty), bind);
      end;


//      var editor: IEditorPanel;
//      if Interfaces.Supports<IEditorPanel>(c, editor) then
//      begin
//        var p1 := DataType.PropertyByName(propertyName);
//        if p1 <> nil then
//        begin
////          if (ObjectType <> nil) and (ObjectType.PropertyDescriptor <> nil) then
////            editor.PropertyDescriptor := ObjectType.PropertyDescriptor[names[1]];
//
//          EditorManager.AddEditorBinding(WrapProperty(nil, p1), editor);
//        end;
//      end
//      else if (c is TDataControl) and (propertyName = 'Model') then
//        (c as TDataControl).Model := AModel
//      else
//      begin
//        // Gets the property from the parent object (IProject):
//        // propertyName   -> 'Customer'
//        //                -> 'Customer.Address'
//        //                -> 'Customer.Address.Zip'
//        var objectProperty := DataType.PropertyByName(propertyName);
//        if objectProperty <> nil then
//        begin
//          var parentDescriptor: IPropertyDescriptor := nil;
//          var propertyDescriptor: IPropertyDescriptor := nil;
//
//          // Get's type of the property
//          var objectPropertyType := objectProperty.GetType;  // CustomerType
//
//          // Get IObjectType from type register
//          var objectPropertyObjectType := _app.Config.ObjectType[objectPropertyType];
//
//          if objectPropertyObjectType <> nil then
//          begin
//
//
//            // Get property descriptor aka: CustomerType.PropertyDescriptor[CustomerType]
//            parentDescriptor := objectPropertyObjectType.PropertyDescriptor[objectPropertyType.Name];
//            if (parentDescriptor <> nil) and (parentDescriptor.Marshaller <> nil) then
//
//
//
//            propertyDescriptor := objectPropertyObjectType.PropertyDescriptor[objectPropertyName];
//          end;
//
//          var parentProp: _PropertyInfo := nil;
//          if objectPropertyName <> '' then
//            parentProp := DataType.PropertyByName(names[1]);  // Customer
//
//          var bind := TPropertyBinding.CreateBindingByControl(c);
//
//          var wrapped := TObjectModelMarshalledPropertyWrapper.Create(parentProp, parentDescriptor, p2, propertyDescriptor);
//          AModel.ObjectModelContext.Bind(wrapped, bind);
//          // AModel.ObjectModelContext.Bind(WrapProperty(parentProp, p2), bind);
//        end;
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
