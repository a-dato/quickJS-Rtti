unit App.Component.FMX.impl;

interface

uses
  System_,
  FMX.Types,
  App.Component.intf,
  App.Component.impl,
  System.Collections.Generic;

type
  {$M+}
  TFmxComponent = class(App.Component.impl.TComponent)
  protected
    _control: TFMXObject;

    function get_Control: TFMXObject;
    function GetObject: TObject; override;
    function get_Name: CString; override;
    function get_Children: List<IComponent>; override;

  public
    constructor Create(const Parent: IComponent; const Control: TFMXObject);

    // Wraps each control directly contained by Container in a TFmxComponent
    class function CreateChildren(const Parent: IComponent; const Container: TFMXObject) : List<IComponent>; static;

    property Control: TFMXObject read get_Control;
  end;

implementation

uses
  FMX.Controls;

{ TFmxComponent }

constructor TFmxComponent.Create(const Parent: IComponent; const Control: TFMXObject);
begin
  inherited Create(Parent);
  _control := Control;
end;

class function TFmxComponent.CreateChildren(const Parent: IComponent; const Container: TFMXObject) : List<IComponent>;
begin
  Result := CList<IComponent>.Create;

  if (Container <> nil) and (Container.Children <> nil) then
    for var child in Container.Children do
    begin
      var cmp: IComponent;
      if Interfaces.Supports<IComponent>(child, cmp) then
        Result.Add(cmp)
      else if child is TControl then
        Result.Add(TFmxComponent.Create(Parent, child));
    end;
end;

function TFmxComponent.get_Children: List<IComponent>;
begin
  Result := CreateChildren(Self, _control);
end;

function TFmxComponent.GetObject: TObject;
begin
  Result := _control;
end;

function TFmxComponent.get_Name: CString;
begin
  if _control <> nil then
    Result := _control.Name else
    Result := inherited;
end;

function TFmxComponent.get_Control: TFMXObject;
begin
  Result := _control;
end;

end.
