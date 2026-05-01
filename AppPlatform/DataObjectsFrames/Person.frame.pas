unit Person.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.JSON,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, FMX.ScrollControl.Impl,
  FMX.ScrollControl.WithRows.Impl, FMX.ScrollControl.WithCells.Impl,
  FMX.ScrollControl.WithEditableCells.Impl, FMX.ScrollControl.DataControl.Impl,
  FMX.WebBrowser,
  QuickJS.Register.intf,
  QuickJS.Register.dn4d.impl,
  SG.ScriptGate,
  App.Storage.intf,
  System_;

type
  TPersonFrame = class(TFrame)
    Layout1: TLayout;
    WebBrowser1: TWebBrowser;
    procedure Button1Click(Sender: TObject);
  private
    FRuntime: IJSRuntime;
    FContext: IJSContext;
    FScriptGate: TScriptGate;

    class function JavaScriptString(const Value: string): string; static;
    class function ObjectPropertyText(const Value: CObject; const PropertyName: string): string; static;
    procedure SendPersonsToBrowser(const Persons: IStorage);

    const LocalServerUrl = 'http://localhost:3000/';
  public
    constructor Create(AOwner: TComponent); override;

  published
    function GetPersons: IStorage;

  end;

implementation

uses
  App.intf;

{$R *.fmx}

{ TPersonFrame }

constructor TPersonFrame.Create(AOwner: TComponent);
begin
  inherited;

  FRuntime := TJSRuntimeDN4D.Create;
  FContext := FRuntime.CreateContext;
  FScriptGate := TScriptGate.Create(Self, WebBrowser1, 'lynx-local');

  WebBrowser1.Navigate(LocalServerUrl);
end;

function TPersonFrame.GetPersons: IStorage;
begin
  Result := _app.Storage['Persons'];
  SendPersonsToBrowser(Result);
end;

class function TPersonFrame.JavaScriptString(const Value: string): string;
var
  JsonString: TJSONString;
begin
  JsonString := TJSONString.Create(Value);
  try
    Result := JsonString.ToJSON;
  finally
    JsonString.Free;
  end;
end;

class function TPersonFrame.ObjectPropertyText(const Value: CObject; const PropertyName: string): string;
begin
  Result := '';

  if Value = nil then
    Exit;

  try
    var Prop := Value.GetType.PropertyByName(PropertyName);
    if Prop <> nil then
      Result := Prop.GetValue(Value, []).ToString;
  except
    Result := '';
  end;
end;

procedure TPersonFrame.Button1Click(Sender: TObject);
begin
  FScriptGate.CallScript('changeText(' + JavaScriptString('Hello from Delphi') + ');', nil);
end;

procedure TPersonFrame.SendPersonsToBrowser(const Persons: IStorage);
var
  Payload: TJSONObject;
  Items: TJSONArray;
begin
  Payload := TJSONObject.Create;
  try
    Payload.AddPair('name', Persons.Name);
    Payload.AddPair('count', TJSONNumber.Create(Persons.Count));

    Items := TJSONArray.Create;
    Payload.AddPair('items', Items);

    for var I := 0 to Persons.Count - 1 do
    begin
      var Person := Persons[I];
      var Item := TJSONObject.Create;

      Item.AddPair('index', TJSONNumber.Create(I + 1));
      Item.AddPair('id', ObjectPropertyText(Person, 'ID'));
      Item.AddPair('name', ObjectPropertyText(Person, 'Name'));
      Item.AddPair('age', ObjectPropertyText(Person, 'Age'));
      Item.AddPair('text', Person.ToString);

      Items.AddElement(Item);
    end;

    FScriptGate.CallScript(
      'if (window.onPersons) window.onPersons(' + Payload.ToJSON + ');',
      nil);
  finally
    Payload.Free;
  end;
end;

end.
