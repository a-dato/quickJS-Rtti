unit Person.frame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.WebBrowser,
  SG.ScriptGate, System.JSON,
  System.Generics.Collections;

type
  TPersonJavaScriptEvaluator = reference to procedure(const JavaScriptCode: string);

  TPersonFrame = class(TFrame)
    Layout1: TLayout;
    WebBrowser1: TWebBrowser;
  private
    FScriptGate: TScriptGate;
    FPendingJson: string;
    FPageLoaded: Boolean;
    FJavaScriptEvaluator: TPersonJavaScriptEvaluator;
    FActions: TDictionary<string, string>;

    class function JavaScriptString(const Value: string): string; static;
    procedure WebBrowserDidFinishLoad(ASender: TObject);
    procedure TrySetData;

    const LocalServerUrl = 'http://localhost:3000/';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property JavaScriptEvaluator: TPersonJavaScriptEvaluator read FJavaScriptEvaluator write FJavaScriptEvaluator;

  published
    procedure ExecuteAction(const Name, Arg0, Arg1: string);
    procedure RegisterAction(const Name, JavaScriptFunctionSource: string);
    procedure SetData(const Json: string);
  end;

implementation

{$R *.fmx}

constructor TPersonFrame.Create(AOwner: TComponent);
begin
  inherited;

  FActions := TDictionary<string, string>.Create;

  FScriptGate := TScriptGate.Create(Self, WebBrowser1, 'lynx-local');
  WebBrowser1.OnDidFinishLoad := WebBrowserDidFinishLoad;
  WebBrowser1.Navigate(LocalServerUrl);
end;

destructor TPersonFrame.Destroy;
begin
  FActions.Free;
  inherited;
end;

procedure TPersonFrame.ExecuteAction(const Name, Arg0, Arg1: string);
begin
  if not Assigned(FJavaScriptEvaluator) then
    Exit;

  var JavaScriptFunctionSource: string;
  if not FActions.TryGetValue(Name, JavaScriptFunctionSource) then
    Exit;

  FJavaScriptEvaluator(
    '(' + JavaScriptFunctionSource + ')(' + Arg0 + ', ' + Arg1 + ');');
end;

procedure TPersonFrame.RegisterAction(const Name, JavaScriptFunctionSource: string);
begin
  FActions.AddOrSetValue(Name, JavaScriptFunctionSource);
end;

class function TPersonFrame.JavaScriptString(const Value: string): string;
begin
  var JsonString := TJSONString.Create(Value);
  try
    Result := JsonString.ToJSON;
  finally
    JsonString.Free;
  end;
end;

procedure TPersonFrame.SetData(const Json: string);
begin
  FPendingJson := Json;
  TrySetData;
end;

procedure TPersonFrame.TrySetData;
begin
  if not FPageLoaded or (FPendingJson = '') then
    Exit;

  FScriptGate.CallScript(
    'if (window.setData) window.setData(' + JavaScriptString(FPendingJson) + ');',
    nil);
end;

procedure TPersonFrame.WebBrowserDidFinishLoad(ASender: TObject);
begin
  FPageLoaded := True;
  TrySetData;
end;

end.
