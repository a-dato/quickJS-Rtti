unit Integrations.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.WebBrowser,
  SG.ScriptGate;

type
  TIntegrationJavaScriptEvaluator = reference to function(const JavaScriptCode: string): string;

  TIntegrationsframe = class(TFrame)
    Layout1: TLayout;
    WebBrowser1: TWebBrowser;
  private
    FScriptGate: TScriptGate;
    FJavaScriptEvaluator: TIntegrationJavaScriptEvaluator;

    const LocalServerUrl = 'http://localhost:3000/';
  public
    constructor Create(AOwner: TComponent); override;

    property JavaScriptEvaluator: TIntegrationJavaScriptEvaluator read FJavaScriptEvaluator write FJavaScriptEvaluator;

  published
    function ExecuteJavaScript(const JavaScriptCode: string): string;
  end;

implementation

{$R *.fmx}

constructor TIntegrationsframe.Create(AOwner: TComponent);
begin
  inherited;

  FScriptGate := TScriptGate.Create(Self, WebBrowser1, 'lynx-local');
  WebBrowser1.Navigate(LocalServerUrl);
end;

function TIntegrationsframe.ExecuteJavaScript(const JavaScriptCode: string): string;
begin
  if not Assigned(FJavaScriptEvaluator) then
    Exit('');

  Result := FJavaScriptEvaluator(JavaScriptCode);
end;

end.
