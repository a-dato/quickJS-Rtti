unit WebViewTestForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  App.WebView2Frame;

type
  TWebViewTest = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    frame: TWebView2Frame;
    { Public declarations }
  end;

var
  WebViewTest: TWebViewTest;

implementation

uses
  System.IOUtils, App.Component.intf, ADato.ObjectModel.Binders;

{$R *.fmx}

procedure TWebViewTest.FormCreate(Sender: TObject);
begin
  frame := TWebView2Frame.Create(Self);
  AddObject(frame);
  frame.Align := TAlignLayout.Client;
  // The exe runs from Win64\<Config>, the page lives in DataObjectsFrames
  frame.HTML := TFile.ReadAllText(TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\DataObjectsFrames\Customer.html')), TEncoding.UTF8);
end;

procedure TWebViewTest.Button1Click(Sender: TObject);
begin
  var c := (frame as IComponent).Children;
  var bind := c[0] as IControlWithBindingSupport;
  bind.Value := 'Test';
end;

end.
