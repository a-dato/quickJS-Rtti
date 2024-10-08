unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, fmx.fhtmldraw, FMX.Layouts,
  fmx.fhtmlcomp, FMX.ScrollBox, FMX.Memo, QuickJS.Register.intf, FMX.Objects,
  FMX.Memo.Types;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Memo2: TMemo;
    Button3: TButton;
    Memo3: TMemo;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Splitter1: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Image1: TImage;
    HtPanel: THtPanel;
    Button4: TButton;

    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure HtPanelPainting(Sender: TObject; Canvas: TCanvas; const ARect:
        TRectF);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
  public
    _Context: IJSContext;

    procedure StartEngine;
  end;

var
  Form2: TForm2;

implementation

uses
  QuickJS.Register.impl, System.Rtti, System.TypInfo, System.Math.Vectors,
  HtmlCanvas.intf, HtmlCanvas.impl, quickjs, HtDocumentExtension;

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Button2Click(nil);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  StartEngine;

  Image1.Bitmap := TBitmap.Create(Trunc(Image1.Width), Trunc(Image1.Height));

  var canvas := HtPanel.Document.GetElementbyId('myChart');
  var elem: IHtmlCanvasElement := THtmlCanvasElement.Create(canvas, Image1);

  TJSRegister.RegisterLiveObject<IHtmlCanvasElement>(_context.ctx, 'canvasElement', elem);

  var s := AnsiString(Memo2.Lines.Text);
  _Context.eval_buf(PAnsiChar(s), length(s), '', JS_EVAL_TYPE_MODULE);

// Image1.Bitmap := TBitmap.Create(Trunc(Image1.Width), Trunc(Image1.Height));
//  var c := Image1.Bitmap.Canvas;
//  var st := Image1.Canvas.SaveState;
//  c.BeginScene;
//   c.Stroke.Color := TAlphaColorRec.Blue;
//  c.Stroke.Kind := TBrushKind.Solid;
//  c.Stroke.Thickness := 1.4;
//   c.DrawRect(TRect.Create(0, 0, 100, 100), 0, 0, [], 100);
//  c.EndScene;
//  c.RestoreState(st);
end;

procedure TForm2.StartEngine;
begin
  if _Context = nil then
  begin
    var rt: IJSRuntime := TJSRuntime.Create;
    rt.LogString :=
      procedure(s: string)
      begin
        if s.StartsWith('xxx') then
          Memo3.Lines.Add(s) else
          Memo3.Lines.Add(s);
      end;
    _Context := TJSContext.Create(rt);

    var wnd: IWindow := THtmlWindow.Create;
    TJSRegister.RegisterLiveObject<IWindow>(_context.ctx, 'window', wnd);
    TJSRegister.RegisterLiveObject<IWindow>(_context.ctx, 'self', wnd);

    var doc: IHtmlDocument := THtmlDocument.Create(wnd, HtPanel.Document);
    TJSRegister.RegisterLiveObject<IHtmlDocument>(_context.ctx, 'document', doc);
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  HtPanel.HTML.Text := Memo1.Text;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  StartEngine;
  var s := AnsiString(Memo2.Lines.Text);
  _Context.eval_buf(PAnsiChar(s), length(s), '', JS_EVAL_TYPE_MODULE);

  HtPanel.Document.Refresh;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  Image1.Bitmap := TBitmap.Create(Trunc(Image1.Width), Trunc(Image1.Height));
  var c := Image1.Bitmap.Canvas;
  //var st := Image1.Canvas.SaveState;
  c.BeginScene;
//  c.FillRect()
//  c.Stroke.Color := TAlphaColorRec.Blue;
//  c.Stroke.Kind := TBrushKind.Solid;
//  c.Stroke.Thickness := 1.4;
  c.Fill.Kind := TBrushKind.Solid;
  c.Fill.Color := TAlphaColorRec.Black;
  c.FillRect(TRect.Create(0, 0, 100, 100), 0, 0, [], 100);
  c.EndScene;
  //c.RestoreState(st);
end;

procedure TForm2.HtPanelPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);

  procedure fixup_document(const D: THtDocument; const E: TElement);
  begin
    for var c in E.ChildNodeList do
    begin
      if not (c is TElement) then
        continue;
      var elem := TElement(c);
      elem.Document := D;
      fixup_document(D, elem);
    end;
  end;

begin
  fixup_document(HtPanel.Document, HtPanel.Document);
end;

procedure TForm2.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  MyRect: TRectF;
begin
  // sets the rectangle to be drawn
  MyRect := TRectF.Create(50, 40, 500, 500);
  Canvas.BeginScene;
  // draws the rectangle on the canvas
  Canvas.DrawRect(MyRect, 30, 60, AllCorners, 100);
  Canvas.EndScene;
end;

end.
