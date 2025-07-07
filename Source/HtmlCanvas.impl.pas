unit HtmlCanvas.impl;

interface

uses
  System.Generics.Collections,
  FMX.Graphics, FMX.Objects, fmx.fhtmldraw, System.SysUtils,
  quickjs, QuickJS.Register.intf,
  System.Math.Vectors, FMX.Types,
  HtmlCanvas.intf;

type
  THtmlCanvasElement = class;

  THtmlInterface = class(TInterfacedObject, IHtmlInterface)
  protected
    function toString: string;
  end;

  THtmlStyle = class(THtmlInterface, IHtmlStyle)
  protected
    _style: TElementStyle;

    function  get_display: string;
    procedure set_display(Value: string);
    function  get_height: string;
    procedure set_height(Value: string);
    function  get_width: string;
    procedure set_width(Value: string);

    function getPropertyValue(aproperty: string) : string;
  public
    constructor Create(AStyle: TElementStyle);
  end;

  THtmlClassList = class(THtmlInterface, IHtmlClassList)
  protected
    _element: TElement;

    procedure add(AClass: string);
    procedure remove(AClass: string);
    procedure toggle(AClass: string);
  public
    constructor Create(AElement: TElement);
  end;

  THtmlElement = class(THtmlInterface, IHtmlElement)
  protected
    _element: TElement;

    function  get_classList: IHtmlClassList;
    function  get_clientHeight: Integer;
    function  get_clientWidth: Integer;
    function  get_height: Integer;
    procedure set_height(Value: Integer);
    function  get_width: Integer;
    function  get_parentNode: IHtmlElement;
    procedure set_width(Value: Integer);
    function  get_Style: IHtmlStyle;

    procedure addEventListener(Event: string; AFunc: TProc; UseCapture: Boolean);
    function appendChild(const E: IHtmlElement): IHtmlElement;
    function getAttribute(Name: string) : string;
    function setAttribute(const Name: string; const Value: string): boolean;
  public
    constructor Create(AElement: TElement);
  end;

  {$M+}
  TRenderingContext = class(THtmlInterface, IRenderingContext)
  protected
    _beginSceneCount: Integer;
    _Owner: THtmlCanvasElement;
    _canvas: TCanvas;
    _canvasStates: TList<TCanvasSaveState>;
    _lineWidth: Single;
    _matrix: TMatrix;
    _textBaseline: string;
    _textAlign: TTextAlign;
    _path: TPathData;

    procedure BeginScene;
    procedure EndScene;
    function  get_canvas : IHtmlCanvasElement;
    function  get_direction: string;
    procedure set_direction(const Value: string);
		function  get_fillStyle: string;
    procedure set_fillStyle(const Value: string);
		function  get_font: string;
    procedure set_font(const Value: string);
    function  get_strokeStyle: string;
    procedure set_strokeStyle(const Value: string);
    function  get_textAlign: string;
    procedure set_textAlign(const Value: string);
    function  get_textBaseline: string;
    procedure set_textBaseline(const Value: string);
    function  get_lineCap: string;
    procedure set_lineCap(const Value: string);
    function  get_lineDashOffset: Double;
    procedure set_lineDashOffset(const Value: Double);
    function  get_lineJoin: string;
    procedure set_lineJoin(const Value: string);
    function  get_lineWidth: Single;
    procedure set_lineWidth(const Value: Single);

    // Path
    procedure arc(x, y, radius, startAngle, endAngle:Single;
                 [DefaultValueAttribute(False)] anticlockwise: Boolean = False);

    procedure arcTo(x1, y1, x2, y2, radius: Single);
    procedure beginPath;
    procedure bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: Single);
    procedure clip([DefaultValueAttribute('nonzero')] fillRule: string = 'nonzero'); overload;
    procedure clip(path: IPath2D; [DefaultValueAttribute('nonzero')] fillRule: string = 'nonzero'); overload;
    procedure closePath;
    function  isPointInPath(x, y: Single; fillRule: string = 'nonzero') : Boolean; overload;
    function  isPointInPath(path: IPath2D; x, y: Single; fillRule: string = 'nonzero') : Boolean; overload;
    procedure fill(fillRule: string = 'nonzero'); overload;
    procedure fill(path: IPath2D; fillRule: string = 'nonzero'); overload;
    procedure quadraticCurveTo(cpx, cpy, x, y: Single);
    procedure stroke;

    procedure clearRect(x, y, width, height: Single);
    procedure fillRect(x, y, width, height: Single);
    procedure fillText(const AText: string; x, y: Single);
    procedure lineTo(x, y: Single);
    procedure moveTo(x, y: Single);
    function  measureText(AText: string) : ITextMetrics;
    procedure rect(x, y, width, height: Single);
    procedure restore;
    procedure save;
    procedure setLineDash(const segments: TArray<Integer>);
    procedure strokeRect(x, y, width, height: Single);

    procedure translate(x, y: Single);
    procedure rotate(angle: Single);

  public
    constructor Create(AOwner: THtmlCanvasElement; ACanvas: TCanvas);
    destructor Destroy; override;
  end;

  TCanvasRenderingContext2D = class(TRenderingContext, ICanvasRenderingContext2D)

  end;

  THtmlCanvasElement = class(THtmlElement, IHtmlCanvasElement)
  protected
    _context: IRenderingContext;
    _image: TImage;

    function  getContext(Context: string) : IRenderingContext;
  public
    constructor Create(AElement: TElement; AImage: TImage);
    destructor Destroy; override;
  end;

  THtmlDocument = class(THtmlElement, IHtmlDocument)
  protected
    _creationTimestamp: Integer;
    _document: THtDocument;
    [weak] _window: IWindow;

    function get_defaultView: IWindow;
    function get_head: IHtmlElement;

    function  CreateElement(const ATag: string): IHtmlElement;
    function  CreateTextNode(const AContent: string): IHtmlElement;
    procedure DocumentBeforePaint(Sender: TObject);

  public
    constructor Create(AWindow: IWindow; ADocument: THtDocument);
    destructor Destroy; override;
  end;

  THtmlWindow = class(THtmlInterface, IWindow, IJSExtendableObject)
  protected
    class var _renderingContexts: TList<IRenderingContext>;

  private
    _animationFrames: TList<TProc<Double>>;
    _ownPropertyValues: TDictionary<string, JSValue>;

    // IJSExtendableObject
    function  define_own_property(const Name: string) : Boolean;
    function  GetValue(const Name: string): JSValue;
    procedure SetValue(const Name: string; Value: JSValue);

    function  get_devicePixelRatio: Double;
    function  getComputedStyle(element: IHtmlElement;
                [DefaultValueAttribute('')] pseudoElt: string = '') : IHtmlStyle;
    procedure DoRequestAnimationFrame(timestamp: Double);
    procedure requestAnimationFrame(AFunc: TProc<Double>);
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create;
    destructor Destroy; override;
  end;

  TTextMetrics = class(THtmlInterface, ITextMetrics)
  protected
    _width: Double;
    function get_width : Double;

  public
    constructor Create(AWidth: Double);
  end;

implementation

uses
  System.UITypes, System.Types, htmlcss, System_;

{ THtmlCanvas }

procedure TRenderingContext.arc(x, y, radius, startAngle, endAngle: Single;
  anticlockwise: Boolean);
begin

end;

procedure TRenderingContext.arcTo(x1, y1, x2, y2, radius: Single);
begin

end;

procedure TRenderingContext.beginPath;
begin
  _path := TPathData.Create;
end;

function TRenderingContext.get_canvas: IHtmlCanvasElement;
begin
  Result := _owner;
end;

function TRenderingContext.get_direction: string;
begin

end;

function TRenderingContext.get_fillStyle: string;
begin

end;

function TRenderingContext.get_font: string;
begin
  Result := _canvas.Font.Family;
end;

function TRenderingContext.get_lineCap: string;
begin

end;

function TRenderingContext.get_lineDashOffset: Double;
begin

end;

function TRenderingContext.get_lineJoin: string;
begin

end;

function TRenderingContext.get_lineWidth: Single;
begin
  Result := _lineWidth;
end;

function TRenderingContext.get_strokeStyle: string;
begin

end;

function TRenderingContext.get_textAlign: string;
begin

end;

function TRenderingContext.get_textBaseline: string;
begin
  Result := _textBaseline;
end;

function TRenderingContext.isPointInPath(path: IPath2D; x, y: Single;
  fillRule: string): Boolean;
begin

end;

function TRenderingContext.isPointInPath(x, y: Single;
  fillRule: string): Boolean;
begin

end;

procedure TRenderingContext.lineTo(x, y: Single);
begin
  _path.LineTo(TPointF.Create(x, y));
end;

function TRenderingContext.measureText(AText: string): ITextMetrics;
begin
  Result := TTextMetrics.Create(_canvas.TextWidth(AText));
end;

procedure TRenderingContext.moveTo(x, y: Single);
begin
  _path.MoveTo(TPointF.Create(x, y));
end;

procedure TRenderingContext.quadraticCurveTo(cpx, cpy, x, y: Single);
begin

end;

procedure TRenderingContext.BeginScene;
begin
  inc(_beginSceneCount);
  if _beginSceneCount = 1 then
  begin
    _canvas.BeginScene;
    _canvas.SetMatrix(_matrix);
    _canvas.Stroke.Thickness := _lineWidth;
    _canvas.Fill.Color := TAlphaColorRec.Blue;
  end;
end;

procedure TRenderingContext.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: Single);
begin

end;

procedure TRenderingContext.clearRect(x, y, width, height: Single);
begin
  BeginScene;
  _canvas.ClearRect(TRectF.Create(TPointF.Create(x, y), width, height));
  EndScene;
end;

procedure TRenderingContext.clip(path: IPath2D; fillRule: string);
begin
  // Not supported
end;

procedure TRenderingContext.closePath;
begin
  if _path <> nil then
    _path.ClosePath;
end;

procedure TRenderingContext.clip(fillRule: string);
begin
  // Not supported
//  _canvas.IntersectClipRect()
//  if _path <> nil then
//    _path
end;

constructor TRenderingContext.Create(AOwner: THtmlCanvasElement; ACanvas: TCanvas);
begin
  inherited Create;
  _Owner := AOwner;
  _canvas := ACanvas;
  _canvasStates := TList<TCanvasSaveState>.Create;

  _lineWidth := 1;
  _matrix := TMatrix.Identity;
end;

destructor TRenderingContext.Destroy;
begin
  _canvasStates.Free;
  inherited;
end;

procedure TRenderingContext.EndScene;
begin
  dec(_beginSceneCount);
  if _beginSceneCount = 0 then
    _canvas.EndScene;
end;

procedure TRenderingContext.fill(fillRule: string);
begin

end;

procedure TRenderingContext.fill(path: IPath2D; fillRule: string);
begin

end;

procedure TRenderingContext.fillRect(x, y, width, height: Single);
begin
  BeginScene;
  _canvas.FillRect(TRectF.Create(TPointF.Create(x, y), width, height), 100);
  EndScene;
end;

procedure TRenderingContext.fillText(const AText: string; x, y: Single);
begin
  BeginScene;
  var r := TRectF.Create(0, 0, 9999, 9999);
  _canvas.MeasureText(r, AText, False, [], TTextAlign.Leading, TTextAlign.Leading);

  if _textAlign = TTextAlign.Leading then
    r.Offset(x, 0)
  else if _textAlign = TTextAlign.Center then
    r.Offset(x - r.Width / 2, 0)
  else
    r.Offset(x - r.Width, 0);

  if _textBaseline = 'middle' then
    r.Offset(0, y - r.Height / 2)
  else if (_textBaseline = 'bottom') or (_textBaseline = 'alphabetic') or (_textBaseline = 'ideographic') then
    r.Offset(0, y - r.Height)
  else
    r.Offset(0, y);

  _canvas.FillText(r, AText, False, 100, [], TTextAlign.Leading, TTextAlign.Leading);
  EndScene;
end;

procedure TRenderingContext.rect(x, y, width, height: Single);
begin
  BeginScene;
  _canvas.DrawRect(TRectF.Create(x, y, x + width, y + height), 0, 0, [], 100);
  EndScene;
end;

procedure TRenderingContext.restore;
begin
  if _canvasStates.Count > 0 then
  begin
    _canvas.RestoreState(_canvasStates[_canvasStates.Count - 1]);
    _canvasStates.Delete(_canvasStates.Count - 1);

    _lineWidth := _canvas.Stroke.Thickness;
    _matrix := _canvas.Matrix;
  end;
end;

procedure TRenderingContext.rotate(angle: Single);
begin
  _matrix := _matrix * TMatrix.CreateRotation(angle);
  if _beginSceneCount > 0 then
    _canvas.SetMatrix(_matrix);
end;

procedure TRenderingContext.save;
begin
  _canvasStates.Add(_canvas.SaveState);
end;

procedure TRenderingContext.setLineDash(const segments: TArray<Integer>);
begin

end;

procedure TRenderingContext.set_direction(const Value: string);
begin

end;

procedure TRenderingContext.set_fillStyle(const Value: string);
begin
  if Value.StartsWith('rgba') then
  begin
    var e := Value.Substring(5, Value.Length - 6) .Split([',']);
    _canvas.Fill.Color := TAlphaColorF.Create(
        Single.Parse(e[0], TFormatSettings.Invariant),
        Single.Parse(e[1], TFormatSettings.Invariant),
        Single.Parse(e[2], TFormatSettings.Invariant),
        Single.Parse(e[3], TFormatSettings.Invariant)).ToAlphaColor;
  end;
end;

procedure TRenderingContext.set_font(const Value: string);
begin
  // Value looks like 'Normal 12px "Helvetica"'
  //_canvas.Font.Family := Value;
end;

procedure TRenderingContext.set_lineCap(const Value: string);
begin

end;

procedure TRenderingContext.set_lineDashOffset(const Value: Double);
begin

end;

procedure TRenderingContext.set_lineJoin(const Value: string);
begin

end;

procedure TRenderingContext.set_lineWidth(const Value: Single);
begin
  _lineWidth := Value;
end;

procedure TRenderingContext.set_strokeStyle(const Value: string);
begin

end;

procedure TRenderingContext.set_textAlign(const Value: string);
begin
  if (Value = 'left') or (Value = 'Start') then
    _textAlign := TTextAlign.Leading
  else if (Value = 'center') then
    _textAlign := TTextAlign.Center
  else if (Value = 'right') or (Value = 'end') then
    _textAlign := TTextAlign.Trailing;
end;

procedure TRenderingContext.set_textBaseline(const Value: string);
begin
  _textBaseline := Value;
end;

procedure TRenderingContext.stroke;
begin
  if _path <> nil then
  begin
    BeginScene();
    _canvas.DrawPath(_path, 100);
    EndScene;
    FreeAndNil(_path);
  end;
end;

procedure TRenderingContext.strokeRect(x, y, width, height: Single);
begin
  BeginScene;
  _canvas.DrawRect(TRectF.Create(TPointF.Create(x, y), width, height), 0, 0, [], 100);
  EndScene;
end;

procedure TRenderingContext.translate(x, y: Single);
begin
  _matrix := _matrix * TMatrix.CreateTranslation(x, y);
  if _beginSceneCount > 0 then
    _canvas.SetMatrix(_matrix);
end;

constructor THtmlCanvasElement.Create(AElement: TElement; AImage: TImage);
begin
  inherited Create(AElement);
  _image := AImage;
end;

destructor THtmlCanvasElement.Destroy;
begin
  if _context <> nil then
    THtmlWindow._renderingContexts.Remove(_context);
end;

function THtmlCanvasElement.getContext(Context: string): IRenderingContext;
begin
  if _context = nil then
  begin
    _image.Bitmap := TBitmap.Create(Trunc(_image.Width), Trunc(_image.Height));
    _context := TCanvasRenderingContext2D.Create(Self, _image.Bitmap.Canvas);
  end;

  Result := _context;

  THtmlWindow._renderingContexts.Add(_context);
end;

{ THtmlStyle }
constructor THtmlStyle.Create(AStyle: TElementStyle);
begin
  _style := AStyle;
end;

function THtmlStyle.getPropertyValue(aproperty: string): string;
begin
  Exit('0');
//  if aproperty = 'max-width' then
//    // Exit(_style.FindProp(TCSSProperty.cspMaxWidth).M.AsString);
//  if aproperty = 'padding-left' then
//    // Exit(_style.FindProp(TCSSProperty.cspPaddingLeft).M.AsString);
//  if aproperty = 'padding-right' then
//    // Exit(_style.FindProp(TCSSProperty.cspPaddingRight).M.AsString);
end;

function THtmlStyle.get_display: string;
begin
  case _style.Display of
    TDisplayStyle.dsInline:
      Result := 'inline';
//     = (dsInline, dsBlock, dsInlineBlock, dsNone, dsTable,
//    dsTableRow, dsTableRowGroup, dsTableCell, dsListItem, dsFlex, dsInlineFlex, dsInlineTable);
  end;
end;

function THtmlStyle.get_height: string;
begin
  Result := _style.Height.AsString;
end;

function THtmlStyle.get_width: string;
begin
  Result := _style.Width.AsString;
end;

procedure THtmlStyle.set_display(Value: string);
begin
  // _style.Display := Value;
end;

procedure THtmlStyle.set_height(Value: string);
begin
  _style.Height.Parse(Value);
end;

procedure THtmlStyle.set_width(Value: string);
begin
  _style.Width.Parse(Value);
end;

{ THtmlInterface }

function THtmlInterface.toString: string;
begin
  Result := Self.Classname;
end;

{ THtmlElement }
procedure THtmlElement.addEventListener(Event: string; AFunc: TProc; UseCapture: Boolean);
begin

end;

function THtmlElement.appendChild(const E: IHtmlElement): IHtmlElement;
begin
  Result := THtmlElement.Create(_element.AppendChild(THtmlElement(E)._element));
end;

constructor THtmlElement.Create(AElement: TElement);
begin
  inherited Create;
  _element := AElement;
end;

function THtmlElement.getAttribute(Name: string): string;
begin
  Result := _element.getAttribute(Name);
end;

function THtmlElement.get_classList: IHtmlClassList;
begin
  Result := THtmlClassList.Create(_element);
end;

function THtmlElement.get_clientHeight: Integer;
begin
  Result := 400;
end;

function THtmlElement.get_clientWidth: Integer;
begin
  Result := 400;
end;

function THtmlElement.get_height: Integer;
begin
  Result := _element.Style.Height.Pixels;
end;

function THtmlElement.get_parentNode: IHtmlElement;
begin
  Result := THtmlElement.Create(_element.Parent);
end;

function THtmlElement.get_Style: IHtmlStyle;
begin
  Result := THtmlStyle.Create(_element.Style);
end;

function THtmlElement.get_width: Integer;
begin
  Result := _element.Style.Width.Pixels;
end;

function THtmlElement.setAttribute(const Name, Value: string): boolean;
begin
  Result := _element.setAttribute(Name, Value);
end;

procedure THtmlElement.set_height(Value: Integer);
begin
  _element.UserHeight := Value;
end;

procedure THtmlElement.set_width(Value: Integer);
begin
  _element.UserWidth := Value;
end;

{ THtmlDocument }
constructor THtmlDocument.Create(AWindow: IWindow; ADocument: THtDocument);
begin
  inherited Create(ADocument);
  _creationTimestamp := Environment.TickCount;
  _window := AWindow;
  _document := ADocument;
  _document.BeforePaint := DocumentBeforePaint;
end;

function THtmlDocument.CreateElement(const ATag: string): IHtmlElement;
begin
  Result := THtmlElement.Create(_document.CreateElement(ATag));
end;

function THtmlDocument.CreateTextNode(const AContent: string): IHtmlElement;
begin
  Result := THtmlElement.Create(_document.CreateTextNode(AContent));
end;

destructor THtmlDocument.Destroy;
begin

  inherited;
end;

procedure THtmlDocument.DocumentBeforePaint(Sender: TObject);
begin
  var t: Double := Environment.TickCount - _creationTimestamp;
  if _Window <> nil then
    THtmlWindow(_Window).DoRequestAnimationFrame(t);
end;

function THtmlDocument.get_defaultView: IWindow;
begin
  Result := _window;
end;

function THtmlDocument.get_head: IHtmlElement;
begin
  Result := THtmlElement.Create(_document.Head);
end;

{ THtmlClassList }

procedure THtmlClassList.add(AClass: string);
begin
  _element.AddClass(AClass);
end;

constructor THtmlClassList.Create(AElement: TElement);
begin
  inherited Create;
  _element := AElement;
end;

procedure THtmlClassList.remove(AClass: string);
begin
  _element.RemoveClass(AClass);
end;

procedure THtmlClassList.toggle(AClass: string);
begin
  _element.ToggleClass(AClass);
end;

{ THtmlWindow }
class constructor THtmlWindow.Create;
begin
  _renderingContexts := TList<IRenderingContext>.Create;
end;

class destructor THtmlWindow.Destroy;
begin
  _renderingContexts.Free;
end;

constructor THtmlWindow.Create;
begin
  _animationFrames := TList<TProc<Double>>.Create;
  _ownPropertyValues := TDictionary<string, JSValue>.Create;
end;

function THtmlWindow.get_devicePixelRatio: Double;
begin
  Result := 1; // = 96 DPI
end;

destructor THtmlWindow.Destroy;
begin
  _animationFrames.Free;
  _ownPropertyValues.Free;
  inherited;
end;

procedure THtmlWindow.DoRequestAnimationFrame(timestamp: Double);
begin
  if _animationFrames.Count > 0 then
  begin
    for var ctx in _renderingContexts do
      TRenderingContext(ctx).BeginScene;

    for var proc in _animationFrames do
      proc(timestamp);

    _animationFrames.Clear;

    for var ctx in _renderingContexts do
      TRenderingContext(ctx).EndScene;
  end;
end;

function THtmlWindow.getComputedStyle(element: IHtmlElement; pseudoElt: string = ''): IHtmlStyle;
begin
  Result := THtmlStyle.Create(THtmlElement(element)._element.Style);
end;

procedure THtmlWindow.requestAnimationFrame(AFunc: TProc<Double>);
begin
  _animationFrames.Add(AFunc);
end;

// IJSExtendableObject
function THtmlWindow.define_own_property(const Name: string) : Boolean;
begin
  Result := True; // We accept any new property
end;

function THtmlWindow.GetValue(const Name: string): JSValue;
begin
  if not _ownPropertyValues.TryGetValue(Name, Result) then
    Result := JS_NULL;
end;

procedure THtmlWindow.SetValue(const Name: string; Value: JSValue);
begin
  _ownPropertyValues.AddOrSetValue(Name, Value);
end;

{ TTextMetrics }

constructor TTextMetrics.Create(AWidth: Double);
begin
  _width := AWidth;
end;

function TTextMetrics.get_width: Double;
begin
  Result := _width;
end;

end.

