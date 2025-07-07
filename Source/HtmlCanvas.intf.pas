unit HtmlCanvas.intf;

interface

uses
  System.Classes, System.SysUtils;

type
  IHtmlStyle = interface;
  IHtmlCanvasElement = interface;
  IPath2D = interface;
  ITextMetrics = interface;

  {$M+}
  IHtmlInterface = interface
    ['{B3EDF048-8D33-4784-8271-37B0D6A66C7F}']
    function  toString: string;
  end;

  {$M+}
  IHtmlClassList = interface(IHtmlInterface)
    ['{F51D8BEF-6C57-4315-87CA-C770E464D430}']
    procedure add(AClass: string);
    procedure remove(AClass: string);
    procedure toggle(AClass: string);
  end;

  {$M+}
  IHtmlElement = interface(IHtmlInterface)
    ['{06A6B43F-C285-4D70-9E1E-930622C2CB18}']
    function  get_classList: IHtmlClassList;
    function  get_clientHeight: Integer;
    function  get_clientWidth: Integer;
    function  get_height: Integer;
    procedure set_height(Value: Integer);
    function  get_parentNode: IHtmlElement;
    function  get_width: Integer;
    procedure set_width(Value: Integer);
    function  get_Style: IHtmlStyle;

    procedure addEventListener(Event: string; AFunc: TProc; UseCapture: Boolean);
    function appendChild(const E: IHtmlElement): IHtmlElement;
    function getAttribute(Name: string) : string;
    function setAttribute(const Name: string; const Value: string): boolean;

    property classList: IHtmlClassList read get_classList;
    property clientHeight: Integer read get_clientHeight;
    property clientWidth: Integer read get_clientWidth;
    property height: Integer read get_height write set_height;
    property parentNode: IHtmlElement read get_parentNode;
    property style: IHtmlStyle read get_style;
    property width: Integer read get_width write set_width;
  end;

  {$M+}
  IHtmlStyle = interface(IHtmlInterface)
    ['{14DE7DBC-512F-49D0-878E-AA576C822477}']
    function  get_display: string;
    procedure set_display(Value: string);
    function  get_height: string;
    procedure set_height(Value: string);
    function  get_width: string;
    procedure set_width(Value: string);

    function getPropertyValue(aproperty: string) : string;
    property display: string read get_display write set_display;
    property height: string read get_height write set_height;
    property width: string read get_width write set_width;
  end;

  {$M+}
  IRenderingContext = interface(IHtmlInterface)
    ['{286903B2-D1C8-4540-8729-38D9B756E70C}']
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

    // Paths
    procedure arc(x, y, radius, startAngle, endAngle:Single; anticlockwise: Boolean = False);
    procedure arcTo(x1, y1, x2, y2, radius: Single);
    procedure beginPath;
    procedure bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: Single);
    procedure clip(fillRule: string = 'nonzero'); overload;
    procedure clip(path: IPath2D; fillRule: string = 'nonzero'); overload;
    procedure closePath;
    function  isPointInPath(x, y: Single; fillRule: string = 'nonzero') : Boolean; overload;
    function  isPointInPath(path: IPath2D; x, y: Single; fillRule: string = 'nonzero') : Boolean; overload;
    procedure fill(fillRule: string = 'nonzero'); overload;
    procedure fill(path: IPath2D; fillRule: string = 'nonzero'); overload;
    procedure moveTo(x, y: Single);
    procedure lineTo(x, y: Single);
    procedure quadraticCurveTo(cpx, cpy, x, y: Single);
    procedure stroke;

    procedure fillText(const Value: string; x, y: Single);

    // Rects
    procedure clearRect(x, y, width, height: Single);
    procedure fillRect(x, y, width, height: Single);
    function  measureText(AText: string) : ITextMetrics;
    procedure rect(x, y, width, height: Single);
    procedure restore;
    procedure save;
    procedure strokeRect(x, y, width, height: Single);

    procedure setLineDash(const segments: TArray<Integer>);
    procedure translate(x, y: Single);
    procedure rotate(angle: Single);

    property direction: string read get_direction write set_direction;
    property canvas: IHtmlCanvasElement read get_canvas;
    property fillStyle: string read get_fillStyle write set_fillStyle;
		property font: string read get_font write set_font;
    property strokeStyle: string read get_strokeStyle write set_strokeStyle;
    property textAlign: string read get_textAlign write set_textAlign;
    property textBaseline: string read get_textBaseline write set_textBaseline;
    property lineCap: string read get_lineCap write set_lineCap;
    property lineDashOffset: Double read get_lineDashOffset write set_lineDashOffset;
    property lineJoin: string read get_lineJoin write set_lineJoin;
    property linewidth: Single read get_lineWidth write set_lineWidth;

  end;

  {$M+}
  ICanvasRenderingContext2D = interface(IRenderingContext)
    ['{F7A12D3E-2891-469D-961B-EFDD2442ED1D}']

  end;

  {$M+}
  IHtmlCanvasElement = interface(IHtmlElement)
    ['{91D501B4-B60F-4721-B0A8-F9BDDE4F7360}']
    function  getContext(Context: string) : IRenderingContext;
  end;

  IPath2D = interface(IHtmlInterface)
    ['{09F332E2-A5E0-47CC-824E-6540728395CE}']
  end;

  IWindow = interface(IHtmlInterface)
    ['{72E71EC3-E3CF-44F7-9580-37365A6054A5}']
    procedure requestAnimationFrame(AFunc: TProc<Double>);
    function  getComputedStyle(element: IHtmlElement; pseudoElt: string = '') : IHtmlStyle;
    function  get_devicePixelRatio: Double;

    property devicePixelRatio: Double read get_devicePixelRatio;
  end;

  IHtmlDocument = interface(IHtmlElement)
    ['{4FBF49A5-EE5E-4172-ABF9-85821D2916B5}']
    function get_defaultView: IWindow;
    function get_head: IHtmlElement;

    function CreateElement(const ATag: string): IHtmlElement;
    function CreateTextNode(const AContent: string): IHtmlElement;

    property defaultView: IWindow read get_defaultView;
    property head: IHtmlElement read get_head;
  end;

  ITextMetrics = interface(IHtmlInterface)
    ['{C4E3BFA0-EE53-4906-931D-E9565641764C}']
    function get_width : Double;
    property width: Double read get_width;
  end;

implementation

end.

