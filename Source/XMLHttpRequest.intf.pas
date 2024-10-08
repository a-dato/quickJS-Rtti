unit XMLHttpRequest.intf;

interface

uses
  System.Rtti,
  System.Classes,
  System.SysUtils;

type
  TReadyStateChangeEvent = TProc;

  {$M+}
  IXMLHttpRequest = interface
    ['{8CB80592-A126-4F30-8E0D-C1B06B826AEB}']
    function  get_readyState: Word;
    function  get_onreadystatechange: TReadyStateChangeEvent;
    procedure set_onreadystatechange(const Value: TReadyStateChangeEvent);
    function  get_response: TValue;
    function  get_responseText: string;
    function  get_responseType: string;
    procedure set_responseType(const Value: string);
    function  get_responseXML: string;
    function  get_status: Word;

    procedure open(Method: string; Url: string; [DefaultValue(True)] Async: Boolean = True);
    procedure overrideMimeType(Value: string);
    procedure setRequestHeader(Header: string; Value: string);
    procedure send(); overload;
    procedure send(Value: string); overload;
    procedure send(Data: TStream); overload;

    property onreadystatechange: TReadyStateChangeEvent read get_onreadystatechange write set_onreadystatechange;
    property response: TValue read get_response;
    property responseText: string read get_responseText;
    property responseType: string read get_responseType write set_responseType;
    property responseXML: string read get_responseXML;
    property status: Word read get_status;
  end;

implementation

end.
