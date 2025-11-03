unit XMLHttpRequest.impl;

interface

uses
  System.SysUtils,
  System.Net.HttpClientComponent,
  XMLHttpRequest.intf,
  
  System.Rtti,
  System.Net.HttpClient,
  System.Classes;

type
  {$M+}
  TXMLHttpRequest = class(TInterfacedObject, IXMLHttpRequest)
  const
    READYSTATE_UNSENT = 0;
    READYSTATE_OPENED = 1;
    READYSTATE_HEADERS_RECEIVED = 2;
    READYSTATE_LOADING = 3;
    READYSTATE_DONE = 4;

  protected
    _HttpClient: TNetHttpClient;
    _HttpRequest: TNetHttpRequest;
    _response: IHTTPResponse;
    _readyState: Word;
    _responseType: string;
    _onreadystatechange: TReadyStateChangeEvent;
    _sendContent: TStream;

    procedure InternalSetReadyState(State: Word);
    procedure OnReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var Abort: Boolean);
    procedure OnRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);

    function  get_readyState: Word;
    function  get_onreadystatechange: TReadyStateChangeEvent;
    procedure set_onreadystatechange(const Value: TReadyStateChangeEvent);

    function  get_response: TValue;
    function  get_responseText: string;
    function  get_responseType: string;
    procedure set_responseType(const Value: string);
    function  get_responseXML: string;
    function  get_status: Word;

    // procedure open(Method: JSValue; Url: JSValue; Async: JSValue = JS_TAG_NULL; User: JSValue = JS_TAG_NULL; Password: JSValue = JS_TAG_NULL);
    procedure open(Method: string; Url: string; Async: Boolean = true);
    procedure overrideMimeType(Value: string);
    procedure setRequestHeader(Header: string; Value: string);
    procedure send(); overload;
    procedure send(Value: string); overload;
    procedure send(Data: TStream); overload;

  public
    constructor Create;
    destructor  Destroy; override;
  end;

implementation

{ TXMLHttprequest }

constructor TXMLHttpRequest.Create;
begin
  _responseType := 'text';

  _HttpClient := TNetHttpClient.Create(nil);
  _HttpRequest := TNetHttpRequest.Create(nil);
  _HttpRequest.Client := _HttpClient;
  _HttpRequest.OnReceiveData := OnReceiveData;
  _HttpRequest.OnRequestCompleted := OnRequestCompleted;
end;

destructor TXMLHttpRequest.Destroy;
begin
  _sendContent.Free;
  _HttpRequest.Free;
  _HttpClient.Free;
  inherited;
end;

function TXMLHttpRequest.get_onreadystatechange: TReadyStateChangeEvent;
begin
  Result := _onreadystatechange;
end;

procedure TXMLHttpRequest.InternalSetReadyState(State: Word);
begin
  if _readyState <> State then
  begin
    _readyState := State;
    if Assigned(_onreadystatechange) then
      _onreadystatechange;
  end;
end;

procedure TXMLHttpRequest.OnReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  InternalSetReadyState(READYSTATE_LOADING);
end;

procedure TXMLHttpRequest.OnRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
begin
  _response := AResponse;
//  // No need to hang on to source stream any longer
//  _HttpRequest.SourceStream := nil;
//  FreeAndNil(_sendContent);
  InternalSetReadyState(READYSTATE_DONE);

  {$IFDEF DEBUG}
  var s := get_responseText;
  {$ENDIF}
end;

procedure TXMLHttpRequest.open(Method, Url: string; Async: Boolean);
begin
  _HttpRequest.MethodString := Method;
  _HttpRequest.URL := URL;
  _HttpRequest.Asynchronous := Async;
  InternalSetReadyState(READYSTATE_OPENED);
end;

procedure TXMLHttpRequest.overrideMimeType(Value: string);
begin
  _HttpRequest.Accept := Value;
end;

function TXMLHttpRequest.get_readyState: Word;
begin
  Result := _readyState;
end;

function TXMLHttpRequest.get_response: TValue;
begin
  if _response <> nil then
  begin
    if _responseType = 'text' then
      Exit(_response.ContentAsString(TEncoding.Unicode));
    if _responseType = 'arraybuffer' then
    begin
      var arr: TArray<Byte>;
      SetLength(arr, _response.ContentStream.Size);
      _response.ContentStream.Position := 0;
      _response.ContentStream.Read(arr, Length(arr));
      Result := TValue.From<TArray<Byte>>(arr);
    end;
  end;
end;

function TXMLHttpRequest.get_responseText: string;
begin
  if _response <> nil then
    Result := _response.ContentAsString(TEncoding.Default);
end;

function TXMLHttpRequest.get_responseType: string;
begin
  Result := _responseType;
end;

function TXMLHttpRequest.get_responseXML: string;
begin
  if _response <> nil then
    Result := _response.ContentAsString(TEncoding.Default);
end;

procedure TXMLHttpRequest.send();
begin
  InternalSetReadyState(READYSTATE_UNSENT);
  _sendContent.Free;
  _HttpRequest.SourceStream := nil;
  _response := _HttpRequest.Execute();
end;

procedure TXMLHttpRequest.send(Value: string);
begin
  InternalSetReadyState(READYSTATE_UNSENT);
  _sendContent.Free;
  _sendContent := TStringStream.Create(Value);
  _HttpRequest.SourceStream := _sendContent;
  _response := _HttpRequest.Execute();
end;

procedure TXMLHttpRequest.send(Data: TStream);
begin
  InternalSetReadyState(READYSTATE_UNSENT);
  _sendContent.Free;
  _sendContent := Data;
  _HttpRequest.SourceStream := _sendContent;
  _response := _HttpRequest.Execute();
end;

procedure TXMLHttpRequest.setRequestHeader(Header, Value: string);
begin
  _HttpRequest.CustomHeaders[Header] := value;
end;

procedure TXMLHttpRequest.set_onreadystatechange(const Value: TReadyStateChangeEvent);
begin
  _onreadystatechange := Value;
end;

procedure TXMLHttpRequest.set_responseType(const Value: string);
begin
  _responseType := Value;
end;

function TXMLHttpRequest.get_status: Word;
begin
  if _response <> nil then
    Result := _response.StatusCode else
    Result := 0;
end;

end.

