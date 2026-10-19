unit SyncTest.Trace;

// Call trace shown in the Delphi panel. Every call that crosses a boundary
// (webview / Delphi / QuickJS), every binding notification we own and every
// rule writes a line. Enter/Leave indent, so nested lines were caused by the
// line above them. A new block starts when activity resumes after a pause.

interface

uses
  System.SysUtils, System.DateUtils, System.JSON, System.Diagnostics;

type
  TTraceSink = reference to procedure(const Line: string);

  Trace = record
  private
    class var FDepth: Integer;
    class var FLast: TDateTime;
    class procedure Write(const Text: string); static;
  public
    class var Sink: TTraceSink;
    // Time spent in Sink and lines written (to measure what tracing costs)
    class var SinkTicks: Int64;
    class var SinkLines: Integer;

    // One line at the current depth: [Tag] Text
    class procedure Line(const Tag, Text: string); static;
    // Line, then everything until Leave is indented below it
    class procedure Enter(const Tag, Text: string); static;
    // Ends the indentation; optionally shows what the call returned
    class procedure Leave(const ReturnValue: string = ''); static;

    // Short form of a JSON payload (snapshot answers, long values)
    class function Summary(const Json: string): string; static;
    class function Quote(const S: string): string; static;
  end;

implementation

class procedure Trace.Write(const Text: string);
begin
  if not Assigned(Sink) then Exit;
  // New burst of activity: start a new block
  if (FDepth = 0) and (MilliSecondsBetween(Now, FLast) > 300) then
  begin
    Sink('');
    Sink('---- ' + FormatDateTime('hh:nn:ss', Now) + ' ----');
  end;
  FLast := Now;
  var sw := TStopwatch.StartNew;
  Sink(StringOfChar(' ', FDepth * 2) + Text);
  Inc(SinkTicks, sw.ElapsedTicks);
  Inc(SinkLines);
end;

class procedure Trace.Line(const Tag, Text: string);
begin
  Write('[' + Tag + '] ' + Text);
end;

class procedure Trace.Enter(const Tag, Text: string);
begin
  Line(Tag, Text);
  Inc(FDepth);
end;

class procedure Trace.Leave(const ReturnValue: string);
begin
  if ReturnValue <> '' then
    Write('<- returns ' + ReturnValue);
  if FDepth > 0 then
    Dec(FDepth);
  FLast := Now;
end;

class function Trace.Quote(const S: string): string;
begin
  Result := '"' + S + '"';
end;

class function Trace.Summary(const Json: string): string;
begin
  if Json.Length > 160 then
    Result := Json.Substring(0, 160) + '...'
  else
    Result := Json;
end;

end.
