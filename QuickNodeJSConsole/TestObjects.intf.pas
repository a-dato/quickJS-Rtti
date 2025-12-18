unit TestObjects.intf;

interface

uses
  System_,
  System.Collections.Generic, System.Collections;

type
  TimeInterval = {$IFDEF DOTNET}public{$ENDIF} record
  private
    var _Start          : CDateTime;
    var _Stop           : CDateTime;

  public
    constructor Create(const AStart, AStop: CDateTime); overload;
    constructor Create(TicksStart, TicksStop: Int64); overload;
    class function Empty: TimeInterval; static;
    function  Contains(const Other: TimeInterval) : Boolean;
    function  DateWithin(const ADate: CDateTime) : Boolean;
    function  IntersectsWith(const Other: TimeInterval) : Boolean;
    function  Ceil: TimeInterval;
    function  Duration: CTimeSpan;
    function  Equals(const Other: TimeInterval) : Boolean;  overload;
    class function Equals(const l, r: CObject) : Boolean;  overload; static;
    function  Extend(const Period: TimeInterval) : TimeInterval; overload;
    function  Extend(const L, R: CDateTime) : TimeInterval; overload;
    function  Extend(const ADate: CDateTime) : TimeInterval; overload;
    function  IsEmpty: Boolean;
    function  IsNull: Boolean;
    class function MaxValue: TimeInterval; static;
    function  Move(const MoveBy: CTimeSpan) : TimeInterval;
    function  Normalize : TimeInterval;
    function  Subtract(const Period: TimeInterval) : TimeInterval;
    function  Trim(const Period: TimeInterval) : TimeInterval; overload;
    function  Trim(const Start: CDateTime; const Stop: CDateTime) : TimeInterval; overload;
    function  TrimLeft(const Start: CDateTime) : TimeInterval;
    function  TrimRight(const Stop: CDateTime) : TimeInterval;
    function  ToString: CString;

    property Start: CDateTime read _Start;
    property Stop: CDateTime read _Stop;
  end;

  // Forward declarations
  ITestObject2 = interface;
  ITestObject3 = interface;

  {$M+}
  ITestObject = interface(IBaseInterface)
    ['{455B5ABD-E751-4FD4-999F-3B70301B4AA5}']
    function get_TestArray: IList<CString>;
    function EchoDateTime(const ADateTime: CDateTime): CDateTime;
    function CreateTimeInterval(const Start, Stop: CDateTime): TimeInterval;
    function ProcessNumbers(const Numbers: IList<Integer>): IList<Integer>;
    function CreateNumberList(const Count: Integer): IList<Integer>;
    function CreateStringList(const Items: IList<CString>): IList<CString>;
    function CreateEmptyStringList: IList<CString>;
    function CreateTimeIntervalList(const Items: IList<TimeInterval>): IList<TimeInterval>;
    function CreateEmptyTimeIntervalList: IList<TimeInterval>;
    function ValidateEmail(const Email: CString): Boolean;
    function FormatMessage(const Template: CString; const Args: IList<CString>): CString;
    function get_TestObject2: ITestObject2;
    function GetTestObject2List(const Count: Integer): IList<ITestObject2>;

    property TestArray: IList<CString> read get_TestArray;
    property TestObject2: ITestObject2 read get_TestObject2;
  end;

  ITestObject2 = interface(ITestObject)
    ['{B8C2F1A4-3E7D-4A5B-9F2C-1D8E6A7B3C9F}']
    function ProcessTimeIntervals(const Intervals: IList<TimeInterval>): IList<TimeInterval>;
    function GetRandomNumbers(const Count: Integer): IList<Double>;
    function MergeArrays(const Array1, Array2: IList<CString>): IList<CString>;
    function GetObjectInfo(const Obj: CObject): CString;
    function get_TestObject3: ITestObject3;
    function GetTestObject3List(const Count: Integer): IList<ITestObject3>;
    function get_MixedObjectList: IList<IBaseInterface>;

    property TestObject3: ITestObject3 read get_TestObject3;
    property MixedObjectList: IList<IBaseInterface> read get_MixedObjectList;
  end;

  ITestObject3 = interface(ITestObject2)
    ['{F4E8D2C6-9A1B-4F3E-8D7C-2B5A9E4F1C8D}']
    function get_TestObjects: IList<ITestObject>;
    function get_MixedTestObjects: IList<IBaseInterface>;
    function ProcessTestObjectArray(const Objects: IList<ITestObject>): IList<CString>;
    function FindTestObjectByProperty(const Objects: IList<ITestObject>; const SearchValue: CString): ITestObject;
    function CreateTestObjectChain(const Count: Integer): IList<ITestObject>;
    function GetTestObjectsWithData(const Count: Integer): IList<ITestObject>;
    // Memory test methods
    function VerifyObjectsAlive(const Objects: IList<ITestObject>): Integer;

    property TestObjects: IList<ITestObject> read get_TestObjects;
    property MixedTestObjects: IList<IBaseInterface> read get_MixedTestObjects;
  end;

implementation

uses
  ADato.DatetimeHelper;

constructor TimeInterval.Create(const AStart, AStop: CDateTime);
begin
  _Start := AStart;
  _Stop := AStop;
end;

constructor TimeInterval.Create(TicksStart, TicksStop: Int64);
begin
  Create(CDateTime.Create(TicksStart), CDateTime.Create(TicksStop));
end;

class function TimeInterval.Empty: TimeInterval;
begin
  Result := TimeInterval.Create(CDateTime.MinValue, CDateTime.MinValue);
end;

function TimeInterval.DateWithin(const ADate: CDateTime) : Boolean;
begin
  Result := (ADate >= _Start) and (ADate < _Stop);
end;

function TimeInterval.Contains(const Other: TimeInterval) : Boolean;
begin
  Result := (_Start <= Other.Start) and (_Stop >= Other.Stop);
end;

function TimeInterval.IntersectsWith(const Other: TimeInterval) : Boolean;
begin
  Result := (_Start < Other.Stop) and (_Stop >= Other.Start);
end;

function TimeInterval.Ceil: TimeInterval;
begin
  Result := TimeInterval.Create(_Start.Date, _Stop.Ceil);
end;

function TimeInterval.Duration: CTimeSpan;
begin
  Result := _Stop.Subtract(_Start);
end;

function TimeInterval.Equals(const Other: TimeInterval): Boolean;
begin
  Result := _Start.Equals(Other.Start) and _Stop.Equals(Other.Stop);
end;

class function TimeInterval.Equals(const l, r: CObject) : Boolean;
var
  t1, t2: TimeInterval;
begin
  if l = nil then
    Exit(r = nil);
  if r = nil then
    Exit(False);
  if l.TryGetValue<TimeInterval>(t1) and r.TryGetValue<TimeInterval>(t2) then
    Exit(t1.Equals(t2));
  Exit(False);
end;

function TimeInterval.IsEmpty: Boolean;
begin
  Result := _Start.Ticks >= _Stop.Ticks;
end;

function TimeInterval.IsNull: Boolean;
begin
  Result := (_Start.Ticks = 0) and (_Stop.Ticks = 0);
end;

class function TimeInterval.MaxValue: TimeInterval;
begin
  Result := TimeInterval.Create(CDateTime.MinValue, CDateTime.MaxValue);
end;

function TimeInterval.Trim(const Period: TimeInterval) : TimeInterval;
begin
  Result := Trim(Period.Start, Period.Stop);
end;

function TimeInterval.Trim(const Start: CDateTime; const Stop: CDateTime) : TimeInterval;
var
  m1, m2: CDateTime;
begin
  // Get max start date
  case CDateTime.Compare(_Start, Start) of
    -1:
      m1 := Start;
     0:
      m1 := _Start;
     1:
      m1 := _Start;
  end; // case

  // get min stop date
  case CDateTime.Compare(_Stop, Stop) of
    -1:
      m2 := _Stop;
     0:
      m2 := _Stop;
     1:
      m2 := Stop;
  end; // case

  if m2 <= m1 then
    Result := Empty else
    Result := TimeInterval.Create(m1, m2);
end;

function TimeInterval.TrimLeft(const Start: CDateTime) : TimeInterval;
var
  m1: CDateTime;

begin
  m1 := CMath.Max(_Start, Start);
  Result := TimeInterval.Create(m1, CMath.Max(m1, _Stop));
end;

function TimeInterval.TrimRight(const Stop: CDateTime) : TimeInterval;
var
  m1: CDateTime;
begin
  m1 := CMath.Min(_Stop, Stop);
  Result := TimeInterval.Create(CMath.Min(m1, _Start), m1);
end;

function TimeInterval.Move(const MoveBy: CTimeSpan) : TimeInterval;
begin
  Result := TimeInterval.Create(_start.Add(MoveBy), _stop.Add(MoveBy));
end;

function TimeInterval.Normalize : TimeInterval;
begin
  if _Start > _Stop then
    Result := TimeInterval.Create(_Stop, _Start) else
    Result := Self;
end;

function TimeInterval.Subtract(const Period: TimeInterval) : TimeInterval;
begin
  if Period.IsEmpty or (Period.Start >= Self.Stop) or (Period.Stop <= Self.Start) then
    Exit(Self);

  if Period.Start <= Self.Start then
  begin
    if Period.Stop < Self.Stop then
      Result := TimeInterval.Create(Period.Stop, Self.Stop) else
      Result := TimeInterval.Empty;
  end else
    Result := TimeInterval.Create(Self.Start, Period.Start);
end;

function TimeInterval.Extend(const Period: TimeInterval) : TimeInterval;
begin
  Exit(Extend(Period.Start, Period.Stop));
end;

function TimeInterval.Extend(const ADate: CDateTime) : TimeInterval;
begin
  Exit(Extend(ADate, ADate));
end;

function TimeInterval.Extend(const L, R: CDateTime) : TimeInterval;
var
  m1, m2: CDateTime;

begin
  if _Start.Equals(CDateTime.MinValue) then
  begin
    // get max stop date
    case CDateTime.Compare(_Stop, R) of
      -1:
        m1 := R;
       0:
        m1 := _Stop;
       1:
        m1 := _Stop;
    end; // case

    Result := TimeInterval.Create(L, m1);
  end
  else
  begin
    // Get min start date
    case CDateTime.Compare(_Start, L) of
      -1:
        m1 := _Start;
       0:
        m1 := _Start;
       1:
        m1 := L;
    end; // case

    // get max stop date
    case CDateTime.Compare(_Stop, R) of
      -1:
        m2 := R;
       0:
        m2 := _Stop;
       1:
        m2 := _Stop;
    end; // case

    Result := TimeInterval.Create(m1, m2);
  end;
end;

function TimeInterval.ToString : CString;
begin
  Result := CString.Format('{0} -> {1}', Start.ToString, Stop.ToString);
end;

end.
