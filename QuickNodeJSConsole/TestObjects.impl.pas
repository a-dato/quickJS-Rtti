unit TestObjects.impl;

interface

uses
  System_,
  System.Collections,
  TestObjects.intf,
  System.Collections.Generic;

type
  TTestObject = class(TBaseInterfacedObject, ITestObject)
  protected
    // ITestObject
    function GetTestArray: IList<CString>;
    function EchoDateTime(const ADateTime: CDateTime): CDateTime;
    function CreateTimeInterval(const Start, Stop: CDateTime): TimeInterval;
    function ProcessNumbers(const Numbers: IList<Integer>): IList<Integer>;
    function CreateNumberList(const Count: Integer): IList<Integer>;
    function CreateStringList(const Items: array of CString): IList<CString>;
    function CreateTimeIntervalList(const Items: array of TimeInterval): IList<TimeInterval>;
    function ValidateEmail(const Email: CString): Boolean;
    function FormatMessage(const Template: CString; const Args: IList<CString>): CString;
  public
    constructor Create;
  end;

  TTestObject2 = class(TBaseInterfacedObject, ITestObject2)
  protected
    // ITestObject2
    function ProcessTimeIntervals(const Intervals: IList<TimeInterval>): IList<TimeInterval>;
    function GetRandomNumbers(const Count: Integer): IList<Double>;
    function MergeArrays(const Array1, Array2: IList<CString>): IList<CString>;
    function GetObjectInfo(const Obj: CObject): CString;
  public
    constructor Create;
  end;

  TTestObject3 = class(TBaseInterfacedObject, ITestObject3)
  protected
    // ITestObject3
    function GetTestObjects: IList<ITestObject>;
    function GetMixedTestObjects: IList<IBaseInterface>;
    function ProcessTestObjectArray(const Objects: IList<ITestObject>): IList<CString>;
    function FindTestObjectByProperty(const Objects: IList<ITestObject>; const SearchValue: CString): ITestObject;
    function CreateTestObjectChain(const Count: Integer): IList<ITestObject>;
  public
    constructor Create;
  end;

implementation

{ TTestObject }

constructor TTestObject.Create;
begin
  inherited Create;
end;

function TTestObject.GetTestArray: IList<CString>;
begin
  // Create a simple list with some test data
  Result := CList<CString>.Create;
  Result.Add('Item 1');
  Result.Add('Item 2');
  Result.Add('Item 3');
  Result.Add('Item 4');
  Result.Add('Item 5');
end;

function TTestObject.EchoDateTime(const ADateTime: CDateTime): CDateTime;
begin
  Result := ADateTime;
end;

function TTestObject.CreateTimeInterval(const Start, Stop: CDateTime): TimeInterval;
begin
  Result := TimeInterval.Create(Start, Stop);
end;

function TTestObject.ProcessNumbers(const Numbers: IList<Integer>): IList<Integer>;
begin
  Result := CList<Integer>.Create;
  for var i := 0 to Numbers.Count - 1 do
  begin
    var num := Numbers[i];
    Result.Add(num * 2); // Double each number
  end;
end;

function TTestObject.CreateNumberList(const Count: Integer): IList<Integer>;
begin
  Result := CList<Integer>.Create;
  for var i := 1 to Count do
  begin
    Result.Add(i);
  end;
end;

function TTestObject.CreateStringList(const Items: array of CString): IList<CString>;
begin
  Result := CList<CString>.Create;
  for var item in Items do
  begin
    Result.Add(item);
  end;
end;

function TTestObject.CreateTimeIntervalList(const Items: array of TimeInterval): IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
  for var item in Items do
  begin
    Result.Add(item);
  end;
end;

function TTestObject.ValidateEmail(const Email: CString): Boolean;
begin
  Result := Email.Contains('@') and Email.Contains('.') and (Email.Length > 5);
end;

function TTestObject.FormatMessage(const Template: CString; const Args: IList<CString>): CString;
begin
  Result := Template;
  for var i := 0 to Args.Count - 1 do
  begin
    var placeholder := CString.Format('{{{0}}}', i);
    Result := Result.Replace(placeholder, Args[i]);
  end;
end;

{ TTestObject2 }

constructor TTestObject2.Create;
begin
  inherited Create;
end;

function TTestObject2.ProcessTimeIntervals(const Intervals: IList<TimeInterval>): IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
  for var i := 0 to Intervals.Count - 1 do
  begin
    var interval := Intervals[i];
//    var extended := interval.Extend(CTimeSpan.Create(1, 0, 0));
    Result.Add(interval);
  end;
end;

function TTestObject2.GetRandomNumbers(const Count: Integer): IList<Double>;
begin
  Result := CList<Double>.Create;
  for var i := 0 to Count - 1 do
  begin
    var randomValue := 5 * 100.0;
    Result.Add(randomValue);
  end;
end;

function TTestObject2.MergeArrays(const Array1, Array2: IList<CString>): IList<CString>;
begin
  Result := CList<CString>.Create;
  
  for var i := 0 to Array1.Count - 1 do
    Result.Add(Array1[i]);
    
  for var i := 0 to Array2.Count - 1 do
    Result.Add(Array2[i]);
end;

function TTestObject2.GetObjectInfo(const Obj: CObject): CString;
begin
  if Obj = nil then
    Result := 'Object is nil'
  else
    Result := CString.Format('Object type: {0}, Value: {1}', Obj.GetType.Name, Obj.ToString);
end;

{ TTestObject3 }

constructor TTestObject3.Create;
begin
  inherited Create;
end;

function TTestObject3.GetTestObjects: IList<ITestObject>;
begin
  Result := CList<ITestObject>.Create;
  
  for var i := 1 to 3 do
  begin
    var testObj := TTestObject.Create;
    Result.Add(testObj);
  end;
end;

function TTestObject3.GetMixedTestObjects: IList<IBaseInterface>;
begin
  Result := CList<IBaseInterface>.Create;
  
  Result.Add(TTestObject.Create);
  Result.Add(TTestObject2.Create);
  Result.Add(TTestObject3.Create);
  Result.Add(TTestObject.Create);
end;

function TTestObject3.ProcessTestObjectArray(const Objects: IList<ITestObject>): IList<CString>;
begin
  Result := CList<CString>.Create;
  
  for var i := 0 to Objects.Count - 1 do
  begin
    var testObj := Objects[i];
    var testArray := testObj.GetTestArray;
    var summary := CString.Format('Object {0} has {1} items', i + 1, testArray.Count);
    Result.Add(summary);
  end;
end;

function TTestObject3.FindTestObjectByProperty(const Objects: IList<ITestObject>; const SearchValue: CString): ITestObject;
begin
  Result := nil;
  
  for var i := 0 to Objects.Count - 1 do
  begin
    var testObj := Objects[i];
    var testArray := testObj.GetTestArray;
    
    for var j := 0 to testArray.Count - 1 do
    begin
      if testArray[j].Equals(SearchValue) then
      begin
        Result := testObj;
        Exit;
      end;
    end;
  end;
end;

function TTestObject3.CreateTestObjectChain(const Count: Integer): IList<ITestObject>;
begin
  Result := CList<ITestObject>.Create;
  
  for var i := 0 to Count - 1 do
  begin
    var testObj := TTestObject.Create;
    Result.Add(testObj);
  end;
end;

end.
