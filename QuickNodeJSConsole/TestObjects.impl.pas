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
  public
    constructor Create;
  end;

  TTestObject2 = class(TBaseInterfacedObject, ITestObject2)
  protected
    // ITestObject (inherited)
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
    // ITestObject2
    function ProcessTimeIntervals(const Intervals: IList<TimeInterval>): IList<TimeInterval>;
    function GetRandomNumbers(const Count: Integer): IList<Double>;
    function MergeArrays(const Array1, Array2: IList<CString>): IList<CString>;
    function GetObjectInfo(const Obj: CObject): CString;
    function get_TestObject3: ITestObject3;
    function GetTestObject3List(const Count: Integer): IList<ITestObject3>;
    function get_MixedObjectList: IList<IBaseInterface>;
  public
    constructor Create;
  end;

  TTestObject3 = class(TBaseInterfacedObject, ITestObject3, ITestObject2, ITestObject)
  protected
    // ITestObject (inherited)
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
    // ITestObject2 (inherited)
    function ProcessTimeIntervals(const Intervals: IList<TimeInterval>): IList<TimeInterval>;
    function GetRandomNumbers(const Count: Integer): IList<Double>;
    function MergeArrays(const Array1, Array2: IList<CString>): IList<CString>;
    function GetObjectInfo(const Obj: CObject): CString;
    function get_TestObject3: ITestObject3;
    function GetTestObject3List(const Count: Integer): IList<ITestObject3>;
    function get_MixedObjectList: IList<IBaseInterface>;
    // ITestObject3
    function get_TestObjects: IList<ITestObject>;
    function get_MixedTestObjects: IList<IBaseInterface>;
    function ProcessTestObjectArray(const Objects: IList<ITestObject>): IList<CString>;
    function FindTestObjectByProperty(const Objects: IList<ITestObject>; const SearchValue: CString): ITestObject;
    function CreateTestObjectChain(const Count: Integer): IList<ITestObject>;
    function GetTestObjectsWithData(const Count: Integer): IList<ITestObject>;
  public
    constructor Create;
  end;

implementation

{ TTestObject }

constructor TTestObject.Create;
begin
  inherited Create;
end;

function TTestObject.get_TestArray: IList<CString>;
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

function TTestObject.CreateStringList(const Items: IList<CString>): IList<CString>;
begin
  Result := CList<CString>.Create;
  for var i := 0 to Items.Count - 1 do
  begin
    Result.Add(Items[i]);
  end;
end;

function TTestObject.CreateEmptyStringList: IList<CString>;
begin
  Result := CList<CString>.Create;
end;

function TTestObject.CreateTimeIntervalList(const Items: IList<TimeInterval>): IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
  for var i := 0 to Items.Count - 1 do
  begin
    Result.Add(Items[i]);
  end;
end;

function TTestObject.CreateEmptyTimeIntervalList: IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
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

function TTestObject.get_TestObject2: ITestObject2;
begin
  Result := TTestObject2.Create;
end;

function TTestObject.GetTestObject2List(const Count: Integer): IList<ITestObject2>;
begin
  Result := CList<ITestObject2>.Create;
  for var i := 0 to Count - 1 do
  begin
    Result.Add(TTestObject2.Create);
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

function TTestObject2.get_TestObject3: ITestObject3;
begin
  Result := TTestObject3.Create;
end;

function TTestObject2.GetTestObject3List(const Count: Integer): IList<ITestObject3>;
begin
  Result := CList<ITestObject3>.Create;
  for var i := 0 to Count - 1 do
  begin
    Result.Add(TTestObject3.Create);
  end;
end;

function TTestObject2.get_MixedObjectList: IList<IBaseInterface>;
begin
  Result := CList<IBaseInterface>.Create;
  
  // Add different types of objects to test polymorphism
  Result.Add(TTestObject.Create);
  Result.Add(TTestObject2.Create);
  Result.Add(TTestObject3.Create);
  Result.Add(TTestObject.Create);
  Result.Add(TTestObject2.Create);
end;

// ITestObject inherited methods for TTestObject2
function TTestObject2.get_TestArray: IList<CString>;
begin
  Result := CList<CString>.Create;
  Result.Add('TestObject2 Item 1');
  Result.Add('TestObject2 Item 2');
  Result.Add('TestObject2 Item 3');
end;

function TTestObject2.EchoDateTime(const ADateTime: CDateTime): CDateTime;
begin
  Result := ADateTime;
end;

function TTestObject2.CreateTimeInterval(const Start, Stop: CDateTime): TimeInterval;
begin
  Result := TimeInterval.Create(Start, Stop);
end;

function TTestObject2.ProcessNumbers(const Numbers: IList<Integer>): IList<Integer>;
begin
  Result := CList<Integer>.Create;
  for var i := 0 to Numbers.Count - 1 do
  begin
    var num := Numbers[i];
    Result.Add(num * 3); // Triple each number (different from TTestObject)
  end;
end;

function TTestObject2.CreateNumberList(const Count: Integer): IList<Integer>;
begin
  Result := CList<Integer>.Create;
  for var i := 1 to Count do
  begin
    Result.Add(i * 10); // Different from TTestObject
  end;
end;

function TTestObject2.CreateStringList(const Items: IList<CString>): IList<CString>;
begin
  Result := CList<CString>.Create;
  for var i := 0 to Items.Count - 1 do
  begin
    Result.Add('TestObject2: ' + Items[i]);
  end;
end;

function TTestObject2.CreateEmptyStringList: IList<CString>;
begin
  Result := CList<CString>.Create;
end;

function TTestObject2.CreateTimeIntervalList(const Items: IList<TimeInterval>): IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
  for var i := 0 to Items.Count - 1 do
  begin
    Result.Add(Items[i]);
  end;
end;

function TTestObject2.CreateEmptyTimeIntervalList: IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
end;

function TTestObject2.ValidateEmail(const Email: CString): Boolean;
begin
  Result := Email.Contains('@') and Email.Contains('.') and (Email.Length > 5);
end;

function TTestObject2.FormatMessage(const Template: CString; const Args: IList<CString>): CString;
begin
  Result := 'TestObject2: ' + Template;
  for var i := 0 to Args.Count - 1 do
  begin
    var placeholder := CString.Format('{{{0}}}', i);
    Result := Result.Replace(placeholder, Args[i]);
  end;
end;

function TTestObject2.get_TestObject2: ITestObject2;
begin
  Result := TTestObject2.Create;
end;

function TTestObject2.GetTestObject2List(const Count: Integer): IList<ITestObject2>;
begin
  Result := CList<ITestObject2>.Create;
  for var i := 0 to Count - 1 do
  begin
    Result.Add(TTestObject2.Create);
  end;
end;

{ TTestObject3 }

constructor TTestObject3.Create;
begin
  inherited Create;
end;

function TTestObject3.get_TestObjects: IList<ITestObject>;
begin
  Result := CList<ITestObject>.Create;
  
  for var i := 1 to 3 do
  begin
    var testObj := TTestObject.Create;
    Result.Add(testObj);
  end;
end;

function TTestObject3.get_MixedTestObjects: IList<IBaseInterface>;
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
    var testArray := testObj.TestArray;
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
    var testArray := testObj.TestArray;
    
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

function TTestObject3.GetTestObjectsWithData(const Count: Integer): IList<ITestObject>;
begin
  Result := CList<ITestObject>.Create;
  
  for var i := 0 to Count - 1 do
  begin
    var testObj := TTestObject.Create;
    Result.Add(testObj);
  end;
end;

// ITestObject inherited methods for TTestObject3
function TTestObject3.get_TestArray: IList<CString>;
begin
  Result := CList<CString>.Create;
  Result.Add('TestObject3 Item 1');
  Result.Add('TestObject3 Item 2');
  Result.Add('TestObject3 Item 3');
  Result.Add('TestObject3 Item 4');
end;

function TTestObject3.EchoDateTime(const ADateTime: CDateTime): CDateTime;
begin
  Result := ADateTime;
end;

function TTestObject3.CreateTimeInterval(const Start, Stop: CDateTime): TimeInterval;
begin
  Result := TimeInterval.Create(Start, Stop);
end;

function TTestObject3.ProcessNumbers(const Numbers: IList<Integer>): IList<Integer>;
begin
  Result := CList<Integer>.Create;
  for var i := 0 to Numbers.Count - 1 do
  begin
    var num := Numbers[i];
    Result.Add(num * 5); // Multiply by 5 (different from TTestObject and TTestObject2)
  end;
end;

function TTestObject3.CreateNumberList(const Count: Integer): IList<Integer>;
begin
  Result := CList<Integer>.Create;
  for var i := 1 to Count do
  begin
    Result.Add(i * 100); // Different from TTestObject and TTestObject2
  end;
end;

function TTestObject3.CreateStringList(const Items: IList<CString>): IList<CString>;
begin
  Result := CList<CString>.Create;
  for var i := 0 to Items.Count - 1 do
  begin
    Result.Add('TestObject3: ' + Items[i]);
  end;
end;

function TTestObject3.CreateEmptyStringList: IList<CString>;
begin
  Result := CList<CString>.Create;
end;

function TTestObject3.CreateTimeIntervalList(const Items: IList<TimeInterval>): IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
  for var i := 0 to Items.Count - 1 do
  begin
    Result.Add(Items[i]);
  end;
end;

function TTestObject3.CreateEmptyTimeIntervalList: IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
end;

function TTestObject3.ValidateEmail(const Email: CString): Boolean;
begin
  Result := Email.Contains('@') and Email.Contains('.') and (Email.Length > 5);
end;

function TTestObject3.FormatMessage(const Template: CString; const Args: IList<CString>): CString;
begin
  Result := 'TestObject3: ' + Template;
  for var i := 0 to Args.Count - 1 do
  begin
    var placeholder := CString.Format('{{{0}}}', i);
    Result := Result.Replace(placeholder, Args[i]);
  end;
end;

function TTestObject3.get_TestObject2: ITestObject2;
begin
  Result := TTestObject2.Create;
end;

function TTestObject3.GetTestObject2List(const Count: Integer): IList<ITestObject2>;
begin
  Result := CList<ITestObject2>.Create;
  for var i := 0 to Count - 1 do
  begin
    Result.Add(TTestObject2.Create);
  end;
end;

// ITestObject2 inherited methods for TTestObject3
function TTestObject3.ProcessTimeIntervals(const Intervals: IList<TimeInterval>): IList<TimeInterval>;
begin
  Result := CList<TimeInterval>.Create;
  for var i := 0 to Intervals.Count - 1 do
  begin
    var interval := Intervals[i];
    Result.Add(interval);
  end;
end;

function TTestObject3.GetRandomNumbers(const Count: Integer): IList<Double>;
begin
  Result := CList<Double>.Create;
  for var i := 0 to Count - 1 do
  begin
    var randomValue := 7 * 100.0; // Different from TTestObject2
    Result.Add(randomValue);
  end;
end;

function TTestObject3.MergeArrays(const Array1, Array2: IList<CString>): IList<CString>;
begin
  Result := CList<CString>.Create;
  
  for var i := 0 to Array1.Count - 1 do
    Result.Add('Merged: ' + Array1[i]); // Different prefix
    
  for var i := 0 to Array2.Count - 1 do
    Result.Add('Merged: ' + Array2[i]);
end;

function TTestObject3.GetObjectInfo(const Obj: CObject): CString;
begin
  if Obj = nil then
    Result := 'TestObject3: Object is nil'
  else
    Result := CString.Format('TestObject3 - Object type: {0}, Value: {1}', Obj.GetType.Name, Obj.ToString);
end;

function TTestObject3.get_TestObject3: ITestObject3;
begin
  Result := TTestObject3.Create;
end;

function TTestObject3.GetTestObject3List(const Count: Integer): IList<ITestObject3>;
begin
  Result := CList<ITestObject3>.Create;
  for var i := 0 to Count - 1 do
  begin
    Result.Add(TTestObject3.Create);
  end;
end;

function TTestObject3.get_MixedObjectList: IList<IBaseInterface>;
begin
  Result := CList<IBaseInterface>.Create;
  
  // Add different types of objects to test polymorphism
  Result.Add(TTestObject.Create);
  Result.Add(TTestObject2.Create);
  Result.Add(TTestObject3.Create);
  Result.Add(TTestObject.Create);
  Result.Add(TTestObject2.Create);
  Result.Add(TTestObject3.Create);
end;

end.

