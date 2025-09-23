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

end.
