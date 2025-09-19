unit TestObjects.intf;

interface

uses
  System_,
  System.Collections.Generic, System.Collections;

type
  {$M+}
  ITestObject = interface(IBaseInterface)
    ['{455B5ABD-E751-4FD4-999F-3B70301B4AA5}']
    function GetTestArray: IList<CString>;
    function EchoDateTime(const ADateTime: CDateTime): CDateTime;

    property TestArray: IList<CString> read GetTestArray;
  end;

implementation

end.
