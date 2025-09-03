unit App.Base.intf;

interface

uses
  System_;

type
  ITypeConverter = interface
    ['{EDFE3735-DD75-44AC-8B76-CF2D1FB27C30}']
    function TryAsType(const AType: &Type; out Value: CObject) : Boolean;
  end;

  IConverterSupport = interface
    ['{998C1024-9C97-446F-95BB-324B277D3EFD}']

    function  get_Converter: ITypeConverter;
    procedure set_Converter(const Value: ITypeConverter);

    property Converter: ITypeConverter read get_Converter write set_Converter;
  end;

implementation

end.

