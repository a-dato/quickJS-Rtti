unit App.Content.impl;

interface

uses
  System_,
  App.Content.intf;

type
  JSContentProvider = class(TBaseInterfacedObject, IContentProvider)
  protected
    function get_Data: CObject;
  end;

implementation

{ JSContentProvider }

function JSContentProvider.get_Data: CObject;
begin
;
end;

end.
