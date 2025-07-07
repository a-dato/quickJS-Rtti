unit App.Windows.FMX.impl;

interface

uses
  System_,
  App.Windows.intf,
  FMX.Forms;

type
  WindowFrame = class(TBaseInterfacedObject, IWindowFrame)
  protected
    _content: CObject;
    _handle: TForm;

    function  get_Content: CObject;
    procedure set_Content(const Value: CObject);
    function  get_Owner: CObject;

    procedure Show;

  public
    constructor Create(AHandle: Pointer);
  end;

implementation


{ WindowFrame }

constructor WindowFrame.Create(AHandle: Pointer);
begin
  _handle := AHandle;
end;

function WindowFrame.get_Content: CObject;
begin
  Result := _content;

end;

function WindowFrame.get_Owner: CObject;
begin
  Result := _handle.Owner;
end;

procedure WindowFrame.set_Content(const Value: CObject);
begin
  _content := Value;

  var fr: TFrame;
  if _content.TryAsType<TFrame>(fr) then
    _handle.AddObject(fr);
end;

procedure WindowFrame.Show;
begin
  _handle.Show;
end;

end.

