unit App.Windows.FMX.impl;

interface

uses
  System_,
  App.Windows.intf,
  App.Component.impl,
  FMX.Forms, System.UITypes;

type
  TWindowFrame = class(TComponent, IWindowFrame)
  protected
    _content: CObject;
    _handle: TForm;
    _onClose: TWindowClose;

    function  get_Content: CObject;
    procedure set_Content(const Value: CObject);

    procedure Show(OnClose: TWindowClose);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  public
    constructor Create(AHandle: Pointer);
  end;

implementation


{ WindowFrame }

constructor TWindowFrame.Create(AHandle: Pointer);
begin
  _handle := AHandle;
end;

function TWindowFrame.get_Content: CObject;
begin
  Result := _content;
end;

procedure TWindowFrame.set_Content(const Value: CObject);
begin
  _content := Value;

  var fr: TFrame;
  if _content.TryAsType<TFrame>(fr) then
    _handle.AddObject(fr);
end;

procedure TWindowFrame.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(_onClose) then
    _onClose(nil);
end;

procedure TWindowFrame.Show(OnClose: TWindowClose);
begin
  _onClose := OnClose;
  _handle.OnClose := FormClose;
  _handle.Show;
end;

end.

