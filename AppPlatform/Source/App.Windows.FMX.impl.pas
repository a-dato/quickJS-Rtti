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
    _control: TObject;
    _handle: TForm;
    _onClose: WindowClose;

    function  get_Control: TObject;
    procedure set_Control(const Value: TObject);

    procedure Show(OnClose: WindowClose);

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

function TWindowFrame.get_Control: TObject;
begin
  Result := _control;
end;

procedure TWindowFrame.set_Control(const Value: TObject);
begin
  _control := Value;

  if _control is TFrame then
    _handle.AddObject(_control as TFrame);
end;

procedure TWindowFrame.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(_onClose) then
    _onClose(nil);
end;

procedure TWindowFrame.Show(OnClose: WindowClose);
begin
  _onClose := OnClose;
  _handle.OnClose := FormClose;
  _handle.Show;
end;

end.

