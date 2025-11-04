unit App.Windows.intf;

interface

uses
  System_,
  System.Collections.Generic,
  App.Component.intf,
  App.Storage.intf,
  App.Content.intf;

type
  {$M+}
  IWindow = interface;
  IWindowFrame = interface;

  TFrameCreateFunc = reference to function(const AOwner: IWindow) : IWindowFrame;
  TWindowClose = reference to procedure(const Window: IWindow);

  {$M+}
  IWindowFrame = interface(IComponent)

    function  get_Control: TObject;
    procedure set_Control(const Value: TObject);

    property Control: TObject read get_Control write set_Control;
  end;

  {$M+}
  IWindowType = interface(IBaseInterface)
    ['{C5A7A2EB-1B9B-48A1-8B86-DCFFC2E9CDEA}']
    function get_Name: CString;

    function CreateFrame(const AOwner: IWindow) : IWindowFrame;

    property Name: CString read get_Name;
  end;

  {$M+}
  IWindow = interface(IComponent)

    function  get_Control: TObject;
    function  get_Frame: IWindowFrame;
    procedure set_Frame(const Value: IWindowFrame);

    function  CreateFrame(const Name: string) : IWindow;

    function  Bind(const Storage: IStorage): IWindow;
    function  Show(OnClose: TWindowClose): IWindow;

    property Control: TObject read get_Control;
    property Frame: IWindowFrame read get_Frame write set_Frame;
  end;

implementation

end.

