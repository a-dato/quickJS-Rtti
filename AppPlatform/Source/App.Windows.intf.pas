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

  TWindowClose = reference to procedure(const Window: IWindow);

  {$M+}
  IWindowFrame = interface(IComponent)

    function  get_Content: CObject;
    procedure set_Content(const Value: CObject);

    procedure Show(OnClose: TWindowClose);

    property Content: CObject read get_Content write set_Content;
  end;

  {$M+}
  IWindowType = interface(IBaseInterface)

    function get_Name: CString;

    property Name: CString read get_Name;
  end;

  {$M+}
  IWindow = interface(IComponent)

    function  get_Frame: IWindowFrame;

    function  Build: IWindow; overload;
    function  Build(const Builder: IContentBuilder): IWindow; overload;
    function  Bind(const Storage: IStorage): IWindow;
    function  Show(OnClose: TWindowClose): IWindow;

    property Frame: IWindowFrame read get_Frame;
  end;

  IWindows = interface(List<IWindow>)
    function  CreateWindow(const AOwner: CObject; const AType: &Type) : IWindow;
  end;

implementation

end.

