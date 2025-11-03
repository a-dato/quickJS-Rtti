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

  WindowCreateFunc = reference to function(const AOwner: CObject) : IWindow;
  WindowClose = reference to procedure(const Window: IWindow);

  {$M+}
  IWindowFrame = interface(IComponent)

    function  get_Content: CObject;
    procedure set_Content(const Value: CObject);

    procedure Show(OnClose: WindowClose);

    property Content: CObject read get_Content write set_Content;
  end;

  {$M+}
  IWindowType = interface(IBaseInterface)
    ['{C5A7A2EB-1B9B-48A1-8B86-DCFFC2E9CDEA}']
    function get_Name: CString;
    function get_ObjectType: &Type;

    function CreateInstance(const AOwner: CObject) : IWindow;

    property ObjectType: &Type read get_ObjectType;
    property Name: CString read get_Name;
  end;

  {$M+}
  IWindow = interface(IComponent)

    function  get_Frame: IWindowFrame;

    function  Build: IWindow; overload;
    function  Build(const Builder: IContentBuilder): IWindow; overload;
    function  Bind(const Storage: IStorage): IWindow;
    function  Show(OnClose: WindowClose): IWindow;

    property Frame: IWindowFrame read get_Frame;
  end;

  IWindows = interface(List<IWindow>)
    function  CreateWindow(const AType: &Type; const AOwner: CObject) : IWindow; overload;
    function  CreateWindow(const AType: &Type; const AOwner: CObject; const FrameClassName: string) : IWindow; overload;
  end;

implementation

end.

