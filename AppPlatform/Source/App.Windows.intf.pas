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

  WindowFrameCreateFunc = reference to function(const AOwner: CObject) : IWindowFrame;
  WindowClose = reference to procedure(const Window: IWindow);

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
    function get_ObjectType: &Type;

    function CreateFrame(const AOwner: CObject) : IWindowFrame;

    property ObjectType: &Type read get_ObjectType;
    property Name: CString read get_Name;
  end;

  {$M+}
  IWindow = interface(IComponent)

    function  get_Frame: IWindowFrame;
    procedure set_Frame(const Value: IWindowFrame);

    function  Build: IWindow; overload;
    function  Build(const Builder: IContentBuilder): IWindow; overload;
    function  Bind(const Storage: IStorage): IWindow;
    function  Show(OnClose: WindowClose): IWindow;

    property Frame: IWindowFrame read get_Frame write set_Frame;
  end;

  IWindows = interface(List<IWindow>)
    function  CreateWindow(const AType: &Type; const AOwner: CObject) : IWindow; overload;
    function  CreateWindow(const AType: &Type; const AOwner: CObject; const Name: string) : IWindow; overload;
  end;

implementation

end.

