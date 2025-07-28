unit App.Windows.intf;

interface

uses
  System_,
  System.Collections.Generic,
  ADato.ObjectModel.List.intf, App.Storage.intf;

type
  {$M+}
  IWindowFrame = interface(IBaseInterface)

    function  get_Content: CObject;
    procedure set_Content(const Value: CObject);
    function  get_Owner: CObject;
    procedure Show;

    property Content: CObject read get_Content write set_Content;
    property Owner: CObject read get_Owner;
  end;

  {$M+}
  IWindowType = interface(IBaseInterface)

    function get_Name: CString;

    property Name: CString read get_Name;
  end;

  {$M+}
  IWindow = interface(IBaseInterface)

    function get_Frame: IWindowFrame;
    function get_Name: CString;

    function  Build: IWindow;
    function  Bind(const Storage: IAppStorage): IWindow;
    function  Show: IWindow;

    property Frame: IWindowFrame read get_Frame;
    property Name: CString read get_Name;
  end;

  IWindows = interface(List<IWindow>)
    function  CreateWindow(const AOwner: CObject; const AType: &Type) : IWindow;
  end;

implementation

end.

