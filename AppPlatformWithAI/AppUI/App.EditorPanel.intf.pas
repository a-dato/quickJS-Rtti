unit App.EditorPanel.intf;

interface

uses
  System_,
  System.Classes,
  System.SysUtils,
  FMX.Types,
  FMX.Controls,
  App.PropertyDescriptor.intf;

type
  IEditorPanel = interface
    ['{95F63F57-8D3F-44D5-B199-501586103EED}']
    function  get_Editor: TControl;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Order: Integer;
    procedure set_Order(const Value: Integer);
    function  get_OnRealigned: TProc;
    procedure set_OnRealigned(const Value: TProc);
    function  get_PropertyDescriptor: IPropertyDescriptor;
    procedure set_PropertyDescriptor(const Value: IPropertyDescriptor);
    function  get_TitleControl: TControl;
    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);

    property Editor: TControl read get_Editor;
    property Enabled: Boolean read get_Enabled write set_Enabled;
    property OnRealigned: TProc read get_OnRealigned write set_OnRealigned;
    property Order: Integer read get_Order write set_Order;
    property PropertyDescriptor: IPropertyDescriptor read get_PropertyDescriptor write set_PropertyDescriptor;
    property TitleControl: TControl read get_TitleControl;
    property Visible: Boolean read get_Visible write set_Visible;
  end;

implementation

end.

