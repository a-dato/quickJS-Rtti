unit App.KeyNavigator.intf;

interface

uses
  System.Classes,
  System.UITypes,
  FMX.Layouts,
  FMX.Controls;

const
  NavKeys = [vkHome, vkEnd, vkPrior, vkNext, vkUp, vkDown, vkLeft, vkRight, vkTab];
  FocusKeys = [vkPrior, vkNext, vkUp, vkDown, vkTab];
  NavFrameNavKeys = [vkLeft, vkRight];
  SelectorKeys = [vkEscape, vkReturn];

type
  TNavigatorType = (ControlNavigator, EditorNavigator, TabNavigator, ListControlNavigator);

  IKeyNavigator = interface
    ['{B9799D8D-3F74-4FB8-9D57-92F67C6C2F54}']
    function  TryHandleKeyNavigation(var Key: Word; Shift: TShiftState; const Current: TControl; ChangeFocus: Boolean = True): Boolean;
    function  GetControl: TControl;
    function  InnerNavigationKeys: TArray<Word>;
    function  GetScrollControl: TCustomScrollBox;
    function  NavigatorType: TNavigatorType;

    function  SetFocusIfValid(const ForceInnerControl: TControl = nil): Boolean;
  end;

  ISelectorKeyNavigator = interface(IkeyNavigator)
    ['{2723BC6A-B7AE-4608-81EB-70F03451A614}']
    procedure SelectFirstAvailable;
//    procedure SelectLastEditor;
  end;

implementation

end.

