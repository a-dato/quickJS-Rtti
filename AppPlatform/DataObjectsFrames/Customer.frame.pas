unit Customer.frame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  App.FrameWithWindowFrame, FMX.Controls.Presentation, FMX.Edit, FMX.Objects,
  FMX.Layouts;

type
  TCustomerWindowFrame = class(TFmxFrameWithWindowFrameSupport)
    Customer_Name: TEdit;
    Layout1: TLayout;
    Rectangle1: TRectangle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CustomerWindowFrame: TCustomerWindowFrame;

implementation

{$R *.fmx}

end.
