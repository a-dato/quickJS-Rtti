unit App.ObjectModelWithDescriptor.intf;

interface

uses
  System_,
  ADato.ObjectModel.List.Tracking.intf, App.PropertyDescriptor.intf;

type

  IObjectModelWithDescriptor = interface(IObjectListModelChangeTracking)
    ['{1C5AD094-C82D-42D5-BD7E-7AECF200446D}']

    function get_PropertyDescriptor: IPropertyDescriptor;

    property PropertyDescriptor: IPropertyDescriptor read get_PropertyDescriptor;

  end;

implementation

end.

