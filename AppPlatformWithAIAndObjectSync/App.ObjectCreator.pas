unit App.ObjectCreator;

// Whether new objects of a type can be made in AppPlatform is up to the type's
// provider: a provider that owns its objects (local data, sample data) can
// make new ones; a provider that reads them from elsewhere (an API such as
// Trello, another application) cannot, so the Studio's "+ Add object" is off.
//
//   Delphi providers   implement IObjectCreator (ProjectProvider does)
//   script providers   define Add(values) in their Provider_ class (js\platform.js)

interface

uses
  System_,
  System.Collections;

type
  IObjectCreator = interface
    ['{4E7B2C91-0A5D-4F63-9B18-6C2E7D1A3F55}']
    // A new object for the storage whose objects are Existing; the caller adds it
    function NewObject(const Existing: IList): CObject;
  end;

implementation

end.
