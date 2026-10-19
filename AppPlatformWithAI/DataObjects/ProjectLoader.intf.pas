unit ProjectLoader.intf;

interface

uses
  App.TypeMetadata;

type
  {$M+}
  [TBehaviorAttribute(
    'ProjectLoader',
    'Controls explicit opening and closing of the project Gantt data module.',
    'Use when project data model collections such as Tasks and Dependencies must be available before normal lazy loading.')]
  IProjectLoader = interface
    ['{F2EA4199-AA0D-49DE-9FEA-B0DF2450E20D}']
    function  IsLoaded: Boolean;
    //function  DetailObjectsLoaded: Boolean;

    [TMethodMetadataAttribute('Allocates the project Gantt data module so project data model-backed collections become accessible, including Tasks, Dependencies, and ResourceRequirements.',
                              'Call this on the real IProject instance before reading or writing project collections.')]
    procedure ForceOpen;

    [TMethodMetadataAttribute('Forces the project data to close and release loaded state.', 'Call when project data should no longer remain open.')]
    procedure ForceClose;
    //procedure Load(ProjectChangedHandler: ProjectChangedEventHandlerProc);
    //procedure ReloadWithoutSaving;
    {$IFDEF APP_PLATFORM}
    //procedure ReloadCaptures(ForceReload: Boolean);
    {$ENDIF}
    //function  Unload(UnloadFromServer: Boolean; ProjectChangedHandler: ProjectChangedEventHandlerProc) : Boolean;
  end;

implementation

end.
