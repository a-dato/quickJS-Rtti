unit BulkUpdate.intf;

interface

uses
  App.TypeMetadata;

type
  {$M+}
  [TBehaviorAttribute(
    'BulkUpdate',
    'Allows many changes to be applied while temporarily disabling event/update notifications.',
    'Use during mass import or when adding many child objects.')]
  IBulkUpdate = interface
    ['{DE7CB831-C68B-4144-BDE6-2043E09003BF}']

    [TMethodMetadataAttribute('Starts a bulk update block. Event/update notifications should be suppressed until EndBulkUpdate is called.',
                              'Always pair with EndBulkUpdate in a finally block.')]
    procedure BeginBulkUpdate;

    [TMethodMetadataAttribute('Ends a bulk update block and allows event/update notifications to resume.',
                              'Call this once for every BeginBulkUpdate call.')]
    procedure EndBulkUpdate;
  end;

implementation

end.
