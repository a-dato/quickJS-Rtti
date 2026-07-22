export default {
    ResourceRequirement: {
        Constructor: {
            Description: 'Creates a resource requirement for a task, optionally constrained to a resource class and/or a specific resource.',
            UsagePattern: "ingestor.CreateObjectOfType('IResourceRequirement', app.Factory.NextID(), taskOrID, resourceClass, resource)",
            Parameters: [
                {
                    Name: 'ID',
                    Type: 'CObject',
                    Description: 'Unique identifier for the new resource requirement; pass app.Factory.NextID().'
                },
                {
                    Name: 'TaskOrID',
                    Type: 'CObject',
                    Description: 'Owning task instance or its task ID.'
                },
                {
                    Name: 'ResourceClass',
                    Type: 'CObject',
                    Description: 'Required resource class or skill as an IS95ResourceClass-compatible object; pass null when no class is required.'
                },
                {
                    Name: 'Resource',
                    Type: 'CObject',
                    Description: 'Specifically assigned resource as an IS95Resource-compatible object; pass null when no specific resource is assigned.'
                }
            ]
        },
        ID: 'Unique identifier used to distinguish this resource requirement.',
        Skill: 'Skill required by the task. May be null when a specific resource is selected directly.',
        Resource: 'Resource assigned to the task. May be null when only a skill is specified.',
        Duration: 'Scheduled duration for this resource requirement before applying its units of work.',
        Status: 'Current status of the resource requirement.',
        Units: 'Required proportion of the resource, expressed from 0.0 to 1.0.',
        WorkRemaining: 'Remaining work after accounting for the required resource units.',
        SoftAssignment: 'Whether the scheduling engine may automatically select a suitable resource for the specified skill.'
    },
    Holiday: {
        PeriodStart: 'Start date of the holiday period.',
        PeriodEnd: 'End date of the holiday period. The boundaries alone do not provide enough information to determine working-time duration.',
        Description: 'Description of the holiday or absence.'
    }
};
