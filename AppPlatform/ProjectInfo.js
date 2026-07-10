export default {
    Project: {
        Description: 'Name of the project, for example "Build a bridge".',
        ReleaseDate: 'Date on which the project was released.',
        StartDate: 'Date on which the project actually started.',
        MasterScenarioReleaseDate: "Release date stored by the project's master scenario.",
        MasterScenarioStartDate: "Start date stored by the project's master scenario.",
        EndDate: 'Estimated project completion date. This is not the actual completion date and may be in the future.',
        Priority: 'Project priority as an integer value.',
        ProjectManager: 'User responsible for managing the project.',
        HoursPerDay: 'Working duration represented by one project day. Day-based task durations use this value rather than 24 hours.',
        Tasks: "Regular tasks in the project's task hierarchy. Released projects are loaded by default; data for an existing non-released project must be loaded before accessing its tasks.",
        BufferTasks: 'Project, feeding, and milestone buffer tasks in the project.',
        SummaryTasks: 'Summary tasks that contain other tasks in the project task hierarchy.',
        LUID: 'Project locally unique identifier.',
        Status: 'Current project status. A project is complete only when its status is Completed.',
        BufferConsumption: "Highest project-buffer penetration among the project's buffer tasks, expressed from 0.0 to 1.0. Returns 0 when no buffer tasks exist.",
        LongestChainComplete: "Completed proportion of the project's critical chain, expressed from 0.0 to 1.0.",
        Color: 'ARGB color value optionally used to visually identify or group the project.',
        ProjectBuffer: 'Longest project-buffer task. Available only for released projects that contain buffer tasks.',
        CCPMBufferHealthColor: 'CCPM buffer-health zone based on project-buffer penetration: green, yellow, or red.',
        CCPMPriority: 'Critical Chain Project Management priority of the project.',
        ProjectResourceAvailability: 'Per-resource-class availability overrides for this project. Missing entries use the default availability rules.',
        ScheduleStatistics: 'Calculated schedule statistics. Available only for released projects.',
        Cards: 'Cards associated with this project, used to represent and organize project-related work items.',
        ResourceRequirements: "Resource requirements across the project's tasks. Each requirement describes the resource, skill, quantity, duration, or scheduling behavior needed to perform a task.",
        Dependencies: "Relationships between the project's tasks that constrain their scheduling. Each dependency identifies a predecessor, a successor, a relation type such as finish-to-start, and an optional lag."
    }
    // ,
    // ProjectResourceAvailability: {
    //     ResourceClass: 'Skill, team, virtual skill, or global skill to which this project availability override applies.',
    //     Availability: 'Explicit project availability limit. A value of -1 means that default availability should be used.',
    //     Calendar: "Calendar override for this resource class. A null value uses the resource class's default calendar.",
    //     SelectedResources: 'Explicit resources allowed for this project and resource class. A null or empty list lets the scheduler choose from all capable resources.',
    //     PropertyClassProperties: 'Property constraints and default property values used when selecting capable resources. An empty list applies no additional property filter.'
    // },
    // PropertyValueWithOperator: {
    //     PropertyClass: 'Property class being constrained when selecting capable resources.',
    //     Value: 'Required or default value for the property class.',
    //     PropertyOperator: 'Comparison operator used with the property value when filtering capable resources.'
    // },
    // ScheduleStatistics: {
    //     CalculatedFinish: 'Calculated project completion date based on the current schedule.',
    //     CurrentLongestChainDuration: 'Current duration of the longest chain of dependent tasks.',
    //     DueDatePerformance: 'Time difference between scheduled completion and the project due date.',
    //     LongestChainComplete: 'Completed proportion of the critical chain, expressed from 0.0 to 1.0.',
    //     LongestChainDuration: 'Total duration of the longest chain of dependent tasks.',
    //     ProjectDuration: 'Total scheduled duration of the project.',
    //     ScheduleDirection: 'Direction in which the project is scheduled: forward or backward.',
    //     TotalWork: 'Total work duration across all project tasks.',
    //     LateFinish: 'Latest allowable project finish date.',
    //     UnassignedWorkTotal: 'Total work duration that is not assigned to a resource.',
    //     UnassignedWorkRemaining: 'Remaining work duration that is not assigned to a resource.',
    //     WorkRemaining: 'Total remaining work duration for the project.'
    // }
};
