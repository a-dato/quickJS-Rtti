export default {
    Task: {
        Description: 'Name of the task, for example "Buy steel".',
        Priority: 'Task priority as an integer value.',
        IsCritical: 'Whether the task is on the critical chain.',
        TaskManagers: 'Resources responsible for managing the task.',
        Successors: 'Dependencies in which this task precedes another task.',
        Predecessors: 'Dependencies in which another task precedes this task.',
        Status: 'Current task status.',
        TaskType: 'Task category, such as Normal, FeedingBuffer, ProjectBuffer, SummaryTask, MilestoneBuffer, LevelOfEffort, WorkPackage, Stage, or VirtualDrum.',
        ChildTasks: 'Tasks contained by this task. Applicable to summary tasks.',
        BufferPenetration: 'Highest buffer penetration associated with the task, expressed from 0.0 to 1.0.',
        ProjectBufferPenetration: 'Penetration into the project buffer, expressed from 0.0 to 1.0.',
        CCPMPriority: 'Critical Chain Project Management priority. Higher values indicate higher priority.',
        ReadyToStart: 'Whether the task has become ready to start.',
        ReadyToStartDate: 'Date on which the task became ready to start. Null until the task is ready.',
        ScheduledStart: 'Planned start date calculated by the scheduling engine.',
        ScheduledStop: 'Planned completion date calculated by the scheduling engine.',
        MasterScenarioScheduledStart: "Scheduled start date from the project's master scenario. Use only when comparing scenarios.",
        MasterScenarioScheduledStop: "Scheduled completion date from the project's master scenario. Use only when comparing scenarios.",
        ActualStart: 'Actual start date. Null when the task has not started.',
        ActualStop: 'Actual completion date. Null until the task is completed.',
        Duration: 'Configured total duration of the task.',
        ProgressDate: 'Date on which the task progress information was last updated.',
        BaselineStartDate: 'Baseline start date captured when the project was released or its baseline was set.',
        BaselineStopDate: 'Baseline completion date captured when the project was released or its baseline was set.',
        ProjectDuration: "Task duration calculated using the project calendar. Prefer this duration for display because it accounts for working and non-working time.",
        Start: 'Task start value that is not calculated by the scheduling engine.',
        Stop: 'Task stop value that is not calculated by the scheduling engine.',
        ForwardScheduleInformation: 'Early-start schedule result describing the earliest interval in which the task can occur.',
        BackwardsScheduleInformation: 'Late-start schedule result describing the latest interval in which the task can occur.',
        EstimatedTimeToComplete: 'Estimated time to complete, also known as work remaining. Accounts for the current task status and progress.',
        ActualDuration: 'Elapsed task duration between its actual start and actual completion.',
        ScheduledDuration: 'Scheduled duration between its planned start and planned completion.',
        BaselineDuration: 'Baseline duration between the baseline start and completion dates.',
        BufferZone: 'Critical-chain buffer zone: None, Green, Orange, Red, White, or Black.',
        LUID: 'Task locally unique identifier.',
        ScheduleStatus: 'Scheduling status using the same values as task status.',
        ResourceRequirements: 'Resources or skills required to perform the task.'
    },
    TaskDependency: {
        Predecessor: 'Task that must precede the successor according to this dependency.',
        Successor: 'Task constrained by the predecessor according to this dependency.'
    },
    ScheduleInformation: {
        MakeSpan: 'Scheduled time interval from start to stop. Forward information represents the earliest interval and backward information the latest interval.'
    }
};
