export default {
    Resource: {
        Description: 'Name or description of the resource, for example "Bob Smith".',
        Skills: 'Skills possessed by this resource.',
        Comments: 'Additional comments about the resource.',
        NettoAvailability: 'Net availability of the resource, expressed from 0.0 to 1.0.',
        Tasks: 'Tasks to which this resource is assigned.',
        ManagedTasks: 'Tasks for which this resource acts as task manager.',
        ID: 'Unique resource identifier.',
        User: 'Associated user account. Null for resources such as equipment, rooms, or people without an account.',
        ResourceManager: 'User responsible for managing this resource.',
        Holidays: 'Holiday or absence periods during which the resource is unavailable.'
    },
    Holiday: {
        PeriodStart: 'Start date of the holiday period.',
        PeriodEnd: 'End date of the holiday period. The boundaries alone do not provide enough information to determine working-time duration.',
        Description: 'Description of the holiday or absence.'
    }
};
