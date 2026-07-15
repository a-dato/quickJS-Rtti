export default {
    Booking: {
        When: 'Date and time for which the booking was recorded.',
        HoursBooked: 'Booked working time. Use TotalHours when a numeric hour value is needed.',
        Comment: 'Comment entered with the booking.',
        Project: 'Related project when it still exists; otherwise null.',
        ProjectID: 'Identifier of the related project.',
        User: 'Identifier of the user who made the booking.',
        Task: 'Related task for a task booking. Usually null for a card booking.',
        TaskID: 'Identifier of the related task, including when the task no longer exists.',
        Card: 'Related card for a card booking. Usually null for a task booking.',
        CardID: 'Identifier of the related card, including when the card no longer exists.'
    },
    TimeTrackDetail: {
        When: 'Date and time for which the booking was recorded.',
        HoursBooked: 'Booked working time.',
        Comment: 'Comment entered with the booking.',
        ProjectID: 'Identifier of the related project.',
        User: 'Identifier of the user who made the booking.',
        TaskID: 'Identifier of the related task for a task booking.',
        CardID: 'Identifier of the related card for a card booking.'
    }
};
