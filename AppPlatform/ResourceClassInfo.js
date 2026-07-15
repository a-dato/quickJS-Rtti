export default {
    ResourceClass: {
        Description: 'Name or description of the skill, team, virtual skill, or global skill.',
        Resources: 'Resources that possess this skill or belong to this team.',
        Tasks: 'Tasks whose resource requirements reference this skill.',
        SkillType: 'Skill category: Normal, Virtual, Team, or GlobalSkill.'
    },
    S95PropertyValue: {
        Value: 'Value assigned to a resource for a property defined by a global skill.',
        PropertyClass: 'Property definition associated with this value.'
    },
    S95PropertyClass: {
        Description: 'Name of the global-skill property, for example Country or Certification Level.',
        UnitOfMeasure: 'Definition of the value type and its formatting or selection behavior.',
        ValuePickList: 'Predefined values available when the property uses a pick list.',
        ResourceClass: 'Global skill that owns this property definition.'
    }
};
