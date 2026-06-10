export class Ingestor {
    constructor() {
        this.initializedProjects = new Set();
    }

    CreateObjectOfType(typeName, ...args) {
        const descriptor = app.Config.TypeDescriptorByName(typeName);

        if(descriptor == null)
            throw new Error(`No type descriptor registered with name ${typeName}`);

        return this.CreateInstance(descriptor, args);
    }
    
    Types() {
        return this.GetAvailableTypes();
    }

    GetAvailableTypes() {
        const types = app.Config.Types;
        if(types == null)
            throw new Error('app.Config does not expose its registered types.');

        const result = [];
        for(const type of types) {
            if(type == null || type === 0)
                continue;

            const descriptor = app.Config.TypeDescriptor(type);

            result.push({
                Name: type.name ?? type.Name,
                Type: type,
                Descriptor: descriptor,
                DescriptorName: descriptor?.Name ?? descriptor?.TypeName ?? descriptor?.ClassName,
                Properties: this.GetTypeProperties(type)
            });
        }

        return result;
    }

    GetTypeProperties(type) {
        const properties = [];

        for(const property of app.Config.GetProperties(type)) {
            properties.push({
                Name: property.Name,
                Type: typeof property.GetType === 'function'
                    ? property.GetType()
                    : property.Type,
                PropertyInfo: property,
            });
        }

        return properties;
    }

    CreateInstance(descriptor, args = null) {
        const createArgs = args ?? [];
        const instanceArgs = [this.NewID(), ...createArgs];

        let instance;
        if(typeof descriptor.CreateInstance === 'function') {
            instance = descriptor.CreateInstance(...instanceArgs);
        } else {
            const targetType = descriptor.GetType();
            instance = app.Factory.CreateInstance(targetType, ...instanceArgs);
        }

        return instance;
    }

    NewID() {
        return app.Factory.NextID();
    }

    AddTaskToProject(project, task) {
        this.RememberProject(project);
        this.PrepareProjectParent(project);
        let tasks = project.Tasks;
        tasks.Add(task);
        return task;
    }

    AddDependencyToProject(project, dependency) {
        this.RememberProject(project);
        this.PrepareProjectParent(project);
        let dependencies = project.Dependencies;
        dependencies.Add(dependency);
        return dependency;
    }

    AddResourceRequirementToTask(task, requirement) {
        if(task == null)
            throw new Error('Task is required.');

        let resourceRequirements = task.ResourceRequirements;
        resourceRequirements.Add(requirement);
        return requirement;
    }

    AttachItem(parent, collectionName, item) {
        return this.AddToParent(parent, collectionName, item);
    }

    PrepareProjectParent(parent) {
        const project = this.TryAsTypeName(parent, 'IProject');
        if(project == null)
            return null;

        const key = String(project.ID ?? project.LUID ?? '');
        if(this.initializedProjects.has(key))
            return project;

        this.initializedProjects.add(key);

        const loader = this.TryAsType(parent, globalThis.IProjectLoader)
            ?? this.TryAsTypeName(project, 'IProjectLoader');
        if(loader != null)
            loader.ForceOpen();

        // Only a new (never saved) project may be filled with the event
        // mechanism shut down (BeginUpdate). Saving such a project goes
        // through Project.SaveAll. Existing projects must keep their events
        // running, otherwise the normal save mechanism misses the changes.
        if(this.IsNewProject(project)) {
            const updateable = this.TryAsTypeName(project, 'IUpdateableObjectWithUpdateFlag');
            if(updateable != null)
                updateable.BeginUpdate(globalThis.UpdateFlag?.IgnoreUpdate ?? 1);
        }

        return project;
    }

    IsNewProject(project) {
        // New (unsaved) objects carry a negative ID, the server assigns a positive ID when the object is saved.
        const id = Number(project.ID);
        return Number.isFinite(id) && id < 0;
    }

    TryAsTypeName(item, typeName) {
        try {
            const descriptor = app.Config.TypeDescriptorByName(typeName);
            if(descriptor == null)
                return null;

            return this.TryAsType(item, descriptor.GetType());
        } catch {
            return null;
        }
    }

    TryAsType(item, type) {
        try {
            return item.AsType(type);
        } catch {
            return null;
        }
    }

    SaveNewProject(project) {
        if(project == null)
            throw new Error('Project is required.');
        if(typeof project.SaveAll !== 'function')
            throw new Error('Project does not expose SaveAll.');

        project.SaveAll();
        return project;
    }

    RememberProject(project) {
        if(project != null)
            globalThis.IntegrationManagerLastProject = project;
    }
}