import PROJECT_INFO from '../ProjectInfo.js';

export class Ingestor {
    constructor() {
        this.initializedProjects = new Set();
    }

    RegisterProjectCustomersProperty() {
        const descriptor = globalThis.CustomerType.Instance.PropertyDescriptor.Customer;
        return app.Config.AddProperty(
            globalThis.IProject,
            'Customers',
            'Customers',
            List,
            descriptor
        );
    }

    CreateObjectOfType(type, ...args) {
        const descriptor = app.Config.TypeDescriptor(type);

        if(descriptor == null)
            throw new Error(`No type descriptor registered for type ${type?.Name ?? type?.name ?? String(type)}.`);

        const instance = this.CreateInstance(descriptor, args);
        // if(type === globalThis.IProject)
        //     this.PrepareProjectParent(instance);

        return instance;
    }

    ProjectInfo() {
        return PROJECT_INFO;
    }
    
    Types() {
        const types = app.Config.Types;
        if(types == null)
            throw new Error('app.Config does not expose its registered types.');

        const result = [];
        for(const type of types) {
            if(type == null || type === 0)
                continue;

            const descriptor = app.Config.TypeDescriptor(type);

            result.push({
                Name: type.Name ?? type.name,
                Type: type,
                Descriptor: descriptor,
                DescriptorName: descriptor?.Name ?? descriptor?.TypeName ?? descriptor?.ClassName,
                Properties: this.GetTypeProperties(type)
            });
        }

        return result;
    }

    TypeDescriptors() {
        const descriptors = app.Config.TypeDescriptors;
        if(descriptors == null)
            throw new Error('app.Config does not expose its registered type descriptors.');

        const result = [];
        for(const descriptor of descriptors) {
            if(descriptor == null || descriptor === 0)
                continue;

            let type = null;
            if(typeof descriptor.GetType === 'function')
                type = descriptor.GetType();

            const properties = [];
            if(type != null) {
                for(const property of this.GetTypeProperties(type))
                    properties.push(property);
            } else if(descriptor.PropertyDescriptor != null) {
                for(const name of Object.keys(descriptor.PropertyDescriptor)) {
                    const propertyDescriptor = descriptor.PropertyDescriptor[name];
                    const description = this.GetPropertyDescription(descriptor.ClassName ?? descriptor.TypeName ?? descriptor.FullName, name);
                    const entry = {
                        Name: name,
                        IsCustomField: String(name).startsWith('FIELD_'),
                        Type: propertyDescriptor?.Type ?? null,
                        PropertyDescriptor: propertyDescriptor
                    };
                    if(description != null)
                        entry.Description = description;

                    properties.push(entry);
                }
            }

            result.push({
                ClassName: descriptor.ClassName,
                FullName: descriptor.FullName,
                StorageName: descriptor.StorageName,
                Type: type,
                Descriptor: descriptor,
                SupportedInterfaces: this.GetSupportedInterfaces(descriptor),
                Properties: properties
            });
        }

        return result;
    }

    CreateInstance(descriptor, args = null) {
        const createArgs = args ?? [];
        //const instanceArgs = [this.NewID(), ...createArgs];

        let instance;
        if(typeof descriptor.CreateInstance === 'function') {
            instance = descriptor.CreateInstance(...createArgs);
        } else {
            const targetType = descriptor.GetType();
            instance = app.Factory.CreateInstance(targetType, ...createArgs);
        }

        return instance;
    }

    NewID() {
        return app.Factory.NextID();
    }

    // AddTaskToProject(project, task) {
    //     //this.RememberProject(project);
    //     this.PrepareProjectParent(project);
    //     const tasks = project.Tasks;
    //     tasks.Add(task);
    //     return task;
    // }

    // AddDependencyToProject(project, dependency) {
    //     //this.RememberProject(project);
    //     this.PrepareProjectParent(project);
    //     const dependencies = project.Dependencies;
    //     dependencies.Add(dependency);
    //     return dependency;
    // }

    // AddResourceRequirementToTask(task, requirement) {
    //     if(task == null)
    //         throw new Error('Task is required.');

    //     const resourceRequirements = task.ResourceRequirements;
    //     resourceRequirements.Add(requirement);
    //     return requirement;
    // }

    // PrepareProjectParent(parent) {
    //     const project = this.TryAsTypeName(parent, 'IProject');
    //     if(project == null)
    //         return null;

    //     const loader = this.TryAsType(parent, globalThis.IProjectLoader)
    //         ?? this.TryAsTypeName(project, 'IProjectLoader');
    //     if(loader != null)
    //         loader.ForceOpen();

    //     console.log(`PrepareProjectParent: project="${project.Description ?? project.LUID ?? project.ID}", ID=${project.ID}, initialized=${this.initializedProjects.has(project)}.`);
    //     console.log(`PrepareProjectParent: typeof IsUpdating before checks = ${typeof project.IsUpdating}.`);
    //     if(typeof project.IsUpdating === 'function')
    //         console.log(`PrepareProjectParent: IsUpdating before checks = ${project.IsUpdating()}.`);

    //     console.log("ProjectID: " + project.ID + " >= 0: " + (Number(project.ID) >= 0));
    //     if(Number(project.ID) >= 0) {
    //         console.log(`PrepareProjectParent: saved project, no BeginUpdate. initializedCount=${this.initializedProjects.size}.`);
    //         return project;
    //     }

    //     if(this.initializedProjects.has(project)) {
    //         console.log(`PrepareProjectParent: project already initialized, no extra BeginUpdate. initializedCount=${this.initializedProjects.size}.`);
    //         return project;
    //     }

    //     const updateable = parent.AsType(globalThis.IUpdateableObjectWithUpdateFlag);
    //     if(updateable == null) {
    //         console.log(`PrepareProjectParent: project does not expose IUpdateableObjectWithUpdateFlag, no BeginUpdate.`);
    //         return project;
    //     }

    //     console.log(`PrepareProjectParent: calling BeginUpdate. initializedCountBefore=${this.initializedProjects.size}.`);
    //     updateable.BeginUpdate(UpdateFlag.ApplyUpdate);
    //     this.initializedProjects.add(project);
    //     console.log(`PrepareProjectParent: BeginUpdate completed. initializedCountAfter=${this.initializedProjects.size}.`);
    //     if(typeof project.IsUpdating === 'function')
    //         console.log(`PrepareProjectParent: IsUpdating after BeginUpdate = ${project.IsUpdating()}.`);

    //     return project;
    // }

    // FinalizeProjects(project = null) {
    //     const initializedProjects = Array.from(this.initializedProjects);
    //     this.initializedProjects.clear();

    //     console.log(`FinalizeProjects: ${initializedProjects.length} initialized project(s).`);

    //     for(const project of initializedProjects) {
    //         if(project == null)
    //             continue;

    //         try {
    //             console.log(`FinalizeProjects: calling EndUpdate for "${project.Description ?? project.LUID ?? project.ID}".`);
    //             console.log(`FinalizeProjects: typeof IsUpdating before EndUpdate = ${typeof project.IsUpdating}.`);
    //             if(typeof project.IsUpdating === 'function')
    //                 console.log(`FinalizeProjects: IsUpdating before EndUpdate = ${project.IsUpdating()}.`);

    //             const updateable = project.AsType(globalThis.IUpdateableObjectWithUpdateFlag);
    //             if(updateable != null) {
    //                 updateable.EndUpdate(UpdateFlag.ApplyUpdate);
    //                 console.log(`FinalizeProjects: EndUpdate completed.`);
    //                 if(typeof project.IsUpdating === 'function')
    //                     console.log(`FinalizeProjects: IsUpdating after EndUpdate = ${project.IsUpdating()}.`);
    //             } else {
    //                 console.log(`FinalizeProjects: project does not expose IUpdateableObjectWithUpdateFlag.`);
    //             }
    //         } catch(e) {
    //             console.log(`FinalizeProjects failed: ${e?.message ?? e}`);
    //         }
    //     }

    //     if(project == null)
    //         return;

    //     try {
    //         console.log(`FinalizeProjects: calling NotifyProjectChanged for "${project.Description ?? project.LUID ?? project.ID}".`);
    //         const projectChanged = project.AsType(globalThis.IProjectChangedEvent);
    //         if(projectChanged != null) {
    //             projectChanged.NotifyProjectChanged();
    //             console.log(`FinalizeProjects: NotifyProjectChanged completed.`);
    //         } else {
    //             console.log(`FinalizeProjects: project does not expose IProjectChangedEvent.`);
    //         }
    //     } catch(e) {
    //         console.log(`FinalizeProjects NotifyProjectChanged failed: ${e?.message ?? e}`);
    //     }
    // }

    TryAsTypeName(item, typeName) {
        const descriptor = app.Config.TypeDescriptorByName(typeName);
        if(descriptor == null)
            return null;

        return this.TryAsType(item, descriptor.GetType());
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

    // RememberProject(project) {
    //     if(project != null)
    //         globalThis.IntegrationManagerLastProject = project;
    // }

    Find(source, keyProperty, keyValue) {
        const list = typeof source === 'string' ? this.GetStorage(source) : source;
        if(list == null) {
            console.log(`Find: ${keyProperty}="${keyValue}" in ${source} -> list is null`);
            return null;
        }

        if(typeof list.FindByProperty === 'function') {
            const found = list.FindByProperty(keyProperty, keyValue) ?? null;
            console.log(`Find: ${keyProperty}="${keyValue}" in ${source} -> ${found != null ? 'match' : 'no match'}`);
            return found;
        }

        const wanted = String(keyValue);
        let inspected = 0;
        for(const item of this.Items(list)) {
            inspected++;
            if(item == null)
                continue;

            const key = item[keyProperty];
            if(key != null && String(key) === wanted) {
                console.log(`Find: ${keyProperty}="${wanted}" in ${source} -> match`);
                return item;
            }
        }

        console.log(`Find: ${keyProperty}="${wanted}" in ${source} -> no match (${inspected} item(s) inspected)`);
        return null;
    }

    // Iterates a bridged Delphi list or plain JS array, whichever access pattern it supports
    *Items(list) {
        let count = typeof list.Count === 'function' ? list.Count() : list.Count;
        if(typeof count !== 'number')
            count = list.length;

        if(typeof count === 'number' && count >= 0) {
            for(let i = 0; i < count; i++)
                yield list[i];
            return;
        }

        if(typeof list.GetEnumerator === 'function') {
            const enumerator = list.GetEnumerator();
            while(enumerator.MoveNext())
                yield enumerator.Current;
            return;
        }

        if(typeof list[Symbol.iterator] === 'function') {
            yield* list;
            return;
        }

        console.log('Items: no way to iterate this list (no Count, length, GetEnumerator or iterator)');
    }

    GetStorage(name) {
        const storage = app.Storage[name];
        if(storage == null)
            throw new Error(`Storage '${name}' does not exist.`);

        return storage;
    }
}