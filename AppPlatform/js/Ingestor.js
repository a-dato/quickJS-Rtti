export class ImportContext {
    constructor(options = null) {
        this.ParentStorageName = options?.ParentStorageName ?? null;
        this.ParentId = options?.ParentId ?? null;
        this.ParentCollectionName = options?.ParentCollectionName ?? null;
        this.ParentLoaderTypeName = options?.ParentLoaderTypeName ?? null;
    }
}

export class Ingestor {
    constructor(context = null) {
        this.Context = context ?? new ImportContext();
    }

    CreateObjectOfType(typeName, ...args) {
        const descriptor = app.Config.TypeDescriptorByName(typeName);

        if(descriptor == null)
            throw new Error(`No type descriptor registered with name ${typeName}`);

        return this.CreateInstance(descriptor, null, args);
    }
    
    CreateItem(sourceItem) {
        const descriptor = app.Config.TypeDescriptorByName(sourceItem.TypeName);

        if(descriptor == null)
            throw new Error(`No type descriptor registered with name ${sourceItem.TypeName}`);

        const item = this.CreateInstance(descriptor, sourceItem);

        for(const propertyName of Object.keys(sourceItem.Data ?? {})) {
            item[propertyName] = sourceItem.Value(propertyName);
        }

        this.AttachItem(item, sourceItem);

        return item;
    }

    CreateList(sourceItems) {
        const result = new List();

        for(const sourceItem of sourceItems) {
            result.Add(this.CreateItem(sourceItem));
        }

        return result;
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

    CreateInstance(descriptor, sourceItem = null, args = null) {
        const createArgs = this.CreateArgs(sourceItem, args);
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

    CreateArgs(sourceItem = null, args = null) {
        const createArgs = args ?? [];
        if(createArgs.length > 0)
            return createArgs;
        const parent = this.ParentItem(sourceItem);
        if(parent != null)
            return [parent];
        return [];
    }

    ParentItem(sourceItem) {
        if(sourceItem == null)
            return null;

        const parentStorageName = this.OptionValue(sourceItem, 'ParentStorageName');
        const parentId = this.OptionValue(sourceItem, 'ParentId');

        if(parentStorageName == null || parentId == null)
            return null;

        const storage = app.Storage[parentStorageName];

        for(const item of storage) {
            if(item.ID == parentId)
                return item;
        }

        throw new Error(`Parent not found: ${parentStorageName}[${parentId}]`);
    }

    AttachItem(item, sourceItem) {
        const parentCollectionName = this.OptionValue(sourceItem, 'ParentCollectionName');

        if(parentCollectionName == null)
            return;

        const parent = this.ParentItem(sourceItem);
        this.ForceOpenParent(parent, sourceItem);
        const collection = parent[parentCollectionName];

        if(collection == null || typeof collection.Add !== 'function')
            throw new Error(`Parent ${parent.ID} does not expose a ${parentCollectionName} collection.`);

        collection.Add(item);
    }

    ForceOpenParent(parent, sourceItem) {
        const parentLoaderTypeName = this.OptionValue(sourceItem, 'ParentLoaderTypeName');

        if(parentLoaderTypeName == null || parent.AsType == null)
            return;

        const loaderType = globalThis[parentLoaderTypeName];
        if(loaderType != null)
            parent.AsType(loaderType).ForceOpen();
    }

    OptionValue(sourceItem, name) {
        return sourceItem?.[name] ?? this.Context[name];
    }
}