export class SourceItem {

	constructor(typeName, data = null, options = null) {
		this.TypeName = typeName;
		this.Data = data ?? {};
		this.ParentStorageName = options?.ParentStorageName ?? null;
		this.ParentId = options?.ParentId ?? null;
		this.ParentCollectionName = options?.ParentCollectionName ?? null;
		this.ParentLoaderTypeName = options?.ParentLoaderTypeName ?? null;
	}

	HasValue(name) {
		return Object.prototype.hasOwnProperty.call(this.Data, name);
	}

	Value(name, defaultValue = null) {
		const value = this.HasValue(name) ? this.Data[name] : null;
		return value ?? defaultValue;
	}

}

export class SourceDataPerson {
	constructor(name = 'Imported person', age = 30) {
		this.Name = name;
		this.Age = age;
	}
}

export class SourceDataTask {
	constructor(luid = 'Imported-task-1', description = 'Imported task from source data') {
		this.LUID = luid;
		this.Description = description;
	}
}