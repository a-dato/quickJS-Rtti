export class SourceItem {
	constructor(id, name, data = null) {
		this.ID = id;
		this.Name = name;
		this.Data = data ?? {};
	}

	Value(name, defaultValue = null) {
		const value = this.Data?.[name];
		return value ?? defaultValue;
	}
}
