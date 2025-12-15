
export const TypeCode = {
	Empty: 0,
	Object: 1,
	DBNull: 2, 
	Boolean: 3, 
	Char: 4, 
	SByte: 5, 
	Byte: 6, 
	Int16: 7,
  UInt16: 8, 
	Int32: 9, 
	UInt32: 10, 
	Int64: 11, 
	UInt64: 12, 
	Single: 13, 
	Double: 14, 
	Decimal: 15, 
	DateTime: 16, 
	String: 17,
  // Delphi specific
  Enum: 18, 
	Set: 19, 
	Method: 20, 
	Variant: 21, 
	Array: 22, 
	Record: 23, 
	Interface: 24, 
	Type: 25, 
	Pointer: 26};

export class IBaseInterface_ {
	get ClassName() {
		return this.constructor.name;
	}

  GetHashCode() {
		return this;
	}

  Equals(other) {
		return this == other;
	}
	
	GetType() {
		return new Type_(this.constructor);
	}

	ToString(format) {
		return this.constructor.name;
	}

	toString() {
		return this.ToString();
	}
	
	QueryInterface(type) {
		if(type === IBaseInterface || type === IInterfaceWithID || type === ICloneable) {
			return this;
		}
	}	
}

export class PropertyInfo_ {
	constructor(name, type) {
		this.name = name; // Must be a 'constructor'
		this.type = type;
  }

	get Name() {
		return this.name;
	}

	get Type() {
		return this.type;
	}

	toString() {
		return this.Name;
	}

}

export class ITypeDescriptor_ extends IBaseInterface_ {

}

export class Type_ {
	constructor(type) {
		this.type = type; // Must be a 'constructor'
  }
	
	get Name() {
		return this.type.name;
	}
	
	GetProperties() {
		let props = [];
		let descr = app.Config.TypeDescriptor(this.type)?.PropertyDescriptor;
		for(const prop of Object.getOwnPropertyNames(this.type.prototype)) {
			let type = TypeCode.string;
			if(descr != null) {
				type = descr[prop]?.Type;
			}
			let propinfo = new PropertyInfo_(prop, type);			
			props.push(propinfo);
		}
		return props;
	}
	
	PropertyByName(name) {
	}
	
	toString() {
		return this.Name;
	}	
}

