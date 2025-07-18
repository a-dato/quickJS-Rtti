export const visible = {
  hidden: 0,
  visible: 1
};

export const editor = {
	Text: 0,
	Edit: 1,
	Date: 2,
	Time: 3,
	DateTime: 4, 
	Check: 5,
	Memo: 6,
	Number: 7, 
	Combo: 8, 
	ComboEdit: 9, 
	Tags: 10, 
	Color: 11, 
	CustomEditor: 12
}

export const status = {
	None: 0,
	Started: 1,
	Released: 3,
	Closed: 4
}

class AddNew {
	AddNew() {
		console.log('AddNew()');
		return 'test';
	}
}

export class IBaseInterface_ {
  GetHashCode() {
		return this;
	}

  Equals(other) {
		console.log('Eq-0');
		return this == other;
	}
	
	GetType() {
		return this.constructor;
	}

	ToString() {
		return this.constructor.name;
	}
	
	QueryInterface(intf_type) {
		if(intf_type === IBaseInterface) {
			return this;
		}
		if(intf_type === IInterfaceWithID) {
			return this;
		}		
		if(intf_type === IAddNew) {
			return new AddNew();			
		}
	}
}

export class IInterfaceWithID_ extends IBaseInterface_ {
	
	constructor(id) {
		super();
		this._id = id;
  }

  get ID() {
		return this._id;
  }

  GetHashCode() {
		return this._id;
	}

  Equals(other) {
		return this._id == other.ID;
	}

}

export class LynxObject extends IInterfaceWithID_ {

  Clone() {
		return Object.assign(Object.create(Object.getPrototypeOf(this)), this);
	}
	
}

export class LynxType extends IBaseInterface_ {
}

export class LynxProvider extends IBaseInterface_ {
}
