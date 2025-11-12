import {Type_, PropertyInfo_, IBaseInterface_} from './app.js';

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
	Scheduling: 1,
	Released: 2,
	Started: 4,
	Completed: 8,
	Paused: 16,
	Aborted: 32
}

export const access_right = {
	NoAccess: 0,
  SpaceReader: 1,
	NormalRead: 2,
	ReadWrite: 4,
  CanDelete: 8,
  CanAddSpaces: 16,
  CanAddCalendars: 32,
  CanAddResources: 64,
  CanAddResourceClasses: 128,
  CanAddProjects: 256,
  CanAddTasks: 512,
  CanAddCards: 1024,
  CanAddNotes: 2048,
  CanAddDocuments: 4096,
  CanAddUnitsOfMeasure: 8192,
  CanAddResourceRequirement: 16384,
  CanAddTemplates: 32768,
  CanAddUsers: 65536,
  CanAddStages: 131072,
  CanUpdateProgress: 262144,
  CanAddTaskDependencies: 524288
}

export const UpdateFlag = {
	ApplyUpdate: 0,
	IgnoreUpdate: 1
}

export const InsertPosition = {
  None: 0,
	Before: 1,
	After: 2,
	Child: 3,
	ChildFirst: 4
}

export function hoursToTimeSpan(hours)
{
	return BigInt(hours*36000000000);
}

export function bigmultiply(i64, f) {
	// Choose a scale to preserve decimals (e.g. 106 for 6 decimal places)
	let scale = 1_000n;
	let scaledFactor = BigInt(Math.round(f * Number(scale)));
	return BigInt((i64 * scaledFactor) / scale);
}

class AddingNew {
	CreateInstance() {
		console.log('CreateInstance()');
		return 'test';
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
	
	get Rights() {
		return access_right.ReadWrite;
	}
}

export class ICloneable_ extends IInterfaceWithID_ {
  Clone() {
		return Object.assign(Object.create(Object.getPrototypeOf(this)), this);
	}
}

export class Object_ extends ICloneable_ {

}

export class Type_ extends IBaseInterface_ {
}

export class Provider_ extends IBaseInterface_ {
}

class ILynxDialog
{
}

export class LynxDialog extends ILynxDialog
{

}

export class Project_ extends Object_ {
	QueryInterface(type) {
		if(type === IProject) {
			return this;
		}
		return super.QueryInterface(type);
	}
}

export class Task_ extends Object_ {
	QueryInterface(type) {
		if(type === ITask) {
			return this;
		}
		return super.QueryInterface(type);
	}
}
