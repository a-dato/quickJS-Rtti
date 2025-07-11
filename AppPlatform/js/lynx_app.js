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
		return 'test';
	}
}

export class IBaseInterface {
	QueryInterface(interface_type) {
		console.log('QueryInterface');
		if(interface_type === IAddNew)
			return new AddNew();
	}
}

export class LynxObject extends IBaseInterface {
}

export class LynxType extends IBaseInterface {
}

export class LynxProvider extends IBaseInterface {
}
