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

export class IBaseInterface {
	QueryInterface(intf_type) {
		console.log(`QueryInterface: ${intf_type}`);
		if(intf_type === IAddNew) {
			console.log('Is AddNew');
			return new AddNew();			
		}

		console.log('Is NOTAddNew');
		
/*
		if(IID === IAddNew)
			return new AddNew();
			
		return new AddNew();			
*/
	}
}

export class LynxObject extends IBaseInterface {
}

export class LynxType extends IBaseInterface {
}

export class LynxProvider extends IBaseInterface {
}
