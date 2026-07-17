import {TypeCode, ITypeDescriptor_} from './app.js';
import {visible,editor,status,Object_,Type_,Provider_} from './lynx.js';
//import {Ingestor} from 'D:/adato_webservice/MicroServices/IntegrationProject/ForContextNotPartOfProject/Ingestor.js';

export class Person extends Object_ {

	constructor(id, name, age) {
		super(id);

		this._Name = name;
		this._Age = age;
	}

    get Name() {
        return this._Name;
    }

    set Name(name) {
        this._Name = name;
    }

    get Age() {
        return this._Age;
    }

    set Age(age) {
        this._Age = age;
    }

}

export class PersonType extends ITypeDescriptor_ {
    static Instance = null;

    static register() {
        PersonType.Instance = new PersonType();
        globalThis.Person = Person;
        globalThis.PersonType = PersonType;
        app.Config.RegisterType(Person, PersonType.Instance);
    };

    static SetSource(source) {
        PersonType.Instance.Provider.Source = source;
    }

    constructor() {
        
        super();
        this.Provider = new PersonProvider();
        this.PropertyDescriptor = {
            Person: {
                GetType: () => {
                    return Person;
                },
                EditorType: editor.Edit,
                Label: 'Person',
                Picklist: {
                    Items: (filter) => {
                        return this.Provider.Data(filter);
                    }
                }                    
            },
            Name: {
                Type: TypeCode.String,
                EditorType: editor.Edit
            },
            Age: {
                Type: TypeCode.Int16,
                EditorType: editor.Edit
            }        
        }
    }

    CreateInstance(id) {
        return new Person(id);
    }

    get ClassName() {
        return 'Person';
    }

	static StorageName() {
		return 'Persons';
	}    

	ToString(format) {
		let frmt = PersonType.Instance?.PropertyDescriptor?.Person?.Formatter;
		if(frmt != undefined)
			return frmt.Format(null, this, format);
		else
			return this.Name;
	} 

}

export class PersonProvider extends Provider_ {
    
    constructor(source = null) {
        super();
        this.Source = source;
    }

    async Data(filter) {
        
        if(this.Source == null) {
            throw new Error('PersonProvider.Source is not configured.');
        }

        const items = await this.Source.Items(filter);
        //const ingestor = new Ingestor();
        // return ingestor.CreateList(items);
    }

}

