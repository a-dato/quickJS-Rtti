class Customer {
  constructor(n) {
    this._Name = n;
    this._Age = 10;
  }

  get Name() {
   return this._Name;
  }

  get Age() {
  return this._Age;
  }
}

class CustomerProvider {
	get Data() {
		var l = new List();
		l.Add(new Customer('Customer 1'));
	  l.Add(new Customer('Customer 2'));
	  return l;
	}
}

class CustomerType {
  constructor() {
    this.Binder = new JSBinder();
    this.Builder = new JSFrameBuilder();
    this.Provider = new CustomerProvider();
  }
}

var c = new Customer();
var ct = new CustomerType();
app.Config.RegisterJSType(c, ct);

var w = app.Windows.CreateWindow(null, c);
//w.Build;
//w.Bind(app.Models.CreateOrGet(proto, customers));
w.Show;
