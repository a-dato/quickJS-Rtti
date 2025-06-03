export default function fetch(url, options = {}) {
	return new Promise((resolve,reject) => {  
		const xhr = new XMLHttpRequest();
		const method = options.method || 'GET';

		// Set headers if provided
		if (options.headers) {
		  for (const [key, value] of Object.entries(options.headers)) {
			xhr.setRequestHeader(key, value);
		  }
		}

		xhr.open(method, url, false /* run synchronous*/);

		if(options.body != null)
			xhr.send(options.body);
		else
			xhr.send();
		
		if(xhr.readyState == 4) {
			const response = {
				ok: xhr.status >= 200 && xhr.status < 300,
				status: xhr.status,
				statusText: xhr.statusText,
				url: xhr.responseURL,
				text: () => Promise.resolve(xhr.responseText),
				json: () => Promise.resolve(JSON.parse(xhr.responseText)),
				blob: () => Promise.resolve(new Blob([xhr.response])),
			};
			resolve(response);    
		} else
			reject('XMLHttpRequest.readyState invalid: ' + xhr.readyState);
	});
};

/* Sample call
fetch('https://google.com')
.then(res => res.text())
.then(data => console.log(data))
.catch(err => console.log(err));
*/