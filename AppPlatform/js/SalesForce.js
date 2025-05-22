fetch("https://login.salesforce.com/services/oauth2/token", {
  method: "POST",
  headers: {
    "Content-Type": "application/x-www-form-urlencoded",
  },
  body: {
  	"grant_type": "password",
		"client_id": "YOUR_CLIENT_ID",
		"client_secret": "YOUR_CLIENT_SECRET",
		"username": "YOUR_USERNAME",
		"password": "YOUR_PASSWORD+SECURITY_TOKEN"
  },
})
  .then((res) => res.json())
  .then((data) => {
    console.log("Access Token:", data.access_token);
    getAccounts(data.access_token, data.instance_url);
  })
  .catch(console.error);