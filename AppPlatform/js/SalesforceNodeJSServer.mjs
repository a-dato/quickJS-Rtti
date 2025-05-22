import express from 'express';
import fetch from 'node-fetch';
import cors from 'cors';

const app = express();
app.use(cors()); // Enable CORS for all routes

app.post("/api/auth", async (req, res) => {
  const params = new URLSearchParams();
		params.append("grant_type", "password");
		params.append("client_id", "   ");
		params.append("client_secret", "  ");
		params.append("username", " ");
		params.append("password", "  ");
  

  const response = await fetch("https://login.salesforce.com/services/oauth2/token", {
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: params.toString(),
  });

  const data = await response.json();
  res.json(data);
});

app.listen(3000, () => console.log("Server running on http://localhost:3000"));
