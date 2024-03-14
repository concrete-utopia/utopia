# utopia-remix

## Local setup

0. cd to `utopia-remix`, run `pnpm install`
1. Set up the `.env` file, there's a `.env.sample` file you can use as a blueprint. See [#environment-variables]([#environment-variables]) for more details.
2. Restart Utopia.
3. Requests to the backend APIs will now be proxied via the Remix BFF.
4. If you want to change that and have direct connections, update the value of `BACKEND_TYPE` inside `env-vars.ts`.

## Environment variables

For local development they can be put in a `.env` file, using `.env.sample` as a blueprint.

| Name                        | Description                                                                                                                                                                                          | Example                                                 |
| --------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| `APP_ENV`                   | The environment the app is running on, with possible values shown [here](https://github.com/concrete-utopia/utopia/blob/e881cbf330e2ab68f8ea45f5afdbe8ed2c59ebca/utopia-remix/app/env.server.ts#L4). | `local`                                                 |
| `BACKEND_URL`               | The base URL for the Haskell server, as `<scheme>://<host>:<port>`                                                                                                                                   | `http://127.0.0.1:8001`                                 |
| `CORS_ORIGIN`               | The allowed origin for CORS requests, it should match the host of the browser app.                                                                                                                   | `http://localhost:8000`                                 |
| `EDITOR_URL`                | The base URL for the editor, as used in the frontend portions of the Remix app.                                                                                                                      | `http://localhost:8000`                                 |
| `AUTH0_ENDPOINT`            | Auth0 endpoint                                                                                                                                                                                       | `http://foo.bar.auth0.com`                              |
| `AUTH0_CLIENT_ID`           | Auth0 client ID                                                                                                                                                                                      | `xxyyzz`                                                |
| `AUTH0_REDIRECT_URI`        | Auth0 login redirect URI                                                                                                                                                                             | `http://localhost:8000/authenticate`                    |
| `GITHUB_OAUTH_CLIENT_ID`    | Github OAuth app client ID URI                                                                                                                                                                       | `xxyyzz`                                                |
| `GITHUB_OAUTH_REDIRECT_URL` | Github OAuth app login redirect URL                                                                                                                                                                  | `http://localhost:8000/v1/github/authentication/finish` |
| `UTOPIA_CDN_URL`            | Link to the root of the Utopia CDN                                                                                                                                                                   | `http://localhost:8000`                                 |

These are the required environment variables for local FGA dev (against a remote server), they can be put in a `.env` file, using `.env.sample` as a blueprint.

| Name                   | Description             | Example                    |
| ---------------------- | ----------------------- | -------------------------- |
| `FGA_SECRET`           | Okta's FGA Secret       | `<hash>`                   |
| `FGA_CLIENT_ID`        | Okta's FGA Client ID    | `<hash>`                   |
| `FGA_STORE_ID`         | Okta's FGA Store ID     | `<hash>`                   |
| `FGA_API_AUDIENCE`     | Okta's FGA API audience | `https://api.us1.fga.dev/` |
| `FGA_API_TOKEN_ISSUER` | Okta's FGA token issuer | `fga.us.auth0.com`         |
| `FGA_API_HOST`         | Okta's FGA API host     | `api.us1.fga.dev`          |
| `FGA_API_SCHEME`       | Okta's FGA API scheme   | `https`                    |
