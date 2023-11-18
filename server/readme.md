# Utopia Server

## Getting it up and running.

This project leans on the [Nix](https://nixos.org/nix/) build tool/package manager because it's cool to get all the right tooling.
So once that has been installed all that needs to be done is to run `nix-shell` in the same folder as this readme and everything should be setup for you.
That shell session is self contained so if it is left by running `exit`, or a new session is started, all of the scripts and tools will be unavailable.

In the new shell session for the first time run `cabal new-update` to get a local copy of the Haskell package database.

There are some shell scripts provided as part of the shell session for your convenience:

- `watch-server` - Runs the server but with file watching so that if any of the source changes the server will be restarted.
- `watch-tests` - Runs the tests but with file watching so that if any of the source changes the tests will be rerun.
- `style-project` - Runs `stylish-haskell` on the project to make the source look pretty.

You may occasionally want or need to delete the data stored locally. To do that delete `server/utopia-local.db` to remove the database, and `server/utopia-local/` to delete any assets and thumbnails.

## Environment variables

### Auth0

Once all of these environment variables are set Auth0 support will be enabled.

- `AUTH0_CLIENT_ID` - The client ID from the Auth0 console.
- `AUTH0_CLIENT_SECRET` - The client secret from the Auth0 console.
- `AUTH0_ENDPOINT` - The endpoint from the Auth0 console.
- `AUTH0_REDIRECT_URI` - The endpoint that Auth0 should redirect to on our side, likely `http://localhost:8000/authenticate` for local development.

### Database

Once this environment variable is set then Postgresql will take over from sqlite.

- `DATABASE_URL` - A url for the Postgresql database to use for data storage.

### S3

Once all of these environment variables are set then S3 will be used to store assets (like images from projects) instead of saving them on the local disk.

- `AWS_ACCESS_KEY_ID` - Access key for AWS to use with S3 when storing assets.
- `AWS_SECRET_ACCESS_KEY` - Secret key for AWS to use with S3 when storing assets.
- `AWS_BUCKET_NAME` - Bucket name in S3 to use when storing assets.

### Github

First of all, you will need to create an OAuth app in Github by going to ["Settings" -> "Developer settings" -> "OAuth Apps"](https://github.com/settings/applications/new) and selecting "New OAuth App" and setting the following fields:

- "Application name": Whatever name you want to see it referred to by.
- "Homepage URL": Just put `http://localhost:8000`, this is mostly for the UI that Github presents.
- "Authorization callback URL": Set this to `http://localhost:8000/v1/github/authentication/finish` for local development or whatever appropriate URL for deployment, this is the URL that Github redirects the page to when completing the authorization.

Then hit the "Register application" button to complete the setup. In the page that follows there will be a "Client ID" value and when you press the "Generate a new client secret" button also a client secret value,
make a note of these.

In your `.envrc` file if locally developing or wherever environment variables are set when deploying, set and export the following variables:

- `GITHUB_OAUTH_CLIENT_ID` - Set this to the client ID from the OAuth app just created.
- `GITHUB_OAUTH_CLIENT_SECRET` - Set this to match the client secret just generated.
- `GITHUB_OAUTH_REDIRECT_URL` - Set this to `http://localhost:8000/v1/github/authentication/finish` for local development or whatever appropriate URL for deployment.

E.g.:

```plain
use nix

export GITHUB_OAUTH_CLIENT_ID=
export GITHUB_OAUTH_CLIENT_SECRET=
export GITHUB_OAUTH_REDIRECT_URL=http://localhost:8000/v1/github/authentication/finish
```

Finally restart your environment, which if running locally might mean running something like `direnv allow` to permit the new settings to apply before rerunning `start-minimal` for instance to start your local environment.

### Liveblocks

You need a Liveblocks account to work on the collaboration features of Utopia. To use commenting, you also need to add the `Comments` add-on to your Liveblocks plan.
In the project dashboard for your given project, the "API keys" option from the side allows access to the secret key. To utilise this then add that to the environment in `.envrc` with a snippet like the following:

```
export LIVEBLOCKS_SECRET_KEY=yoursecretkey
```
