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
