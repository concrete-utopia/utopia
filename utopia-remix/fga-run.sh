#!/usr/bin/env bash

# Start the FGA local server with either Docker or Podman.

set -e

HOST_ENV_KEY="FGA_API_HOST"
STORE_ENV_KEY="FGA_STORE_ID"

# Only run this if the .env file uses the local FGA store
if ! grep -qE "^[^#]*\b${HOST_ENV_KEY}\b=['\"]?localhost" .env; then
	echo "The FGA_API_HOST is either not defined or not set to localhost, so I won't do anything."
	echo "If you want to enable the local FGA server, please add FGA_API_HOST=localhost:8003 to your environment variables."
	exit
fi

if type docker &> /dev/null; then
	RUNTIME=docker
elif type podman &> /dev/null; then
	RUNTIME=podman
else
	echo "The docker or podman commands are not found."
	exit
fi

${RUNTIME} compose -f docker-compose.fga.yml up
