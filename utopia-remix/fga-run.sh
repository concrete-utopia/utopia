#!/usr/bin/env bash

set -e

HOST_ENV_KEY="FGA_API_HOST"
STORE_ENV_KEY="FGA_STORE_ID"

# Only run this if the .env file uses the local FGA store
if ! grep -qE "^[^#]*\b${HOST_ENV_KEY}\b=['\"]?localhost" .env; then
	echo "The FGA_API_HOST is either not defined or not set to localhost, so I won't do anything."
	exit
fi

if command docker &> /dev/null; then
	RUNTIME=docker
elif command podman &> /dev/null; then
	RUNTIME=podman
else
	echo "The docker or podman commands are not found."
	exit
fi

${RUNTIME} compose -f docker-compose.fga.yml up
