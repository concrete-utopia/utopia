#!/usr/bin/env bash

## Please run this script from shell.nix with `watch-remix` or provide the
## PNPM= environment variable with the pnpm executable path.

set -e

if [ "${LOCAL}" == 'true' ] ; then
	# Make sure the .env file exists
	touch .env

	HOST_ENV_KEY="FGA_API_HOST"
	STORE_ENV_KEY="FGA_STORE_ID"

	if grep -qE "^[^#]*\b${HOST_ENV_KEY}\b=['\"]?localhost" .env; then
		if ! grep -qE "^[^#]*\b${STORE_ENV_KEY}\b=['\"]?.+" .env; then
			echo "* FGA is set to run locally, but no FGA_STORE_ID is set. Creating it…"
			./fga-create-store.sh
		fi
	fi
fi

${PNPM} run dev
