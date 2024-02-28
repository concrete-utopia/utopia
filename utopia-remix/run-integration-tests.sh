#!/usr/bin/env bash

set -e

podman compose down -v
podman compose up -d

until podman exec utopia-remix-db-test pg_isready ; do sleep 1 ; done

export APP_ENV='test'
export DATABASE_URL='postgres://postgres:password@localhost:54322/postgres?sslmode=disable'
export CORS_ORIGIN='*'
export BACKEND_URL=''
export REACT_APP_EDITOR_URL=''

pnpm exec prisma db push
jest --runInBand --verbose

podman compose down -v
