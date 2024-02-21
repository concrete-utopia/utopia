#!/usr/bin/env bash

set -e

docker compose down -v
docker compose up -d

until docker exec utopia-remix-db-test pg_isready ; do sleep 1 ; done

export APP_ENV='test'
export DATABASE_URL='postgres://postgres:password@localhost:54322/postgres?sslmode=disable'
export CORS_ORIGIN='*'
export BACKEND_URL=''
export REACT_APP_EDITOR_URL=''
export LIVEBLOCKS_SECRET_KEY='secret'

pnpm exec prisma db push
jest --runInBand --verbose

docker compose down -v
