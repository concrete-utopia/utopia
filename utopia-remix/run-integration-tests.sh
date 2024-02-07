#!/usr/bin/env bash

set -e

docker compose down -v
docker compose up -d

sleep 5s

export DATABASE_URL='postgres://postgres:password@localhost:54322/postgres?sslmode=disable'

pnpm exec prisma db push
jest --runInBand --verbose

docker compose down -v
