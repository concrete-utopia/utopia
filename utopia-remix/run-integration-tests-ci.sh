#!/usr/bin/env bash

set -e

sleep 10

pnpm install

pnpm exec prisma generate
pnpm exec prisma db push

pnpm exec jest --runInBand --verbose

