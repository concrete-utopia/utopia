FROM node:16-alpine AS deps
RUN apk add --no-cache libc6-compat git
WORKDIR /usr/src/app/
COPY . .
WORKDIR /usr/src/app/website-next
COPY website-next/package.json website-next/pnpm-lock.yaml ./
RUN npm install -g pnpm
RUN pnpm install --unsafe-perm


CMD ["pnpm", "run", "dev"]
