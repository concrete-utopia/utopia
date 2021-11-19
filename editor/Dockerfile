# Install dependencies only when needed
FROM node:16-alpine AS deps
# Check https://github.com/nodejs/docker-node/tree/b4117f9333da4138b03a546ec926ef50a31506c3#nodealpine to understand why libc6-compat might be needed.
RUN apk add --no-cache libc6-compat git
WORKDIR /usr/src/app/
COPY . .

WORKDIR /usr/src/app/editor
COPY editor/package.json editor/pnpm-lock.yaml ./
RUN npm install -g pnpm
RUN pnpm install --unsafe-perm

WORKDIR /usr/src/app/website-next
RUN pnpm install --unsafe-perm

EXPOSE 8088
WORKDIR /usr/src/app/editor
CMD ["npm", "run", "production"]
