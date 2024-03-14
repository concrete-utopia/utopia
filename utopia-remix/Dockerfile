####
# 1. deps: pnpm install + prisma generate
# 2. build: pnpm build
# 3. [entrypoint]: pnpm start
####

#---------- DEPS
FROM node:18.12.1-bullseye-slim as deps
RUN corepack enable
WORKDIR /utopia-remix

COPY package.json package.json
RUN pnpm install

COPY schema.prisma schema.prisma
RUN pnpm exec prisma generate

#---------- BUILD
FROM deps as build
COPY . .
COPY --from=deps /utopia-remix/node_modules /utopia-remix/node_modules

RUN pnpm build

#---------- ENTRYPOINT
FROM build as entrypoint

COPY --from=build /utopia-remix/build /utopia-remix/build
COPY --from=build /utopia-remix/public /utopia-remix/public

ARG PORT

ARG APP_ENV
ARG BACKEND_URL
ARG CORS_ORIGIN
ARG DATABASE_URL
ARG LIVEBLOCKS_SECRET_KEY
ARG EDITOR_URL

CMD ["pnpm", "start"]
