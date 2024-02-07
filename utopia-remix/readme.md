# utopia-remix

## Local setup

0. cd to `utopia-remix`, run `pnpm install`
1. Set up the `.env` file, there's a `.env.sample` file you can use as a blueprint.
2. Restart Utopia.
3. Requests to the backend APIs will now be proxied via the Remix BFF.
4. If you want to change that and have direct connections, update the value of `BACKEND_TYPE` inside `env-vars.ts`.
