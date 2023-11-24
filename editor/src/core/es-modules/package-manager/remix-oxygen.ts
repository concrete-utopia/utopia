import { type AppLoadContext } from '@remix-run/server-runtime'

export function createRequestHandler<Context = unknown>({
  getLoadContext,
}: {
  getLoadContext?: (request: Request) => Promise<Context> | Context
}) {
  return async (request: Request) => {
    const context =
      getLoadContext != null ? ((await getLoadContext(request)) as AppLoadContext) : undefined

    const response = new Response()

    return {
      ...response,
      context: context,
    }
  }
}
