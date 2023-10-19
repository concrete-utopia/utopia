import { type AppLoadContext } from '@remix-run/server-runtime'

export function createRequestHandler<Context = unknown>({
  getLoadContext,
}: {
  getLoadContext?: (request: Request) => Promise<Context> | Context
}) {
  return async (request: Request) => {
    const context =
      getLoadContext != null ? ((await getLoadContext(request)) as AppLoadContext) : undefined

    // This would maybe have to encode the context in the body and return a real Response
    return {
      status: 200,
      context: context,
    }
  }
}
