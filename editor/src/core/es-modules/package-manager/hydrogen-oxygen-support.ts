import { type AppLoadContext } from '@remix-run/server-runtime'
import { type MapLike } from 'typescript'
import { getProjectFileByFilePath, type ProjectContentTreeRoot } from '../../../components/assets'
import { type CurriedUtopiaRequireFn } from '../../../components/custom-code/code-file'

// remix-oxygen hack to create and return the context
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

export type CustomServerJSExecutor = (innerProjectContents: ProjectContentTreeRoot) => MapLike<any>

export function getCustomServerJSExecutor(
  projectContents: ProjectContentTreeRoot,
  curriedRequireFn: CurriedUtopiaRequireFn,
): CustomServerJSExecutor | null {
  const serverFileName = '/server.js'
  const hasCustomServer = getProjectFileByFilePath(projectContents, serverFileName) != null
  if (!hasCustomServer) {
    return null
  }

  return (innerProjectContents: ProjectContentTreeRoot) => {
    return curriedRequireFn(innerProjectContents)('.', serverFileName, false)
  }
}

export async function patchServerJSContextIntoArgs(
  customServerJSExecutor: CustomServerJSExecutor | null,
  projectContents: ProjectContentTreeRoot,
  args: any,
): Promise<any> {
  if (customServerJSExecutor == null) {
    return args
  }

  const customServerJSScope = customServerJSExecutor(projectContents)

  if (customServerJSScope.default?.fetch == null) {
    return { context: {} }
  }

  const { context: serverJSContext } = await customServerJSScope.default.fetch(
    args.request,
    {
      SESSION_SECRET: 'foobar',
      PUBLIC_STORE_DOMAIN: 'mock.shop',
    },
    {
      waitUntil: () => {},
    },
  )

  return {
    ...args,
    context: {
      ...args.context,
      ...serverJSContext,
    },
  }
}
