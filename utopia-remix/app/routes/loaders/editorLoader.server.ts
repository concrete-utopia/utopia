import type { LoaderFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../../handlers/validators'
import { handle } from '../../util/api.server'
import { proxy } from '../../util/proxy.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      handler: getProjectForEditor,
      validator: ALLOW,
    },
  })
}

export async function getProjectForEditor(
  req: Request,
  options?: { rawOutput?: boolean; path?: string },
) {
  const proxyResponse: Response = (await proxy(req, {
    rawOutput: true,
    path: options?.path,
  })) as Response
  const body = await proxyResponse.text()
  return new Response(body, {
    headers: { 'content-type': 'text/html' },
    status: proxyResponse.status,
  })
}
