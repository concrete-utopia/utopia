import type { LoaderFunctionArgs } from '@remix-run/node'
import { ensure, handle, handleOptions } from '../util/api.server'
import { proxy } from '../util/proxy.server'
import { ALLOW } from '../handlers/validators'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: handleFinish, validator: ALLOW },
  })
}

async function handleFinish(req: Request) {
  const proxyResponse = await proxy(req, { rawOutput: true })
  ensure(proxyResponse instanceof Response, 'invalid response', Status.INTERNAL_ERROR)
  ensure(proxyResponse.ok, proxyResponse.statusText, proxyResponse.status)

  const body = await proxyResponse.text()
  return new Response(body, {
    headers: { 'content-type': 'text/html' },
    status: 200,
  })
}
