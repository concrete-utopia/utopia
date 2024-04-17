import type { LoaderFunctionArgs } from '@remix-run/node'
import { proxy } from '../util/proxy.server'

export async function loader({ request }: LoaderFunctionArgs) {
  // Call the old server to check if it's online.
  const url = new URL(request.url)
  url.pathname = '/online-status'
  const onlineStatusRequest = new Request(url, {
    method: 'GET',
  })
  await proxy(onlineStatusRequest, { rawOutput: true })

  // Return a simple response.
  return new Response('Online', {
    headers: { 'content-type': 'text/plain', 'cache-control': 'no-cache' },
  })
}
