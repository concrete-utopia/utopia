import urljoin from 'url-join'
import { ServerEnvironment } from '../env.server'
import { proxiedResponse } from './api.server'
import dns from 'dns'

if (ServerEnvironment.environment === 'local') {
  // this is a workaround for default DNS resolution order with Node > 17 (where ipv6 comes first)
  // https://github.com/node-fetch/node-fetch/issues/1624#issuecomment-1235826631
  dns.setDefaultResultOrder('ipv4first')
}

const BASE_URL = ServerEnvironment.BackendURL

function buildProxyUrl(url: URL, path: string | null): string {
  const { pathname, search } = url

  if (path != null) {
    return urljoin(BASE_URL, path)
  }

  return urljoin(BASE_URL, `${pathname}${search}`)
}

export async function proxy(req: Request, options?: { rawOutput?: boolean; path?: string }) {
  const url = buildProxyUrl(new URL(req.url), options?.path ?? null)

  console.log(`proxying call to ${url}`)

  const response = await fetch(url, {
    credentials: 'include',
    method: req.method,
    body: req.body,
    headers: req.headers,
  })
  if (options?.rawOutput) {
    return response
  }
  return proxiedResponse(response)
}
