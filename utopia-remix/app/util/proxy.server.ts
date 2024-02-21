import urljoin from 'url-join'
import { ServerEnvironment } from '../env.server'
import { proxiedResponse } from './api.server'
import dns from 'dns'

const ProxyMode = ServerEnvironment.environment !== 'local' ? 'same-origin' : undefined

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
  const originalURL = new URL(req.url)
  const url = buildProxyUrl(originalURL, options?.path ?? null)

  console.log(`proxying call to ${url}`)

  const headers = new Headers()
  for (const [key, value] of req.headers) {
    headers.set(key, value)
  }
  if (ServerEnvironment.environment === 'prod' || ServerEnvironment.environment === 'stage') {
    console.log(`setting proxied host to ${originalURL.host}`)
    headers.set('host', originalURL.host)
  }

  const requestInitWithoutBody: RequestInit = {
    credentials: 'include',
    method: req.method,
    headers: headers,
    mode: ProxyMode,
  }
  console.log(`proxied request data: ${JSON.stringify(requestInitWithoutBody)}`)

  const response = await fetch(url, {
    ...requestInitWithoutBody,
    body: req.body,
  })
  if (options?.rawOutput) {
    return response
  }
  return proxiedResponse(response)
}
