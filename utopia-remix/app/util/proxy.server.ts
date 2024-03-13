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
  const url = buildProxyUrl(new URL(req.url), options?.path ?? null)

  const headers = new Headers()

  setCopyHeader(req.headers, headers, 'accept-encoding')
  setCopyHeader(req.headers, headers, 'connection')
  setCopyHeader(req.headers, headers, 'content-length')
  setCopyHeader(req.headers, headers, 'content-type')
  setCopyHeader(req.headers, headers, 'cookie')

  const response = await fetch(url, {
    credentials: 'include',
    method: req.method,
    headers: headers,
    mode: ProxyMode,

    body: req.body,
  })
  if (options?.rawOutput) {
    return response
  }
  return proxiedResponse(response)
}

function setCopyHeader(originalHeaders: Headers, targetHeaders: Headers, key: string) {
  const value = originalHeaders.get(key)
  if (value != null) {
    targetHeaders.set(key, value)
  }
}
