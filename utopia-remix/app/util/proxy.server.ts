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

const BASE_URL = ServerEnvironment.BACKEND_URL

function buildProxyUrl(url: URL, path: string | null): string {
  const { pathname, search } = url

  if (path != null) {
    return urljoin(BASE_URL, path)
  }

  return urljoin(BASE_URL, `${pathname}${search}`)
}

export async function proxy(req: Request, options?: { rawOutput?: boolean; path?: string }) {
  const url = buildProxyUrl(new URL(req.url), options?.path ?? null)

  let headers = new Headers()
  req.headers.forEach((value, key) => {
    // add headers to ignore here, with simple comparisons to make it faster than i.e. an array lookup
    // NOTE! this should match the check in `server.js`!
    if (key !== 'host') {
      headers.set(key, value)
    }
  })

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
