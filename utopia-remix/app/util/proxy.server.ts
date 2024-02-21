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

  console.log(`proxying call to ${url}`)

  const headers = new Headers()

  setCopyHeader(req.headers, headers, 'accept-encoding')
  setCopyHeader(req.headers, headers, 'content-type')
  setCopyHeader(req.headers, headers, 'host')
  setCopyHeader(req.headers, headers, 'cookie')
  if (ServerEnvironment.environment === 'prod' || ServerEnvironment.environment === 'stage') {
    const proxiedURL = new URL(url)
    console.log(`setting proxied host to ${proxiedURL.host}`)
    headers.set('host', proxiedURL.host)
  }

  const requestInitWithoutBody: RequestInit = {
    credentials: 'include',
    method: req.method,
    headers: headers,
    mode: ProxyMode,
  }

  console.log(`proxied request data: ${JSON.stringify(requestInitWithoutBody)}`)
  const requestHeaders = getHeadersArray(headers)
  console.log(`request headers: ${JSON.stringify(requestHeaders)}`)

  const response = await fetch(url, {
    ...requestInitWithoutBody,
    body: req.body,
  })
  const responseHeaders = getHeadersArray(response.headers)
  console.log(`response headers: ${JSON.stringify(responseHeaders)}`)
  if (options?.rawOutput) {
    return response
  }
  return proxiedResponse(response)
}

function getHeadersArray(headers: Headers): string[] {
  const headersString: string[] = []
  for (const [key, value] of headers) {
    headersString.push(`${key}=${value}`)
  }
  return headersString
}

function setCopyHeader(originalHeaders: Headers, targetHeaders: Headers, key: string) {
  const value = originalHeaders.get(key)
  if (value != null) {
    targetHeaders.set('accept-encoding', value)
  }
}
