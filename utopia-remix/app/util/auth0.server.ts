import { ServerEnvironment } from '../env.server'

export function auth0LoginURL(): string {
  const url = new URL(`https://${ServerEnvironment.AUTH0_ENDPOINT}/authorize`)
  url.searchParams.set('scope', 'openid profile email')
  url.searchParams.set('response_type', 'code')
  url.searchParams.set('client_id', ServerEnvironment.AUTH0_CLIENT_ID)

  const redirectURL = new URL(ServerEnvironment.AUTH0_REDIRECT_URI)
  redirectURL.searchParams.set('onto', 'auto-close')

  url.searchParams.set('redirect_uri', redirectURL.href)
  return url.href
}
