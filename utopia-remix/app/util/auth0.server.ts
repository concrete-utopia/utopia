import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'

export function auth0LoginURL(): string {
  const behaviour: 'auto-close' | 'authd-redirect' = 'authd-redirect'

  const useAuth0 =
    ServerEnvironment.AUTH0_ENDPOINT !== '' &&
    ServerEnvironment.AUTH0_CLIENT_ID !== '' &&
    ServerEnvironment.AUTH0_REDIRECT_URI !== ''
  if (!useAuth0) {
    console.warn(
      'Auth0 is disabled, if you need it be sure to set the AUTH0_ENDPOINT, AUTH0_CLIENT_ID, AUTH0_REDIRECT_URI environment variables',
    )
    const url = new URL(urlJoin(ServerEnvironment.BACKEND_URL, 'authenticate'))
    url.searchParams.set('code', 'logmein')
    url.searchParams.set('onto', behaviour)
    return url.href
  }

  const url = new URL(`https://${ServerEnvironment.AUTH0_ENDPOINT}/authorize`)
  url.searchParams.set('scope', 'openid profile email')
  url.searchParams.set('response_type', 'code')
  url.searchParams.set('client_id', ServerEnvironment.AUTH0_CLIENT_ID)

  const redirectURL = new URL(ServerEnvironment.AUTH0_REDIRECT_URI)
  redirectURL.searchParams.set('onto', behaviour)

  url.searchParams.set('redirect_uri', redirectURL.href)
  return url.href
}
