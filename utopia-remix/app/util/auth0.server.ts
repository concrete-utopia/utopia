import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'

export function auth0LoginURL(): string {
  const behaviour = 'auto-close'

  const useAuth0 =
    ServerEnvironment.Auth0Endpoint !== '' &&
    ServerEnvironment.Auth0ClientId !== '' &&
    ServerEnvironment.Auth0RedirectUri !== ''
  if (!useAuth0) {
    console.warn(
      'Auth0 is disabled, if you need it be sure to set the AUTH0_ENDPOINT, AUTH0_CLIENT_ID, AUTH0_REDIRECT_URI environment variables',
    )
    const url = new URL(urlJoin(ServerEnvironment.BackendURL, 'authenticate'))
    url.searchParams.set('code', 'logmein')
    url.searchParams.set('onto', behaviour)
    return url.href
  }

  const url = new URL(`https://${ServerEnvironment.Auth0Endpoint}/authorize`)
  url.searchParams.set('scope', 'openid profile email')
  url.searchParams.set('response_type', 'code')
  url.searchParams.set('client_id', ServerEnvironment.Auth0ClientId)

  const redirectURL = new URL(ServerEnvironment.Auth0RedirectUri)
  redirectURL.searchParams.set('onto', behaviour)

  url.searchParams.set('redirect_uri', redirectURL.href)
  return url.href
}
