import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'

export function auth0LoginURL({
  redirectTo,
  fakeUser,
}: { redirectTo?: string | null; fakeUser?: string | null } = {}): string {
  const behaviour: 'auto-close' | 'authd-redirect' = 'authd-redirect'

  if (fakeUser != null) {
    const url = authenticateUrl()
    console.info('Authenticating with fake user:', fakeUser, url.toString())
    url.searchParams.set('code', fakeUser)
    url.searchParams.set('onto', behaviour)
    if (redirectTo != null) {
      url.searchParams.set('redirectTo', redirectTo)
    }
    return url.href
  }

  const useAuth0 =
    ServerEnvironment.AUTH0_ENDPOINT !== '' &&
    ServerEnvironment.AUTH0_CLIENT_ID !== '' &&
    ServerEnvironment.AUTH0_REDIRECT_URI !== ''
  if (!useAuth0) {
    console.warn(
      'Auth0 is disabled, if you need it be sure to set the AUTH0_ENDPOINT, AUTH0_CLIENT_ID, AUTH0_REDIRECT_URI environment variables',
    )
    const url = authenticateUrl()
    url.searchParams.set('code', 'logmein')
    url.searchParams.set('onto', behaviour)
    if (redirectTo != null) {
      url.searchParams.set('redirectTo', redirectTo)
    }
    return url.href
  }

  const url = new URL(`https://${ServerEnvironment.AUTH0_ENDPOINT}/authorize`)
  url.searchParams.set('scope', 'openid profile email')
  url.searchParams.set('response_type', 'code')
  url.searchParams.set('client_id', ServerEnvironment.AUTH0_CLIENT_ID)

  const redirectURL = new URL(ServerEnvironment.AUTH0_REDIRECT_URI)
  redirectURL.searchParams.set('onto', behaviour)
  if (redirectTo != null) {
    redirectURL.searchParams.set('redirectTo', redirectTo)
  }

  url.searchParams.set('redirect_uri', redirectURL.href)
  return url.href
}

export function authenticateUrl(): URL {
  return new URL(
    ServerEnvironment.AUTH0_REDIRECT_URI || urlJoin(ServerEnvironment.CORS_ORIGIN, 'authenticate'),
  )
}
