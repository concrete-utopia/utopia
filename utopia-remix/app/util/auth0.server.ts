import urlJoin from 'url-join'
import { ServerEnvironment } from '../env.server'
import { UserInfoClient, AuthenticationClient } from 'auth0'
import { singleton } from '../singleton.server'
import { getOrCreateDummyUser } from '../models/userDetails.server'
import { v4 } from 'uuid'

export const Auth0TokenExpiration: {
  amount: number
  unit: moment.unitOfTime.Base
} = { amount: 1, unit: 'month' }

export const auth0UserClient = singleton(
  'auth0UserClient',
  () =>
    new UserInfoClient({
      domain: ServerEnvironment.AUTH0_ENDPOINT,
    }),
)

export const auth0AuthClient = singleton(
  'auth0AuthClient',
  () =>
    new AuthenticationClient({
      domain: ServerEnvironment.AUTH0_ENDPOINT,
      clientId: ServerEnvironment.AUTH0_CLIENT_ID,
      clientSecret: ServerEnvironment.AUTH0_CLIENT_SECRET,
    }),
)

export function auth0Enabled(): boolean {
  return (
    ServerEnvironment.AUTH0_ENDPOINT !== '' &&
    ServerEnvironment.AUTH0_CLIENT_ID !== '' &&
    ServerEnvironment.AUTH0_REDIRECT_URI !== ''
  )
}

export function auth0LoginURL(): string {
  if (!auth0Enabled()) {
    console.warn(
      'Auth0 is disabled, if you need it be sure to set the AUTH0_ENDPOINT, AUTH0_CLIENT_ID, AUTH0_REDIRECT_URI environment variables',
    )
    const url = new URL(urlJoin(ServerEnvironment.CORS_ORIGIN, '/authenticate'))
    url.searchParams.set('code', 'logmein')
    return url.href
  }

  const url = new URL(`https://${ServerEnvironment.AUTH0_ENDPOINT}/authorize`)
  url.searchParams.set('scope', 'openid profile email')
  url.searchParams.set('response_type', 'code')
  url.searchParams.set('client_id', ServerEnvironment.AUTH0_CLIENT_ID)

  const redirectURL = new URL(ServerEnvironment.AUTH0_REDIRECT_URI)
  url.searchParams.set('redirect_uri', redirectURL.href)

  return url.href
}

export async function getAuth0Grant(
  authCode: string,
): Promise<{ key: string; userId: string } | null> {
  if (!auth0Enabled()) {
    const dummyUser = await getOrCreateDummyUser()
    return authCode === 'logmein' ? { key: v4(), userId: dummyUser.user_id } : null
  }

  // get the token from auth0
  const grant = await auth0AuthClient.oauth.authorizationCodeGrant({
    client_id: ServerEnvironment.AUTH0_CLIENT_ID,
    client_secret: ServerEnvironment.AUTH0_CLIENT_SECRET,
    code: authCode,
    redirect_uri: ServerEnvironment.AUTH0_REDIRECT_URI,
  })
  const accessToken = grant.data.access_token

  // grab the user info for the token
  const userInfo = await auth0UserClient.getUserInfo(accessToken)

  return { key: v4(), userId: userInfo.data.sub }
}
