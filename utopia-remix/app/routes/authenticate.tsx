import { redirect, type LoaderFunctionArgs } from '@remix-run/node'
import { AuthenticationClient, UserInfoClient } from 'auth0'
import { ServerEnvironment } from '../env.server'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { createSession } from '../models/session.server'
import * as cookie from 'cookie'
import moment from 'moment'
import { singleton } from '../singleton.server'

const userClient = singleton(
  'auth0UserClient',
  () =>
    new UserInfoClient({
      domain: ServerEnvironment.AUTH0_ENDPOINT,
    }),
)

const authClient = singleton(
  'auth0AuthClient',
  () =>
    new AuthenticationClient({
      domain: ServerEnvironment.AUTH0_ENDPOINT,
      clientId: ServerEnvironment.AUTH0_CLIENT_ID,
      clientSecret: ServerEnvironment.AUTH0_CLIENT_SECRET,
    }),
)

export async function loader(args: LoaderFunctionArgs) {
  // ! TODO: Refresh token

  // 1. get the code from the url
  const url = new URL(args.request.url)
  const authCode = url.searchParams.get('code')
  ensure(authCode != null, 'missing auth code', Status.BAD_REQUEST)

  // 2. get the token from auth0
  const grant = await authClient.oauth.authorizationCodeGrant({
    client_id: ServerEnvironment.AUTH0_CLIENT_ID,
    client_secret: ServerEnvironment.AUTH0_CLIENT_SECRET,
    code: authCode,
    redirect_uri: ServerEnvironment.AUTH0_REDIRECT_URI,
  })
  const accessToken = grant.data.access_token

  // 3. grab the user info for the token
  const userInfo = await userClient.getUserInfo(accessToken)

  // 4. create the new session
  const session = await createSession({ key: accessToken, userId: userInfo.data.sub })

  // 5. set cookie
  const token = cookie.serialize('JSESSIONID', session.key, {
    path: '/',
    httpOnly: true,
    secure: ServerEnvironment.environment === 'prod' || ServerEnvironment.environment === 'stage',
    expires: moment().add(1, 'month').toDate(),
    sameSite: 'lax',
  })

  // 6. redirect and set cookie
  return redirect('/projects', {
    headers: {
      'Set-Cookie': token,
    },
  })
}
