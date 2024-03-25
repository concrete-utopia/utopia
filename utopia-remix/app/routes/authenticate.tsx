import { redirect, type LoaderFunctionArgs } from '@remix-run/node'
import * as cookie from 'cookie'
import moment from 'moment'
import { ServerEnvironment, isProductionOrStaging } from '../env.server'
import { createSession } from '../models/session.server'
import { ensure } from '../util/api.server'
import { Auth0TokenExpiration, getAuth0Grant } from '../util/auth0.server'
import { Status } from '../util/statusCodes'

export async function loader(args: LoaderFunctionArgs) {
  // 1. get the code from the url
  const url = new URL(args.request.url)
  const authCode = url.searchParams.get('code')
  ensure(authCode != null, 'missing auth code', Status.BAD_REQUEST)

  // 2. get the grant
  const grant = await getAuth0Grant(authCode)
  ensure(grant != null, 'invalid grant', Status.UNAUTHORIZED)

  // 3. create the new session
  const session = await createSession({ key: grant.key, userId: grant.userId })

  // 4. set cookie
  const token = cookie.serialize('JSESSIONID', session.key, {
    path: '/',
    httpOnly: true,
    sameSite: 'lax',
    secure: isProductionOrStaging(ServerEnvironment.environment),
    expires: moment().add(Auth0TokenExpiration.amount, Auth0TokenExpiration.unit).toDate(),
  })

  // 5. redirect and set cookie
  return redirect('/projects', {
    headers: {
      'Set-Cookie': token,
    },
  })
}
