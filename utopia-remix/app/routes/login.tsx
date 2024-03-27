import { redirect, type LoaderFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../handlers/validators'
import { handle } from '../util/api.server'
import type { Params } from '@remix-run/react'
import { auth0LoginURL } from '../util/auth0.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: ALLOW,
      handler: handleLogin,
    },
  })
}

export async function handleLogin(req: Request, params: Params<string>) {
  const url = new URL(req.url)
  const redirectTo = url.searchParams.get('redirectTo')
  const fakeUser = url.searchParams.get('fakeUser')
  const auth0Login = auth0LoginURL({
    redirectTo: redirectTo,
    fakeUser: fakeUser,
  })
  return redirect(auth0Login)
}
