import { LoaderFunctionArgs, redirect } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { ServerEnvironment } from '../env.server'
import { ALLOW } from '../handlers/validators'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: { handler: redirectToGithubAuthStart, validator: ALLOW },
  })
}

async function redirectToGithubAuthStart() {
  const scopes = 'repo'

  return redirect(
    `https://github.com/login/oauth/authorize?client_id=${ServerEnvironment.GithubOAuthClientId}&redirect_uri=${ServerEnvironment.GithubOAuthRedirectUrl}&scope=${scopes}`,
  )
}
