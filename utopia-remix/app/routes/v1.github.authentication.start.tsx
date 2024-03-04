import { LoaderFunctionArgs, redirect } from '@remix-run/node'
import { handle, handleOptions } from '../util/api.server'
import { ServerEnvironment } from '../env.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
    GET: redirectToGithubAuthStart,
  })
}

async function redirectToGithubAuthStart() {
  const scopes = 'repo'

  return redirect(
    `https://github.com/login/oauth/authorize?client_id=${ServerEnvironment.GITHUB_OAUTH_CLIENT_ID}&redirect_uri=${ServerEnvironment.GITHUB_OAUTH_REDIRECT_URL}&scope=${scopes}`,
  )
}
