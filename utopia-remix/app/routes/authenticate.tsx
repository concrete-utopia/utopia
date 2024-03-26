import { redirect, type LoaderFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../handlers/validators'
import { handle } from '../util/api.server'
import { proxy } from '../util/proxy.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: ALLOW,
      handler: handleAuthenticate,
    },
  })
}

async function handleAuthenticate(req: Request) {
  const url = new URL(req.url)
  const autoClose = url.searchParams.get('onto') === 'auto-close'
  const redirectTo = url.searchParams.get('redirectTo')
  const resp = await proxy(req, { rawOutput: true })

  if (resp instanceof Response && resp.ok) {
    return autoClose
      ? // auto-close the window when logins come from the editor
        authFromEditor(resp)
      : // redirect to projects
        authFromRemix(resp, redirectTo)
  }

  return resp
}

function authFromEditor(resp: Response) {
  return new Response(resp.body, {
    headers: {
      'content-type': 'text/html',
      'cache-control': 'no-cache',
      'set-cookie': resp.headers.get('set-cookie') ?? '',
    },
    status: resp.status,
  })
}

function authFromRemix(resp: Response, redirectTo: string | null) {
  return redirect(redirectTo ?? '/projects', { headers: resp.headers })
}
