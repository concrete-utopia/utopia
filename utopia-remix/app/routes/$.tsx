import type { LoaderFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../handlers/validators'
import { proxy } from '../util/proxy.server'
import { handle } from '../util/api.server'
import { getProxyAssetPath } from '../util/assets.server'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: ALLOW,
      handler: handleLoadAssetFromRoot,
    },
  })
}

/**
 * Some editor routes can reference assets as if they are on the root path.
 * This function will look at the incoming request and if the requested resource
 * has an allowed extension it will try to derive the project ID
 * from the request's referer header.
 * If a match is found, and the project can be accessed, the request will be proxied to
 * /p/<id>/<resource>.
 */
async function handleLoadAssetFromRoot(req: Request) {
  const proxyProjectAssetPath = await getProxyAssetPath(req)
  if (proxyProjectAssetPath == null) {
    return {}
  }

  return proxy(req, {
    path: proxyProjectAssetPath,
    rawOutput: true,
  })
}
