import type { LoaderFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../handlers/validators'
import { proxy } from '../util/proxy.server'
import { getResponseWithValidation, requireUserOrRedirectToLogin } from '../util/api.server'

// due to Remix's current issue with gzip responses (https://github.com/remix-run/remix/issues/6697),
// we need to remove this header from the response
const excludeHeaders = new Set(['content-encoding'])

export async function loggedInRequest(args: LoaderFunctionArgs) {
  await requireUserOrRedirectToLogin(args.request)

  return await getResponseWithValidation(
    args.request,
    args.params,
    (req: Request) => proxy(req, { rawOutput: true }),
    { validator: ALLOW, excludeHeaders: excludeHeaders },
  )
}
