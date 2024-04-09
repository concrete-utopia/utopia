import type { LoaderFunctionArgs } from '@remix-run/node'
import { validateProjectAccess } from '../handlers/validators'
import { proxy } from '../util/proxy.server'
import { UserProjectPermission } from '~/types'
import { redirect } from '@remix-run/react'
import {
  getProjectIdFromParams,
  getResponseWithValidation,
  requireUserOrRedirectToLogin,
} from '../util/api.server'

const validator = validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
  canRequestAccess: true,
  getProjectId: (params) => getProjectIdFromParams(params, 'id'),
})
// due to Remix's current issue with gzip responses (https://github.com/remix-run/remix/issues/6697),
// we need to remove this header from the response
const excludeHeaders = new Set(['content-encoding'])

export async function loader(args: LoaderFunctionArgs) {
  await requireUserOrRedirectToLogin(args.request)

  try {
    return await getResponseWithValidation(
      args.request,
      args.params,
      (req: Request) => proxy(req, { rawOutput: true }),
      { validator: validator, excludeHeaders: excludeHeaders },
    )
  } catch (e) {
    const url = new URL(args.request.url)
    return redirect(`/project/${args.params.id}${url.search}`, {
      headers: { 'cache-control': 'no-cache' },
    })
  }
}
