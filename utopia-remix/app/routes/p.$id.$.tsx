import type { LoaderFunctionArgs } from '@remix-run/node'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'
import { handle } from '../util/api.server'
import { handleSplatLoad } from '../handlers/splatLoad'

/**
 * This is the splat route for project assets served from the editor
 * that don't go through the `/assets/` path, for example
 * Remix assets served from the `public` folder.
 *
 * Only links to static files with the allowed extensions will be allowed
 * to be loaded.
 */

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    GET: {
      validator: validateProjectAccess(UserProjectPermission.CAN_VIEW_PROJECT, {
        getProjectId: (params) => params.id?.split('-')[0] ?? null,
      }),
      handler: handleSplatLoad,
    },
  })
}
