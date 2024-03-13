import type { ActionFunctionArgs } from '@remix-run/node'
import { renameProject } from '../models/project.server'
import { ensure, handle, requireUser } from '../util/api.server'
import slugify from 'slugify'
import { Status } from '../util/statusCodes'
import type { Params } from '@remix-run/react'
import { validateProjectAccess } from '../handlers/validators'
import { UserProjectPermission } from '../types'

export const SLUGIFY_OPTIONS = { lower: true, remove: /[^a-z0-9A-Z ]/ }

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleRenameProject,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleRenameProject(req: Request, params: Params<string>) {
  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const formData = await req.formData()

  const title = formData.get('title')
  ensure(title != null, 'title is null', Status.BAD_REQUEST)
  ensure(typeof title === 'string', 'title is not a string', Status.BAD_REQUEST)

  const slug = slugify(title, SLUGIFY_OPTIONS)
  ensure(slug.length > 0, 'title is too short', Status.BAD_REQUEST)

  await renameProject({ id: id, userId: user.user_id, title: slug })

  return {}
}
