import { ActionFunctionArgs } from '@remix-run/node'
import { renameProject } from '../models/project.server'
import { ensure, handle, requireUser } from '../util/api.server'
import { slugify } from '../util/slugify'
import { Status } from '../util/statusCodes.server'
import { Params } from '@remix-run/react'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: handleRenameProject })
}

export async function handleRenameProject(req: Request, params: Params<string>) {
  ensure(req.method === 'POST', 'invalid method', Status.METHOD_NOT_ALLOWED)

  const user = await requireUser(req)

  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const formData = await req.formData()

  const title = formData.get('title')
  ensure(title != null, 'title is null', Status.BAD_REQUEST)
  ensure(typeof title === 'string', 'title is not a string', Status.BAD_REQUEST)

  const slug = slugify(title)
  ensure(slug.length > 0, 'title is too short', Status.BAD_REQUEST)

  await renameProject({ id: id, userId: user.user_id, title: slug })

  return {}
}
