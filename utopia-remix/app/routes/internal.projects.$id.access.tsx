import { ActionFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { setProjectAccess } from '../models/projectAccess.server'
import { asNumber } from '../util/common'
import { AccessLevel } from '../types'
import { getProject } from '~/models/project.server'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: handleChangeProjectAccess })
}

export async function handleChangeProjectAccess(req: Request, params: Params<string>) {
  const user = await requireUser(req)
  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const project = await getProject({ id: id, owner_id: user.user_id })
  ensure(project != null, 'project not found', Status.NOT_FOUND)

  const formData = await req.formData()
  const accessLevel = formData.get('accessLevel')
  const accessLevelNumber = asNumber(accessLevel) as AccessLevel
  ensure(!isNaN(accessLevelNumber), 'accessLevel is not a number', Status.BAD_REQUEST)
  ensure(
    Object.values(AccessLevel).includes(accessLevelNumber),
    'accessLevel is not a valid AccessLevel',
    Status.BAD_REQUEST,
  )
  await setProjectAccess({ projectId: id, accessLevel: accessLevelNumber })

  return {}
}
