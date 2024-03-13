import type { ActionFunctionArgs } from '@remix-run/node'
import type { Params } from '@remix-run/react'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes'
import { setProjectAccess } from '../models/projectAccess.server'
import { asNumber } from '../util/common'
import { AccessLevel, UserProjectPermission, asAccessLevel } from '../types'
import { validateProjectAccess } from '../handlers/validators'

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      handler: handleChangeProjectAccess,
      validator: validateProjectAccess(UserProjectPermission.CAN_MANAGE_PROJECT, {
        getProjectId: (params) => params.id,
      }),
    },
  })
}

export async function handleChangeProjectAccess(req: Request, params: Params<string>) {
  await requireUser(req)
  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)

  const formData = await req.formData()
  const accessLevelStr = formData.get('accessLevel')
  const accessLevelNumber = asNumber(accessLevelStr)
  ensure(!isNaN(accessLevelNumber), 'accessLevel is not a number', Status.BAD_REQUEST)
  const accessLevel = asAccessLevel(accessLevelNumber)
  ensure(
    accessLevel != null && Object.values(AccessLevel).includes(accessLevel),
    'accessLevel is not a valid AccessLevel',
    Status.BAD_REQUEST,
  )
  await setProjectAccess({ projectId: id, accessLevel: accessLevel })

  return {}
}
