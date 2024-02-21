import { ActionFunctionArgs } from '@remix-run/node'
import { Params } from '@remix-run/react'
import { ensure, handle, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes.server'
import { getProjectAccess, setProjectAccess } from '../models/projectAccess.server'
import { asNumber } from '../util/common'
import { AccessLevel } from '../types'

export async function action(args: ActionFunctionArgs) {
  return handle(args, { POST: handleChangeProjectAccess })
}

export async function handleChangeProjectAccess(req: Request, params: Params<string>) {
  // const user = await requireUser(req)
  const { id } = params
  const formData = await req.formData()
  ensure(id != null, 'id is null', Status.BAD_REQUEST)
  const accessLevel = formData.get('accessLevel')
  const accessLevelNumber = asNumber(accessLevel)
  ensure(!isNaN(accessLevelNumber), 'accessLevel is not a number', Status.BAD_REQUEST)
  ensure(
    AccessLevel[accessLevelNumber] != null,
    'accessLevel is not a valid AccessLevel',
    Status.BAD_REQUEST,
  )

  try {
    setProjectAccess({ projectId: id, accessLevel: accessLevelNumber })
  } catch (e) {
    console.error('error', e)
    return new Response('error', {
      status: Status.INTERNAL_ERROR,
    })
  }

  return {}
}

export async function getProjectAccessLevel(req: Request, params: Params<string>) {
  const { id } = params
  ensure(id != null, 'id is null', Status.BAD_REQUEST)
  const accessLevel = await getProjectAccess({ projectId: id })
  return new Response(JSON.stringify({ accessLevel }), {
    headers: {
      'content-type': 'application/json',
    },
  })
}
