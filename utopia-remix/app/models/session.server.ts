import { PersistentSession, UserDetails } from 'prisma-client'
import { prisma } from '../db.server'
import { ensure } from '../util/api.server'
import { Status } from '../util/statusCodes'

export async function getSession(params: { key: string }): Promise<PersistentSession | null> {
  return await prisma.persistentSession.findFirst({
    where: { key: params.key },
  })
}

type SessionJSONData = {
  userID: string
}

function isSessionJSONData(v: unknown): v is SessionJSONData {
  return typeof v === 'object' && (v as SessionJSONData).userID != null
}

export async function getUserFromSession(params: { key: string }): Promise<UserDetails | null> {
  const session = await getSession({ key: params.key })
  ensure(session != null, 'session not found', Status.UNAUTHORIZED)
  ensure(isSessionJSONData(session.session_json), 'invalid session', Status.UNAUTHORIZED)

  return prisma.userDetails.findFirst({
    where: { user_id: session.session_json.userID },
  })
}
