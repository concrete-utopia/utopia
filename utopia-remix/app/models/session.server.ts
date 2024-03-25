import type { PersistentSession, UserDetails } from 'prisma-client'
import { prisma } from '../db.server'

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

export async function maybeGetUserFromSession(params: {
  key: string
}): Promise<UserDetails | null> {
  const session = await getSession({ key: params.key })
  if (session == null || !isSessionJSONData(session.session_json)) {
    return null
  }

  return prisma.userDetails.findFirst({
    where: { user_id: session.session_json.userID },
  })
}
