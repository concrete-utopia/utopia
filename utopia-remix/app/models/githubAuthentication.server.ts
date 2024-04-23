import type { GithubAuthentication } from 'prisma-client'
import { prisma } from '../db.server'

export function getGithubAuthentication(params: {
  userId: string
}): Promise<GithubAuthentication | null> {
  return prisma.githubAuthentication.findFirst({ where: { user_id: params.userId } })
}
