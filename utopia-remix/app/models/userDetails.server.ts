import type { UserDetails } from 'prisma-client'
import { prisma } from '../db.server'
import { ServerEnvironment, isProductionOrStaging } from '../env.server'
import { ApiError } from '../util/errors'
import { Status } from '../util/statusCodes'

export async function getUserDetails(userId: string) {
  return prisma.userDetails.findFirst({ where: { user_id: userId } })
}

export async function getManyUserDetails(userIds: string[]) {
  return prisma.userDetails.findMany({ where: { user_id: { in: userIds } } })
}

export async function getOrCreateDummyUser(): Promise<UserDetails> {
  if (isProductionOrStaging(ServerEnvironment.environment)) {
    throw new ApiError('dummy users are not allowed', Status.INTERNAL_ERROR)
  }

  return prisma.userDetails.upsert({
    where: { user_id: '1' },
    create: {
      user_id: '1',
      email: 'team@utopia.app',
      name: 'Utopian Worker #296',
      picture: 'http://localhost:8000/editor/avatars/utopino3.png',
    },
    update: {},
  })
}
