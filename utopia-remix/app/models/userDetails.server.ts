import { prisma } from '../db.server'

export async function getUserDetails(userId: string) {
  return prisma.userDetails.findFirst({ where: { user_id: userId } })
}

export async function getManyUserDetails(userIds: string[]) {
  return prisma.userDetails.findMany({ where: { user_id: { in: userIds } } })
}
