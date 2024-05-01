import { prisma } from '../db.server'
import { createTestGithubAuth, createTestUser, truncateTables } from '../test-util'
import { getGithubAuthentication } from './githubAuthentication.server'

describe('githubAuthentication', () => {
  describe('getGithubAuthentication', () => {
    afterEach(async () => {
      await truncateTables([prisma.githubAuthentication, prisma.userDetails])
    })
    afterEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestGithubAuth(prisma, { userId: 'alice', token: 'the-token' })
    })
    it('returns null if the auth is the user does not exist', async () => {
      const got = await getGithubAuthentication({ userId: 'WRONG' })
      expect(got).toBe(null)
    })
    it('returns null if the auth is not found for the user id', async () => {
      const got = await getGithubAuthentication({ userId: 'bob' })
      expect(got).toBe(null)
    })
    it('returns the auth if the user id matches', async () => {
      const got = await getGithubAuthentication({ userId: 'alice' })
      expect(got).not.toBe(null)
      expect(got?.access_token).toBe('the-token')
    })
  })
})
