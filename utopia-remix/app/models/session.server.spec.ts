import { prisma } from '../db.server'
import { createTestSession, createTestUser, truncateTables } from '../test-util'
import { maybeGetUserFromSession } from './session.server'

describe('session', () => {
  afterEach(async () => {
    await truncateTables([prisma.persistentSession, prisma.userDetails])
  })
  describe('maybeGetUserFromSession', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestSession(prisma, { userId: 'bob', key: 'bob-key' })
    })
    it('returns null if the session is not found', async () => {
      const got = await maybeGetUserFromSession({ key: '' })
      expect(got).toBe(null)
    })
    it('returns null if the session data is invalid', async () => {
      const got = await maybeGetUserFromSession({ key: '{{WRONG}}' })
      expect(got).toBe(null)
    })
    it('returns null if the user is not found', async () => {
      const got = await maybeGetUserFromSession({ key: 'unknown' })
      expect(got).toBe(null)
    })
    it('returns the user details if found', async () => {
      const got = await maybeGetUserFromSession({ key: 'bob-key' })
      expect(got).not.toBe(null)
      expect(got?.user_id).toBe('bob')
    })
  })
})
