import { prisma } from '../db.server'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'
import { handleListProjects } from './listProjects'

describe('handleListProjects', () => {
  afterEach(async () => {
    // cleanup
    await truncateTables([
      prisma.projectID,
      prisma.project,
      prisma.userDetails,
      prisma.persistentSession,
    ])
  })

  beforeEach(async () => {
    // seed
    await createTestUser(prisma, { id: 'bob', name: 'Bob Bobson' })
    await createTestUser(prisma, { id: 'alice', name: 'Alice Alisson' })
    await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'bar', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'baz', ownerId: 'alice' })
    await createTestProject(prisma, { id: 'qux', ownerId: 'bob' })
  })

  it('requires a user', async () => {
    const req = newTestRequest({ authCookie: 'wrong' })

    const fn = async () => handleListProjects(req)

    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })

  describe('with an authorized user', () => {
    beforeEach(async () => {
      await createTestSession(prisma, { key: 'bobs-key', userId: 'bob' })
    })

    it('returns the list of projects', async () => {
      const req = newTestRequest({ authCookie: 'bobs-key' })

      const got = await handleListProjects(req)
      expect(got.projects.length).toBe(3)
      expect(
        got.projects.map((p) => ({
          id: p.id,
          ownerName: p.ownerName,
          title: p.title,
        })),
      ).toEqual([
        { id: 'qux', ownerName: 'Bob Bobson', title: 'qux' },
        { id: 'bar', ownerName: 'Bob Bobson', title: 'bar' },
        { id: 'foo', ownerName: 'Bob Bobson', title: 'foo' },
      ])
    })
  })
})
