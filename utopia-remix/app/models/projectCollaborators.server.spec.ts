import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectCollaborator,
  createTestUser,
  truncateTables,
} from '../test-util'
import {
  getCollaborators,
  addToProjectCollaborators,
  listProjectCollaborators,
} from './projectCollaborators.server'

describe('projectCollaborators model', () => {
  afterEach(async () => {
    // cleanup
    await truncateTables([
      prisma.projectCollaborator,
      prisma.projectID,
      prisma.project,
      prisma.userDetails,
    ])
  })

  describe('getCollaborators', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestUser(prisma, { id: 'wendy' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'three', ownerId: 'alice' })
      await createTestProject(prisma, { id: 'four', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'five', ownerId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'one', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'wendy' })
      await createTestProjectCollaborator(prisma, { projectId: 'three', userId: 'alice' })
      await createTestProjectCollaborator(prisma, { projectId: 'three', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'five', userId: 'alice' })
      await createTestProjectCollaborator(prisma, { projectId: 'five', userId: 'wendy' })
    })
    it('returns an empty object if no ids are passed', async () => {
      const got = await getCollaborators({ ids: [], userId: 'bob' })
      expect(got).toEqual({})
    })
    it("returns an empty object if ids don't match the given user id", async () => {
      let got = await getCollaborators({ ids: ['one', 'two'], userId: 'alice' })
      expect(got).toEqual({})
      got = await getCollaborators({ ids: ['one', 'two'], userId: 'NOBODY' })
      expect(got).toEqual({})
    })
    it('returns the collaborator details by project id', async () => {
      const ids = ['one', 'two', 'four', 'five']
      const got = await getCollaborators({ ids: ids, userId: 'bob' })
      expect(Object.keys(got)).toEqual(ids)
      expect(got['one'].map((c) => c.id)).toEqual(['bob'])
      expect(got['two'].map((c) => c.id)).toEqual(['bob', 'wendy'])
      expect(got['four'].map((c) => c.id)).toEqual([])
      expect(got['five'].map((c) => c.id)).toEqual(['alice', 'wendy'])
    })
    it('ignores mismatching projects', async () => {
      const ids = ['one', 'two', 'three']
      const got = await getCollaborators({ ids: ids, userId: 'bob' })
      expect(Object.keys(got)).toEqual(['one', 'two'])
      expect(got['one'].map((c) => c.id)).toEqual(['bob'])
      expect(got['two'].map((c) => c.id)).toEqual(['bob', 'wendy'])
    })
  })

  describe('addToProjectCollaborators', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
      await createTestProject(prisma, { id: 'three', ownerId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'one', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'alice' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'bob' })
    })

    it('errors if the project is not found', async () => {
      const fn = async () => addToProjectCollaborators({ projectId: 'WRONG', userId: 'bob' })
      await expect(fn).rejects.toThrow('Foreign key constraint failed')
    })
    it('errors if the user is not found', async () => {
      const fn = async () => addToProjectCollaborators({ projectId: 'one', userId: 'WRONG' })
      await expect(fn).rejects.toThrow('Foreign key constraint failed')
    })
    it('does nothing if the user is already a collaborator', async () => {
      await addToProjectCollaborators({ projectId: 'one', userId: 'bob' })
      const collaborators = await prisma.projectCollaborator.findMany({
        where: { project_id: 'one' },
      })
      expect(collaborators.map((p) => p.user_id)).toEqual(['bob'])
    })
    it('adds the user to the collaborators if missing', async () => {
      await addToProjectCollaborators({ projectId: 'one', userId: 'alice' })
      let collaborators = await prisma.projectCollaborator.findMany({
        where: { project_id: 'one' },
        orderBy: { id: 'asc' },
      })
      expect(collaborators.map((p) => p.user_id)).toEqual(['bob', 'alice'])

      await addToProjectCollaborators({ projectId: 'three', userId: 'alice' })
      collaborators = await prisma.projectCollaborator.findMany({
        where: { project_id: 'three' },
        orderBy: { id: 'asc' },
      })
      expect(collaborators.map((p) => p.user_id)).toEqual(['alice'])
    })
  })

  describe('listProjectCollaborators', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
      await createTestProject(prisma, { id: 'three', ownerId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'one', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'alice' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'bob' })
    })

    it('returns nothing if the project is not found', async () => {
      const collaborators = await listProjectCollaborators({ id: 'WRONG' })
      await expect(collaborators).toEqual([])
    })
    it('returns the collaborators', async () => {
      let collaborators = await listProjectCollaborators({ id: 'two' })
      expect(collaborators.map((c) => c.user_id)).toEqual(['alice', 'bob'])

      collaborators = await listProjectCollaborators({ id: 'one' })
      expect(collaborators.map((c) => c.user_id)).toEqual(['bob'])

      collaborators = await listProjectCollaborators({ id: 'three' })
      expect(collaborators).toEqual([])
    })
  })
})
