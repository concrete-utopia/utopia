import { LiveblocksAPI, RoomCollaborators, RoomStorage } from '../clients/liveblocks.server'
import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectCollaborator,
  createTestUser,
  truncateTables,
} from '../test-util'
import { getCollaborators, updateCollaborators } from './projectCollaborators.server'

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

  describe('updateCollaborators', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestUser(prisma, { id: 'wendy' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'three', ownerId: 'alice' })
      await createTestProject(prisma, { id: 'four', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'five', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'six', ownerId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'one', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'wendy' })
      await createTestProjectCollaborator(prisma, { projectId: 'three', userId: 'alice' })
      await createTestProjectCollaborator(prisma, { projectId: 'three', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'five', userId: 'alice' })
      await createTestProjectCollaborator(prisma, { projectId: 'five', userId: 'wendy' })
    })

    const mockGetRoomStorage = (collabs: RoomCollaborators) => async (): Promise<RoomStorage> => {
      return {
        data: {
          collaborators: collabs,
        },
      }
    }

    describe('when the project has collaborators', () => {
      describe('when the room storage has existing users', () => {
        it('updates the collaborators with the data from the room storage', async () => {
          LiveblocksAPI.getRoomStorage = mockGetRoomStorage({
            data: {
              alice: { data: { id: 'alice', name: 'Alice Alisson', avatar: 'alice.png' } },
            },
          })
          await updateCollaborators({ id: 'one' })

          const got = await prisma.projectCollaborator.findMany({ where: { project_id: 'one' } })
          expect(got.map((c) => c.user_id)).toEqual(['bob', 'alice'])
        })
      })
      describe('when the room storage has duplicate users', () => {
        it('only adds the missing ones', async () => {
          LiveblocksAPI.getRoomStorage = mockGetRoomStorage({
            data: {
              alice: { data: { id: 'alice', name: 'Alice Alisson', avatar: 'alice.png' } },
              bob: { data: { id: 'bob', name: 'Bob Bobson', avatar: 'bob.png' } },
            },
          })
          await updateCollaborators({ id: 'one' })

          const got = await prisma.projectCollaborator.findMany({
            where: { project_id: 'one' },
          })
          expect(got.map((c) => c.user_id)).toEqual(['bob', 'alice'])
        })
      })
      describe('when the room storage has non-existing users', () => {
        it('updates the collaborators with only the existing users', async () => {
          LiveblocksAPI.getRoomStorage = mockGetRoomStorage({
            data: {
              alice: {
                data: {
                  id: 'alice',
                  name: 'Alice Alisson',
                  avatar: 'alice.png',
                },
              },
              johndoe: {
                data: {
                  id: 'johndoe',
                  name: 'John Doe',
                  avatar: 'johndoe.png',
                },
              },
            },
          })
          await updateCollaborators({ id: 'one' })

          const got = await prisma.projectCollaborator.findMany({ where: { project_id: 'one' } })
          expect(got.map((c) => c.user_id)).toEqual(['bob', 'alice'])
        })
      })
    })

    describe('when the project has no collaborators', () => {
      it('adds the collaborators with the data from the room storage', async () => {
        LiveblocksAPI.getRoomStorage = mockGetRoomStorage({
          data: {
            alice: { data: { id: 'alice', name: 'Alice Alisson', avatar: 'alice.png' } },
            johndoe: { data: { id: 'johndoe', name: 'John Doe', avatar: 'johndoe.png' } },
            bob: { data: { id: 'bob', name: 'Bob Bobson', avatar: 'bob.png' } },
          },
        })
        await updateCollaborators({ id: 'six' })

        const got = await prisma.projectCollaborator.findMany({ where: { project_id: 'six' } })
        expect(got.map((c) => c.user_id)).toEqual(['bob', 'alice'])
      })
    })

    describe('when the project does not exist', () => {
      it('errors', async () => {
        LiveblocksAPI.getRoomStorage = mockGetRoomStorage({
          data: {
            alice: { data: { id: 'alice', name: 'Alice Alisson', avatar: 'alice.png' } },
          },
        })
        const fn = async () => updateCollaborators({ id: 'unknown' })
        await expect(fn).rejects.toThrow()
      })
    })
  })
})
