import moment from 'moment'
import { prisma } from '../db.server'
import { createTestProject, truncateTables } from '../test-util'
import { listProjects } from './project.server'

describe('project model', () => {
  afterEach(async () => {
    // cleanup
    await truncateTables([prisma.projectID, prisma.project])
  })

  describe('listProjects', () => {
    describe('when the user is not found', () => {
      it('returns an empty array', async () => {
        const got = await listProjects({ ownerId: 'not-found' })
        expect(got.length).toBe(0)
      })
    })

    describe('when the user is passed as undefined', () => {
      it('throws an error', async () => {
        const fn = async () => listProjects({ ownerId: undefined as any })
        await expect(fn).rejects.toThrow()
      })
    })

    describe('when the user is found', () => {
      it('returns the user projects', async () => {
        await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'bar', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'baz', ownerId: 'alice' })
        await createTestProject(prisma, { id: 'qux', ownerId: 'bob' })

        const bobProjects = await listProjects({ ownerId: 'bob' })
        expect(bobProjects.length).toBe(3)
        expect(bobProjects.map((p) => p.proj_id)).toEqual(['qux', 'bar', 'foo'])

        const aliceProjects = await listProjects({ ownerId: 'alice' })
        expect(aliceProjects.length).toBe(1)
      })

      it('sorts the results by modified time', async () => {
        const now = new Date()
        await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
        await createTestProject(prisma, {
          id: 'bar',
          ownerId: 'bob',
          modifiedAt: moment(now).add(1, 'day').toDate(),
        })
        await createTestProject(prisma, { id: 'baz', ownerId: 'alice' })
        await createTestProject(prisma, {
          id: 'qux',
          ownerId: 'bob',
          modifiedAt: moment(now).add(-1, 'day').toDate(),
        })

        const bobProjects = await listProjects({ ownerId: 'bob' })
        expect(bobProjects.map((p) => p.proj_id)).toEqual(['bar', 'foo', 'qux'])

        const aliceProjects = await listProjects({ ownerId: 'alice' })
        expect(aliceProjects.map((p) => p.proj_id)).toEqual(['baz'])
      })

      it('can paginate results', async () => {
        await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'two', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'three', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'four', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'five', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'six', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'seven', ownerId: 'bob' })

        expect((await listProjects({ ownerId: 'bob', limit: 3 })).map((p) => p.proj_id)).toEqual([
          'seven',
          'six',
          'five',
        ])

        expect(
          (await listProjects({ ownerId: 'bob', limit: 3, offset: 3 })).map((p) => p.proj_id),
        ).toEqual(['four', 'three', 'two'])

        expect(
          (await listProjects({ ownerId: 'bob', limit: 3, offset: 6 })).map((p) => p.proj_id),
        ).toEqual(['one'])
      })
    })
  })
})
