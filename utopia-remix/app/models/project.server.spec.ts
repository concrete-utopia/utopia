import moment from 'moment'
import { prisma } from '../db.server'
import { createTestProject, createTestUser, truncateTables } from '../test-util'
import {
  listDeletedProjects,
  listProjects,
  renameProject,
  restoreDeletedProject,
  softDeleteProject,
} from './project.server'

describe('project model', () => {
  afterEach(async () => {
    // cleanup
    await truncateTables([prisma.projectID, prisma.project, prisma.userDetails])
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

      it('ignores soft-deleted projects', async () => {
        await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'bar', ownerId: 'bob', deleted: true })
        await createTestProject(prisma, { id: 'baz', ownerId: 'bob' })

        const bobProjects = await listProjects({ ownerId: 'bob' })
        expect(bobProjects.length).toBe(2)
        expect(bobProjects.map((p) => p.proj_id)).toEqual(['baz', 'foo'])
      })
    })
  })

  describe('renameProject', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestProject(prisma, { id: 'foo', ownerId: 'bob', title: 'the-project' })
      await createTestProject(prisma, {
        id: 'deleted-project',
        ownerId: 'bob',
        title: 'the-project',
        deleted: true,
      })
    })
    it('requires the user', async () => {
      const fn = async () => renameProject({ id: 'foo', userId: 'JOHN-DOE', title: 'test' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project', async () => {
      const fn = async () => renameProject({ id: 'bar', userId: 'bob', title: 'test' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project ownership', async () => {
      const fn = async () => renameProject({ id: 'foo', userId: 'alice', title: 'test' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project not to be soft-deleted', async () => {
      const fn = async () => renameProject({ id: 'deleted-project', userId: 'bob', title: 'test' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('renames the project', async () => {
      await renameProject({ id: 'foo', userId: 'bob', title: 'renamed' })
      const got = await prisma.project.findFirst({ where: { proj_id: 'foo' } })
      expect(got?.title).toEqual('renamed')
    })
  })

  describe('softDeleteProject', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
      await createTestProject(prisma, {
        id: 'deleted-project',
        ownerId: 'bob',
        deleted: true,
      })
    })
    it('requires the user', async () => {
      const fn = async () => softDeleteProject({ id: 'foo', userId: 'JOHN-DOE' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project', async () => {
      const fn = async () => softDeleteProject({ id: 'bar', userId: 'bob' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project ownership', async () => {
      const fn = async () => softDeleteProject({ id: 'foo', userId: 'alice' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project not to be soft-deleted', async () => {
      const fn = async () => softDeleteProject({ id: 'deleted-project', userId: 'bob' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('soft-deletes the project', async () => {
      await softDeleteProject({ id: 'foo', userId: 'bob' })
      const got = await prisma.project.findFirst({ where: { proj_id: 'foo' } })
      expect(got?.deleted).toEqual(true)
    })
  })

  describe('restoreDeletedProject', () => {
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
      await createTestProject(prisma, {
        id: 'deleted-project',
        ownerId: 'bob',
        deleted: true,
      })
    })
    it('requires the user', async () => {
      const fn = async () => restoreDeletedProject({ id: 'foo', userId: 'JOHN-DOE' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project', async () => {
      const fn = async () => restoreDeletedProject({ id: 'bar', userId: 'bob' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project ownership', async () => {
      const fn = async () => restoreDeletedProject({ id: 'foo', userId: 'alice' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('requires the project to be soft-deleted', async () => {
      const fn = async () => restoreDeletedProject({ id: 'foo', userId: 'bob' })
      await expect(fn).rejects.toThrow('Record to update not found')
    })
    it('restores the project', async () => {
      await restoreDeletedProject({ id: 'deleted-project', userId: 'bob' })
      const got = await prisma.project.findFirst({ where: { proj_id: 'foo' } })
      expect(got?.deleted).toEqual(null)
    })
  })

  describe('listDeletedProjects', () => {
    describe('when the user is not found', () => {
      it('returns an empty array', async () => {
        const got = await listDeletedProjects({ ownerId: 'not-found' })
        expect(got.length).toBe(0)
      })
    })

    describe('when the user is passed as undefined', () => {
      it('throws an error', async () => {
        const fn = async () => listDeletedProjects({ ownerId: undefined as any })
        await expect(fn).rejects.toThrow()
      })
    })

    describe('when the user is found', () => {
      it('returns the user deleted projects', async () => {
        await createTestProject(prisma, { id: 'foo', ownerId: 'bob' })
        await createTestProject(prisma, { id: 'bar', ownerId: 'bob', deleted: true })
        await createTestProject(prisma, { id: 'baz', ownerId: 'alice' })
        await createTestProject(prisma, { id: 'qux', ownerId: 'bob', deleted: true })

        const bobProjects = await listDeletedProjects({ ownerId: 'bob' })
        expect(bobProjects.length).toBe(2)
        expect(bobProjects.map((p) => p.proj_id)).toEqual(['qux', 'bar'])

        const aliceProjects = await listDeletedProjects({ ownerId: 'alice' })
        expect(aliceProjects.length).toBe(0)
      })
    })
  })
})
