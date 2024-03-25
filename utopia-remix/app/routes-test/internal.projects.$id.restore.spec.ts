import { prisma } from '../db.server'
import { handleRestoreDeletedProject } from '../routes/internal.projects.$id.restore'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'

describe('handleRestoreDeletedProject', () => {
  afterEach(async () => {
    await truncateTables([
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'foo' })
    await createTestUser(prisma, { id: 'bar' })
    await createTestSession(prisma, { key: 'the-key', userId: 'foo' })
    await createTestProject(prisma, {
      id: 'one',
      ownerId: 'foo',
      title: 'project-one',
      deleted: true,
    })
    await createTestProject(prisma, { id: 'two', ownerId: 'foo', title: 'project-two' })
  })

  it('requires a user', async () => {
    const fn = async () =>
      handleRestoreDeletedProject(newTestRequest({ method: 'POST', authCookie: 'wrong-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })
  it('requires a valid id', async () => {
    const fn = async () =>
      handleRestoreDeletedProject(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('id is null')
  })
  it('requires a valid project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleRestoreDeletedProject(req, { id: 'doesnt-exist' })
    }

    await expect(fn).rejects.toThrow('Record to update not found')
  })
  it('requires ownership of the project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleRestoreDeletedProject(req, { id: 'two' })
    }

    await expect(fn).rejects.toThrow('Record to update not found')
  })
  it('requires a deleted project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleRestoreDeletedProject(req, { id: 'two' })
    }

    await expect(fn).rejects.toThrow('Record to update not found')
  })
  it('restores the project', async () => {
    const fn = async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key' })
      return handleRestoreDeletedProject(req, { id: 'one' })
    }

    await fn()
    const project = await prisma.project.findFirst({ where: { proj_id: 'one' } })
    expect(project?.deleted).toEqual(null)
  })
})
