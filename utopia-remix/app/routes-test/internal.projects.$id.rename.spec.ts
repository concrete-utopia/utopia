import { prisma } from '../db.server'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newFormData,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'
import { handleRenameProject } from '../routes/internal.projects.$id.rename'

describe('handleRenameProject', () => {
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
    await createTestProject(prisma, { id: 'one', ownerId: 'foo', title: 'project-one' })
    await createTestProject(prisma, { id: 'two', ownerId: 'bar', title: 'project-two' })
  })

  it('requires a user', async () => {
    const fn = async () =>
      handleRenameProject(newTestRequest({ method: 'POST', authCookie: 'wrong-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })
  it('requires a valid id', async () => {
    const fn = async () =>
      handleRenameProject(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('id is null')
  })
  it('requires a valid title', async () => {
    const fn = (data: FormData) => async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key', formData: data })
      return handleRenameProject(req, { id: 'project-one' })
    }

    await expect(fn(newFormData({}))).rejects.toThrow(ApiError)
    await expect(fn(newFormData({}))).rejects.toThrow('title is null')

    await expect(fn(newFormData({ title: '' }))).rejects.toThrow(ApiError)
    await expect(fn(newFormData({ title: '' }))).rejects.toThrow('title is too short')
  })
  it('requires a valid project', async () => {
    const fn = (data: FormData) => async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key', formData: data })
      return handleRenameProject(req, { id: 'doesnt-exist' })
    }

    await expect(fn(newFormData({ title: 'hello' }))).rejects.toThrow('Record to update not found')
  })
  it('requires ownership of the project', async () => {
    const fn = (data: FormData) => async () => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key', formData: data })
      return handleRenameProject(req, { id: 'two' })
    }

    await expect(fn(newFormData({ title: 'hello' }))).rejects.toThrow('Record to update not found')
  })
  it('renames the project', async () => {
    const fn = async (data: FormData) => {
      const req = newTestRequest({ method: 'POST', authCookie: 'the-key', formData: data })
      return handleRenameProject(req, { id: 'one' })
    }

    await fn(newFormData({ title: 'hello' }))
    let project = await prisma.project.findFirst({ where: { proj_id: 'one' } })

    expect(project?.title).toEqual('hello')

    await fn(newFormData({ title: 'hello AGAIN!' }))
    project = await prisma.project.findFirst({ where: { proj_id: 'one' } })

    expect(project?.title).toEqual('hello-again')
  })
})
