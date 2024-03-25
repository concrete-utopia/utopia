import { prisma } from '../db.server'
import { handleUpdateGithubRepository } from '../routes/internal.projects.$id.github.repository.update'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'

describe('handleUpdateGithubRepository', () => {
  afterEach(async () => {
    await truncateTables([
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestSession(prisma, { key: 'the-key', userId: 'bob' })
    await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
  })

  it('requires a user', async () => {
    const fn = async () =>
      handleUpdateGithubRepository(newTestRequest({ method: 'POST', authCookie: 'wrong-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })
  it('requires a valid id', async () => {
    const fn = async () =>
      handleUpdateGithubRepository(newTestRequest({ method: 'POST', authCookie: 'the-key' }), {})
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('id is null')
  })
  it('requires a valid request body', async () => {
    const fn = async () => {
      const req = newTestRequest({
        method: 'POST',
        authCookie: 'the-key',
        body: JSON.stringify({}),
      })
      return handleUpdateGithubRepository(req, { id: 'one' })
    }

    await expect(fn).rejects.toThrow('invalid request')
  })
  it('requires a valid project', async () => {
    const fn = async () => {
      const req = newTestRequest({
        method: 'POST',
        authCookie: 'the-key',
        body: JSON.stringify({ githubRepository: null }),
      })
      return handleUpdateGithubRepository(req, { id: 'doesnt-exist' })
    }

    await expect(fn).rejects.toThrow('Record to update not found')
  })
  it('requires ownership of the project', async () => {
    const fn = async () => {
      const req = newTestRequest({
        method: 'POST',
        authCookie: 'the-key',
        body: JSON.stringify({ githubRepository: null }),
      })
      return handleUpdateGithubRepository(req, { id: 'two' })
    }

    await expect(fn).rejects.toThrow('Record to update not found')
  })
  it('updates the github repository', async () => {
    const fn = async () => {
      const req = newTestRequest({
        method: 'POST',
        authCookie: 'the-key',
        body: JSON.stringify({
          githubRepository: { owner: 'foo', repository: 'bar', branch: 'baz' },
        }),
      })
      return handleUpdateGithubRepository(req, { id: 'one' })
    }

    await fn()
    const got = await prisma.project.findUnique({
      where: { proj_id: 'one' },
      select: { github_repository: true },
    })
    expect(got?.github_repository).toEqual('foo/bar:baz')
  })
})
