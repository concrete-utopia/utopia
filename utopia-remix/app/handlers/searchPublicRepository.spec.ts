import { prisma } from '../db.server'
import {
  createTestGithubAuth,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import * as githubUtil from '../util/github.server'
import { toApiFailure, toApiSuccess } from '../types'
import { handleSearchPublicRepository } from './searchPublicRepository'

describe('handleSearchPublicRepository', () => {
  let mockOctokit: jest.SpyInstance
  afterEach(async () => {
    await truncateTables([
      prisma.persistentSession,
      prisma.githubAuthentication,
      prisma.userDetails,
    ])
    mockOctokit.mockRestore()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestSession(prisma, { userId: 'bob', key: 'bob-key' })
    await createTestSession(prisma, { userId: 'alice', key: 'alice-key' })
    await createTestGithubAuth(prisma, { userId: 'alice', token: 'the-token' })
    mockOctokit = jest.spyOn(githubUtil, 'newOctokitClient')
  })

  const doRequest = async ({
    owner,
    repo,
    auth,
  }: {
    owner?: string
    repo?: string
    auth?: string
  }) =>
    handleSearchPublicRepository(
      newTestRequest({ method: 'POST', authCookie: auth, body: JSON.stringify({ owner, repo }) }),
    )

  it('requires a user', async () => {
    await expect(doRequest({ owner: 'foo', repo: 'bar' })).rejects.toThrow('missing session cookie')
  })
  it('requires a valid request', async () => {
    await expect(doRequest({ auth: 'bob-key' })).rejects.toThrow('invalid request')
  })
  it('requires a valid owner', async () => {
    await expect(doRequest({ owner: '', repo: 'bar', auth: 'bob-key' })).rejects.toThrow(
      'invalid owner',
    )
  })
  it('requires a valid repo', async () => {
    await expect(doRequest({ owner: 'foo', repo: '', auth: 'bob-key' })).rejects.toThrow(
      'invalid repo',
    )
  })
  it('requires a gh auth token', async () => {
    await expect(doRequest({ owner: 'foo', repo: 'bar', auth: 'bob-key' })).rejects.toThrow(
      'missing auth',
    )
  })
  describe('when the gh call fails', () => {
    it('(generic error) returns the data as a failure', async () => {
      mockOctokit.mockReturnValue({
        request: () => {
          throw new Error('boom')
        },
      })

      const got = await doRequest({ owner: 'foo', repo: 'bar', auth: 'alice-key' })
      if (!(got instanceof Response)) {
        throw new Error('should be a response')
      }
      const body = await got.json()
      expect(body).toEqual(toApiFailure('Error: boom'))
    })
    it('(non 2xx status) returns the data as a failure', async () => {
      mockOctokit.mockReturnValue({
        request: () => {
          return { status: 418 }
        },
      })

      const got = await doRequest({ owner: 'foo', repo: 'bar', auth: 'alice-key' })
      if (!(got instanceof Response)) {
        throw new Error('should be a response')
      }
      const body = await got.json()
      expect(body).toEqual(toApiFailure('Error: repository not found (418)'))
    })
  })
  describe('when the gh call succeeds', () => {
    it('returns the data as a success', async () => {
      mockOctokit.mockReturnValue({
        request: () => {
          return {
            status: 200,
            data: { owner: { avatar_url: 'test.png' }, name: 'bar', default_branch: 'main' },
          }
        },
      })
      const got = await doRequest({ owner: 'foo', repo: 'bar', auth: 'alice-key' })
      expect(got).toEqual(
        toApiSuccess({
          repository: {
            avatarUrl: 'test.png',
            defaultBranch: 'main',
            name: 'bar',
          },
        }),
      )
    })
  })
})
