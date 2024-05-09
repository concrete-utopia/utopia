import type { Params } from '@remix-run/react'
import { prisma } from '../db.server'
import {
  createTestGithubAuth,
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { action } from '../routes/internal.projects.$id.github.branches.$owner.$repo.branch.$branch'
import type { ApiResponse } from '../util/api.server'
import { Status } from '../util/statusCodes'
import * as githubUtil from '../util/github'
import path from 'path'
import urlJoin from 'url-join'
import * as fs from 'fs'
import type { ApiSuccess } from '../types'
import type { BranchResponse } from '../util/github-branch-contents.server'

describe('get branch project contents', () => {
  let mockOctokit: jest.SpyInstance

  afterEach(async () => {
    await truncateTables([
      prisma.githubAuthentication,
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])

    mockOctokit.mockRestore()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestSession(prisma, { key: 'bob-key', userId: 'bob' })
    await createTestSession(prisma, { key: 'alice-key', userId: 'alice' })
    await createTestProject(prisma, {
      id: 'one',
      ownerId: 'bob',
      title: 'project-one',
    })
    await createTestProject(prisma, {
      id: 'two',
      ownerId: 'alice',
      title: 'project-two',
    })
    await createTestGithubAuth(prisma, { userId: 'alice', token: 'alice-token' })

    mockOctokit = jest.spyOn(githubUtil, 'newOctokitClient')
  })

  it('requires a valid project id', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST' }), {})
    expect(got).toEqual({
      message: 'Invalid project id',
      status: Status.BAD_REQUEST,
      error: 'Error',
    })
  })

  it('requires an existing project', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST' }), { id: 'WRONG' })
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('requires a valid user', async () => {
    const got = await getActionResult(newTestRequest({ method: 'POST' }), { id: 'one' })
    expect(got).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('requires a valid body', async () => {
    const got = await getActionResult(
      newTestRequest({
        method: 'POST',
        authCookie: 'bob-key',
        body: JSON.stringify({ hello: 42 }),
      }),
      {
        id: 'one',
        owner: 'foo',
        repo: 'bar',
        branch: 'baz',
      },
    )
    expect(got).toEqual({
      message: 'invalid request',
      status: Status.BAD_REQUEST,
      error: 'Error',
    })
  })

  it('requires github auth', async () => {
    const got = await getActionResult(
      newTestRequest({
        method: 'POST',
        authCookie: 'bob-key',
        body: JSON.stringify({
          existingAssets: [],
          uploadAssets: true,
        }),
      }),
      {
        id: 'one',
        owner: 'foo',
        repo: 'bar',
        branch: 'baz',
      },
    )
    expect(got).toEqual({
      message: 'unauthorized',
      status: Status.UNAUTHORIZED,
      error: 'Error',
    })
  })

  it('returns the project contents', async () => {
    const dirname = path.dirname(__filename)
    const archiveName = `utopia-default-project-main`
    const archivePath = urlJoin(dirname, `../test-assets/${archiveName}.zip`)
    const zipball = fs.readFileSync(archivePath)

    const commit = '0000000000000000000000000000000000000000'

    mockOctokit.mockReturnValue({
      request: (url: string) => {
        switch (url) {
          case 'GET /repos/{owner}/{repo}/branches/{branch}':
            return {
              status: 200,
              data: { commit: { sha: commit } },
            }
          case 'GET /repos/{owner}/{repo}/zipball/{ref}':
            return {
              status: 200,
              data: zipball,
            }
          default:
            throw new Error(`unexpected call to ${url}`)
        }
      },
    })

    const got = await getActionResult(
      newTestRequest({
        method: 'POST',
        authCookie: 'alice-key',
        body: JSON.stringify({
          existingAssets: [],
          uploadAssets: true,
        }),
      }),
      {
        id: 'two',
        owner: 'foo',
        repo: 'bar',
        branch: 'baz',
      },
    )

    const response = got as unknown as ApiSuccess<BranchResponse>
    expect(response.branch.branchName).toBe('baz')
    expect(response.branch.originCommit).toBe(commit)

    const expected = JSON.parse(
      fs.readFileSync(urlJoin(dirname, `../test-assets/${archiveName}.json`)).toString(),
    )
    expect(response.branch.content).toEqual(expected)
  })
})

async function getActionResult(req: Request, params: Params<string>) {
  const response = await (action({
    request: req,
    params: params,
    context: {},
  }) as Promise<ApiResponse<Record<string, never>>>)
  return await response.json()
}
