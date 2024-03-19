import type { Params } from '@remix-run/react'
import { handleGetUserProjectPermissions } from '../routes/internal.projects.$id.permissions'
import {
  createTestProject,
  createTestProjectAccess,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'
import { Status } from '../util/statusCodes'
import { prisma } from '../db.server'
import type { PermissionsMatrix } from '../services/fgaService.server'
import * as fgaService from '../services/fgaService.server'
import { ANONYMOUS_USER_ID } from '../services/permissionsService.server'

describe('handleGetUserProjectPermissions', () => {
  let getAllPermissionsMock: jest.SpyInstance
  let getPermissionsOverrideMock: jest.SpyInstance

  afterEach(async () => {
    await truncateTables([
      prisma.projectID,
      prisma.projectAccess,
      prisma.project,
      prisma.userDetails,
      prisma.persistentSession,
    ])
    getAllPermissionsMock.mockRestore()
    getPermissionsOverrideMock.mockRestore()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
    await createTestSession(prisma, { userId: 'bob', key: 'bob-key' })
    await createTestSession(prisma, { userId: 'alice', key: 'alice-key' })

    getAllPermissionsMock = jest.spyOn(fgaService, 'getAllPermissions')
    getPermissionsOverrideMock = jest.spyOn(fgaService, 'getPermissionsOverride')
  })

  const fn = (req: Request, params: Params<string>) => async () =>
    handleGetUserProjectPermissions(req, params)

  it('requires a valid project id', async () => {
    const req = fn(newTestRequest(), {})
    await expect(req).rejects.toThrow(new ApiError('Invalid project id', Status.BAD_REQUEST))
  })

  it('returns 404 if the project is not found', async () => {
    const req = fn(newTestRequest(), { id: 'unknown' })
    await expect(req).rejects.toThrow(new ApiError('Project not found', Status.NOT_FOUND))
  })

  it('returns the user permissions for that project', async () => {
    const perms: PermissionsMatrix = {
      can_view: true,
      can_fork: true,
      can_play: true,
      can_edit: false,
      can_comment: true,
      can_show_presence: false,
      can_see_live_changes: true,
      can_request_access: false,
      can_manage: true,
    }
    getAllPermissionsMock.mockResolvedValue(perms)

    const req = fn(newTestRequest({ authCookie: 'alice-key' }), { id: 'one' })
    const got = await req()
    expect(got).toEqual(perms)
  })

  it('returns the overridden permissions if the user is the owner', async () => {
    const perms: PermissionsMatrix = {
      can_view: true,
      can_fork: true,
      can_play: true,
      can_edit: false,
      can_comment: true,
      can_show_presence: false,
      can_see_live_changes: true,
      can_request_access: false,
      can_manage: true,
    }
    getPermissionsOverrideMock.mockResolvedValue(perms)

    const req = fn(newTestRequest({ authCookie: 'bob-key' }), { id: 'one' })
    const got = await req()
    expect(got).toEqual(perms)
  })

  it('uses the anonymous user id if no cookie is provided', async () => {
    const perms: PermissionsMatrix = {
      can_view: true,
      can_fork: true,
      can_play: true,
      can_edit: false,
      can_comment: true,
      can_show_presence: false,
      can_see_live_changes: true,
      can_request_access: false,
      can_manage: true,
    }
    getAllPermissionsMock.mockResolvedValue(perms)

    const req = fn(newTestRequest({}), { id: 'one' })
    const got = await req()
    expect(got).toEqual(perms)
    expect(getAllPermissionsMock).toHaveBeenCalledWith('one', ANONYMOUS_USER_ID)
  })
})
