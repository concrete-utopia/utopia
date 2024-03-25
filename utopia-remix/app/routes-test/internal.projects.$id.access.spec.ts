jest.mock('@openfga/sdk')
import { prisma } from '../db.server'
import { handleChangeProjectAccess } from '../routes/internal.projects.$id.access'
import {
  createTestProject,
  createTestProjectAccess,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'
import * as permissionsService from '../services/permissionsService.server'
import { AccessLevel, UserProjectPermission } from '../types'
import { action } from '../routes/internal.projects.$id.access'
import type { ApiResponse } from '../util/api.server'
import { Status } from '../util/statusCodes'

describe('handleChangeAccess', () => {
  let hasUserProjectPermission: jest.SpyInstance

  afterAll(async () => {
    jest.restoreAllMocks()
  })
  afterEach(async () => {
    await truncateTables([
      prisma.projectAccess,
      prisma.projectCollaborator,
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])
    hasUserProjectPermission.mockClear()
    jest.spyOn(permissionsService, 'setProjectAccess').mockRestore()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'foo' })
    await createTestUser(prisma, { id: 'bar' })
    await createTestSession(prisma, { key: 'the-key', userId: 'foo' })
    await createTestProject(prisma, { id: 'one', ownerId: 'foo', title: 'project-one' })
    await createTestProjectAccess(prisma, { projectId: 'one', accessLevel: AccessLevel.PRIVATE })
    await createTestProject(prisma, { id: 'two', ownerId: 'bar', title: 'project-two' })
    jest.spyOn(permissionsService, 'setProjectAccess').mockResolvedValue()
    hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
  })

  it('changes access level to private', async () => {
    const formData = new FormData()
    formData.append('accessLevel', AccessLevel.PRIVATE.toString())
    const fn = async () =>
      handleChangeProjectAccess(
        newTestRequest({
          method: 'POST',
          authCookie: 'the-key',
          formData: formData,
        }),
        { id: 'one' },
      )

    await fn()
    const projectAccess = await prisma.projectAccess.findFirst({
      where: { project_id: 'one' },
    })
    expect(projectAccess?.access_level).toEqual(AccessLevel.PRIVATE)
    expect(permissionsService.setProjectAccess).toHaveBeenCalledWith('one', AccessLevel.PRIVATE)
  })

  it('doesnt accept wrong access level', async () => {
    const formData = new FormData()
    // set an access level that doesn't exist
    formData.append('accessLevel', '34')
    const fn = async () =>
      handleChangeProjectAccess(
        newTestRequest({
          method: 'POST',
          authCookie: 'the-key',
          formData: formData,
        }),
        { id: 'one' },
      )

    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('accessLevel is not a valid AccessLevel')
  })

  it('should deny access for an unauthorized project', async () => {
    hasUserProjectPermission.mockImplementation((projectId, userId, permission) => {
      if (permission === UserProjectPermission.CAN_MANAGE_PROJECT) {
        return Promise.resolve(false)
      } else {
        return Promise.resolve(true)
      }
    })
    const error = await getActionResult('two', AccessLevel.PRIVATE)
    expect(error).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('should deny access for an anonymous user', async () => {
    hasUserProjectPermission.mockImplementation((projectId, userId, permission) => {
      if (permission === UserProjectPermission.CAN_MANAGE_PROJECT) {
        return Promise.resolve(false)
      } else {
        return Promise.resolve(true)
      }
    })
    const error = await getActionResult('two', AccessLevel.PRIVATE, 'no-cookie')
    expect(error).toEqual({
      message: 'Project not found',
      status: Status.NOT_FOUND,
      error: 'Error',
    })
  })

  it('should allow access to the owner even if permission is false', async () => {
    hasUserProjectPermission.mockImplementation((projectId, userId, permission) => {
      if (permission === UserProjectPermission.CAN_MANAGE_PROJECT) {
        return Promise.resolve(false)
      } else {
        return Promise.resolve(true)
      }
    })
    const result = await getActionResult('one', AccessLevel.PRIVATE)
    expect(result).toEqual({})
  })
})

async function getActionResult(
  projectId: string,
  accessLevel: AccessLevel,
  authCookie: string = 'the-key',
) {
  const formData = new FormData()
  formData.append('accessLevel', accessLevel.toString())
  const req = newTestRequest({ method: 'POST', authCookie: authCookie, formData: formData })
  const response = await (action({
    request: req,
    params: { id: projectId },
    context: {},
  }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
  return await response.json()
}
