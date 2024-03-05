jest.mock('@openfga/sdk')
import { prisma } from '../db.server'

import {
  clearDb,
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
} from '../test-util'
import { loader } from '../routes/v1.project.$id'
import * as proxyServer from '../util/proxy.server'
import * as permissionsService from '../services/permissionsService.server'
import { UserProjectPermission } from '../types'
import { ApiResponse } from '../util/api.server'
import { Status } from '../util/statusCodes'

describe('getProject', () => {
  beforeAll(async () => {
    await clearDb(prisma)
  })
  afterAll(async () => {
    jest.restoreAllMocks()
  })
  describe('access model', () => {
    const projectId = 'project1'
    const userId = 'user1'

    let projectProxy: jest.SpyInstance
    let hasUserProjectPermission: jest.SpyInstance
    afterEach(async () => {
      await clearDb(prisma)

      projectProxy.mockClear()
      hasUserProjectPermission.mockClear()
    })

    beforeEach(async () => {
      await createTestUser(prisma, { id: userId })
      await createTestSession(prisma, { key: 'the-key', userId: userId })

      projectProxy = jest.spyOn(proxyServer, 'proxy')
      hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
    })

    it('should check access level for a project', async () => {
      await createTestProject(prisma, { id: projectId, ownerId: 'user2' })
      projectProxy.mockResolvedValue({ id: projectId, ownerId: 'user2' })
      hasUserProjectPermission.mockResolvedValue(true)
      const req = newTestRequest({ method: 'GET', authCookie: 'the-key' })
      const response = await (loader({
        request: req,
        params: { id: projectId },
        context: {},
      }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
      const project = await response.json()
      expect(project).toEqual({ id: projectId, ownerId: 'user2' })
      expect(hasUserProjectPermission).toHaveBeenCalledWith(
        projectId,
        userId,
        UserProjectPermission.CAN_VIEW_PROJECT,
      )
    })

    it('should deny access for an unauthorized project', async () => {
      await createTestProject(prisma, { id: projectId, ownerId: 'user2' })
      projectProxy.mockResolvedValue({ id: projectId, ownerId: 'user2' })
      hasUserProjectPermission.mockResolvedValue(false)
      const req = newTestRequest({ method: 'GET', authCookie: 'the-key' })
      const response = await (loader({
        request: req,
        params: { id: projectId },
        context: {},
      }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
      const error = await response.json()
      expect(error).toEqual({
        message: 'Project not found',
        status: Status.NOT_FOUND,
        error: 'Error',
      })
    })

    it('should allow access to the owner', async () => {
      await createTestProject(prisma, { id: projectId, ownerId: userId })
      projectProxy.mockResolvedValue({ id: projectId, ownerId: userId })
      // mock the permission check to return false
      hasUserProjectPermission.mockResolvedValue(false)
      const req = newTestRequest({ method: 'GET', authCookie: 'the-key' })
      const response = await (loader({
        request: req,
        params: { id: projectId },
        context: {},
      }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
      const projectResult = await response.json()
      expect(projectResult).toEqual({ id: projectId, ownerId: userId })
    })
  })
})
