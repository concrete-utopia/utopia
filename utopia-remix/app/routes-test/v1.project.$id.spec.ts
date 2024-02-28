jest.mock('@openfga/sdk')
import { prisma } from '../db.server'

import { createTestSession, createTestUser, newTestRequest, truncateTables } from '../test-util'
import { getProject } from '../routes/v1.project.$id'
import * as serverProxy from '../util/proxy.server'
import * as permissionsService from '../services/permissionsService.server'
import { UserProjectPermission } from '../types'
import { ApiError } from '../util/errors'

describe('getProject', () => {
  afterAll(async () => {
    jest.restoreAllMocks()
  })
  describe('access model', () => {
    const projectId = 'project1'
    const userId = 'user1'

    let projectProxyMock: jest.SpyInstance
    let hasUserProjectPermission: jest.SpyInstance
    afterEach(async () => {
      await truncateTables([
        prisma.userDetails,
        prisma.persistentSession,
        prisma.project,
        prisma.projectID,
        prisma.projectAccess,
      ])

      projectProxyMock.mockClear()
      hasUserProjectPermission.mockClear()
    })

    beforeEach(async () => {
      await createTestUser(prisma, { id: userId })
      await createTestSession(prisma, { key: 'the-key', userId: userId })

      projectProxyMock = jest.spyOn(serverProxy, 'proxy')
      hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
    })

    it('should check access level for a project', async () => {
      projectProxyMock.mockResolvedValue({ id: projectId, ownerId: userId })
      hasUserProjectPermission.mockResolvedValue(true)
      const req = newTestRequest({ method: 'GET', authCookie: 'the-key' })
      const projectResult = await getProject(req, { id: projectId })
      expect(projectResult).toEqual({ id: projectId, ownerId: userId })
      expect(hasUserProjectPermission).toHaveBeenCalledWith(
        projectId,
        userId,
        UserProjectPermission.CAN_VIEW_PROJECT,
      )
    })

    it('should deny access for an unauthorized project', async () => {
      projectProxyMock.mockResolvedValue({ id: projectId, ownerId: 'user2' })
      hasUserProjectPermission.mockResolvedValue(false)
      const req = newTestRequest({ method: 'GET', authCookie: 'the-key' })
      const fn = async () => {
        getProject(req, { id: projectId })
      }
      expect(fn).rejects.toThrow(ApiError)
    })

    it('should allow access to the owner', async () => {
      projectProxyMock.mockResolvedValue({ id: projectId, ownerId: userId })
      // mock the permission check to return false
      hasUserProjectPermission.mockResolvedValue(false)
      const req = newTestRequest({ method: 'GET', authCookie: 'the-key' })
      const projectResult = await getProject(req, { id: projectId })
      expect(projectResult).toEqual({ id: projectId, ownerId: userId })
    })
  })
})
