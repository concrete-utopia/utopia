jest.mock('@openfga/sdk')
import { prisma } from '../db.server'

import {
  createTestProject,
  createTestProjectAccess,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { loader, action } from '../routes/v1.project._index'
import * as proxyServer from '../util/proxy.server'
import * as permissionsService from '../services/permissionsService.server'
import * as projectAccessModel from '../models/projectAccess.server'
import { AccessLevel, UserProjectPermission } from '../types'
import type { ApiResponse } from '../util/api.server'
import { Status } from '../util/statusCodes'
import newProjectContents from './data/newProjectContents.json'

describe('create new project', () => {
  beforeAll(async () => {
    await truncateTables([
      prisma.projectAccess,
      prisma.projectCollaborator,
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
    ])
  })
  afterAll(async () => {
    jest.restoreAllMocks()
  })

  describe('project creation', () => {
    const projectId = 'project1'
    const userId = 'user1'

    let projectProxy: jest.SpyInstance
    let createProjectAccessMock: jest.SpyInstance

    afterEach(async () => {
      await truncateTables([
        prisma.projectAccess,
        prisma.projectCollaborator,
        prisma.userDetails,
        prisma.persistentSession,
        prisma.project,
        prisma.projectID,
      ])

      projectProxy.mockClear()
      createProjectAccessMock.mockClear()
    })

    beforeEach(async () => {
      await createTestUser(prisma, { id: userId })
      await createTestSession(prisma, { key: 'the-key', userId: userId })

      projectProxy = jest.spyOn(proxyServer, 'proxy')
      createProjectAccessMock = jest.spyOn(projectAccessModel, 'createProjectAccess')
    })

    it('should create a new project with an expected id', async () => {
      projectProxy.mockResolvedValue({ id: projectId, ownerId: userId })
      createProjectAccessMock.mockResolvedValue(null)
      const req = newTestRequest({
        method: 'PUT',
        authCookie: 'the-key',
        body: JSON.stringify({
          name: 'test-project',
          content: newProjectContents,
        }),
      })
      const response = await (action({
        request: req,
        params: {},
        context: {},
      }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
      const project = await response.json()
      expect(project).toEqual({ id: projectId, ownerId: userId })
      expect(createProjectAccessMock).toHaveBeenCalledWith({
        projectId: projectId,
        creatorId: userId,
        accessLevel: AccessLevel.PRIVATE,
      })
    })
  })

  describe('access level', () => {
    const projectId = 'project1'
    const userId = 'user1'

    let projectProxy: jest.SpyInstance
    let createProjectAccessMock: jest.SpyInstance
    afterEach(async () => {
      await truncateTables([
        prisma.projectAccess,
        prisma.projectCollaborator,
        prisma.userDetails,
        prisma.persistentSession,
        prisma.project,
        prisma.projectID,
      ])

      projectProxy.mockClear()
      createProjectAccessMock.mockClear()
    })

    beforeEach(async () => {
      await createTestUser(prisma, { id: userId })
      await createTestSession(prisma, { key: 'the-key', userId: userId })

      projectProxy = jest.spyOn(proxyServer, 'proxy')
      createProjectAccessMock = jest.spyOn(projectAccessModel, 'createProjectAccess')
    })

    it('should set access level for a new project', async () => {
      projectProxy.mockResolvedValue({ id: projectId, ownerId: userId })
      createProjectAccessMock.mockResolvedValue(null)
      const req = newTestRequest({
        method: 'PUT',
        authCookie: 'the-key',
        body: JSON.stringify({
          name: 'test-project',
          content: newProjectContents,
          accessLevel: 'public',
        }),
      })
      const response = await (action({
        request: req,
        params: {},
        context: {},
      }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
      const project = await response.json()
      expect(project).toEqual({ id: projectId, ownerId: userId })
      expect(createProjectAccessMock).toHaveBeenCalledWith({
        projectId: projectId,
        creatorId: userId,
        accessLevel: AccessLevel.PUBLIC,
      })
    })
    it('should set access level to PRIVATE for a new project if not provided', async () => {
      projectProxy.mockResolvedValue({ id: projectId, ownerId: userId })
      createProjectAccessMock.mockResolvedValue(null)
      const req = newTestRequest({
        method: 'PUT',
        authCookie: 'the-key',
        body: JSON.stringify({
          name: 'test-project',
          content: newProjectContents,
        }),
      })
      const response = await (action({
        request: req,
        params: {},
        context: {},
      }) as Promise<ApiResponse<{ id: string; projectId: string }>>)
      const project = await response.json()
      expect(project).toEqual({ id: projectId, ownerId: userId })
      expect(createProjectAccessMock).toHaveBeenCalledWith({
        projectId: projectId,
        creatorId: userId,
        accessLevel: AccessLevel.PRIVATE,
      })
    })
    it('should throw an error for a new project if access level is invalid', async () => {
      projectProxy.mockResolvedValue({ id: projectId, ownerId: userId })
      createProjectAccessMock.mockResolvedValue(null)
      const req = newTestRequest({
        method: 'PUT',
        authCookie: 'the-key',
        body: JSON.stringify({
          name: 'test-project',
          content: newProjectContents,
          accessLevel: 'none',
        }),
      })
      const response = (await action({
        request: req,
        params: {},
        context: {},
      })) as ApiResponse<{ id: string; projectId: string }>
      const result = await response.json()
      expect(result).toEqual({
        error: 'Error',
        message: 'Invalid access level',
        status: Status.BAD_REQUEST,
      })
    })
  })
})
