jest.mock('@openfga/sdk')
import { AccessLevel, UserProjectPermission } from '../types'
import * as permissionsService from './permissionsService.server'
import { fgaUserProjectPermission } from './fgaService.server'
import { fgaClient } from './fgaClient.server'
const { CheckResponse } = jest.requireActual('@openfga/sdk')

describe('permissionsService', () => {
  afterAll(() => {
    jest.restoreAllMocks()
  })

  describe('setProjectAccess', () => {
    const projectId = 'projectId'
    it('calls fga for setting project access level as public', async () => {
      const accessLevel = AccessLevel.PUBLIC
      const fgaWrite = jest.spyOn(fgaClient, 'write')
      await permissionsService.setProjectAccess(projectId, accessLevel)
      expect(fgaWrite).toHaveBeenCalledWith({
        writes: [
          {
            user: 'user:*',
            relation: 'viewer',
            object: `project:${projectId}`,
          },
        ],
      })
    })
    it('calls fga for setting project access level as private', async () => {
      const accessLevel = AccessLevel.PRIVATE
      const fgaWrite = jest.spyOn(fgaClient, 'write')
      await permissionsService.setProjectAccess(projectId, accessLevel)
      expect(fgaWrite).toHaveBeenCalledWith({
        deletes: [
          {
            user: 'user:*',
            relation: 'viewer',
            object: `project:${projectId}`,
          },
        ],
      })
    })
  })

  describe('hasUserProjectPermission', () => {
    const projectId = 'projectId'
    const userId = 'userId'
    it('calls fga for checking user project permission', async () => {
      const fgaCheck = jest.spyOn(fgaClient, 'check')
      fgaCheck.mockResolvedValue({ allowed: true } as typeof CheckResponse)
      await permissionsService.hasUserProjectPermission(
        projectId,
        userId,
        UserProjectPermission.CAN_VIEW_PROJECT,
      )
      expect(fgaCheck).toHaveBeenCalledWith({
        user: `user:${userId}`,
        relation: 'can_view',
        object: `project:${projectId}`,
      })
    })
  })

  describe('getAllPermissions', () => {
    it('calls fga for getting all permissions', async () => {
      const projectId = 'projectId'
      const userId = 'userId'
      const ownerId = 'ownerId'
      const fgaListRelations = jest.spyOn(fgaClient, 'listRelations')
      fgaListRelations.mockResolvedValue({
        relations: ['can_view', 'can_fork', 'can_play'],
      })
      const result = await permissionsService.getAllPermissions(projectId, userId, ownerId)
      expect(fgaListRelations).toHaveBeenCalledWith({
        user: `user:${userId}`,
        object: `project:${projectId}`,
        relations: fgaUserProjectPermission,
      })
      const expected = {
        ...fgaUserProjectPermission.reduce((acc, permission) => {
          return { ...acc, [permission]: false }
        }, {}),
        ...{
          can_view: true,
          can_fork: true,
          can_play: true,
        },
      }
      expect(result).toEqual(expected)
    })

    it('returns all permissions as true if user is the owner', async () => {
      const projectId = 'projectId'
      const userId = 'userId'
      const ownerId = userId
      const fgaListRelations = jest.spyOn(fgaClient, 'listRelations')
      fgaListRelations.mockResolvedValue({
        relations: ['can_view', 'can_fork', 'can_play'],
      })
      const result = await permissionsService.getAllPermissions(projectId, userId, ownerId)
      const expected = fgaUserProjectPermission.reduce((acc, permission) => {
        return { ...acc, [permission]: true }
      }, {})
      expect(result).toEqual(expected)
    })
  })
})
