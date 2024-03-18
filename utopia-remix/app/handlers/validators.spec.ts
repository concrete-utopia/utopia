import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectAccess,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { validateProjectAccess } from './validators'
import * as permissionsService from '../services/permissionsService.server'
import { AccessLevel, UserProjectPermission } from '../types'
import { ApiError } from '../util/errors'
import type { ValidationResult } from '../util/api.server'

describe('validators', () => {
  describe('validateProjectAccess', () => {
    let hasUserProjectPermission: jest.SpyInstance
    let userHasRequestProjectAccessPermission: jest.SpyInstance

    afterEach(async () => {
      await truncateTables([
        prisma.projectID,
        prisma.projectAccess,
        prisma.project,
        prisma.userDetails,
        prisma.persistentSession,
      ])
      hasUserProjectPermission.mockClear()
      jest.spyOn(permissionsService, 'hasUserProjectPermission').mockRestore()
      userHasRequestProjectAccessPermission.mockClear()
      jest.spyOn(permissionsService, 'userHasRequestProjectAccessPermission').mockRestore()
    })
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestSession(prisma, { key: 'bob-key', userId: 'bob' })
      await createTestSession(prisma, { key: 'alice-key', userId: 'alice' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })

      hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
      userHasRequestProjectAccessPermission = jest.spyOn(
        permissionsService,
        'userHasRequestProjectAccessPermission',
      )
    })

    const perm = UserProjectPermission.CAN_COMMENT_PROJECT // just any of the permissions is fine

    afterAll(async () => {
      jest.restoreAllMocks()
    })

    it('errors if the project id is not passed', async () => {
      const got = await validateProjectAccess(perm, { getProjectId: () => null })(
        newTestRequest(),
        {},
      )
      expect(got.ok).toBe(false)
    })
    it('errors if the project does not exist', async () => {
      const got = await validateProjectAccess(perm, { getProjectId: () => 'unknown' })(
        newTestRequest(),
        {},
      )
      expect(got.ok).toBe(false)
    })
    it('does nothing if the user is the owner', async () => {
      const got = await validateProjectAccess(perm, { getProjectId: () => 'one' })(
        newTestRequest({ authCookie: 'bob-key' }),
        {},
      )
      expect(got.ok).toBe(true)
    })
    it('does nothing if the user has access on a collaborative project', async () => {
      await createTestProjectAccess(prisma, {
        projectId: 'one',
        accessLevel: AccessLevel.COLLABORATIVE,
      })

      jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(true)
      hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')

      const got = await validateProjectAccess(perm, { getProjectId: () => 'one' })(
        newTestRequest({ authCookie: 'alice-key' }),
        {},
      )
      expect(got.ok).toBe(true)
    })
    it("does nothing if the user has access but it's not a collaborative project", async () => {
      await createTestProjectAccess(prisma, {
        projectId: 'one',
        accessLevel: AccessLevel.PRIVATE,
      })

      jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(true)
      hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')

      const got = await validateProjectAccess(perm, { getProjectId: () => 'one' })(
        newTestRequest({ authCookie: 'alice-key' }),
        {},
      )
      const error = mustBeApiErrorValidator(got)
      expect(error.status).toBe(404)
    })
    describe('when access can be requested', () => {
      it('returns a 404 if the project is not collaborative', async () => {
        await createTestProjectAccess(prisma, {
          projectId: 'one',
          accessLevel: AccessLevel.PRIVATE,
        })

        jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(false)
        hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')

        const got = await validateProjectAccess(perm, {
          getProjectId: () => 'one',
          canRequestAccess: true,
        })(newTestRequest({ authCookie: 'alice-key' }), {})
        expect(got.ok).toBe(false)
      })
      it('returns a 403 if the user can request access', async () => {
        await createTestProjectAccess(prisma, {
          projectId: 'one',
          accessLevel: AccessLevel.COLLABORATIVE,
        })

        // no access
        jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(false)
        hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
        // but can request it
        jest
          .spyOn(permissionsService, 'userHasRequestProjectAccessPermission')
          .mockResolvedValue(true)
        userHasRequestProjectAccessPermission = jest.spyOn(
          permissionsService,
          'userHasRequestProjectAccessPermission',
        )

        const got = await validateProjectAccess(perm, {
          getProjectId: () => 'one',
          canRequestAccess: true,
        })(newTestRequest({ authCookie: 'alice-key' }), {})
        const error = mustBeApiErrorValidator(got)
        expect(error.status).toBe(403)
      })
      it('returns a 404 if the user cannot request access', async () => {
        await createTestProjectAccess(prisma, {
          projectId: 'one',
          accessLevel: AccessLevel.COLLABORATIVE,
        })

        // no access
        jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(false)
        hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
        // and can't request it
        jest
          .spyOn(permissionsService, 'userHasRequestProjectAccessPermission')
          .mockResolvedValue(false)
        userHasRequestProjectAccessPermission = jest.spyOn(
          permissionsService,
          'userHasRequestProjectAccessPermission',
        )

        const got = await validateProjectAccess(perm, {
          getProjectId: () => 'one',
          canRequestAccess: true,
        })(newTestRequest({ authCookie: 'alice-key' }), {})
        const error = mustBeApiErrorValidator(got)
        expect(error.status).toBe(404)
      })
    })
    describe('when access cannot be requested', () => {
      it('returns a 404 if the project is not collaborative', async () => {
        await createTestProjectAccess(prisma, {
          projectId: 'one',
          accessLevel: AccessLevel.PRIVATE,
        })

        // no access
        jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(false)
        hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
        // and can't request it
        jest
          .spyOn(permissionsService, 'userHasRequestProjectAccessPermission')
          .mockResolvedValue(false)
        userHasRequestProjectAccessPermission = jest.spyOn(
          permissionsService,
          'userHasRequestProjectAccessPermission',
        )

        const got = await validateProjectAccess(perm, {
          getProjectId: () => 'one',
        })(newTestRequest({ authCookie: 'alice-key' }), {})
        const error = mustBeApiErrorValidator(got)
        expect(error.status).toBe(404)
      })
      it('returns a 404 if the project collaborative but the user has no access', async () => {
        await createTestProjectAccess(prisma, {
          projectId: 'one',
          accessLevel: AccessLevel.COLLABORATIVE,
        })

        // no access
        jest.spyOn(permissionsService, 'hasUserProjectPermission').mockResolvedValue(false)
        hasUserProjectPermission = jest.spyOn(permissionsService, 'hasUserProjectPermission')
        // and can't request it
        jest
          .spyOn(permissionsService, 'userHasRequestProjectAccessPermission')
          .mockResolvedValue(false)
        userHasRequestProjectAccessPermission = jest.spyOn(
          permissionsService,
          'userHasRequestProjectAccessPermission',
        )

        const got = await validateProjectAccess(perm, {
          getProjectId: () => 'one',
        })(newTestRequest({ authCookie: 'alice-key' }), {})
        const error = mustBeApiErrorValidator(got)
        expect(error.status).toBe(404)
      })
    })
  })
})

function mustBeApiErrorValidator(got: ValidationResult): ApiError {
  if (got.ok) {
    throw new Error('expected validation error')
  }
  if (!(got.error instanceof ApiError)) {
    throw new Error('expected api error')
  }
  return got.error
}
