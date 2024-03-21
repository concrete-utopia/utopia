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
import { Status } from '../util/statusCodes'

describe('validators', () => {
  describe('validateProjectAccess', () => {
    let hasUserProjectPermissionMock: jest.SpyInstance

    afterEach(async () => {
      await truncateTables([
        prisma.projectID,
        prisma.projectAccess,
        prisma.project,
        prisma.userDetails,
        prisma.persistentSession,
      ])

      hasUserProjectPermissionMock.mockRestore()
    })

    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestSession(prisma, { key: 'bob-key', userId: 'bob' })
      await createTestSession(prisma, { key: 'alice-key', userId: 'alice' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })

      hasUserProjectPermissionMock = jest.spyOn(permissionsService, 'hasUserProjectPermission')
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

      const error = mustBeApiErrorValidator(got)
      expect(error.status).toBe(Status.BAD_REQUEST)
    })

    it('errors if the project does not exist', async () => {
      const got = await validateProjectAccess(perm, { getProjectId: () => 'unknown' })(
        newTestRequest(),
        {},
      )

      const error = mustBeApiErrorValidator(got)
      expect(error.status).toBe(Status.NOT_FOUND)
    })

    it('does nothing if the user is the owner', async () => {
      await createTestProjectAccess(prisma, {
        projectId: 'one',
        accessLevel: AccessLevel.PRIVATE,
      })
      const got = await validateProjectAccess(perm, { getProjectId: () => 'one' })(
        newTestRequest({ authCookie: 'bob-key' }),
        {},
      )
      expect(got.ok).toBe(true)
    })

    it('does nothing if the user has access', async () => {
      await createTestProjectAccess(prisma, {
        projectId: 'one',
        accessLevel: AccessLevel.COLLABORATIVE,
      })

      hasUserProjectPermissionMock.mockResolvedValue(true)

      const got = await validateProjectAccess(perm, { getProjectId: () => 'one' })(
        newTestRequest({ authCookie: 'alice-key' }),
        {},
      )
      expect(got.ok).toBe(true)
    })

    describe('when the user is not the owner', () => {
      describe('when the project is collaborative', () => {
        beforeEach(async () => {
          await createTestProjectAccess(prisma, {
            projectId: 'one',
            accessLevel: AccessLevel.COLLABORATIVE,
          })
        })

        describe('when access can be requested', () => {
          it('returns a 404 if the user cannot request access', async () => {
            hasUserProjectPermissionMock.mockResolvedValue(false)

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
              canRequestAccess: true,
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })

          it('returns a 403 if the user can request access', async () => {
            hasUserProjectPermissionMock.mockImplementation(
              async (_, __, p) => p === UserProjectPermission.CAN_REQUEST_ACCESS,
            )

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
              canRequestAccess: true,
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.FORBIDDEN)
          })
        })

        describe('when access cannot be requested', () => {
          it('returns a 404 even if the user can request access', async () => {
            hasUserProjectPermissionMock.mockImplementation(
              async (_, __, p) => p === UserProjectPermission.CAN_REQUEST_ACCESS,
            )

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })

          it('returns a 404', async () => {
            hasUserProjectPermissionMock.mockResolvedValue(false)

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })
        })
      })

      describe('when the project is private', () => {
        beforeEach(async () => {
          await createTestProjectAccess(prisma, {
            projectId: 'one',
            accessLevel: AccessLevel.PRIVATE,
          })
        })

        describe('when access can be requested', () => {
          it('returns a 404', async () => {
            hasUserProjectPermissionMock.mockResolvedValue(false)

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
              canRequestAccess: true,
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })

          it('returns a 404 even if the user can request access', async () => {
            hasUserProjectPermissionMock.mockImplementation(
              async (_, __, p) => p === UserProjectPermission.CAN_REQUEST_ACCESS,
            )

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
              canRequestAccess: true,
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })
        })

        describe('when access cannot be requested', () => {
          it('returns a 404', async () => {
            hasUserProjectPermissionMock.mockResolvedValue(false)

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })

          it('returns a 404 even if the user can request access', async () => {
            hasUserProjectPermissionMock.mockImplementation(
              async (_, __, p) => p === UserProjectPermission.CAN_REQUEST_ACCESS,
            )

            const got = await validateProjectAccess(perm, {
              getProjectId: () => 'one',
            })(newTestRequest({ authCookie: 'alice-key' }), {})

            const error = mustBeApiErrorValidator(got)
            expect(error.status).toBe(Status.NOT_FOUND)
          })
        })
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
