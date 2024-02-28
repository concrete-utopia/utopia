import { prisma } from '../db.server'
import { handleChangeProjectAccess } from '../routes/internal.projects.$id.access'
import {
  createTestProject,
  createTestSession,
  createTestUser,
  newTestRequest,
  truncateTables,
} from '../test-util'
import { ApiError } from '../util/errors'
import * as permissionsService from '../services/permissionsService.server'
import { AccessLevel } from '../types'

describe('handleChangeAccess', () => {
  afterEach(async () => {
    await truncateTables([
      prisma.userDetails,
      prisma.persistentSession,
      prisma.project,
      prisma.projectID,
      prisma.projectAccess,
    ])
    jest.restoreAllMocks()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'foo' })
    await createTestSession(prisma, { key: 'the-key', userId: 'foo' })
    await createTestProject(prisma, { id: 'one', ownerId: 'foo', title: 'project-one' })
    jest.spyOn(permissionsService, 'setProjectAccess').mockResolvedValue()
  })

  it('changes access level to private', async () => {
    const formData = new FormData()
    formData.append('accessLevel', '0')
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
    expect(projectAccess?.access_level).toEqual(0)
    expect(permissionsService.setProjectAccess).toHaveBeenCalledWith('one', AccessLevel.PRIVATE)
  })

  it('doesnt accept wrong access level', async () => {
    const formData = new FormData()
    formData.append('accessLevel', '3')
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
})
