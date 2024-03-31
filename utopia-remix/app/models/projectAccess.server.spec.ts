jest.mock('@openfga/sdk')
import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectAccess,
  createTestUser,
  truncateTables,
} from '../test-util'
import { createProjectAccess, setProjectAccess } from './projectAccess.server'
import * as permissionsService from '../services/permissionsService.server'

describe('projectAccess model', () => {
  afterAll(async () => {
    jest.restoreAllMocks()
  })

  describe('setProjectAccess', () => {
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
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'bob' })
      await createTestProjectAccess(prisma, { projectId: 'one', accessLevel: 0 })
      await createTestProjectAccess(prisma, { projectId: 'two', accessLevel: 1 })
      jest.spyOn(permissionsService, 'setProjectAccess').mockResolvedValue()
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
      jest.spyOn(permissionsService, 'setProjectAccess').mockRestore()
    })
    it('sets the access level for a project', async () => {
      await setProjectAccess({ projectId: 'one', accessLevel: 1 })
      const projectAccess = await prisma.projectAccess.findFirst({
        where: { project_id: 'one' },
      })
      expect(projectAccess?.access_level).toEqual(1)
      expect(permissionsService.setProjectAccess).toHaveBeenCalledWith('one', 1)
    })
    it('updates the modified_at field', async () => {
      await setProjectAccess({ projectId: 'one', accessLevel: 1 })
      const projectAccess = await prisma.projectAccess.findFirst({
        where: { project_id: 'one' },
      })
      expect(projectAccess?.modified_at).not.toBeNull()
    })
    it('sets the access level on the project model itself', async () => {
      await setProjectAccess({ projectId: 'one', accessLevel: 1 })
      const project = await prisma.project.findFirst({
        where: { proj_id: 'one' },
        include: { ProjectAccess: true },
      })
      expect(project?.ProjectAccess?.access_level).toEqual(1)
    })
  })
})

describe('create project access', () => {
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
  beforeEach(async () => {
    await createTestUser(prisma, { id: 'bob' })
    await createTestUser(prisma, { id: 'alice' })
    await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
    await createTestProject(prisma, { id: 'two', ownerId: 'bob' })
    jest.spyOn(permissionsService, 'setProjectAccess').mockResolvedValue()
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
    jest.spyOn(permissionsService, 'setProjectAccess').mockRestore()
  })
  it('creates the access level for a project', async () => {
    await createProjectAccess({ projectId: 'one', accessLevel: 1, creatorId: 'alice' })
    const projectAccess = await prisma.projectAccess.findFirst({
      where: { project_id: 'one' },
    })
    expect(projectAccess?.access_level).toEqual(1)
    expect(permissionsService.setProjectAccess).toHaveBeenCalledWith('one', 1)
  })
  it('doesnt override the access level if it already exists', async () => {
    await createProjectAccess({ projectId: 'one', accessLevel: 1, creatorId: 'alice' })
    await createProjectAccess({ projectId: 'one', accessLevel: 0, creatorId: 'alice' })
    const projectAccess = await prisma.projectAccess.findFirst({
      where: { project_id: 'one' },
    })
    expect(projectAccess?.access_level).toEqual(1)
    expect(permissionsService.setProjectAccess).not.toHaveBeenCalledWith('one', 0)
    expect(permissionsService.setProjectAccess).toHaveBeenCalledTimes(1)
  })
})
