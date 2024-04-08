import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectAccessRequest,
  createTestProjectCollaborator,
  createTestUser,
  truncateTables,
} from '../test-util'
import { AccessRequestStatus, UserProjectRole } from '../types'
import {
  createAccessRequest,
  destroyAccessRequest,
  listProjectAccessRequests,
  updateAccessRequestStatus,
} from './projectAccessRequest.server'
import * as permissionsService from '../services/permissionsService.server'

describe('projectAccessRequest', () => {
  describe('createAccessRequest', () => {
    afterEach(async () => {
      await truncateTables([
        prisma.projectID,
        prisma.projectAccessRequest,
        prisma.projectCollaborator,
        prisma.project,
        prisma.userDetails,
      ])
    })
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestUser(prisma, { id: 'that-guy' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
    })
    it('requires an existing project', async () => {
      const fn = async () => createAccessRequest({ projectId: 'unknown', userId: 'that-guy' })
      await expect(fn).rejects.toThrow('project not found')
    })
    it('requires a non-deleted project', async () => {
      await createTestProject(prisma, { id: 'deleted', ownerId: 'bob', deleted: true })
      const fn = async () => createAccessRequest({ projectId: 'deleted', userId: 'that-guy' })
      await expect(fn).rejects.toThrow('project not found')
    })
    describe('when the project is ok', () => {
      it('does nothing if the user is the owner', async () => {
        await createAccessRequest({ projectId: 'one', userId: 'bob' })
        const requests = await prisma.projectAccessRequest.count()
        expect(requests).toBe(0)
      })
      it('creates the new request in a pending state', async () => {
        await createAccessRequest({ projectId: 'one', userId: 'that-guy' })
        const requests = await prisma.projectAccessRequest.findMany()
        expect(requests.length).toBe(1)
        expect(requests[0].project_id).toBe('one')
        expect(requests[0].user_id).toBe('that-guy')
        expect(requests[0].status).toBe(AccessRequestStatus.PENDING)
        expect(requests[0].token.length).toBeGreaterThan(0)
      })
      it('errors if the request is for a non existing user', async () => {
        const fn = async () =>
          createAccessRequest({ projectId: 'one', userId: 'a-guy-that-doesnt-exist' })
        await expect(fn).rejects.toThrow()
      })
      it('does nothing if the request already exists for the same user', async () => {
        await createTestProjectAccessRequest(prisma, {
          projectId: 'one',
          userId: 'that-guy',
          status: AccessRequestStatus.REJECTED,
          token: 'something',
        })
        await createAccessRequest({ projectId: 'one', userId: 'that-guy' })
        const requests = await prisma.projectAccessRequest.findMany()
        expect(requests.length).toBe(1)
        expect(requests[0].project_id).toBe('one')
        expect(requests[0].user_id).toBe('that-guy')
        expect(requests[0].status).toBe(AccessRequestStatus.REJECTED)
        expect(requests[0].token.length).toBeGreaterThan(0)
      })
    })
  })

  describe('updateAccessRequestStatus', () => {
    let revokeAllRolesFromUserMock: jest.SpyInstance
    let grantProjectRoleToUserMock: jest.SpyInstance

    afterEach(async () => {
      await truncateTables([
        prisma.projectID,
        prisma.projectAccessRequest,
        prisma.projectCollaborator,
        prisma.project,
        prisma.userDetails,
      ])

      revokeAllRolesFromUserMock.mockRestore()
      grantProjectRoleToUserMock.mockRestore()
    })
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestUser(prisma, { id: 'that-guy' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'alice' })

      revokeAllRolesFromUserMock = jest.spyOn(permissionsService, 'revokeAllRolesFromUser')
      grantProjectRoleToUserMock = jest.spyOn(permissionsService, 'grantProjectRoleToUser')
    })
    it('requires an existing project', async () => {
      const fn = async () =>
        updateAccessRequestStatus({
          projectId: 'unknown',
          ownerId: 'that-guy',
          token: 'something',
          status: AccessRequestStatus.APPROVED,
        })
      await expect(fn).rejects.toThrow('project not found')
    })
    it('requires the user to be the owner of the project', async () => {
      const fn = async () =>
        updateAccessRequestStatus({
          projectId: 'one',
          ownerId: 'alice',
          token: 'something',
          status: AccessRequestStatus.APPROVED,
        })
      await expect(fn).rejects.toThrow('project not found')
    })
    it('errors if the request is not find by its token', async () => {
      await createTestProjectAccessRequest(prisma, {
        projectId: 'one',
        userId: 'alice',
        token: 'something',
        status: AccessRequestStatus.PENDING,
      })
      const fn = async () =>
        updateAccessRequestStatus({
          projectId: 'one',
          ownerId: 'bob',
          token: 'WRONG',
          status: AccessRequestStatus.APPROVED,
        })
      await expect(fn).rejects.toThrow('not found')
    })
    it("updates the request's status", async () => {
      await createTestProjectAccessRequest(prisma, {
        projectId: 'one',
        userId: 'alice',
        token: 'something',
        status: AccessRequestStatus.PENDING,
      })
      await updateAccessRequestStatus({
        projectId: 'one',
        ownerId: 'bob',
        token: 'something',
        status: AccessRequestStatus.APPROVED,
      })
      const requests = await prisma.projectAccessRequest.findMany()
      expect(requests.length).toBe(1)
      expect(requests[0].project_id).toBe('one')
      expect(requests[0].user_id).toBe('alice')
      expect(requests[0].status).toBe(AccessRequestStatus.APPROVED)
    })
    it('adds the user to the collaborators if approved', async () => {
      const existingCollabs = await prisma.projectCollaborator.findMany({
        where: { project_id: 'one' },
      })
      expect(existingCollabs.length).toBe(0)

      await createTestProjectAccessRequest(prisma, {
        projectId: 'one',
        userId: 'alice',
        token: 'something',
        status: AccessRequestStatus.PENDING,
      })
      await updateAccessRequestStatus({
        projectId: 'one',
        ownerId: 'bob',
        token: 'something',
        status: AccessRequestStatus.APPROVED,
      })

      const collabs = await prisma.projectCollaborator.findMany({ where: { project_id: 'one' } })
      expect(collabs.length).toBe(1)
      expect(collabs[0].user_id).toBe('alice')

      expect(grantProjectRoleToUserMock).toHaveBeenCalledWith(
        'one',
        'alice',
        UserProjectRole.VIEWER,
      )
      expect(revokeAllRolesFromUserMock).not.toHaveBeenCalled()
    })
    it('removes the user from the collaborators if rejected', async () => {
      await createTestProjectCollaborator(prisma, { projectId: 'one', userId: 'alice' })
      const existingCollabs = await prisma.projectCollaborator.findMany({
        where: { project_id: 'one' },
      })
      expect(existingCollabs.length).toBe(1)

      await createTestProjectAccessRequest(prisma, {
        projectId: 'one',
        userId: 'alice',
        token: 'something',
        status: AccessRequestStatus.PENDING,
      })
      await updateAccessRequestStatus({
        projectId: 'one',
        ownerId: 'bob',
        token: 'something',
        status: AccessRequestStatus.REJECTED,
      })

      const collabs = await prisma.projectCollaborator.findMany({ where: { project_id: 'one' } })
      expect(collabs.length).toBe(0)

      expect(grantProjectRoleToUserMock).not.toHaveBeenCalled()
      expect(revokeAllRolesFromUserMock).toHaveBeenCalledWith('one', 'alice')
    })
  })

  describe('listProjectAccessRequests', () => {
    afterEach(async () => {
      await truncateTables([
        prisma.projectID,
        prisma.projectAccessRequest,
        prisma.project,
        prisma.userDetails,
      ])
    })
    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestUser(prisma, { id: 'p1', name: 'person1' })
      await createTestUser(prisma, { id: 'p2', name: 'person2' })
      await createTestUser(prisma, { id: 'p3', name: 'person3' })
      await createTestUser(prisma, { id: 'p4', name: 'person4' })
      await createTestUser(prisma, { id: 'p5', name: 'person5' })
      await createTestUser(prisma, { id: 'p6', name: 'person5' })
      await createTestUser(prisma, { id: 'p7', name: 'person5' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
      await createTestProject(prisma, { id: 'three', ownerId: 'alice' })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p1',
        projectId: 'two',
        status: AccessRequestStatus.REJECTED,
        token: 'test1',
      })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p2',
        projectId: 'two',
        status: AccessRequestStatus.PENDING,
        token: 'test2',
      })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p3',
        projectId: 'two',
        status: AccessRequestStatus.PENDING,
        token: 'test3',
      })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p4',
        projectId: 'two',
        status: AccessRequestStatus.REJECTED,
        token: 'test4',
      })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p5',
        projectId: 'three',
        status: AccessRequestStatus.PENDING,
        token: 'test5',
      })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p6',
        projectId: 'three',
        status: AccessRequestStatus.PENDING,
        token: 'test6',
      })
      await createTestProjectAccessRequest(prisma, {
        userId: 'p7',
        projectId: 'three',
        status: AccessRequestStatus.REJECTED,
        token: 'test7',
      })
    })

    it('requires an existing project', async () => {
      const fn = async () =>
        listProjectAccessRequests({
          projectId: 'unknown',
          userId: 'bob',
        })
      await expect(fn).rejects.toThrow('project not found')
    })
    it('requires the user to own the project', async () => {
      const fn = async () =>
        listProjectAccessRequests({
          projectId: 'two',
          userId: 'bob',
        })
      await expect(fn).rejects.toThrow('project not found')
    })
    it('returns an empty list if there are no requests', async () => {
      const got = await listProjectAccessRequests({
        projectId: 'one',
        userId: 'bob',
      })
      expect(got.length).toBe(0)
    })
    it('returns the list of requests including their user details', async () => {
      const got = await listProjectAccessRequests({
        projectId: 'two',
        userId: 'alice',
      })
      expect(got.length).toBe(4)
      expect(got[0].token).toBe('test1')
      expect(got[0].user_id).toBe('p1')
      expect(got[0].User?.name).toBe('person1')
      expect(got[1].token).toBe('test2')
      expect(got[1].user_id).toBe('p2')
      expect(got[1].User?.name).toBe('person2')
      expect(got[2].token).toBe('test3')
      expect(got[2].user_id).toBe('p3')
      expect(got[2].User?.name).toBe('person3')
      expect(got[3].token).toBe('test4')
      expect(got[3].user_id).toBe('p4')
      expect(got[3].User?.name).toBe('person4')
    })
  })

  describe('destroyAccessRequest', () => {
    let revokeAllRolesFromUserMock: jest.SpyInstance

    afterEach(async () => {
      await truncateTables([
        prisma.projectID,
        prisma.projectAccessRequest,
        prisma.projectCollaborator,
        prisma.project,
        prisma.userDetails,
      ])

      revokeAllRolesFromUserMock.mockRestore()
    })

    beforeEach(async () => {
      await createTestUser(prisma, { id: 'bob' })
      await createTestUser(prisma, { id: 'alice' })
      await createTestUser(prisma, { id: 'carol' })
      await createTestUser(prisma, { id: 'dorothy' })
      await createTestProject(prisma, { id: 'one', ownerId: 'bob' })
      await createTestProject(prisma, { id: 'two', ownerId: 'alice' })
      await createTestProject(prisma, { id: 'three', ownerId: 'alice' })
      await createTestProjectAccessRequest(prisma, {
        projectId: 'two',
        token: 'token1',
        userId: 'carol',
        status: AccessRequestStatus.PENDING,
      })
      await createTestProjectAccessRequest(prisma, {
        projectId: 'three',
        token: 'token2',
        userId: 'dorothy',
        status: AccessRequestStatus.PENDING,
      })
      await createTestProjectAccessRequest(prisma, {
        projectId: 'two',
        token: 'token3',
        userId: 'bob',
        status: AccessRequestStatus.PENDING,
      })

      revokeAllRolesFromUserMock = jest.spyOn(permissionsService, 'revokeAllRolesFromUser')
    })
    it('errors if the project is not found', async () => {
      const fn = async () =>
        destroyAccessRequest({ projectId: 'UNKNOWN', ownerId: 'bob', token: 'token1' })
      await expect(fn).rejects.toThrow('Record to delete does not exist')
    })
    it('errors if the project is not owned by the user', async () => {
      const fn = async () =>
        destroyAccessRequest({ projectId: 'two', ownerId: 'bob', token: 'token1' })
      await expect(fn).rejects.toThrow('Record to delete does not exist')
    })
    it('errors if the token is not found', async () => {
      const fn = async () =>
        destroyAccessRequest({ projectId: 'two', ownerId: 'alice', token: 'tokenWRONG' })
      await expect(fn).rejects.toThrow('Record to delete does not exist')
    })
    it('errors if the token exists but not on that project', async () => {
      const fn = async () =>
        destroyAccessRequest({ projectId: 'two', ownerId: 'alice', token: 'token2' })
      await expect(fn).rejects.toThrow('Record to delete does not exist')
    })
    it('deletes the request and revokes roles on FGA', async () => {
      const existing = await prisma.projectAccessRequest.findMany({ where: { project_id: 'two' } })
      expect(existing.length).toBe(2)
      await destroyAccessRequest({ projectId: 'two', ownerId: 'alice', token: 'token1' })
      const current = await prisma.projectAccessRequest.findMany({
        where: { project_id: 'two' },
      })
      expect(current.length).toBe(1)
      expect(current[0].user_id).toBe('bob')

      expect(revokeAllRolesFromUserMock).toHaveBeenCalledWith('two', 'carol')
    })
    it('rolls back if the FGA revoking fails', async () => {
      revokeAllRolesFromUserMock.mockImplementation(() => {
        throw new Error('boom')
      })

      const existing = await prisma.projectAccessRequest.findMany({ where: { project_id: 'two' } })
      expect(existing.length).toBe(2)

      const fn = async () =>
        await destroyAccessRequest({ projectId: 'two', ownerId: 'alice', token: 'token1' })
      await expect(fn).rejects.toThrow('boom')

      const current = await prisma.projectAccessRequest.findMany({
        where: { project_id: 'two' },
      })
      expect(current.length).toBe(2)
    })
    it('removes the user from the collaborators, if present', async () => {
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'carol' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'bob' })
      await createTestProjectCollaborator(prisma, { projectId: 'two', userId: 'alice' })

      const existingCollaborators = await prisma.projectCollaborator.findMany({
        where: { project_id: 'two' },
      })
      expect(existingCollaborators.length).toBe(3)
      expect(existingCollaborators[0].user_id).toBe('carol')
      expect(existingCollaborators[1].user_id).toBe('bob')
      expect(existingCollaborators[2].user_id).toBe('alice')

      await destroyAccessRequest({ projectId: 'two', ownerId: 'alice', token: 'token1' })

      const currentCollaborators = await prisma.projectCollaborator.findMany({
        where: { project_id: 'two' },
      })
      expect(currentCollaborators.length).toBe(2)
      expect(currentCollaborators[0].user_id).toBe('bob')
      expect(currentCollaborators[1].user_id).toBe('alice')
    })
  })
})
