import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectAccessRequest,
  createTestUser,
  truncateTables,
} from '../test-util'
import { AccessRequestStatus } from '../types'
import {
  createAccessRequest,
  listProjectAccessRequests,
  updateAccessRequestStatus,
} from './projectAccessRequest.server'

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
        status: AccessRequestStatus.APPROVED,
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
})
