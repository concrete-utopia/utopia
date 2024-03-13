import { prisma } from '../db.server'
import {
  createTestProject,
  createTestProjectAccessRequest,
  createTestUser,
  truncateTables,
} from '../test-util'
import { AccessRequestStatus } from '../types'
import { createAccessRequest, updateAccessRequestStatus } from './projectAccessRequest.server'

describe('projectAccessRequest', () => {
  describe('createAccessRequest', () => {
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
      await expect(fn).rejects.toThrow('request not found')
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
  })
})
