import { prisma } from '../db.server'
import { createTestSession, createTestUser, newTestRequest, truncateTables } from '../test-util'
import type { ApiResponse } from './api.server'
import { handle, requireUser, validationOk } from './api.server'
import { ApiError } from './errors'
import { Status } from './statusCodes'

describe('requireUser', () => {
  beforeEach(async () => {
    await createTestUser(prisma, { id: 'alice', name: 'Alice Alisson' })
    await createTestUser(prisma, { id: 'bob', name: 'Bob Bobson' })
    await createTestSession(prisma, { userId: 'alice', key: 'the-key' })
    await createTestSession(prisma, { userId: 'bob', key: 'another-key' })
    await createTestSession(prisma, {
      userId: 'user-that-does-not-exist',
      key: 'invalid-key',
    })
  })

  afterEach(async () => {
    await truncateTables([prisma.persistentSession, prisma.userDetails])
  })

  it('needs a cookie set', async () => {
    const req = newTestRequest()
    const fn = async () => requireUser(req)
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('missing session cookie')
  })

  it('needs a valid session cookie', async () => {
    const req = newTestRequest({ authCookie: 'wrong' })
    const fn = async () => requireUser(req)
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })

  it('needs a user for the session cookie', async () => {
    const req = newTestRequest({ authCookie: 'invalid-key' })
    const fn = async () => requireUser(req)
    await expect(fn).rejects.toThrow(ApiError)
    await expect(fn).rejects.toThrow('unauthorized')
  })

  it('returns the user details', async () => {
    let req = newTestRequest({ authCookie: 'the-key' })
    let got = await requireUser(req)
    expect(got.user_id).toBe('alice')
    expect(got.name).toBe('Alice Alisson')

    req = newTestRequest({ authCookie: 'another-key' })
    got = await requireUser(req)
    expect(got.user_id).toBe('bob')
    expect(got.name).toBe('Bob Bobson')
  })
})

describe('handle', () => {
  it('runs handler', async () => {
    const request = newTestRequest({ method: 'POST' })
    const response = await (handle(
      { request: request, params: {} },
      {
        POST: {
          handler: async () => {
            return { data: 'success' }
          },
          validator: async () => {
            return validationOk()
          },
        },
      },
    ) as Promise<ApiResponse<{ data: string }>>)
    const result = await response.json()
    expect(result).toEqual({
      data: 'success',
    })
  })
  it('fails on validator', async () => {
    const request = newTestRequest({ method: 'POST' })
    const response = await (handle(
      { request: request, params: {} },
      {
        POST: {
          handler: async () => {
            return { data: 'success' }
          },
          validator: async () => {
            throw new ApiError('failed', Status.BAD_REQUEST)
          },
        },
      },
    ) as Promise<ApiResponse<{ data: string }>>)
    const result = await response.json()
    expect(result).toEqual({
      error: 'Error',
      message: 'failed',
      status: Status.BAD_REQUEST,
    })
  })
})
