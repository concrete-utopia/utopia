import { prisma } from '../db.server'

import { createTestSession, createTestUser, newTestRequest, truncateTables } from '../test-util'
import { loader as p_loader } from '../routes/p._index'
import { ServerEnvironment } from '../env.server'
import { authenticateUrl } from '../util/auth0.server'
import * as serverProxy from '../util/proxy.server'
import { urlToRelative } from '../util/common'

describe('handleEditorWithLogin', () => {
  let pageProxy: jest.SpyInstance
  beforeAll(async () => {
    await truncateTables([
      prisma.projectCollaborator,
      prisma.userDetails,
      prisma.persistentSession,
      prisma.projectID,
    ])
  })
  afterAll(async () => {
    jest.restoreAllMocks()
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
    pageProxy.mockClear()
  })

  beforeEach(async () => {
    await createTestUser(prisma, { id: 'user1' })
    await createTestSession(prisma, { key: 'the-key', userId: 'user1' })
    pageProxy = jest.spyOn(serverProxy, 'proxy')
    pageProxy.mockImplementation(async (req: Request) => new Response('ok'))
  })
  it('should redirect to auth0 login if not logged in', async () => {
    const request = newTestRequest({
      method: 'GET',
      path: '/p',
    })
    let response: Response | undefined = undefined
    try {
      response = (await p_loader({ request: request, params: {}, context: {} })) as Response
    } catch (err) {
      const redirectResponse = err as Response
      expect(redirectResponse.status).toBe(302)
      const redirectLocation = redirectResponse.headers.get('Location')
      const redirectToParam = getParamFromLoginURL(redirectLocation ?? '', 'redirectTo')
      expect(redirectToParam).toBe(urlToRelative(request.url))
    }
    expect(response).toBeUndefined()
  })

  it('should return the page if logged in', async () => {
    const request = newTestRequest({
      method: 'GET',
      path: '/p',
      authCookie: 'the-key',
    })
    const response = (await p_loader({ request: request, params: {}, context: {} })) as Response
    expect(pageProxy).toHaveBeenCalledWith(request, { rawOutput: true })
    expect(response.status).toBe(200)
  })

  it('should redirect to authenticate login with the fakeUser', async () => {
    const request = newTestRequest({
      method: 'GET',
      path: '/p',
      search: { fakeUser: 'alice' },
    })
    let response: Response | undefined = undefined
    try {
      response = (await p_loader({ request: request, params: {}, context: {} })) as Response
    } catch (err) {
      const redirectResponse = err as Response
      expect(redirectResponse.status).toBe(302)
      const redirectLocation = redirectResponse.headers.get('Location')
      expect(redirectLocation).toContain(authenticateUrl().toString())
      const redirectToParam = getDecodedParam(redirectLocation, 'redirectTo')
      const expectedRedirectUrl = new URL(request.url)
      expectedRedirectUrl.searchParams.delete('fakeUser')
      expect(redirectToParam).toBe(urlToRelative(expectedRedirectUrl.toString()))
      const codeParam = getDecodedParam(redirectLocation, 'code')
      expect(codeParam).toBe('alice')
    }
    expect(response).toBeUndefined()
  })
})

function getParamFromLoginURL(url: string, param: string): string | null {
  const redirect_uri = url.includes(`${ServerEnvironment.AUTH0_ENDPOINT}/authorize`)
    ? getDecodedParam(url ?? '', 'redirect_uri')
    : url
  return getDecodedParam(redirect_uri ?? '', param)
}

function getDecodedParam(url: string | null, param: string): string | null {
  const value = new URL(url ?? '').searchParams.get(param)
  return value ? decodeURIComponent(value) : null
}
