import { newTestRequest } from '../test-util'
import { handleLogin } from '../routes/login'
import { ServerEnvironment } from '../env.server'
import { authenticateUrl } from '../util/auth0.server'

describe('handleLogin', () => {
  it('should redirect to auth0 login with the correct redirectTo param', async () => {
    const request = newTestRequest({
      method: 'GET',
      path: '/login',
      search: { redirectTo: encodeURIComponent('/p/test-project') },
    })
    const response = await handleLogin(request, {})
    expect(response.status).toBe(302)
    const redirectLocation = response.headers.get('Location')
    const redirectToParam = getParamFromLoginURL(redirectLocation ?? '', 'redirectTo')
    expect(redirectToParam).toBe('/p/test-project')
  })

  it('should redirect to auth0 login without redirectTo if not specified', async () => {
    const request = newTestRequest({
      method: 'GET',
      path: '/login',
    })
    const response = await handleLogin(request, {})
    expect(response.status).toBe(302)
    const redirectLocation = response.headers.get('Location')
    const redirectToParam = getParamFromLoginURL(redirectLocation ?? '', 'redirectTo')
    expect(redirectToParam).toBe(null)
  })

  it('should redirect to authenticate login with the correct redirectTo param and fakeUser', async () => {
    const request = newTestRequest({
      method: 'GET',
      path: '/login',
      search: { redirectTo: encodeURIComponent('/p/test-project'), fakeUser: 'alice' },
    })
    const response = await handleLogin(request, {})
    expect(response.status).toBe(302)
    const redirectLocation = response.headers.get('Location')
    expect(redirectLocation).toContain(authenticateUrl().toString())
    const redirectToParam = getDecodedParam(redirectLocation, 'redirectTo')
    expect(redirectToParam).toBe('/p/test-project')
    const codeParam = getDecodedParam(redirectLocation, 'code')
    expect(codeParam).toBe('alice')
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
