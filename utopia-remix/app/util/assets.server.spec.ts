import { newTestRequest } from '../test-util'
import { getProjectIdFromReferer, getProxyAssetPath } from './assets.server'
import * as validators from '../handlers/validators'

describe('assets', () => {
  let validatorsMock: jest.SpyInstance

  afterEach(() => {
    validatorsMock.mockRestore()
  })
  beforeEach(() => {
    validatorsMock = jest.spyOn(validators, 'canAccessProject')
  })

  describe('getProjectIdFromReferer', () => {
    it("returns null if there's no referer", async () => {
      const got = getProjectIdFromReferer(newTestRequest())
      expect(got).toBe(null)
    })
    it('returns null if the origin does not match', async () => {
      const got = getProjectIdFromReferer(
        newTestRequest({
          headers: {
            referer: 'http://foo.bar',
          },
        }),
      )
      expect(got).toBe(null)
    })
    it("returns null if the path is not a project's one", async () => {
      const got = getProjectIdFromReferer(
        newTestRequest({
          headers: {
            referer: 'http://localhost:8000/wrong',
          },
        }),
      )
      expect(got).toBe(null)
    })
    it('returns null if the tokenization is not correct', async () => {
      const got = getProjectIdFromReferer(
        newTestRequest({
          headers: {
            referer: 'http://localhost:8000/p/',
          },
        }),
      )
      expect(got).toBe(null)
    })
    it('returns the project id', async () => {
      const got = getProjectIdFromReferer(
        newTestRequest({
          headers: {
            referer: 'http://localhost:8000/p/foo-1-2-3/bar/baz',
          },
        }),
      )
      expect(got).toBe('foo')
    })
  })
  describe('getProxyAssetPath', () => {
    it('returns null if the extension is not allowed', async () => {
      const got = await getProxyAssetPath(newTestRequest({ path: '/foo.WRONG' }))
      expect(got).toBe(null)
    })
    it('returns null if the project id cannot be derived', async () => {
      const got = await getProxyAssetPath(
        newTestRequest({ path: '/foo.png', headers: { referer: 'http://localhost:8000/p' } }),
      )
      expect(got).toBe(null)
    })
    it('returns null if the project cannot be accessed', async () => {
      validatorsMock.mockResolvedValue({ ok: false })
      const got = await getProxyAssetPath(
        newTestRequest({
          path: '/foo.png',
          headers: { referer: 'http://localhost:8000/p/one' },
        }),
      )
      expect(got).toBe(null)
    })
    it('returns the proxied path', async () => {
      validatorsMock.mockResolvedValue({ ok: true })
      const got = await getProxyAssetPath(
        newTestRequest({
          path: '/foo.png',
          headers: { referer: 'http://localhost:8000/p/one' },
        }),
      )
      expect(got).toBe(`/p/one/foo.png`)
    })
  })
})
