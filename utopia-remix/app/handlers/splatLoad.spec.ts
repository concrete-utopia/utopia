import { newTestRequest } from '../test-util'
import { handleSplatLoad } from './splatLoad'
import * as proxyUtil from '../util/proxy.server'

describe('handleSplatLoad', () => {
  let proxyMock: jest.SpyInstance

  afterEach(async () => {
    proxyMock.mockRestore()
  })

  beforeEach(async () => {
    proxyMock = jest.spyOn(proxyUtil, 'proxy')
  })

  it('errors if the path does not have an allowed extension', async () => {
    const fn = (path: string) => async () =>
      handleSplatLoad(newTestRequest({ method: 'GET', path: path }))
    await expect(fn('')).rejects.toThrow('invalid extension')
    await expect(fn('/foo')).rejects.toThrow('invalid extension')
    await expect(fn('/foo/bar')).rejects.toThrow('invalid extension')
    await expect(fn('/foo/bar/wrong.zip')).rejects.toThrow('invalid extension')
  })

  it('forwards the request to the desired file', async () => {
    proxyMock.mockImplementation((req) => {
      return req.url
    })
    const fn = (path: string) => handleSplatLoad(newTestRequest({ method: 'GET', path: path }))

    expect(await fn('foo.png')).toBe('http://localhost:8000/foo.png')
    expect(await fn('foo/bar/baz.png')).toBe('http://localhost:8000/foo/bar/baz.png')
    expect(await fn('foo/bar/baz.jpg')).toBe('http://localhost:8000/foo/bar/baz.jpg')
  })
})
