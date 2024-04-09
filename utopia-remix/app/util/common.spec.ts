import { urlToRelative } from './common'

describe('urlToRelative', () => {
  it('should return the relative path of a URL', () => {
    const url = 'https://example.com:8080/some/path#hash?query=string'
    expect(urlToRelative(url)).toBe('/some/path#hash?query=string')
  })
})
