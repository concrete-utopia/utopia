import { correctProjectContentsPath } from '../core/model/project-file-utils'

describe('correctProjectContentsPath', () => {
  it('returns the path unchanged if it includes the first forward slash', () => {
    const actualResult = correctProjectContentsPath(correctProjectContentsPath('/src/app.js'))
    const expectedResult = '/src/app.js'
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the path unchanged if it includes the first forward slash with a single element path', () => {
    const actualResult = correctProjectContentsPath(correctProjectContentsPath('/app.js'))
    const expectedResult = '/app.js'
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the path with the forward slash prefixed', () => {
    const actualResult = correctProjectContentsPath(correctProjectContentsPath('src/app.js'))
    const expectedResult = '/src/app.js'
    expect(actualResult).toEqual(expectedResult)
  })
  it('returns the path with the forward slash prefixed with a single element path', () => {
    const actualResult = correctProjectContentsPath(correctProjectContentsPath('app.js'))
    const expectedResult = '/app.js'
    expect(actualResult).toEqual(expectedResult)
  })
})
