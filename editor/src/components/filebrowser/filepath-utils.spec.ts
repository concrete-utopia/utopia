import { getFilePathToImport } from './filepath-utils'

describe('getFilePathToImport', () => {
  it('creates importline string when both files are on root', () => {
    const from = '/hello-button'
    const target = '/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = './hello-button'
    expect(actualResult).toEqual(expectedResult)
  })
  it('creates importline string when target is on root, inserted file is in folders', () => {
    const from = '/src/components/button'
    const target = '/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = './src/components/button'
    expect(actualResult).toEqual(expectedResult)
  })
  it('creates importline string when both files are in the same folder', () => {
    const from = '/src/nice-button'
    const target = '/src/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = './nice-button'
    expect(actualResult).toEqual(expectedResult)
  })
  it('creates importline string when both files are inside src folder, but more complex', () => {
    const from = '/src/nice-button'
    const target = '/src/uifiles/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = '../nice-button'
    expect(actualResult).toEqual(expectedResult)
  })
  it('creates importline string when target is somewhere inside src folder, inserted file is not', () => {
    const from = '/components/button'
    const target = '/src/uifiles/final/folder2/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = '../../../../components/button'
    expect(actualResult).toEqual(expectedResult)
  })
  it('creates importline when the filepaths have same folder name in the middle part', () => {
    const from = '/components/final/button'
    const target = '/src/final/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = '../../components/final/button'
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not change import which is not a file path', () => {
    const from = 'utopia-api'
    const target = '/src/app.js'
    const actualResult = getFilePathToImport(from, target)

    const expectedResult = 'utopia-api'
    expect(actualResult).toEqual(expectedResult)
  })
})
