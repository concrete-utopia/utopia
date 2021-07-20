import { absolutePathFromRelativePath } from './path-utils'

describe('absolutePathFromRelativePath', () => {
  it('returns an absolute path directly', () => {
    const actualResult = absolutePathFromRelativePath('/src/app.js', false, '/src/card.js')
    expect(actualResult).toEqual('/src/card.js')
  })
  it('handles a basic relative path from a file', () => {
    const actualResult = absolutePathFromRelativePath('/src/app.js', false, './card.js')
    expect(actualResult).toEqual('/src/card.js')
  })
  it('handles a basic relative path from a directory', () => {
    const actualResult = absolutePathFromRelativePath('/src/', true, './card.js')
    expect(actualResult).toEqual('/src/card.js')
  })
  it('handles a weird relative path from a file', () => {
    const actualResult = absolutePathFromRelativePath(
      '/src/app.js',
      false,
      './other/../path/.././card.js',
    )
    expect(actualResult).toEqual('/src/card.js')
  })
  it('handles a weird relative path from a directory', () => {
    const actualResult = absolutePathFromRelativePath('/src/', true, './other/../path/.././card.js')
    expect(actualResult).toEqual('/src/card.js')
  })

  it('handles non-relative path', () => {
    const actualResult = absolutePathFromRelativePath('/src/', true, 'component-library')
    expect(actualResult).toEqual('component-library')
  })
})
