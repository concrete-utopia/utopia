import {
  preparsedLayer,
  traverseForPreparsedLayers,
  cssValueOnlyContainsComments,
} from './css-parser-utils'

describe('preparse layers', () => {
  it('preparses backgroundSize', () => {
    const testString = '/*auto auto*/ cover, /*auto,*/ /*auto*/ 100px auto'
    expect(traverseForPreparsedLayers(testString)).toMatchObject([
      preparsedLayer('auto auto', false),
      preparsedLayer('cover', true),
      preparsedLayer('auto', false),
      preparsedLayer('auto', false),
      preparsedLayer('100px auto', true),
    ])
  })

  it('handles a first non-commented item followed by commented items', () => {
    const testString = 'auto /* auto */ cover, /*auto,*/ /*auto*/ 100px auto'
    expect(traverseForPreparsedLayers(testString)).toMatchObject([
      preparsedLayer('auto', true),
      preparsedLayer('auto', false),
      preparsedLayer('cover', true),
      preparsedLayer('auto', false),
      preparsedLayer('auto', false),
      preparsedLayer('100px auto', true),
    ])
  })
})

describe('cssValueOnlyContainsComments', () => {
  it('identifies single comment', () => {
    const testStringOnlyComments = '/*green*/'
    expect(cssValueOnlyContainsComments(testStringOnlyComments)).toBeTruthy()
  })

  it('identifies multiple comments and no value', () => {
    const testStringOnlyComments = '/*red*/ /*green*/'
    expect(cssValueOnlyContainsComments(testStringOnlyComments)).toBeTruthy()
  })

  it('identifies one comment and one real value', () => {
    const testStringWithValue = 'red /*green*/'
    expect(cssValueOnlyContainsComments(testStringWithValue)).toBeFalsy()
  })

  it('identifies one comment and one real value 2', () => {
    const testStringWithValue = '/*red*/ green'
    expect(cssValueOnlyContainsComments(testStringWithValue)).toBeFalsy()
  })
})
