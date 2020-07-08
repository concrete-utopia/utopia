import { preparsedLayer, traverseForPreparsedLayers } from './css-parser-utils'

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
})
