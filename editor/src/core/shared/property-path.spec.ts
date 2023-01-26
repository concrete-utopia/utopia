import { create } from './property-path'

describe('create', () => {
  it('distinguishes between paths containing strings and numbers', () => {
    const firstEndsWithString = create('path', '0')
    const firstEndsWithNumber = create('path', 0)
    const secondEndsWithString = create('path', '0')
    const secondEndsWithNumber = create('path', 0)
    expect(firstEndsWithString).toStrictEqual({ propertyElements: ['path', '0'] })
    expect(firstEndsWithNumber).toStrictEqual({ propertyElements: ['path', 0] })
    expect(secondEndsWithString).toStrictEqual({ propertyElements: ['path', '0'] })
    expect(secondEndsWithNumber).toStrictEqual({ propertyElements: ['path', 0] })
  })
})
