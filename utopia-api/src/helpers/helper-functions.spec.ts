import { cssBorderToBoxShadowString } from './helper-functions'

describe('Converting a border to a shadow', () => {
  it('Handles a single numeric border', () => {
    const expected = `inset 0 0 0 10px blue`
    const actual = cssBorderToBoxShadowString('solid', 10, 'blue')
    expect(actual).toEqual(expected)
  })

  it('Handles a single string border', () => {
    const expected = `inset 0 0 0 10px blue`
    const actual = cssBorderToBoxShadowString('solid', '10px', 'blue')
    expect(actual).toEqual(expected)
  })

  it('Handles a split border', () => {
    const expected = [
      `inset 0 1px 0 0 blue`,
      `inset -2px 0 0 0 blue`,
      `inset 0 -3px 0 0 blue`,
      `inset 4px 0 0 0 blue`,
    ].join(',\n')
    const actual = cssBorderToBoxShadowString('solid', [1, '2px', 3, '4px'], 'blue')
    expect(actual).toEqual(expected)
  })

  it('Returns an empty string on a null border width', () => {
    const expected = ''
    const actual = cssBorderToBoxShadowString('solid', undefined, 'blue')
    expect(actual).toEqual(expected)
  })

  it('Returns an empty string on a split border width with the wrong number of parts', () => {
    const expected = ''
    const actual = cssBorderToBoxShadowString('solid', [1, '2px', 3] as any, 'blue')
    expect(actual).toEqual(expected)
  })

  it('Returns an empty string on a null border color', () => {
    const expected = ''
    const actual = cssBorderToBoxShadowString('solid', 1)
    expect(actual).toEqual(expected)
  })

  it('Defaults to solid if no border style is supplied', () => {
    const expected = `inset 0 0 0 10px blue`
    const actual = cssBorderToBoxShadowString(undefined, '10px', 'blue')
    expect(actual).toEqual(expected)
  })

  it('Returns an empty string if no params are supplied', () => {
    const expected = ''
    const actual = cssBorderToBoxShadowString()
    expect(actual).toEqual(expected)
  })

  it('Is not enabled if "none" border style is supplied', () => {
    const expected = `/*inset 0 0 0 10px blue*/`
    const actual = cssBorderToBoxShadowString('none', '10px', 'blue')
    expect(actual).toEqual(expected)
  })
})
