import { parseThenPrint } from './parser-printer.test-utils'
import { applyPrettier } from './prettier-utils'

describe('Parsing and then printing code', () => {
  ;['var', 'let', 'const'].forEach((varLetOrConst) => {
    it(`retains the variable declaration keyword ${varLetOrConst}`, () => {
      const code = applyPrettier(
        `export ${varLetOrConst} whatever = (props) => {
          return <div data-uid='aaa' />
        }`,
        false,
      ).formatted

      const parsedThenPrinted = parseThenPrint(code)
      expect(parsedThenPrinted).toEqual(code)
    })
  })

  it('does not replace a function with a const', () => {
    const code = applyPrettier(
      `export default function whatever(props) {
        return <div data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a parenthesized expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => (
        <div data-uid='aaa' />
      )`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a non-parenthesized expression body on an arrow function component', () => {
    const code = applyPrettier(`export const whatever = (props) => <div data-uid='aaa' />`, false)
      .formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a block expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not surround a jsx attribute value in braces when it was not previously surrounded in braces', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-something='something' data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  xit('does not remove the braces surrounding a jsx attribute value', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-something={'something'} data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  xit('does not remove a trailing export default statement', () => {
    const code = applyPrettier(
      `const whatever = (props) => {
        return <div data-uid='aaa' />
      }
      
      export default whatever`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })
})
