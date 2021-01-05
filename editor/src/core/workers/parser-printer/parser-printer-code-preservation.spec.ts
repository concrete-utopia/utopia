import { parseThenPrint } from './parser-printer.test-utils'
import { applyPrettier } from './prettier-utils'

describe('Parsing and then printing code', () => {
  ;['var', 'let', 'const'].forEach((varLetOrConst) => {
    it(`retains the variable declaration keyword ${varLetOrConst}`, () => {
      const code = applyPrettier(
        `export ${varLetOrConst} whatever = (props) => {
          return <div data-uid={'aaa'} />
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
        return <div data-uid={'aaa'} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })
})
