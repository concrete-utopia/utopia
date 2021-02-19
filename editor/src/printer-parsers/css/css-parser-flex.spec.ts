import { syntaxParsers } from './css-parser-map'
import { printFlexAsAttributeValue } from './css-parser-flex'

describe('parse flex css shorthand', () => {
  it("parses unitless number <'flex'> property as flexgrow, 1-value", () => {
    const value = 15
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toMatchInlineSnapshot()
  })
  it("parses a simple number with unit <'flex'> property as flexbasis, 1-value", () => {
    const value = '10px'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toMatchInlineSnapshot()
  })
  it("parses a simple number with %unit <'flex'> property as flexbasis, 1-value", () => {
    const value = '20%'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toMatchInlineSnapshot()
  })
  it("parses <'flex'> property as flexgrow and flexbasis, 2-value", () => {
    const value = '1 30px'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toMatchInlineSnapshot()
  })
  it("parses <'flex'> property as flexgrow and flexshrink, 2-value", () => {
    const value = '2 2'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toMatchInlineSnapshot()
  })
  it("parses a full <'flex'> property as flexgrow, flexshrink and flexbasis, 3-value", () => {
    const value = '2 2 10%'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toMatchInlineSnapshot()
  })
})
