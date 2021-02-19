import { syntaxParsers } from './css-parser-map'
import { AssumedFlexDefaults, printFlexAsAttributeValue } from './css-parser-flex'
import { right } from '../../core/shared/either'
import { cssFlex, cssNumber } from '../../components/inspector/common/css-utils'

describe('parse flex css shorthand', () => {
  it("parses unitless number <'flex'> property as flexgrow, 1-value", () => {
    const value = 15
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toEqual(right(cssFlex(15, AssumedFlexDefaults.flexShrink, cssNumber(0))))
  })
  it("parses a simple number with unit <'flex'> property as flexbasis, 1-value", () => {
    const value = '10px'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toEqual(
      right(
        cssFlex(AssumedFlexDefaults.flexGrow, AssumedFlexDefaults.flexShrink, cssNumber(10, 'px')),
      ),
    )
  })
  it("parses a simple number with %unit <'flex'> property as flexbasis, 1-value", () => {
    const value = '20%'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toEqual(
      right(
        cssFlex(AssumedFlexDefaults.flexGrow, AssumedFlexDefaults.flexShrink, cssNumber(20, '%')),
      ),
    )
  })
  it("parses <'flex'> property as flexgrow and flexbasis, 2-value", () => {
    const value = '2 30px'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toEqual(
      right(cssFlex(2, AssumedFlexDefaults.flexShrink, cssNumber(30, 'px'))),
    )
  })
  it("parses <'flex'> property as flexgrow and flexshrink, 2-value", () => {
    const value = '3 2'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toEqual(right(cssFlex(3, 2, cssNumber(0))))
  })
  it("parses a full <'flex'> property as flexgrow, flexshrink and flexbasis, 3-value", () => {
    const value = '3 2 10%'
    const parseResults = syntaxParsers['<flex>'](value)
    expect(parseResults).toEqual(right(cssFlex(3, 2, cssNumber(10, '%'))))
  })
})
