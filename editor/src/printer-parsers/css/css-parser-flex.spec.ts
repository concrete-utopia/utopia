import { syntaxParsers } from './css-parser-map'
import { AssumedFlexDefaults, printFlexAsAttributeValue } from './css-parser-flex'
import { right } from '../../core/shared/either'
import { cssFlex, cssNumber } from '../../components/inspector/common/css-utils'
import {
  clearExpressionUniqueIDs,
  emptyComments,
  jsExpressionValue,
} from '../../core/shared/element-template'

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

describe('print flex css shorthand', () => {
  it('prints one value flex-grow', () => {
    const printResult = clearExpressionUniqueIDs(
      printFlexAsAttributeValue(cssFlex(5, 1, cssNumber(0, null))),
    )
    expect(printResult).toEqual(clearExpressionUniqueIDs(jsExpressionValue('5', emptyComments)))
  })

  it('does NOT print one value flexBasis, instead opts for three-value representation!', () => {
    const printResult = clearExpressionUniqueIDs(
      printFlexAsAttributeValue(cssFlex(1, 1, cssNumber(5, 'px'))),
    )
    expect(printResult).toEqual(
      clearExpressionUniqueIDs(jsExpressionValue('1 1 5px', emptyComments)),
    )
  })

  it('two value flex-grow, flex-shrink', () => {
    const printResult = clearExpressionUniqueIDs(
      printFlexAsAttributeValue(cssFlex(9, 4, cssNumber(0, null))),
    )
    expect(printResult).toEqual(clearExpressionUniqueIDs(jsExpressionValue('9 4', emptyComments)))
  })

  it('does NOT print two value flex-grow, flex-basis, option for three value rep', () => {
    const printResult = clearExpressionUniqueIDs(
      printFlexAsAttributeValue(cssFlex(9, 1, cssNumber(5, 'px'))),
    )
    expect(printResult).toEqual(
      clearExpressionUniqueIDs(jsExpressionValue('9 1 5px', emptyComments)),
    )
  })

  it('prints the full three value syntax', () => {
    const printResult = clearExpressionUniqueIDs(
      printFlexAsAttributeValue(cssFlex(9, 4, cssNumber(5, 'px'))),
    )
    expect(printResult).toEqual(
      clearExpressionUniqueIDs(jsExpressionValue('9 4 5px', emptyComments)),
    )
  })
})
