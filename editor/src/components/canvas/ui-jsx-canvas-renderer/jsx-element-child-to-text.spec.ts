import { isLeft } from '../../../core/shared/either'
import type { JSElementAccess } from '../../../core/shared/element-template'
import {
  emptyComments,
  isJSElementAccess,
  isJSXElement,
  isUtopiaJSXComponent,
  jsElementAccess,
  jsIdentifier,
  jsPropertyAccess,
} from '../../../core/shared/element-template'
import { fromField, fromTypeGuard, traverseArray } from '../../../core/shared/optics/optic-creators'
import { toFirst } from '../../../core/shared/optics/optic-utilities'
import type { Optic } from '../../../core/shared/optics/optics'
import type { ParsedTextFile } from '../../../core/shared/project-file-types'
import { isParseSuccess } from '../../../core/shared/project-file-types'
import { emptySet } from '../../../core/shared/set-utils'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { jsxElementChildToText } from './jsx-element-child-to-text'

describe('jsxElementChildToText', () => {
  it('identifier (jsx and outermost)', () => {
    const identifier = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const actualResult = jsxElementChildToText(identifier, null, null, 'jsx', 'outermost')
    expect(actualResult).toEqual('{anIdentifier}')
  })
  it('identifier (javascript and outermost)', () => {
    const identifier = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const actualResult = jsxElementChildToText(identifier, null, null, 'javascript', 'outermost')
    expect(actualResult).toEqual('anIdentifier')
  })
  it('identifier (jsx and inner)', () => {
    const identifier = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const actualResult = jsxElementChildToText(identifier, null, null, 'jsx', 'inner')
    expect(actualResult).toEqual('anIdentifier')
  })
  it('identifier (javascript and inner)', () => {
    const identifier = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const actualResult = jsxElementChildToText(identifier, null, null, 'javascript', 'inner')
    expect(actualResult).toEqual('anIdentifier')
  })
  it('property access (not optionally chained, jsx and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier.someProperty',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(propertyAccess, null, null, 'jsx', 'outermost')
    expect(actualResult).toEqual('{anIdentifier.someProperty}')
  })
  it('property access (not optionally chained, javascript and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier.someProperty',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(
      propertyAccess,
      null,
      null,
      'javascript',
      'outermost',
    )
    expect(actualResult).toEqual('anIdentifier.someProperty')
  })
  it('property access (not optionally chained, jsx and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier.someProperty',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(propertyAccess, null, null, 'jsx', 'inner')
    expect(actualResult).toEqual('anIdentifier.someProperty')
  })
  it('property access (not optionally chained, javascript and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier.someProperty',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(propertyAccess, null, null, 'javascript', 'inner')
    expect(actualResult).toEqual('anIdentifier.someProperty')
  })
  it('property access (optionally chained, jsx and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier?.someProperty',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(propertyAccess, null, null, 'jsx', 'outermost')
    expect(actualResult).toEqual('{anIdentifier?.someProperty}')
  })
  it('property access (optionally chained, javascript and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier?.someProperty',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(
      propertyAccess,
      null,
      null,
      'javascript',
      'outermost',
    )
    expect(actualResult).toEqual('anIdentifier?.someProperty')
  })
  it('property access (optionally chained, jsx and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier?.someProperty',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(propertyAccess, null, null, 'jsx', 'inner')
    expect(actualResult).toEqual('anIdentifier?.someProperty')
  })
  it('property access (optionally chained, javascript and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const propertyAccess = jsPropertyAccess(
      onValue,
      'someProperty',
      'cba',
      null,
      emptyComments,
      'anIdentifier?.someProperty',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(propertyAccess, null, null, 'javascript', 'inner')
    expect(actualResult).toEqual('anIdentifier?.someProperty')
  })
  it('element access (not optionally chained, jsx and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier[elementIdentifier]',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'jsx', 'outermost')
    expect(actualResult).toEqual('{anIdentifier[elementIdentifier]}')
  })
  it('element access (not optionally chained, javascript and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier[elementIdentifier]',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'javascript', 'outermost')
    expect(actualResult).toEqual('anIdentifier[elementIdentifier]')
  })
  it('element access (not optionally chained, jsx and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier[elementIdentifier]',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'jsx', 'inner')
    expect(actualResult).toEqual('anIdentifier[elementIdentifier]')
  })
  it('element access (not optionally chained, javascript and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier[elementIdentifier]',
      'not-optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'javascript', 'inner')
    expect(actualResult).toEqual('anIdentifier[elementIdentifier]')
  })
  it('element access (optionally chained, jsx and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier?.[elementIdentifier]',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'jsx', 'outermost')
    expect(actualResult).toEqual('{anIdentifier?.[elementIdentifier]}')
  })
  it('element access (optionally chained, javascript and outermost)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier?.[elementIdentifier]',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'javascript', 'outermost')
    expect(actualResult).toEqual('anIdentifier?.[elementIdentifier]')
  })
  it('element access (optionally chained, jsx and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier?.[elementIdentifier]',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'jsx', 'inner')
    expect(actualResult).toEqual('anIdentifier?.[elementIdentifier]')
  })
  it('element access (optionally chained, javascript and inner)', () => {
    const onValue = jsIdentifier('anIdentifier', 'abc', null, emptyComments)
    const element = jsIdentifier('elementIdentifier', 'xyz', null, emptyComments)
    const elementAccess = jsElementAccess(
      onValue,
      element,
      'cba',
      null,
      emptyComments,
      'anIdentifier?.[elementIdentifier]',
      'optionally-chained',
    )
    const actualResult = jsxElementChildToText(elementAccess, null, null, 'javascript', 'inner')
    expect(actualResult).toEqual('anIdentifier?.[elementIdentifier]')
  })
  it('complicated case', () => {
    const parsedResult: ParsedTextFile = lintAndParse(
      'test.js',
      [],
      `const TestComponent = (props) => <div>{something()?.[another().property?.deeperProperty]}</div>`,
      null,
      emptySet(),
      'do-not-trim-bounds',
      'do-not-apply-steganography',
    )
    const toExpressionOptic: Optic<ParsedTextFile, JSElementAccess> = fromTypeGuard(isParseSuccess)
      .compose(fromField('topLevelElements'))
      .compose(traverseArray())
      .compose(fromTypeGuard(isUtopiaJSXComponent))
      .compose(fromField('rootElement'))
      .compose(fromTypeGuard(isJSXElement))
      .compose(fromField('children'))
      .compose(traverseArray())
      .compose(fromTypeGuard(isJSElementAccess))
    const jsExpression = toFirst(toExpressionOptic, parsedResult)
    if (isLeft(jsExpression)) {
      throw new Error(`Unable to obtain expression from ${JSON.stringify(parsedResult, null, 2)}`)
    } else {
      const actualResult = jsxElementChildToText(
        jsExpression.value,
        null,
        null,
        'javascript',
        'outermost',
      )
      expect(actualResult).toEqual(`something()?.[another().property?.deeperProperty]`)
    }
  })
})
