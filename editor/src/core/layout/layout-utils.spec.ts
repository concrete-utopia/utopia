import { styleStringInArray } from '../../utils/common-constants'
import type { JSXAttributes } from '../shared/element-template'
import {
  jsExpressionValue,
  jsExpressionNestedObject,
  jsxPropertyAssignment,
  jsxAttributesFromMap,
  emptyComments,
  clearAttributesUniqueIDs,
} from '../shared/element-template'
import { roundAttributeLayoutValues } from './layout-utils'

describe('roundAttributeLayoutValues', () => {
  it('rounds values within a complex attribute value', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          left: 0,
          top: '0%',
          width: 140.675,
          height: '65.492%',
        },
        emptyComments,
      ),
    })
    const actualResult = clearAttributesUniqueIDs(
      roundAttributeLayoutValues(styleStringInArray, attributes),
    )
    const expectedResult: JSXAttributes = clearAttributesUniqueIDs(
      jsxAttributesFromMap({
        style: jsExpressionNestedObject(
          [
            jsxPropertyAssignment(
              'left',
              jsExpressionValue(0, emptyComments),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'top',
              jsExpressionValue('0%', emptyComments),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'width',
              jsExpressionValue(141, emptyComments),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'height',
              jsExpressionValue('65.5%', emptyComments),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      }),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('rounds values within a nested attribute object', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      style: jsExpressionNestedObject(
        [
          jsxPropertyAssignment(
            'left',
            jsExpressionValue(0, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'top',
            jsExpressionValue('0%', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'width',
            jsExpressionValue(140.675, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'height',
            jsExpressionValue('65.492%', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    const actualResult = clearAttributesUniqueIDs(
      roundAttributeLayoutValues(styleStringInArray, attributes),
    )
    const expectedResult: JSXAttributes = clearAttributesUniqueIDs(
      jsxAttributesFromMap({
        style: jsExpressionNestedObject(
          [
            jsxPropertyAssignment(
              'left',
              jsExpressionValue(0, emptyComments),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'top',
              jsExpressionValue('0%', emptyComments),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'width',
              jsExpressionValue(141, emptyComments),
              emptyComments,
              emptyComments,
            ),
            jsxPropertyAssignment(
              'height',
              jsExpressionValue('65.5%', emptyComments),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      }),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not round irrelevant values', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      sizeOfHat: jsExpressionValue(123.456, emptyComments),
    })
    const actualResult = clearAttributesUniqueIDs(
      roundAttributeLayoutValues(styleStringInArray, attributes),
    )
    const expectedResult: JSXAttributes = clearAttributesUniqueIDs(
      jsxAttributesFromMap({
        sizeOfHat: jsExpressionValue(123.456, emptyComments),
      }),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('keeps the same value if no rounding is necessary', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          left: 0,
          top: '0%',
          width: 141,
          height: '65.5%',
        },
        emptyComments,
      ),
    })

    const actualResult = roundAttributeLayoutValues(styleStringInArray, attributes)
    expect(actualResult).toBe(attributes)
  })
})
