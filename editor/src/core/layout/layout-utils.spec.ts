import { styleStringInArray } from '../../utils/common-constants'
import {
  JSXAttributes,
  jsxAttributeValue,
  jsxAttributeNestedObject,
  jsxPropertyAssignment,
  jsxAttributesFromMap,
  emptyComments,
} from '../shared/element-template'
import { roundAttributeLayoutValues } from './layout-utils'

describe('roundAttributeLayoutValues', () => {
  it('rounds values within a complex attribute value', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          left: 0,
          top: '0%',
          width: 140.675,
          height: '65.492%',
        },
        emptyComments,
      ),
    })
    const actualResult = roundAttributeLayoutValues(styleStringInArray, attributes)
    const expectedResult: JSXAttributes = jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          left: 0,
          top: '0%',
          width: 141,
          height: '65.5%',
        },
        emptyComments,
      ),
    })
    expect(actualResult).toEqual(expectedResult)
  })
  it('rounds values within a nested attribute object', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      style: jsxAttributeNestedObject(
        [
          jsxPropertyAssignment(
            'left',
            jsxAttributeValue(0, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'top',
            jsxAttributeValue('0%', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'width',
            jsxAttributeValue(140.675, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'height',
            jsxAttributeValue('65.492%', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    const actualResult = roundAttributeLayoutValues(styleStringInArray, attributes)
    const expectedResult: JSXAttributes = jsxAttributesFromMap({
      style: jsxAttributeValue(
        {
          left: 0,
          top: '0%',
          width: 141,
          height: '65.5%',
        },
        emptyComments,
      ),
    })
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not round irrelevant values', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      sizeOfHat: jsxAttributeValue(123.456, emptyComments),
    })
    const actualResult = roundAttributeLayoutValues(styleStringInArray, attributes)
    const expectedResult: JSXAttributes = jsxAttributesFromMap({
      sizeOfHat: jsxAttributeValue(123.456, emptyComments),
    })
    expect(actualResult).toEqual(expectedResult)
  })
  it('keeps the same value if no rounding is necessary', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      style: jsxAttributeValue(
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
