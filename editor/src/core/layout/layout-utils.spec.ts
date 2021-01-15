import {
  JSXAttributes,
  jsxAttributeValue,
  jsxAttributeNestedObject,
  jsxPropertyAssignment,
  jsxAttributesFromMap,
} from '../shared/element-template'
import { roundAttributeLayoutValues } from './layout-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'

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
    const actualResult = roundAttributeLayoutValues(attributes)
    const expectedResult: JSXAttributes = jsxAttributesFromMap({
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
            jsxAttributeValue(141, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'height',
            jsxAttributeValue('65.5%', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
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
    const actualResult = roundAttributeLayoutValues(attributes)
    const expectedResult: JSXAttributes = jsxAttributesFromMap({
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
            jsxAttributeValue(141, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'height',
            jsxAttributeValue('65.5%', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not round irrelevant values', () => {
    const attributes: JSXAttributes = jsxAttributesFromMap({
      sizeOfHat: jsxAttributeValue(123.456, emptyComments),
    })
    const actualResult = roundAttributeLayoutValues(attributes)
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
    const actualResult = roundAttributeLayoutValues(attributes)
    expect(actualResult).toBe(attributes)
  })
})
