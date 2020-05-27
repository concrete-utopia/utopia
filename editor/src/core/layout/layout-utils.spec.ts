import {
  JSXAttributes,
  jsxAttributeValue,
  jsxAttributeNestedObject,
  jsxPropertyAssignment,
} from '../shared/element-template'
import { roundAttributeLayoutValues } from './layout-utils'

describe('roundAttributeLayoutValues', () => {
  it('rounds values within a complex attribute value', () => {
    const attributes: JSXAttributes = {
      style: jsxAttributeValue({
        left: 0,
        top: '0%',
        width: 140.675,
        height: '65.492%',
      }),
    }
    const actualResult = roundAttributeLayoutValues(attributes)
    const expectedResult: JSXAttributes = {
      style: jsxAttributeNestedObject([
        jsxPropertyAssignment('left', jsxAttributeValue(0)),
        jsxPropertyAssignment('top', jsxAttributeValue('0%')),
        jsxPropertyAssignment('width', jsxAttributeValue(141)),
        jsxPropertyAssignment('height', jsxAttributeValue('65.5%')),
      ]),
    }
    expect(actualResult).toEqual(expectedResult)
  })
  it('rounds values within a nested attribute object', () => {
    const attributes: JSXAttributes = {
      style: jsxAttributeNestedObject([
        jsxPropertyAssignment('left', jsxAttributeValue(0)),
        jsxPropertyAssignment('top', jsxAttributeValue('0%')),
        jsxPropertyAssignment('width', jsxAttributeValue(140.675)),
        jsxPropertyAssignment('height', jsxAttributeValue('65.492%')),
      ]),
    }
    const actualResult = roundAttributeLayoutValues(attributes)
    const expectedResult: JSXAttributes = {
      style: jsxAttributeNestedObject([
        jsxPropertyAssignment('left', jsxAttributeValue(0)),
        jsxPropertyAssignment('top', jsxAttributeValue('0%')),
        jsxPropertyAssignment('width', jsxAttributeValue(141)),
        jsxPropertyAssignment('height', jsxAttributeValue('65.5%')),
      ]),
    }
    expect(actualResult).toEqual(expectedResult)
  })
  it('does not round irrelevant values', () => {
    const attributes: JSXAttributes = {
      sizeOfHat: jsxAttributeValue(123.456),
    }
    const actualResult = roundAttributeLayoutValues(attributes)
    const expectedResult: JSXAttributes = {
      sizeOfHat: jsxAttributeValue(123.456),
    }
    expect(actualResult).toEqual(expectedResult)
  })
  it('keeps the same value if no rounding is necessary', () => {
    const attributes: JSXAttributes = {
      style: jsxAttributeValue({
        left: 0,
        top: '0%',
        width: 141,
        height: '65.5%',
      }),
    }
    const actualResult = roundAttributeLayoutValues(attributes)
    expect(actualResult).toBe(attributes)
  })
})
