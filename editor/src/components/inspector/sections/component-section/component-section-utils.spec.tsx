import React from 'react'
import type { ArrayControlDescription, ObjectControlDescription } from 'utopia-api/core'
import { inferControlTypeBasedOnValue } from './component-section-utils'

describe('inferControlTypeBasedOnValue', () => {
  it('Correctly infers simple number controls', () => {
    const result = inferControlTypeBasedOnValue(1)
    expect(result.control).toEqual('number-input')
  })

  it('Correctly infers simple string controls', () => {
    const result = inferControlTypeBasedOnValue('one')
    expect(result.control).toEqual('string-input')
  })

  it('Correctly infers color string controls', () => {
    // We could test more colors here, but those tests already exist in the actual function for parsing css colors
    const hexResult = inferControlTypeBasedOnValue('#00112233')
    expect(hexResult.control).toEqual('color')

    const rgbResult = inferControlTypeBasedOnValue('rgb(10,20,30)')
    expect(rgbResult.control).toEqual('color')

    const hslResult = inferControlTypeBasedOnValue('hsl(10,20%,30%)')
    expect(hslResult.control).toEqual('color')

    const keywordResult = inferControlTypeBasedOnValue('blue')
    expect(keywordResult.control).toEqual('color')
  })

  it('Correctly infers simple boolean controls', () => {
    const result = inferControlTypeBasedOnValue(true)
    expect(result.control).toEqual('checkbox')
  })

  it('Correctly infers vector2 controls', () => {
    const result = inferControlTypeBasedOnValue([1, 2])
    expect(result.control).toEqual('vector2')
  })

  it('Correctly infers vector3 controls', () => {
    const result = inferControlTypeBasedOnValue([1, 2, 3])
    expect(result.control).toEqual('vector3')
  })

  it('Correctly infers vector4 controls', () => {
    const result = inferControlTypeBasedOnValue([1, 2, 3, 4])
    expect(result.control).toEqual('vector4')
  })

  it('Correctly infers euler controls', () => {
    const result = inferControlTypeBasedOnValue([1, 2, 3, 'XYZ'])
    expect(result.control).toEqual('euler')
  })

  it('Correctly infers matrix3 controls', () => {
    // prettier-ignore
    const matrix = [
      1, 2, 3,
      4, 5, 6,
      7, 8, 9,
    ]

    const result = inferControlTypeBasedOnValue(matrix)
    expect(result.control).toEqual('matrix3')
  })

  it('Correctly infers matrix4 controls', () => {
    // prettier-ignore
    const matrix = [
       1,  2,  3,  4,
       5,  6,  7,  8,
       9, 10, 11, 12,
      13, 14, 15, 16,
    ]

    const result = inferControlTypeBasedOnValue(matrix)
    expect(result.control).toEqual('matrix4')
  })

  it('Correctly infers simple array controls', () => {
    const numericArrayLength1Result = inferControlTypeBasedOnValue([1])
    expect(numericArrayLength1Result.control).toEqual('array')
    expect((numericArrayLength1Result as ArrayControlDescription).propertyControl.control).toEqual(
      'number-input',
    )

    const numericArrayLength4Result = inferControlTypeBasedOnValue([1, 2, 3, 4, 5])
    expect(numericArrayLength4Result.control).toEqual('array')

    // Ensure non-numeric arrays aren't parsed as vectors
    const nonNumericArrayLength2Result = inferControlTypeBasedOnValue(['one', 'two'])
    expect(nonNumericArrayLength2Result.control).toEqual('array')
    const nonNumericArrayLength3Result = inferControlTypeBasedOnValue(['one', 'two', 'three'])
    expect(nonNumericArrayLength3Result.control).toEqual('array')
  })

  it('Correctly infers simple object controls', () => {
    const emptyObjectResult = inferControlTypeBasedOnValue({})
    expect(emptyObjectResult.control).toEqual('object')

    const result = inferControlTypeBasedOnValue({ k: 'value' })
    expect(result.control).toEqual('object')
    expect((result as ObjectControlDescription).object.k.control).toEqual('string-input')
  })

  it('Ignores the style prop', () => {
    const result = inferControlTypeBasedOnValue({}, 'style')
    expect(result.control).toEqual('none')
  })

  it('Treats a React element as jsx', () => {
    const result = inferControlTypeBasedOnValue(<div />)
    expect(result.control).toEqual('jsx')
  })

  it('treats a function as expression-input', () => {
    const result = inferControlTypeBasedOnValue(() => {})
    expect(result.control).toEqual('expression-input')
  })

  it('Correctly infers nested array / object horror show controls', () => {
    const result = inferControlTypeBasedOnValue({
      arr: [
        {
          k: 'value',
        },
      ],
    })

    expect(result.control).toEqual('object')
    const outerObject = (result as ObjectControlDescription).object
    expect(outerObject.arr.control).toEqual('array')
    const arr = outerObject.arr as ArrayControlDescription
    expect(arr.propertyControl.control).toEqual('object')
    const innerObject = (arr.propertyControl as ObjectControlDescription).object
    expect(innerObject.k.control).toEqual('string-input')
  })
})
