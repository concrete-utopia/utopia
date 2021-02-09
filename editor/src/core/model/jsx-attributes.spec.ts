import * as PP from '../shared/property-path'
import { deepFreeze } from '../../utils/deep-freeze'
import { forceRight, isLeft, isRight, right } from '../shared/either'
import {
  getJSXAttributeForced,
  isJSXAttributeFunctionCall,
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isPartOfJSXAttributeValue,
  jsxArrayValue,
  jsxAttributeFunctionCall,
  jsxAttributeNestedArray,
  jsxAttributeNestedArraySimple,
  jsxAttributeNestedObject,
  jsxAttributeNestedObjectSimple,
  jsxAttributeNotFound,
  jsxAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributesFromMap,
  jsxAttributeValue,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  simplifyAttributeIfPossible,
} from '../shared/element-template'
import {
  dropKeyFromNestedObject,
  getModifiableJSXAttributeAtPath,
  jsxAttributesToProps,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
  unsetJSXValueAtPath,
} from '../shared/jsx-attributes'
import { NO_OP } from '../shared/utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'

const sampleParentProps = {
  hello: 'kitty',
  someShadow: 'shade',
}

function sampleJsxAttributes(): JSXAttributes {
  return deepFreeze<JSXAttributes>(
    jsxAttributesFromMap({
      style: jsxAttributeNestedObject(
        [
          jsxSpreadAssignment(
            jsxAttributeValue({ first: 1, second: 2 }, emptyComments),
            emptyComments,
          ),
          jsxPropertyAssignment(
            'backgroundColor',
            jsxAttributeValue('red', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'shadow',
            jsxAttributeOtherJavaScript(
              'props.someShadow',
              'return props.someShadow;',
              ['props'],
              null,
            ),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'border',
            jsxAttributeValue('1px solid green', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'boxShadow',
            jsxAttributeValue('0 0 0 1px blue', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
      top: jsxAttributeValue(0, emptyComments),
      left: jsxAttributeValue(50, emptyComments),
      height: jsxAttributeValue(150, emptyComments),
      width: jsxAttributeValue(200, emptyComments),
      layout: jsxAttributeValue(
        {
          left: 50,
          deep: {
            path: 'hard!',
          },
        },
        emptyComments,
      ),
      objectWithArray: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          array: jsxAttributeValue([0, 1, 2], emptyComments),
        }),
        emptyComments,
      ),
      doggo: jsxAttributeOtherJavaScript('props.hello', 'return props.hello;', ['props'], null),
      objectValue: jsxAttributeValue(
        {
          deep: {
            object: {
              path: 'yes',
            },
          },
        },
        emptyComments,
      ),
      otherJs: jsxAttributeOtherJavaScript('true ? 10 : 5', 'return true ? 10 : 5', [], null),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    }),
  ) as JSXAttributes
}

const expectedCompiledProps = {
  style: {
    first: 1,
    second: 2,
    backgroundColor: 'red',
    shadow: 'shade',
    border: '1px solid green',
    boxShadow: '0 0 0 1px blue',
  },
  top: 0,
  left: 50,
  height: 150,
  width: 200,
  layout: {
    left: 50,
    deep: {
      path: 'hard!',
    },
  },
  objectWithArray: {
    array: [0, 1, 2],
  },
  doggo: 'kitty',
  objectValue: {
    deep: {
      object: {
        path: 'yes',
      },
    },
  },
  otherJs: 10,
  'data-uid': 'aaa',
}

describe('setJSXValueAtPath', () => {
  it('sets a simple value at a simple path', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['top']),
        jsxAttributeValue(55, emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.top).toEqual(55)
  })

  it('sets a simple value replacing any other attribute that was there', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['otherJs']),
        jsxAttributeValue('shadowy', emptyComments),
      ),
    )
    expect(getJSXAttributeForced(updatedAttributes, 'otherJs').type).toEqual('ATTRIBUTE_VALUE')
  })

  it('sets a simple value at a deep path and creates the necessary objects', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['my', 'property', 'path']),
        jsxAttributeValue('hello', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.my.property.path).toEqual('hello')
  })

  it('updating an ATTRIBUTE_VALUE which is an object at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['layout', 'left']),
        jsxAttributeValue(2000, emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.layout.left).toEqual(2000)
  })

  it('updating an ATTRIBUTE_VALUE which is an object at a deeper path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['layout', 'deep', 'path']),
        jsxAttributeValue('easy!', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.layout.deep.path).toEqual('easy!')
  })

  it('updating an ATTRIBUTE_VALUE which is an array at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['objectWithArray', 'array', 2]),
        jsxAttributeValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.objectWithArray.array).toEqual([0, 1, 'wee'])
  })

  it('updating part of an ATTRIBUTE_VALUE which is a string throws error', () => {
    expect(() => {
      const updatedAttributes = forceRight(
        setJSXValueAtPath(
          sampleJsxAttributes(),
          PP.create(['style', 'backgroundColor', 'red']),
          jsxAttributeValue('wee', emptyComments),
        ),
      )
      const compiledProps = jsxAttributesToProps(
        { props: sampleParentProps },
        updatedAttributes,
        {},
      )
    }).toThrow()
  })

  it('updating a NESTED_OJECT at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['style', 'backgroundColor']),
        jsxAttributeValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.style.backgroundColor).toEqual('wee')
  })

  it('setting two overlapping deep paths does not clash', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['my', 'property', 'path']),
        jsxAttributeValue('hello', emptyComments),
      ),
    )
    const updatedAttributes2 = forceRight(
      setJSXValueAtPath(
        updatedAttributes,
        PP.create(['my', 'property', 'other', 'path']),
        jsxAttributeValue('hola', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes2, {})
    expect(compiledProps.my.property.path).toEqual('hello')
    expect(compiledProps.my.property.other.path).toEqual('hola')
  })

  it('updates a simple value at a deep path that already exists', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['style', 'backgroundColor']),
        jsxAttributeValue('blue', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps({ props: sampleParentProps }, updatedAttributes, {})
    expect(compiledProps.style.backgroundColor).toEqual('blue')
  })

  it('should prevent setting a value inside a special prop', () => {
    const result1 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'shadow', 'left']),
      jsxAttributeValue('shadowy', emptyComments),
    )

    const result2 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'boxShadow', '0']),
      jsxAttributeValue('shadowy', emptyComments),
    )

    const result3 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create(['otherJs', 'wrongProp']),
      jsxAttributeValue('shadowy', emptyComments),
    )

    expect(result1.type).toBe('LEFT')
    expect(result2.type).toBe('LEFT')
    expect(result3.type).toBe('LEFT')
  })
})

describe('jsxAttributesToProps', () => {
  it('works', () => {
    const compiledProps = jsxAttributesToProps(
      { props: sampleParentProps },
      sampleJsxAttributes(),
      {},
    )
    expect(compiledProps).toEqual(expectedCompiledProps)
  })
})

describe('getModifiableJSXAttributeAtPath', () => {
  it('gets a simple value', () => {
    const backgroundColor = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'backgroundColor']),
    )
    expect(isRight(backgroundColor)).toBeTruthy()
    if (isRight(backgroundColor)) {
      expect(backgroundColor).not.toBeUndefined()
      expect(backgroundColor).not.toBeNull()
      if (backgroundColor != null && isJSXAttributeValue(backgroundColor.value)) {
        expect(backgroundColor.value.value).toEqual('red')
      } else {
        fail('backgroundColor is not a JSXAttributeValue')
      }
    }
  })

  it('drills correctly into a simple value containing an object', () => {
    const foundAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['objectValue', 'deep', 'object', 'path']),
    )
    expect(isRight(foundAttribute)).toBeTruthy()
    if (isRight(foundAttribute)) {
      expect(isPartOfJSXAttributeValue(foundAttribute.value)).toBeTruthy()
      expect((foundAttribute.value as any).value).toEqual('yes')
    }

    const missingAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['objectValue', 'deep', 'object', 'missing', 'path']),
    )
    expect(isRight(missingAttribute)).toBeTruthy()
    if (isRight(missingAttribute)) {
      expect(isJSXAttributeNotFound(missingAttribute.value)).toBeTruthy()
    }

    const completelyMissingAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['cats', 'dogs', 'missing', 'path']),
    )
    expect(completelyMissingAttribute).toEqual(right(jsxAttributeNotFound()))
  })

  it('returns Right on a path that CAN be updated by an action', () => {
    const impossibleAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['bad', 'path']),
    )
    expect(isRight(impossibleAttribute)).toBeTruthy()
    expect((impossibleAttribute.value as any).value).toEqual(undefined)

    // this is an interesting case. we actually drilled into a found value object, but then did not find anything for this path
    // so now we return a right(undefined),
    // where the meaning is: you can dispatch an action to set this path, but right now it
    // does not contain any real value
    const impossibleAttributeInsideAValue = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['top', 'what']),
    )
    expect(isRight(impossibleAttributeInsideAValue)).toBeTruthy()
    expect((impossibleAttributeInsideAValue.value as any).value).toEqual(undefined)
  })
})

describe('jsxSimpleAttributeToValue', () => {
  it('gets the value of a nested object attribute', () => {
    const attribute = jsxAttributeNestedObjectSimple(
      jsxAttributesFromMap({
        top: jsxAttributeValue(50, emptyComments),
        left: jsxAttributeValue(100, emptyComments),
      }),
      emptyComments,
    )
    const attributeValue = jsxSimpleAttributeToValue(attribute)
    const expectedAttrValue = {
      top: 50,
      left: 100,
    }
    expect(isRight(attributeValue))
    expect(attributeValue.value).toEqual(expectedAttrValue)
  })
})

describe('unsetJSXValueAtPath', () => {
  it('removes an attribute from the root of the attributes', () => {
    const startingValue = jsxAttributesFromMap({
      left: jsxAttributeValue(0, emptyComments),
      top: jsxAttributeValue(0, emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['left']))
    const expectedValue = right(
      jsxAttributesFromMap({
        top: jsxAttributeValue(0, emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from the root of the attributes that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      top: jsxAttributeValue(0, emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['left']))
    const expectedValue = right(
      jsxAttributesFromMap({
        top: jsxAttributeValue(0, emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute object', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeValue({ left: 0, top: 0 }, emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left']))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeValue({ top: 0 }, emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute object that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeValue({ top: 0 }, emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left']))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeValue({ top: 0 }, emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute array', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeValue([0, 1], emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeValue([0], emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute array that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeValue([0], emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeValue([0], emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('fails when attempting to remove a property from an invalid attribute value', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeOtherJavaScript('undefined', 'undefined', [], null),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    expect(isLeft(actualValue)).toBe(true)
  })

  it('removes an attribute from a nested object', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          left: jsxAttributeValue({ x: 0, y: 0 }, emptyComments),
          top: jsxAttributeValue({ x: 1, y: 1 }, emptyComments),
        }),
        emptyComments,
      ),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left']))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({ top: jsxAttributeValue({ x: 1, y: 1 }, emptyComments) }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a object that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({ top: jsxAttributeValue({ x: 1, y: 1 }, emptyComments) }),
        emptyComments,
      ),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left', 'x']))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({ top: jsxAttributeValue({ x: 1, y: 1 }, emptyComments) }),
          emptyComments,
        ),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a nested array', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedArraySimple([
        jsxAttributeValue(0, emptyComments),
        jsxAttributeValue(1, emptyComments),
      ]),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeNestedArraySimple([jsxAttributeValue(0, emptyComments)]),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a nested array that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedArraySimple([jsxAttributeValue(0, emptyComments)]),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeNestedArraySimple([jsxAttributeValue(0, emptyComments)]),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes a deeply nested value', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          left: jsxAttributeNestedArraySimple([
            jsxAttributeValue('29', emptyComments),
            jsxAttributeNestedObjectSimple(
              jsxAttributesFromMap({
                stateEnabled: jsxAttributeNestedArraySimple([
                  jsxAttributeValue(
                    {
                      lightSide: {
                        eleven: 11,
                        ten: 10,
                      },
                      darkSide: {
                        twelve: 12,
                        nine: 9,
                      },
                    },
                    emptyComments,
                  ),
                ]),
              }),
              emptyComments,
            ),
          ]),
          top: jsxAttributeValue({ x: 1, y: 1 }, emptyComments),
        }),
        emptyComments,
      ),
      backgroundColor: jsxAttributeValue('red', emptyComments),
      'data-uid': jsxAttributeValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(
      startingValue,
      PP.create(['style', 'left', 1, 'stateEnabled', 0, 'lightSide', 'eleven']),
    )
    const expectedValue = right(
      jsxAttributesFromMap({
        style: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            left: jsxAttributeNestedArraySimple([
              jsxAttributeValue('29', emptyComments),
              jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({
                  stateEnabled: jsxAttributeNestedArraySimple([
                    jsxAttributeValue(
                      {
                        lightSide: {
                          ten: 10,
                        },
                        darkSide: {
                          twelve: 12,
                          nine: 9,
                        },
                      },
                      emptyComments,
                    ),
                  ]),
                }),
                emptyComments,
              ),
            ]),
            top: jsxAttributeValue({ x: 1, y: 1 }, emptyComments),
          }),
          emptyComments,
        ),
        backgroundColor: jsxAttributeValue('red', emptyComments),
        'data-uid': jsxAttributeValue('aaa', emptyComments),
      }),
    )
    expect(actualValue).toEqual(expectedValue)
  })
})

describe('dropKeyFromNestedObject', () => {
  it('only removes the pertinent attribute from a nested object', () => {
    const startingValue = jsxAttributeNestedObject(
      [
        jsxSpreadAssignment(jsxAttributeValue('theme', emptyComments), emptyComments),
        jsxPropertyAssignment(
          'backgroundColor',
          jsxAttributeValue('red', emptyComments),
          emptyComments,
          emptyComments,
        ),
      ],
      emptyComments,
    )
    const expectedValue = jsxAttributeNestedObject(
      [jsxSpreadAssignment(jsxAttributeValue('theme', emptyComments), emptyComments)],
      emptyComments,
    )
    const actualValue = dropKeyFromNestedObject(startingValue, 'backgroundColor')
    expect(actualValue).toEqual(expectedValue)
  })
})

describe('simplifyAttributeIfPossible', () => {
  it('nested array with a nested object in it simplifies down', () => {
    const array = jsxAttributeNestedArray(
      [
        jsxArrayValue(jsxAttributeValue(1, emptyComments), emptyComments),
        jsxArrayValue(jsxAttributeValue('hat', emptyComments), emptyComments),
        jsxArrayValue(
          jsxAttributeNestedObject(
            [
              jsxPropertyAssignment(
                'someKey',
                jsxAttributeValue('with a value', emptyComments),
                emptyComments,
                emptyComments,
              ),
            ],
            emptyComments,
          ),
          emptyComments,
        ),
      ],
      emptyComments,
    )
    const actualResult = simplifyAttributeIfPossible(array)
    const expectedResult = jsxAttributeValue([1, 'hat', { someKey: 'with a value' }], emptyComments)
    expect(actualResult).toEqual(expectedResult)
  })
  it('simple value returns as is', () => {
    const expectedValue = jsxAttributeValue('test', emptyComments)
    const actualValue = simplifyAttributeIfPossible(expectedValue)
    expect(actualValue).toBe(expectedValue)
  })
})
