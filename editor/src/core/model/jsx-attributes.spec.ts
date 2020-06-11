import * as PP from '../shared/property-path'
import { deepFreeze } from '../../utils/deep-freeze'
import { forceRight, isLeft, isRight, right } from '../shared/either'
import {
  isJSXAttributeFunctionCall,
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isPartOfJSXAttributeValue,
  jsxAttributeFunctionCall,
  jsxAttributeNestedArraySimple,
  jsxAttributeNestedObject,
  jsxAttributeNestedObjectSimple,
  jsxAttributeNotFound,
  jsxAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributeValue,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
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

const sampleParentProps = {
  hello: 'kitty',
  someShadow: 'shade',
}

function sampleJsxAttributes(): JSXAttributes {
  return deepFreeze({
    style: jsxAttributeNestedObject([
      jsxSpreadAssignment(jsxAttributeValue({ first: 1, second: 2 })),
      jsxPropertyAssignment('backgroundColor', jsxAttributeValue('red')),
      jsxPropertyAssignment(
        'shadow',
        jsxAttributeOtherJavaScript(
          'props.someShadow',
          'return props.someShadow;',
          ['props'],
          null,
        ),
      ),
      jsxPropertyAssignment('border', jsxAttributeValue('1px solid green')),
      jsxPropertyAssignment('boxShadow', jsxAttributeValue('0 0 0 1px blue')),
    ]),
    top: jsxAttributeValue(0),
    left: jsxAttributeValue(50),
    height: jsxAttributeValue(150),
    width: jsxAttributeValue(200),
    layout: jsxAttributeValue({
      left: 50,
      deep: {
        path: 'hard!',
      },
    }),
    objectWithArray: jsxAttributeNestedObjectSimple({
      array: jsxAttributeValue([0, 1, 2]),
    }),
    doggo: jsxAttributeOtherJavaScript('props.hello', 'return props.hello;', ['props'], null),
    objectValue: jsxAttributeValue({
      deep: {
        object: {
          path: 'yes',
        },
      },
    }),
    otherJs: jsxAttributeOtherJavaScript('true ? 10 : 5', 'return true ? 10 : 5', [], null),
    'data-uid': jsxAttributeValue('aaa'),
  })
}

const expectedCompiledProps = {
  style: {
    first: 1,
    second: 2,
    backgroundColor: 'red',
    shadow: 'shade',
    border: '1px solid green',
    boxShadow: 'inset 0 0 0 0px #000, 0 0 0 1px blue',
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
      setJSXValueAtPath(sampleJsxAttributes(), PP.create(['top']), jsxAttributeValue(55)),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.top).toEqual(55)
  })

  it('sets a simple value replacing any other attribute that was there', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['otherJs']),
        jsxAttributeValue('shadowy'),
      ),
    )
    expect(updatedAttributes.otherJs.type).toEqual('ATTRIBUTE_VALUE')
  })

  it('sets a simple value at a deep path and creates the necessary objects', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['my', 'property', 'path']),
        jsxAttributeValue('hello'),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.my.property.path).toEqual('hello')
  })

  it('updating an ATTRIBUTE_VALUE which is an object at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['layout', 'left']),
        jsxAttributeValue(2000),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.layout.left).toEqual(2000)
  })

  it('updating an ATTRIBUTE_VALUE which is an object at a deeper path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['layout', 'deep', 'path']),
        jsxAttributeValue('easy!'),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.layout.deep.path).toEqual('easy!')
  })

  it('updating an ATTRIBUTE_VALUE which is an array at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['objectWithArray', 'array', 2]),
        jsxAttributeValue('wee'),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.objectWithArray.array).toEqual([0, 1, 'wee'])
  })

  it('updating part of an ATTRIBUTE_VALUE which is a string throws error', () => {
    expect(() => {
      const updatedAttributes = forceRight(
        setJSXValueAtPath(
          sampleJsxAttributes(),
          PP.create(['style', 'backgroundColor', 'red']),
          jsxAttributeValue('wee'),
        ),
      )
      const compiledProps = jsxAttributesToProps(
        {},
        updatedAttributes,
        sampleParentProps,
        {},
        NO_OP,
      )
    }).toThrow()
  })

  it('updating a NESTED_OJECT at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['style', 'backgroundColor']),
        jsxAttributeValue('wee'),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.style.backgroundColor).toEqual('wee')
  })

  it('setting two overlapping deep paths does not clash', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['my', 'property', 'path']),
        jsxAttributeValue('hello'),
      ),
    )
    const updatedAttributes2 = forceRight(
      setJSXValueAtPath(
        updatedAttributes,
        PP.create(['my', 'property', 'other', 'path']),
        jsxAttributeValue('hola'),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes2, sampleParentProps, {}, NO_OP)
    expect(compiledProps.my.property.path).toEqual('hello')
    expect(compiledProps.my.property.other.path).toEqual('hola')
  })

  it('updates a simple value at a deep path that already exists', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create(['style', 'backgroundColor']),
        jsxAttributeValue('blue'),
      ),
    )
    const compiledProps = jsxAttributesToProps({}, updatedAttributes, sampleParentProps, {}, NO_OP)
    expect(compiledProps.style.backgroundColor).toEqual('blue')
  })

  it('should prevent setting a value inside a special prop', () => {
    const result1 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'shadow', 'left']),
      jsxAttributeValue('shadowy'),
    )

    const result2 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'boxShadow', '0']),
      jsxAttributeValue('shadowy'),
    )

    const result3 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create(['otherJs', 'wrongProp']),
      jsxAttributeValue('shadowy'),
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
      sampleParentProps,
      {},
      NO_OP,
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

  it('returns a function call attribute correctly', () => {
    const boxShadow = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'boxShadow']),
    )
    expect(isRight(boxShadow)).toBeTruthy()
    expect(isJSXAttributeFunctionCall(boxShadow.value as any)).toBeTruthy()
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

  it('returns Left for a path that is inside a complex attribute', () => {
    // getModifiableJSXAttributeAtPath will return a Left for paths that not only don't contain a value
    // but you are not even allowed to dispatch an update for them, because they are "controlled"
    // meaning one of their parent paths is referring to a node graph value, a props usage, or a function call
    const attributeInsideANonExpandablAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'shadow', 'nothingGood']),
    )
    expect(isLeft(attributeInsideANonExpandablAttribute)).toBeTruthy()
    const attributeInsideANonExpandablAttribute2 = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create(['style', 'boxShadow', 'nothingGood']),
    )
    expect(isLeft(attributeInsideANonExpandablAttribute2)).toBeTruthy()
  })
})

describe('jsxSimpleAttributeToValue', () => {
  it('gets the value of a nested object attribute', () => {
    const attribute = jsxAttributeNestedObjectSimple({
      top: jsxAttributeValue(50),
      left: jsxAttributeValue(100),
    })
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
    const startingValue = {
      left: jsxAttributeValue(0),
      top: jsxAttributeValue(0),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['left']))
    const expectedValue = right({ top: jsxAttributeValue(0), 'data-uid': jsxAttributeValue('aaa') })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from the root of the attributes that does not exist', () => {
    const startingValue = { top: jsxAttributeValue(0), 'data-uid': jsxAttributeValue('aaa') }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['left']))
    const expectedValue = right({ top: jsxAttributeValue(0), 'data-uid': jsxAttributeValue('aaa') })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute object', () => {
    const startingValue = {
      style: jsxAttributeValue({ left: 0, top: 0 }),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left']))
    const expectedValue = right({
      style: jsxAttributeValue({ top: 0 }),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute object that does not exist', () => {
    const startingValue = {
      style: jsxAttributeValue({ top: 0 }),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left']))
    const expectedValue = right({
      style: jsxAttributeValue({ top: 0 }),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute array', () => {
    const startingValue = { style: jsxAttributeValue([0, 1]), 'data-uid': jsxAttributeValue('aaa') }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right({
      style: jsxAttributeValue([0]),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute array that does not exist', () => {
    const startingValue = { style: jsxAttributeValue([0]), 'data-uid': jsxAttributeValue('aaa') }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right({
      style: jsxAttributeValue([0]),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('fails when attempting to remove a property from an invalid attribute value', () => {
    const startingValue = {
      style: jsxAttributeOtherJavaScript('undefined', 'undefined', [], null),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    expect(isLeft(actualValue)).toBe(true)
  })

  it('removes an attribute from a nested object', () => {
    const startingValue = {
      style: jsxAttributeNestedObjectSimple({
        left: jsxAttributeValue({ x: 0, y: 0 }),
        top: jsxAttributeValue({ x: 1, y: 1 }),
      }),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left']))
    const expectedValue = right({
      style: jsxAttributeNestedObjectSimple({ top: jsxAttributeValue({ x: 1, y: 1 }) }),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a object that does not exist', () => {
    const startingValue = {
      style: jsxAttributeNestedObjectSimple({ top: jsxAttributeValue({ x: 1, y: 1 }) }),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 'left', 'x']))
    const expectedValue = right({
      style: jsxAttributeNestedObjectSimple({ top: jsxAttributeValue({ x: 1, y: 1 }) }),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a nested array', () => {
    const startingValue = {
      style: jsxAttributeNestedArraySimple([jsxAttributeValue(0), jsxAttributeValue(1)]),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right({
      style: jsxAttributeNestedArraySimple([jsxAttributeValue(0)]),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a nested array that does not exist', () => {
    const startingValue = {
      style: jsxAttributeNestedArraySimple([jsxAttributeValue(0)]),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create(['style', 1]))
    const expectedValue = right({
      style: jsxAttributeNestedArraySimple([jsxAttributeValue(0)]),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes a deeply nested value', () => {
    const startingValue = {
      style: jsxAttributeNestedObjectSimple({
        left: jsxAttributeNestedArraySimple([
          jsxAttributeValue('29'),
          jsxAttributeNestedObjectSimple({
            stateEnabled: jsxAttributeNestedArraySimple([
              jsxAttributeValue({
                lightSide: {
                  eleven: 11,
                  ten: 10,
                },
                darkSide: {
                  twelve: 12,
                  nine: 9,
                },
              }),
            ]),
          }),
        ]),
        top: jsxAttributeValue({ x: 1, y: 1 }),
      }),
      backgroundColor: jsxAttributeValue('red'),
      'data-uid': jsxAttributeValue('aaa'),
    }
    const actualValue = unsetJSXValueAtPath(
      startingValue,
      PP.create(['style', 'left', 1, 'stateEnabled', 0, 'lightSide', 'eleven']),
    )
    const expectedValue = right({
      style: jsxAttributeNestedObjectSimple({
        left: jsxAttributeNestedArraySimple([
          jsxAttributeValue('29'),
          jsxAttributeNestedObjectSimple({
            stateEnabled: jsxAttributeNestedArraySimple([
              jsxAttributeValue({
                lightSide: {
                  ten: 10,
                },
                darkSide: {
                  twelve: 12,
                  nine: 9,
                },
              }),
            ]),
          }),
        ]),
        top: jsxAttributeValue({ x: 1, y: 1 }),
      }),
      backgroundColor: jsxAttributeValue('red'),
      'data-uid': jsxAttributeValue('aaa'),
    })
    expect(actualValue).toEqual(expectedValue)
  })
})

describe('dropKeyFromNestedObject', () => {
  it('only removes the pertinent attribute from a nested object', () => {
    const startingValue = jsxAttributeNestedObject([
      jsxSpreadAssignment(jsxAttributeValue('theme')),
      jsxPropertyAssignment('backgroundColor', jsxAttributeValue('red')),
    ])
    const expectedValue = jsxAttributeNestedObject([
      jsxSpreadAssignment(jsxAttributeValue('theme')),
    ])
    const actualValue = dropKeyFromNestedObject(startingValue, 'backgroundColor')
    expect(actualValue).toEqual(expectedValue)
  })
})
