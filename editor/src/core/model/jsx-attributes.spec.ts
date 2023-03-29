import * as PP from '../shared/property-path'
import { deepFreeze } from '../../utils/deep-freeze'
import { Either, forceRight, isLeft, isRight, mapEither, right } from '../shared/either'
import {
  emptyComments,
  getJSXAttributeForced,
  modifiableAttributeIsAttributeFunctionCall,
  modifiableAttributeIsAttributeNotFound,
  isJSXAttributeValue,
  modifiableAttributeIsPartOfAttributeValue,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsxAttributeNestedArraySimple,
  jsExpressionNestedObject,
  jsxAttributeNestedObjectSimple,
  jsxAttributeNotFound,
  jsExpressionOtherJavaScript,
  JSXAttributes,
  jsxAttributesFromMap,
  jsxAttributesSpread,
  jsExpressionValue,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  simplifyAttributeIfPossible,
  modifiableAttributeIsAttributeValue,
  clearExpressionUniqueIDs,
  clearAttributesUniqueIDs,
} from '../shared/element-template'
import {
  dropKeyFromNestedObject,
  getAllPathsFromAttributes,
  getModifiableJSXAttributeAtPath,
  jsxAttributesToProps,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
  unsetJSXValueAtPath,
} from '../shared/jsx-attributes'
import { Substores } from '../../components/editor/store/store-hook'

const sampleParentProps = {
  hello: 'kitty',
  someShadow: 'shade',
}

function sampleJsxAttributes(): JSXAttributes {
  return deepFreeze<JSXAttributes>(
    jsxAttributesFromMap({
      style: jsExpressionNestedObject(
        [
          jsxSpreadAssignment(
            jsExpressionValue({ first: 1, second: 2 }, emptyComments),
            emptyComments,
          ),
          jsxPropertyAssignment(
            'backgroundColor',
            jsExpressionValue('red', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'shadow',
            jsExpressionOtherJavaScript(
              'props.someShadow',
              'return props.someShadow;',
              ['props'],
              null,
              {},
            ),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'border',
            jsExpressionValue('1px solid green', emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'boxShadow',
            jsExpressionValue('0 0 0 1px blue', emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
      top: jsExpressionValue(0, emptyComments),
      left: jsExpressionValue(50, emptyComments),
      height: jsExpressionValue(150, emptyComments),
      width: jsExpressionValue(200, emptyComments),
      layout: jsExpressionValue(
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
          array: jsExpressionValue([0, 1, 2], emptyComments),
        }),
        emptyComments,
      ),
      objectWithNestedArray: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          array: jsxAttributeNestedArraySimple([
            jsExpressionValue(0, emptyComments),
            jsExpressionValue(1, emptyComments),
            jsExpressionValue(2, emptyComments),
          ]),
        }),
        emptyComments,
      ),
      doggo: jsExpressionOtherJavaScript('props.hello', 'return props.hello;', ['props'], null, {}),
      objectValue: jsExpressionValue(
        {
          deep: {
            object: {
              path: 'yes',
            },
          },
        },
        emptyComments,
      ),
      otherJs: jsExpressionOtherJavaScript('true ? 10 : 5', 'return true ? 10 : 5', [], null, {}),
      'data-uid': jsExpressionValue('aaa', emptyComments),
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
  objectWithNestedArray: {
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
        PP.create('top'),
        jsExpressionValue(55, emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.top).toEqual(55)
  })

  it('sets a simple value replacing any other attribute that was there', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('otherJs'),
        jsExpressionValue('shadowy', emptyComments),
      ),
    )
    expect(getJSXAttributeForced(updatedAttributes, 'otherJs').type).toEqual('ATTRIBUTE_VALUE')
  })

  it('sets a simple value at a deep path and creates the necessary objects', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('my', 'property', 'path'),
        jsExpressionValue('hello', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.my.property.path).toEqual('hello')
  })

  it('updating an ATTRIBUTE_VALUE which is an object at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('layout', 'left'),
        jsExpressionValue(2000, emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.layout.left).toEqual(2000)
  })

  it('updating an ATTRIBUTE_VALUE which is an object at a deeper path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('layout', 'deep', 'path'),
        jsExpressionValue('easy!', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.layout.deep.path).toEqual('easy!')
  })

  it('updating an ATTRIBUTE_VALUE which is an array at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('objectWithArray', 'array', 2),
        jsExpressionValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.objectWithArray.array).toEqual([0, 1, 'wee'])
  })

  it('updating a NESTED_ARRAY at a deep path works', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('objectWithNestedArray', 'array', 2),
        jsExpressionValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.objectWithNestedArray.array).toEqual([0, 1, 'wee'])
  })

  it('updating a NESTED_ARRAY at a deep path works and it is resilient to string attribute keys', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('objectWithNestedArray', 'array', '2'),
        jsExpressionValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.objectWithNestedArray.array).toEqual([0, 1, 'wee'])
  })

  it('updating a NESTED_ARRAY with a string key converts it to object', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('objectWithNestedArray', 'array', 'wee'),
        jsExpressionValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.objectWithNestedArray.array).toEqual({ 0: 0, 1: 1, 2: 2, wee: 'wee' })
  })

  it('updating part of an ATTRIBUTE_VALUE which is a string throws error', () => {
    expect(() => {
      const updatedAttributes = forceRight(
        setJSXValueAtPath(
          sampleJsxAttributes(),
          PP.create('style', 'backgroundColor', 'red'),
          jsExpressionValue('wee', emptyComments),
        ),
      )
      const compiledProps = jsxAttributesToProps(
        'test.js',
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
        PP.create('style', 'backgroundColor'),
        jsExpressionValue('wee', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.style.backgroundColor).toEqual('wee')
  })

  it('setting two overlapping deep paths does not clash', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('my', 'property', 'path'),
        jsExpressionValue('hello', emptyComments),
      ),
    )
    const updatedAttributes2 = forceRight(
      setJSXValueAtPath(
        updatedAttributes,
        PP.create('my', 'property', 'other', 'path'),
        jsExpressionValue('hola', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes2,
      {},
    )
    expect(compiledProps.my.property.path).toEqual('hello')
    expect(compiledProps.my.property.other.path).toEqual('hola')
  })

  it('updates a simple value at a deep path that already exists', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath(
        sampleJsxAttributes(),
        PP.create('style', 'backgroundColor'),
        jsExpressionValue('blue', emptyComments),
      ),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps.style.backgroundColor).toEqual('blue')
  })

  it('should prevent setting a value inside a special prop', () => {
    const result1 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create('style', 'shadow', 'left'),
      jsExpressionValue('shadowy', emptyComments),
    )

    const result2 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create('style', 'boxShadow', '0'),
      jsExpressionValue('shadowy', emptyComments),
    )

    const result3 = setJSXValueAtPath(
      sampleJsxAttributes(),
      PP.create('otherJs', 'wrongProp'),
      jsExpressionValue('shadowy', emptyComments),
    )

    expect(result1.type).toBe('LEFT')
    expect(result2.type).toBe('LEFT')
    expect(result3.type).toBe('LEFT')
  })

  it('when setting an irregular duplicated property inside a JSX_ATTRIBUTE_VALUE, deduplicate it', () => {
    const attributes = jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          paddingLeft: 5,
          padding: 5,
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          paddingLeft: 15,
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          paddingLeft: 23,
        },
        emptyComments,
      ),
    })

    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      setJSXValueAtPath(
        attributes,
        PP.create('style', 'paddingLeft'),
        jsExpressionValue(100, emptyComments),
      ),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue(
            {
              paddingLeft: 100,
              padding: 5,
            },
            emptyComments,
          ),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })

  it('when setting an irregular duplicated property inside a JSX_ATTRIBUTE_NESTED_OBJECT, deduplicate it', () => {
    const attributes = jsxAttributesFromMap({
      style: jsExpressionNestedObject(
        [
          jsxPropertyAssignment(
            'paddingLeft',
            jsExpressionValue(5, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'padding',
            jsExpressionValue(5, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'paddingLeft',
            jsExpressionValue(15, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'paddingLeft',
            jsExpressionValue(23, emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })

    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      setJSXValueAtPath(
        attributes,
        PP.create('style', 'paddingLeft'),
        jsExpressionValue(100, emptyComments),
      ),
    )

    const expectedValue = right(
      clearAttributesUniqueIDs(
        jsxAttributesFromMap({
          style: jsExpressionValue(
            {
              paddingLeft: 100,
              padding: 5,
            },
            emptyComments,
          ),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })

  it('creates an array if the property path part is a number', () => {
    const updatedAttributes = forceRight(
      setJSXValueAtPath([], PP.create('top', 0), jsExpressionValue(55, emptyComments)),
    )
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      updatedAttributes,
      {},
    )
    expect(compiledProps).toEqual({ top: [55] })
  })
})

describe('jsxAttributesToProps', () => {
  it('works', () => {
    const compiledProps = jsxAttributesToProps(
      'test.js',
      { props: sampleParentProps },
      sampleJsxAttributes(),
      {},
    )
    expect(compiledProps).toEqual(expectedCompiledProps)
  })

  it('even supports irregular duplicated property inside a JSX_ATTRIBUTE_VALUE', () => {
    const attributes = jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          paddingLeft: 5,
          padding: 5,
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          paddingLeft: 15,
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          paddingLeft: 23,
        },
        emptyComments,
      ),
    })
    const compiledProps = jsxAttributesToProps('test.js', {}, attributes, {})

    expect(compiledProps).toEqual({ style: { paddingLeft: 23, padding: 5 } })
    expect(Object.entries(compiledProps.style)).toEqual([
      ['paddingLeft', 23],
      ['padding', 5],
    ])
  })

  it('even supports irregular duplicated property inside a JSX_ATTRIBUTE_NESTED_OBJECT', () => {
    const attributes = jsxAttributesFromMap({
      style: jsExpressionNestedObject(
        [
          jsxPropertyAssignment(
            'paddingLeft',
            jsExpressionValue(5, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'padding',
            jsExpressionValue(5, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'paddingLeft',
            jsExpressionValue(15, emptyComments),
            emptyComments,
            emptyComments,
          ),
          jsxPropertyAssignment(
            'paddingLeft',
            jsExpressionValue(23, emptyComments),
            emptyComments,
            emptyComments,
          ),
        ],
        emptyComments,
      ),
    })

    const compiledProps = jsxAttributesToProps('test.js', {}, attributes, {})

    expect(compiledProps).toEqual({ style: { paddingLeft: 23, padding: 5 } })
    expect(Object.entries(compiledProps.style)).toEqual([
      ['paddingLeft', 23],
      ['padding', 5],
    ])
  })
})

describe('getModifiableJSXAttributeAtPath', () => {
  it('gets a simple value', () => {
    const backgroundColor = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create('style', 'backgroundColor'),
    )
    expect(isRight(backgroundColor)).toBeTruthy()
    if (isRight(backgroundColor)) {
      expect(backgroundColor).not.toBeUndefined()
      expect(backgroundColor).not.toBeNull()
      if (
        backgroundColor != null &&
        backgroundColor.value != null &&
        modifiableAttributeIsAttributeValue(backgroundColor.value)
      ) {
        expect(backgroundColor.value.value).toEqual('red')
      } else {
        throw new Error('backgroundColor is not a JSXAttributeValue')
      }
    }
  })

  it('drills correctly into a simple value containing an object', () => {
    const foundAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create('objectValue', 'deep', 'object', 'path'),
    )
    expect(isRight(foundAttribute)).toBeTruthy()
    if (isRight(foundAttribute)) {
      expect(modifiableAttributeIsPartOfAttributeValue(foundAttribute.value)).toBeTruthy()
      expect((foundAttribute.value as any).value).toEqual('yes')
    }

    const missingAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create('objectValue', 'deep', 'object', 'missing', 'path'),
    )
    expect(isRight(missingAttribute)).toBeTruthy()
    if (isRight(missingAttribute)) {
      expect(modifiableAttributeIsAttributeNotFound(missingAttribute.value)).toBeTruthy()
    }

    const completelyMissingAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create('cats', 'dogs', 'missing', 'path'),
    )
    expect(completelyMissingAttribute).toEqual(right(jsxAttributeNotFound()))
  })

  it('returns Right on a path that CAN be updated by an action', () => {
    const impossibleAttribute = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create('bad', 'path'),
    )
    expect(isRight(impossibleAttribute)).toBeTruthy()
    expect(impossibleAttribute.value as any).toEqual(jsxAttributeNotFound())

    // this is an interesting case. we actually drilled into a found value object, but then did not find anything for this path
    // so now we return a right(JSXAttributeNotFound),
    // where the meaning is: you can dispatch an action to set this path, but right now it
    // does not contain any real value
    const impossibleAttributeInsideAValue = getModifiableJSXAttributeAtPath(
      sampleJsxAttributes(),
      PP.create('top', 'what'),
    )
    expect(isRight(impossibleAttributeInsideAValue)).toBeTruthy()
    expect(impossibleAttributeInsideAValue.value).toEqual(jsxAttributeNotFound())
  })
})

describe('jsxSimpleAttributeToValue', () => {
  it('gets the value of a nested object attribute', () => {
    const attribute = jsxAttributeNestedObjectSimple(
      jsxAttributesFromMap({
        top: jsExpressionValue(50, emptyComments),
        left: jsExpressionValue(100, emptyComments),
      }),
      emptyComments,
    )
    const attributeValue = jsxSimpleAttributeToValue(attribute)
    const expectedAttrValue = {
      top: 50,
      left: 100,
    }
    expect(isRight(attributeValue)).toBeTruthy()
    expect(attributeValue.value).toEqual(expectedAttrValue)
  })
})

describe('unsetJSXValueAtPath', () => {
  it('removes an attribute from the root of the attributes', () => {
    const startingValue = jsxAttributesFromMap({
      left: jsExpressionValue(0, emptyComments),
      top: jsExpressionValue(0, emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('left')),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          top: jsExpressionValue(0, emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from the root of the attributes that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      top: jsExpressionValue(0, emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('left')),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          top: jsExpressionValue(0, emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute object', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsExpressionValue({ left: 0, top: 0 }, emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 'left')),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue({ top: 0 }, emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute object that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsExpressionValue({ top: 0 }, emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 'left')),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue({ top: 0 }, emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute array', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsExpressionValue([0, 1], emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 1)),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue([0], emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from an attribute array that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsExpressionValue([0], emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 1)),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue([0], emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('fails when attempting to remove a property from an invalid attribute value', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsExpressionOtherJavaScript('undefined', 'undefined', [], null, {}),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = unsetJSXValueAtPath(startingValue, PP.create('style', 1))
    expect(isLeft(actualValue)).toBe(true)
  })

  it('removes an attribute from a nested object', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          left: jsExpressionValue({ x: 0, y: 0 }, emptyComments),
          top: jsExpressionValue({ x: 1, y: 1 }, emptyComments),
        }),
        emptyComments,
      ),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 'left')),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue({ top: { x: 1, y: 1 } }, emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a object that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({ top: jsExpressionValue({ x: 1, y: 1 }, emptyComments) }),
        emptyComments,
      ),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 'left', 'x')),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue({ top: { x: 1, y: 1 } }, emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a nested array', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedArraySimple([
        jsExpressionValue(0, emptyComments),
        jsExpressionValue(1, emptyComments),
      ]),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 1)),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue([0], emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes an attribute from a nested array that does not exist', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedArraySimple([jsExpressionValue(0, emptyComments)]),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(startingValue, PP.create('style', 1)),
    )
    const expectedValue = mapEither(
      clearAttributesUniqueIDs,
      right(
        jsxAttributesFromMap({
          style: jsExpressionValue([0], emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
  it('removes a deeply nested value', () => {
    const startingValue = jsxAttributesFromMap({
      style: jsxAttributeNestedObjectSimple(
        jsxAttributesFromMap({
          left: jsxAttributeNestedArraySimple([
            jsExpressionValue('29', emptyComments),
            jsxAttributeNestedObjectSimple(
              jsxAttributesFromMap({
                stateEnabled: jsxAttributeNestedArraySimple([
                  jsExpressionValue(
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
          top: jsExpressionValue({ x: 1, y: 1 }, emptyComments),
        }),
        emptyComments,
      ),
      backgroundColor: jsExpressionValue('red', emptyComments),
      'data-uid': jsExpressionValue('aaa', emptyComments),
    })
    const actualValue = mapEither(
      clearAttributesUniqueIDs,
      unsetJSXValueAtPath(
        startingValue,
        PP.create('style', 'left', 1, 'stateEnabled', 0, 'lightSide', 'eleven'),
      ),
    )
    const expectedValue = right(
      clearAttributesUniqueIDs(
        jsxAttributesFromMap({
          style: jsExpressionValue(
            {
              left: [
                '29',
                {
                  stateEnabled: [
                    {
                      lightSide: {
                        ten: 10,
                      },
                      darkSide: {
                        twelve: 12,
                        nine: 9,
                      },
                    },
                  ],
                },
              ],
              top: { x: 1, y: 1 },
            },
            emptyComments,
          ),
          backgroundColor: jsExpressionValue('red', emptyComments),
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
      ),
    )
    expect(actualValue).toEqual(expectedValue)
  })
})

describe('dropKeyFromNestedObject', () => {
  it('only removes the pertinent attribute from a nested object', () => {
    const startingValue = jsExpressionNestedObject(
      [
        jsxSpreadAssignment(jsExpressionValue(Substores.theme, emptyComments), emptyComments),
        jsxPropertyAssignment(
          'backgroundColor',
          jsExpressionValue('red', emptyComments),
          emptyComments,
          emptyComments,
        ),
      ],
      emptyComments,
    )
    const expectedValue = clearExpressionUniqueIDs(
      jsExpressionNestedObject(
        [jsxSpreadAssignment(jsExpressionValue(Substores.theme, emptyComments), emptyComments)],
        emptyComments,
      ),
    )
    const actualValue = clearExpressionUniqueIDs(
      dropKeyFromNestedObject(startingValue, 'backgroundColor'),
    )
    expect(actualValue).toEqual(expectedValue)
  })
})

describe('simplifyAttributeIfPossible', () => {
  it('nested array with a nested object in it simplifies down', () => {
    const array = jsExpressionNestedArray(
      [
        jsxArrayValue(jsExpressionValue(1, emptyComments), emptyComments),
        jsxArrayValue(jsExpressionValue('hat', emptyComments), emptyComments),
        jsxArrayValue(
          jsExpressionNestedObject(
            [
              jsxPropertyAssignment(
                'someKey',
                jsExpressionValue('with a value', emptyComments),
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
    const actualResult = clearExpressionUniqueIDs(simplifyAttributeIfPossible(array))
    const expectedResult = clearExpressionUniqueIDs(
      jsExpressionValue([1, 'hat', { someKey: 'with a value' }], emptyComments),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('simple value returns as is', () => {
    const expectedValue = jsExpressionValue('test', emptyComments)
    const actualValue = simplifyAttributeIfPossible(expectedValue)
    expect(actualValue).toBe(expectedValue)
  })
})

describe('getAllPathsFromAttributes', () => {
  it('works for a simple case', () => {
    const result = getAllPathsFromAttributes(
      jsxAttributesFromMap({ cica: jsExpressionValue('hello!', emptyComments) }),
    )
    expect(result).toEqual([PP.create('cica')])
  })

  it('works for a nested object', () => {
    const result = getAllPathsFromAttributes(
      jsxAttributesFromMap({
        cica: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            deep: jsxAttributeNestedObjectSimple(
              jsxAttributesFromMap({ path: jsExpressionValue(5, emptyComments) }),
              emptyComments,
            ),
          }),
          emptyComments,
        ),
      }),
    )
    expect(result).toEqual([
      PP.create('cica'),
      PP.create('cica', 'deep'),
      PP.create('cica', 'deep', 'path'),
    ])
  })

  it('works with spread assignment', () => {
    const result = getAllPathsFromAttributes(
      jsxAttributesFromMap({
        cica: jsExpressionNestedObject(
          [
            jsxSpreadAssignment(
              jsxAttributeNestedObjectSimple(
                jsxAttributesFromMap({ path: jsExpressionValue(5, emptyComments) }),
                emptyComments,
              ),
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      }),
    )
    expect(result).toEqual([{ propertyElements: ['cica'] }, { propertyElements: ['cica', 'path'] }])
  })

  it('works for a nested array', () => {
    const result = getAllPathsFromAttributes(
      jsxAttributesFromMap({
        cica: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            deep: jsxAttributeNestedArraySimple([jsExpressionValue(5, emptyComments)]),
          }),
          emptyComments,
        ),
      }),
    )
    expect(result).toEqual([
      PP.create('cica'),
      PP.create('cica', 'deep'),
      PP.create('cica', 'deep', 0),
    ])
  })

  it('does not drill into function parameters', () => {
    const result = getAllPathsFromAttributes(
      jsxAttributesFromMap({
        cica: jsxAttributeNestedObjectSimple(
          jsxAttributesFromMap({
            deep: jsExpressionFunctionCall('functionName', [
              jsExpressionValue(5, emptyComments),
              jsExpressionValue({ objectKey: 'hello' }, emptyComments),
            ]),
          }),
          emptyComments,
        ),
      }),
    )
    expect(result).toEqual([PP.create('cica'), PP.create('cica', 'deep')])
  })

  it('drills into paths of object values too', () => {
    const result = getAllPathsFromAttributes(
      jsxAttributesFromMap({ cica: jsExpressionValue({ deep: { path: 5 } }, emptyComments) }),
    )
    expect(result).toEqual([
      PP.create('cica'),
      PP.create('cica', 'deep'),
      PP.create('cica', 'deep', 'path'),
    ])
  })
})
