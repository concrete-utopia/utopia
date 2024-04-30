import { left, right } from './either'
import {
  attributeReferencesElsewhere,
  deleteJSXAttribute,
  emptyComments,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsExpressionOtherJavaScript,
  jsxAttributesEntry,
  jsxAttributesSpread,
  jsExpressionValue,
  jsxPropertyAssignment,
  setJSXAttributesAttribute,
  modifiableAttributeToValuePath,
  jsIdentifier,
  jsPropertyAccess,
  jsElementAccess,
  jsExpressionOtherJavaScriptSimple,
  jsxMapExpression,
  jsxElement,
  jsOpaqueArbitraryStatement,
  jsxAttributeNestedArraySimple,
  functionParam,
  regularParam,
} from './element-template'

describe('setJSXAttributesAttribute', () => {
  const startingAttributesEntry = jsxAttributesEntry(
    'one',
    jsExpressionValue('thing', emptyComments),
    emptyComments,
  )
  const startingAttributesSpread = jsxAttributesSpread(
    jsExpressionValue('theRest', emptyComments),
    emptyComments,
  )
  const startingAttributes = [startingAttributesEntry, startingAttributesSpread]

  it('Setting a value on a new key adds it to the attributes', () => {
    const newKey = 'newKey'
    const newValue = jsExpressionValue('newValue', emptyComments)
    const expected = [...startingAttributes, jsxAttributesEntry(newKey, newValue, emptyComments)]
    const actual = setJSXAttributesAttribute(startingAttributes, newKey, newValue)
    expect(actual).toEqual(expected)
  })

  it('Setting a value on an existing key updates the attributes', () => {
    const existingKey = 'one'
    const newValue = jsExpressionValue('newValue', emptyComments)
    const expected = [
      jsxAttributesEntry(existingKey, newValue, emptyComments),
      startingAttributesSpread,
    ]
    const actual = setJSXAttributesAttribute(startingAttributes, existingKey, newValue)
    expect(actual).toEqual(expected)
  })
})

describe('deleteJSXAttribute', () => {
  const startingAttributesEntry = jsxAttributesEntry(
    'one',
    jsExpressionValue('thing', emptyComments),
    emptyComments,
  )
  const startingAttributesSpread = jsxAttributesSpread(
    jsExpressionValue('theRest', emptyComments),
    emptyComments,
  )
  const startingAttributes = [startingAttributesEntry, startingAttributesSpread]

  it('deleting a non-existent key leaves the attributes unchanged', () => {
    const actual = deleteJSXAttribute(startingAttributes, 'nonExistentKey')
    expect(actual).toEqual(startingAttributes)
  })

  it('deleting an existing key updates the attributes', () => {
    const existingKey = 'one'
    const expected = [startingAttributesSpread]
    const actual = deleteJSXAttribute(startingAttributes, existingKey)
    expect(actual).toEqual(expected)
  })
})

describe('attributeReferencesElsewhere', () => {
  it('ATTRIBUTE_VALUE always returns false', () => {
    expect(attributeReferencesElsewhere(jsExpressionValue(12, emptyComments))).toEqual(false)
  })
  it('ATTRIBUTE_OTHER_JAVASCRIPT returns true if it has a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionOtherJavaScript(
          [],
          'otherThing',
          'otherThing',
          'return otherThing',
          ['otherThing'],
          null,
          {},
          emptyComments,
        ),
      ),
    ).toEqual(true)
  })
  it('ATTRIBUTE_OTHER_JAVASCRIPT returns false if it does not have a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionOtherJavaScript([], '5', '5', 'return 5', [], null, {}, emptyComments),
      ),
    ).toEqual(false)
  })
  it('ATTRIBUTE_NESTED_ARRAY returns true if it has a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionNestedArray(
          [
            jsxArrayValue(
              jsExpressionOtherJavaScript(
                [],
                'otherThing',
                'otherThing',
                'return otherThing',
                ['otherThing'],
                null,
                {},
                emptyComments,
              ),
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      ),
    ).toEqual(true)
  })
  it('ATTRIBUTE_NESTED_ARRAY returns false if it does not have a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionNestedArray(
          [
            jsxArrayValue(
              jsExpressionOtherJavaScript([], '5', '5', 'return 5', [], null, {}, emptyComments),
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      ),
    ).toEqual(false)
  })
  it('ATTRIBUTE_NESTED_OBJECT returns true if it has a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionNestedObject(
          [
            jsxPropertyAssignment(
              'someKey',
              jsExpressionOtherJavaScript(
                [],
                'otherThing',
                'otherThing',
                'return otherThing',
                ['otherThing'],
                null,
                {},
                emptyComments,
              ),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      ),
    ).toEqual(true)
  })
  it('ATTRIBUTE_NESTED_OBJECT returns false if it does not have a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionNestedObject(
          [
            jsxPropertyAssignment(
              'someKey',
              jsExpressionOtherJavaScript([], '5', '5', 'return 5', [], null, {}, emptyComments),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
      ),
    ).toEqual(false)
  })

  it('ATTRIBUTE_FUNCTION_CALL returns true if it has a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionFunctionCall('someFn', [
          jsExpressionOtherJavaScript(
            [],
            'otherThing',
            'otherThing',
            'return otherThing',
            ['otherThing'],
            null,
            {},
            emptyComments,
          ),
        ]),
      ),
    ).toEqual(true)
  })
  xit('ATTRIBUTE_FUNCTION_CALL returns false if it does not have a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsExpressionFunctionCall('someFn', [
          jsExpressionOtherJavaScript([], '5', '5', 'return 5', [], null, {}, emptyComments),
        ]),
      ),
    ).toEqual(false)
  })
})

describe('modifiableAttributeToValuePath', () => {
  it('works for an identifier', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsIdentifier('thing', 'thing-uid', null, emptyComments),
    )
    expect(actualResult).toEqual(right(['thing']))
  })
  it('works for a simple property access', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsPropertyAccess(
        jsIdentifier('thing', 'thing-uid', null, emptyComments),
        'property',
        'property-uid',
        null,
        emptyComments,
        'thing.property',
        'not-optionally-chained',
      ),
    )
    expect(actualResult).toEqual(right(['thing', 'property']))
  })
  it('works for a simple element access', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsElementAccess(
        jsIdentifier('thing', 'thing-uid', null, emptyComments),
        jsExpressionValue(1, emptyComments, 'one-uid'),
        'element-uid',
        null,
        emptyComments,
        'thing[1]',
        'not-optionally-chained',
      ),
    )
    expect(actualResult).toEqual(right(['thing', 1]))
  })
  it('rejects a property access involving something arbitrary', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsPropertyAccess(
        jsExpressionOtherJavaScriptSimple('something()', ['something']),
        'property',
        'property-uid',
        null,
        emptyComments,
        'thing.property',
        'not-optionally-chained',
      ),
    )
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects an element access with an arbitrary onValue', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsElementAccess(
        jsExpressionOtherJavaScriptSimple('something()', ['something']),
        jsExpressionValue(1, emptyComments, 'one-uid'),
        'element-uid',
        null,
        emptyComments,
        'thing[1]',
        'not-optionally-chained',
      ),
    )
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects an element access with an arbitrary element', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsElementAccess(
        jsIdentifier('thing', 'thing-uid', null, emptyComments),
        jsExpressionOtherJavaScriptSimple('something()', ['something']),
        'element-uid',
        null,
        emptyComments,
        'thing[1]',
        'not-optionally-chained',
      ),
    )
    expect(actualResult).toEqual(left('Unable to handle this element access.'))
  })
  it('rejects an expression value', () => {
    const actualResult = modifiableAttributeToValuePath(jsExpressionValue('thing', emptyComments))
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects an other javascript', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsExpressionOtherJavaScriptSimple('something()', ['something']),
    )
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects a nested array', () => {
    const actualResult = modifiableAttributeToValuePath(jsExpressionNestedArray([], emptyComments))
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects a nested object', () => {
    const actualResult = modifiableAttributeToValuePath(jsExpressionNestedObject([], emptyComments))
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects a function call', () => {
    const actualResult = modifiableAttributeToValuePath(jsExpressionFunctionCall('something', []))
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects a map expression', () => {
    const actualResult = modifiableAttributeToValuePath(
      jsxMapExpression(
        jsxAttributeNestedArraySimple([
          jsExpressionValue(1, emptyComments, ''),
          jsExpressionValue(2, emptyComments, ''),
          jsExpressionValue(3, emptyComments, ''),
        ]),
        jsExpressionOtherJavaScript(
          [functionParam(false, regularParam('a', null))],
          `a => <div />`,
          `a => <div />`,
          `a => <div />`,
          [],
          null,
          {},
          emptyComments,
          '',
        ),
        emptyComments,
        [],
        '',
      ),
    )
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
  it('rejects a jsx element', () => {
    const actualResult = modifiableAttributeToValuePath(jsxElement('div', 'div-uid', [], []))
    expect(actualResult).toEqual(left('Unable to handle this expression type.'))
  })
})
