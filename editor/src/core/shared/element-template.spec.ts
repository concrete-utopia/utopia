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
