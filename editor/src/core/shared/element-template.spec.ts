import {
  attributeReferencesElsewhere,
  deleteJSXAttribute,
  emptyComments,
  jsxArrayValue,
  jsxAttributeFunctionCall,
  jsxAttributeNestedArray,
  jsxAttributeNestedObject,
  jsxAttributeOtherJavaScript,
  jsxAttributesEntry,
  jsxAttributesSpread,
  jsxAttributeValue,
  jsxElement,
  jsxPropertyAssignment,
  setJSXAttributesAttribute,
} from './element-template'

describe('setJSXAttributesAttribute', () => {
  const startingAttributesEntry = jsxAttributesEntry(
    'one',
    jsxAttributeValue('thing', emptyComments),
    emptyComments,
  )
  const startingAttributesSpread = jsxAttributesSpread(
    jsxAttributeValue('theRest', emptyComments),
    emptyComments,
  )
  const startingAttributes = [startingAttributesEntry, startingAttributesSpread]

  it('Setting a value on a new key adds it to the attributes', () => {
    const newKey = 'newKey'
    const newValue = jsxAttributeValue('newValue', emptyComments)
    const expected = [...startingAttributes, jsxAttributesEntry(newKey, newValue, emptyComments)]
    const actual = setJSXAttributesAttribute(startingAttributes, newKey, newValue)
    expect(actual).toEqual(expected)
  })

  it('Setting a value on an existing key updates the attributes', () => {
    const existingKey = 'one'
    const newValue = jsxAttributeValue('newValue', emptyComments)
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
    jsxAttributeValue('thing', emptyComments),
    emptyComments,
  )
  const startingAttributesSpread = jsxAttributesSpread(
    jsxAttributeValue('theRest', emptyComments),
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
    expect(attributeReferencesElsewhere(jsxAttributeValue(12, emptyComments))).toEqual(false)
  })
  it('ATTRIBUTE_OTHER_JAVASCRIPT returns true if it has a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsxAttributeOtherJavaScript('otherThing', 'return otherThing', ['otherThing'], null, {}),
      ),
    ).toEqual(true)
  })
  it('ATTRIBUTE_OTHER_JAVASCRIPT returns false if it does not have a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(jsxAttributeOtherJavaScript('5', 'return 5', [], null, {})),
    ).toEqual(false)
  })
  it('ATTRIBUTE_NESTED_ARRAY returns true if it has a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsxAttributeNestedArray(
          [
            jsxArrayValue(
              jsxAttributeOtherJavaScript(
                'otherThing',
                'return otherThing',
                ['otherThing'],
                null,
                {},
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
        jsxAttributeNestedArray(
          [
            jsxArrayValue(
              jsxAttributeOtherJavaScript('5', 'return 5', [], null, {}),
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
        jsxAttributeNestedObject(
          [
            jsxPropertyAssignment(
              'someKey',
              jsxAttributeOtherJavaScript(
                'otherThing',
                'return otherThing',
                ['otherThing'],
                null,
                {},
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
        jsxAttributeNestedObject(
          [
            jsxPropertyAssignment(
              'someKey',
              jsxAttributeOtherJavaScript('5', 'return 5', [], null, {}),
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
        jsxAttributeFunctionCall('someFn', [
          jsxAttributeOtherJavaScript('otherThing', 'return otherThing', ['otherThing'], null, {}),
        ]),
      ),
    ).toEqual(true)
  })
  it('ATTRIBUTE_FUNCTION_CALL returns false if it does not have a definedElsewhere entry', () => {
    expect(
      attributeReferencesElsewhere(
        jsxAttributeFunctionCall('someFn', [
          jsxAttributeOtherJavaScript('5', 'return 5', [], null, {}),
        ]),
      ),
    ).toEqual(false)
  })
})
