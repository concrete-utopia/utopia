import {
  deleteJSXAttribute,
  emptyComments,
  jsxAttributesEntry,
  jsxAttributesSpread,
  jsxAttributeValue,
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
