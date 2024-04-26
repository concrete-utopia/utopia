import {
  parseBoolean,
  parseNumber,
  parseString,
  descriptionParseError,
  objectParser,
  parseArray,
  parseObject,
  parseConstant,
  optionalProp,
} from './value-parser-utils'
import { left, right } from '../core/shared/either'

describe('parseBoolean', () => {
  it('returns a left for something which is not a boolean', () => {
    expect(parseBoolean(9)).toEqual(left(descriptionParseError('Not a boolean.')))
    expect(parseBoolean('hat')).toEqual(left(descriptionParseError('Not a boolean.')))
    expect(parseBoolean({})).toEqual(left(descriptionParseError('Not a boolean.')))
    expect(parseBoolean([])).toEqual(left(descriptionParseError('Not a boolean.')))
    expect(parseBoolean(null)).toEqual(left(descriptionParseError('Not a boolean.')))
    expect(parseBoolean(undefined)).toEqual(left(descriptionParseError('Not a boolean.')))
  })
  it('returns a right for something which is a boolean', () => {
    expect(parseBoolean(true)).toEqual(right(true))
    expect(parseBoolean(false)).toEqual(right(false))
  })
})

describe('parseNumber', () => {
  it('returns a left for something which is not a number', () => {
    expect(parseNumber('hat')).toEqual(left(descriptionParseError('Not a number.')))
    expect(parseNumber(true)).toEqual(left(descriptionParseError('Not a number.')))
    expect(parseNumber({})).toEqual(left(descriptionParseError('Not a number.')))
    expect(parseNumber([])).toEqual(left(descriptionParseError('Not a number.')))
    expect(parseNumber(null)).toEqual(left(descriptionParseError('Not a number.')))
    expect(parseNumber(undefined)).toEqual(left(descriptionParseError('Not a number.')))
  })
  it('returns a right for something which is a number', () => {
    expect(parseNumber(1)).toEqual(right(1))
    expect(parseNumber(0)).toEqual(right(0))
    expect(parseNumber(-1)).toEqual(right(-1))
    expect(parseNumber(1000000)).toEqual(right(1000000))
    expect(parseNumber(-1000000)).toEqual(right(-1000000))
  })
})

describe('parseString', () => {
  it('returns a left for something which is not a string', () => {
    expect(parseString(9)).toEqual(left(descriptionParseError('Not a string.')))
    expect(parseString(true)).toEqual(left(descriptionParseError('Not a string.')))
    expect(parseString({})).toEqual(left(descriptionParseError('Not a string.')))
    expect(parseString([])).toEqual(left(descriptionParseError('Not a string.')))
    expect(parseString(null)).toEqual(left(descriptionParseError('Not a string.')))
    expect(parseString(undefined)).toEqual(left(descriptionParseError('Not a string.')))
  })
  it('returns a right for something which is a string', () => {
    expect(parseString('')).toEqual(right(''))
    expect(parseString('hat')).toEqual(right('hat'))
  })
})

describe('objectParser', () => {
  it('can parse object', () => {
    const input = {
      string: 'string',
      number: 1,
      bool: true,
      array: [1, 2, 3, 4],
      object: { hello: { there: [true, true, false, true] } },
    }
    const result = objectParser({
      string: parseString,
      number: parseNumber,
      bool: parseBoolean,
      array: parseArray(parseNumber),
      object: objectParser({
        hello: objectParser({
          there: parseArray(parseBoolean),
        }),
      }),
    })(input)
    expect(result).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "array": Array [
            1,
            2,
            3,
            4,
          ],
          "bool": true,
          "number": 1,
          "object": Object {
            "hello": Object {
              "there": Array [
                true,
                true,
                false,
                true,
              ],
            },
          },
          "string": "string",
        },
      }
    `)
  })
  it('can parse object with optional props', () => {
    const parser = objectParser<{ string: string; number: number; boolOptional?: boolean }>({
      string: parseString,
      number: parseNumber,
      boolOptional: optionalProp(parseBoolean),
    })

    const input1 = { string: 'string', number: 1, boolOptional: true }
    const input2 = { string: 'string', number: 1 }

    expect(parser(input1)).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "boolOptional": true,
          "number": 1,
          "string": "string",
        },
      }
    `)
    expect(parser(input2)).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "number": 1,
          "string": "string",
        },
      }
    `)
  })
  it('cannot parse non-object vars', () => {
    const parser = objectParser({})
    for (const example of [null, [], true, 1, 'hello']) {
      const result = parser(example)
      expect(result.type).toEqual('LEFT')
    }
  })
  it('returns left when the object has props other than the props in the spec', () => {
    const input = { legitProp1: 'needed', legitProp2: 'needed', unnecessaryProp: 'not needed' }
    const result = objectParser({
      legitProp1: parseConstant('needed'),
      legitProp2: parseConstant('needed'),
    })(input)

    expect(result).toMatchInlineSnapshot(`
      Object {
        "type": "LEFT",
        "value": Object {
          "description": "Found unknown props: unnecessaryProp",
          "type": "DESCRIPTION_PARSE_ERROR",
        },
      }
    `)
  })

  it('generates a sensible error message for nested objects', () => {
    const input = { nested: { object: false } }
    const result = objectParser({
      nested: objectParser({
        object: objectParser({
          prop: objectParser({}),
        }),
      }),
    })(input)

    expect(result).toMatchInlineSnapshot(`
      Object {
        "type": "LEFT",
        "value": Object {
          "field": "nested",
          "innerError": Object {
            "field": "object",
            "innerError": Object {
              "description": "Not an object",
              "type": "DESCRIPTION_PARSE_ERROR",
            },
            "type": "OBJECT_FIELD_PARSE_ERROR",
          },
          "type": "OBJECT_FIELD_PARSE_ERROR",
        },
      }
    `)
  })
})
