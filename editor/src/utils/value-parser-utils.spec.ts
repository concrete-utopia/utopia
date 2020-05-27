import { parseBoolean, parseNumber, parseString, descriptionParseError } from './value-parser-utils'
import { left, right } from '../core/shared/either'

describe('parseBoolean', () => {
  it('returns a left for something which is not a boolean', () => {
    expect(parseBoolean(9)).toEqual(left(descriptionParseError('Value is not a boolean.')))
    expect(parseBoolean('hat')).toEqual(left(descriptionParseError('Value is not a boolean.')))
    expect(parseBoolean({})).toEqual(left(descriptionParseError('Value is not a boolean.')))
    expect(parseBoolean([])).toEqual(left(descriptionParseError('Value is not a boolean.')))
    expect(parseBoolean(null)).toEqual(left(descriptionParseError('Value is not a boolean.')))
    expect(parseBoolean(undefined)).toEqual(left(descriptionParseError('Value is not a boolean.')))
  })
  it('returns a right for something which is a boolean', () => {
    expect(parseBoolean(true)).toEqual(right(true))
    expect(parseBoolean(false)).toEqual(right(false))
  })
})

describe('parseNumber', () => {
  it('returns a left for something which is not a number', () => {
    expect(parseNumber('hat')).toEqual(left(descriptionParseError('Value is not a number.')))
    expect(parseNumber(true)).toEqual(left(descriptionParseError('Value is not a number.')))
    expect(parseNumber({})).toEqual(left(descriptionParseError('Value is not a number.')))
    expect(parseNumber([])).toEqual(left(descriptionParseError('Value is not a number.')))
    expect(parseNumber(null)).toEqual(left(descriptionParseError('Value is not a number.')))
    expect(parseNumber(undefined)).toEqual(left(descriptionParseError('Value is not a number.')))
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
    expect(parseString(9)).toEqual(left(descriptionParseError('Value is not a string.')))
    expect(parseString(true)).toEqual(left(descriptionParseError('Value is not a string.')))
    expect(parseString({})).toEqual(left(descriptionParseError('Value is not a string.')))
    expect(parseString([])).toEqual(left(descriptionParseError('Value is not a string.')))
    expect(parseString(null)).toEqual(left(descriptionParseError('Value is not a string.')))
    expect(parseString(undefined)).toEqual(left(descriptionParseError('Value is not a string.')))
  })
  it('returns a right for something which is a string', () => {
    expect(parseString('')).toEqual(right(''))
    expect(parseString('hat')).toEqual(right('hat'))
  })
})
