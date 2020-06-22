import {
  ParseResult,
  parseString,
  descriptionParseError,
  objectKeyParser,
} from './value-parser-utils'
import { AnyJson } from '../missing-types/json'
import { flatMapEither, right, left } from '../core/shared/either'

export function parseStringToJSON(value: unknown): ParseResult<AnyJson> {
  return flatMapEither((jsonText) => {
    try {
      const parsed = JSON.parse(jsonText)
      return right(parsed)
    } catch (error) {
      return left(descriptionParseError('Failed to parse JSON.'))
    }
  }, parseString(value))
}

export function parseVersionPackageJsonObject(value: unknown): ParseResult<string> {
  return objectKeyParser(parseString, 'version')(value)
}

export function parseVersionPackageJsonFile(value: unknown): ParseResult<string> {
  return flatMapEither(parseVersionPackageJsonObject, parseStringToJSON(value))
}
