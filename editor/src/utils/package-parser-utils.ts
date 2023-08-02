import type { ParseResult } from './value-parser-utils'
import { parseString, descriptionParseError, objectKeyParser } from './value-parser-utils'
import type { AnyJson } from '../missing-types/json'
import { flatMapEither, right, left, forEachRight } from '../core/shared/either'
import { isEsCodeFile, NodeModules } from '../core/shared/project-file-types'

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
