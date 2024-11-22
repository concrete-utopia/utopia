import type { ParseResult } from './value-parser-utils'
import {
  parseString,
  descriptionParseError,
  objectKeyParser,
  parseObject,
  objectParser,
  parseBoolean,
  optionalProp,
} from './value-parser-utils'
import type { AnyJson } from '../missing-types/json'
import { flatMapEither, right, left, mapEither, isLeft } from '../core/shared/either'

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

interface UtopiaConfig {
  tailwind?: Record<string, unknown>
}

export function parseUtopiaConfigFromPackageJsonFile(
  value: unknown,
): ParseResult<UtopiaConfig | null> {
  const parser = objectParser(
    {
      utopia: optionalProp(
        objectParser({
          tailwind: optionalProp(objectParser<Record<string, unknown>>({})),
        }),
      ),
    },
    { allowUnknownKeys: true },
  )

  const result = flatMapEither(parser, parseStringToJSON(value))

  return mapEither((parsed) => parsed?.utopia ?? null, result)
}
