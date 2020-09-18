import {
  ParseResult,
  parseString,
  descriptionParseError,
  objectKeyParser,
} from './value-parser-utils'
import { AnyJson } from '../missing-types/json'
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

export function parseDependencyVersionFromNodeModules(
  nodeModules: NodeModules,
  dependencyName: string,
): string | null {
  let version: string | null = null
  const packageJsonFile = nodeModules[`/node_modules/${dependencyName}/package.json`]
  if (packageJsonFile != null && isEsCodeFile(packageJsonFile)) {
    const parseResult = parseVersionPackageJsonFile(packageJsonFile.fileContents)
    forEachRight(parseResult, (resolvedVersion) => {
      version = resolvedVersion
    })
  }
  return version
}
