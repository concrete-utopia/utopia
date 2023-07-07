import type {
  PropertyPath,
  ElementPath,
  ElementPropertyPath,
} from '../core/shared/project-file-types'
import * as PP from '../core/shared/property-path'
import * as EP from '../core/shared/element-path'

export function create(elementPath: ElementPath, propertyPath: PropertyPath): ElementPropertyPath {
  return {
    elementPath: elementPath,
    propertyPath: propertyPath,
  }
}

export function pathsEqual(first: ElementPropertyPath, second: ElementPropertyPath): boolean {
  return (
    EP.pathsEqual(first.elementPath, second.elementPath) &&
    PP.pathsEqual(first.propertyPath, second.propertyPath)
  )
}

export function replaceOrDefault(
  path: ElementPropertyPath,
  replaceSearch: ElementPath,
  replaceWith: ElementPath,
): ElementPropertyPath {
  return {
    ...path,
    elementPath: EP.replaceOrDefault(path.elementPath, replaceSearch, replaceWith),
  }
}
