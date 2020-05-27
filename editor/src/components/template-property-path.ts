import { PropertyPath, TemplatePath, TemplatePropertyPath } from '../core/shared/project-file-types'
import * as PP from '../core/shared/property-path'
import * as TP from '../core/shared/template-path'

export function create(
  templatePath: TemplatePath,
  propertyPath: PropertyPath,
): TemplatePropertyPath {
  return {
    templatePath: templatePath,
    propertyPath: propertyPath,
  }
}

export function pathsEqual(first: TemplatePropertyPath, second: TemplatePropertyPath): boolean {
  return (
    TP.pathsEqual(first.templatePath, second.templatePath) &&
    PP.pathsEqual(first.propertyPath, second.propertyPath)
  )
}

export function replaceOrDefault(
  path: TemplatePropertyPath,
  replaceSearch: TemplatePath,
  replaceWith: TemplatePath,
): TemplatePropertyPath {
  return {
    ...path,
    templatePath: TP.replaceOrDefault(path.templatePath, replaceSearch, replaceWith),
  }
}
