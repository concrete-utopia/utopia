import { getSimpleAttributeAtPath } from '../../core/model/element-metadata-utils'
import { isLeft, isRight, right } from '../../core/shared/either'
import type { ElementInstanceMetadata } from '../../core/shared/element-template'
import { isJSXElement } from '../../core/shared/element-template'
import type { PropertyPath } from '../../core/shared/project-file-types'

export function propertyExists(
  property: PropertyPath,
  element: ElementInstanceMetadata | null,
): boolean {
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return false
  }

  return isRight(getSimpleAttributeAtPath(right(element.element.value.props), property))
}
