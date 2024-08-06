import { type ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { isRight } from '../core/shared/either'
import type { JSXElementChild } from '../core/shared/element-template'
import { isJSXElement, type ElementInstanceMetadataMap } from '../core/shared/element-template'
import { getFromPropOrFlagComment } from '../core/shared/comment-flags'

export function dataCanCondenseFromMetadata(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  const target = MetadataUtils.findElementByElementPath(metadata, path)
  return (
    target != null &&
    isRight(target.element) &&
    isJSXElement(target.element.value) &&
    canCondenseJSXElementChild(target.element.value)
  )
}

export function canCondenseJSXElementChild(element: JSXElementChild) {
  return getFromPropOrFlagComment(element, 'can-condense')?.value === true
}
