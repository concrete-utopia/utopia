import { emptyComments, type ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { isRight } from '../core/shared/either'
import type {
  JSXAttributesEntry,
  JSXAttributesPart,
  JSXElementChild,
} from '../core/shared/element-template'
import {
  isJSXAttributeValue,
  isJSXAttributesEntry,
  isJSXElement,
  jsExpressionValue,
  jsxAttributesEntry,
  type ElementInstanceMetadataMap,
} from '../core/shared/element-template'

export const DataCanCondense = 'data-can-condense'

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
  return (
    isJSXElement(element) &&
    element.props.some(
      (prop) =>
        isDataCanCondenseProp(prop) && isJSXAttributeValue(prop.value) && prop.value.value === true,
    )
  )
}

interface DataCanCondenseProp extends JSXAttributesEntry {
  key: typeof DataCanCondense
}

export function isDataCanCondenseProp(prop: JSXAttributesPart): prop is DataCanCondenseProp {
  return isJSXAttributesEntry(prop) && (prop as DataCanCondenseProp).key === 'data-can-condense'
}

export function dataCanCondenseProp(value: boolean): JSXAttributesEntry {
  return jsxAttributesEntry(
    DataCanCondense,
    jsExpressionValue(value, emptyComments),
    emptyComments,
    'include-in-printing',
  )
}
