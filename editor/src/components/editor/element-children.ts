import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { getRegisteredComponent } from '../../core/property-controls/property-controls-utils'
import { intrinsicHTMLElementNamesThatSupportChildren } from '../../core/shared/dom-utils'
import type {
  ElementInstanceMetadataMap,
  JSXElementChild,
} from '../../core/shared/element-template'
import {
  isIntrinsicHTMLElement,
  getJSXElementNameAsString,
} from '../../core/shared/element-template'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import type { EditorAction } from './action-types'
import { deleteView, replaceElementInScope } from './actions/action-creators'
import * as EP from '../../core/shared/element-path'

export function elementSupportsChildrenFromPropertyControls(
  metadata: ElementInstanceMetadataMap,
  propertyControlsInfo: PropertyControlsInfo,
  target: ElementPath,
) {
  const targetElement = MetadataUtils.findElementByElementPath(metadata, target)

  const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(targetElement)
  if (targetJSXElement == null) {
    // this should not happen, erring on the side of true
    return true
  }
  if (isIntrinsicHTMLElement(targetJSXElement.name)) {
    // when it is an intrinsic html element, we check if it supports children from our list
    return intrinsicHTMLElementNamesThatSupportChildren.includes(targetJSXElement.name.baseVariable)
  }

  const elementImportInfo = targetElement?.importInfo
  if (elementImportInfo == null) {
    // erring on the side of true
    return true
  }

  const targetName = getJSXElementNameAsString(targetJSXElement.name)
  const registeredComponent = getRegisteredComponent(
    targetName,
    elementImportInfo.filePath,
    propertyControlsInfo,
  )
  if (registeredComponent == null) {
    // when there is no component annotation default is supporting children
    return true
  }

  return registeredComponent.supportsChildren
}

export function replaceFirstChildAndDeleteSiblings(
  target: ElementPath,
  children: JSXElementChild[],
  replaceWith: JSXElementChild,
): EditorAction[] {
  return [
    // replace the first child
    replaceElementInScope(target, {
      type: 'replace-child-with-uid',
      uid: children[0].uid,
      replaceWith: replaceWith,
    }),
    // get rid of all the others
    ...children.slice(1).map((child) => deleteView(EP.appendToPath(target, child.uid))),
  ]
}

const reNumericComponents = /[0-9\._]/g

export function childrenAreProbablyNumericExpression(children: JSXElementChild[]) {
  return children.some(
    (c) =>
      c.type === 'ATTRIBUTE_OTHER_JAVASCRIPT' &&
      c.originalJavascript.trim().replace(reNumericComponents, '').length > 0,
  )
}
