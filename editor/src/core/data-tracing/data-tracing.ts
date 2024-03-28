import type { ProjectContentTreeRoot } from '../../components/assets'
import { MetadataUtils } from '../model/element-metadata-utils'
import type { UtopiaJSXComponent } from '../shared/element-template'
import { isJSXElement, type ElementInstanceMetadataMap } from '../shared/element-template'
import { forceNotNull } from '../shared/optional-utils'
import type { ElementPath, ElementPropertyPath, PropertyPath } from '../shared/project-file-types'
import type { Either } from '../shared/either'
import { isRight, left, mapEither, right } from '../shared/either'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import invariant from '../../third-party/remix/invariant'
import { getJSXAttributesAtPath } from '../shared/jsx-attribute-utils'
import { withUnderlyingTarget } from '../../components/editor/store/editor-state'
import { findContainingComponentForPath } from '../model/element-template-utils'
import { create } from '../../components/template-property-path'

export type DatPath = Array<string> // this placeholder type will evolve

export type DataTracingToLiteralAttribute = {
  type: 'literal-attribute'
  elementPath: ElementPath
  property: PropertyPath
  dataPathIntoAttribute: DatPath
}

export function dataTracingToLiteralAttribute(
  elementPath: ElementPath,
  property: PropertyPath,
  dataPathIntoAttribute: DatPath,
): DataTracingToLiteralAttribute {
  return {
    type: 'literal-attribute',
    elementPath: elementPath,
    property: property,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingFailed = { type: 'failed'; reason: string }

export function dataTracingFailed(reason: string): DataTracingFailed {
  return { type: 'failed', reason: reason }
}

export type DataTracingResult = DataTracingToLiteralAttribute | DataTracingFailed

function findContainingComponentForElementPath(
  elementPath: ElementPath,
  projectContents: ProjectContentTreeRoot,
): UtopiaJSXComponent | null {
  return withUnderlyingTarget(elementPath, projectContents, null, (success) => {
    const containingComponent = findContainingComponentForPath(
      success.topLevelElements,
      elementPath,
    )
    return containingComponent
  })
}

export function traceDataFromProp(
  startFrom: ElementPropertyPath,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
): DataTracingResult {
  const elementHoldingProp = forceNotNull(
    'traceDataFromProp did not find element at path',
    MetadataUtils.findElementByElementPath(metadata, startFrom.elementPath),
  )
  const componentHoldingElement = forceNotNull(
    'traceDataFromProp did not find containing component for path',
    findContainingComponentForElementPath(startFrom.elementPath, projectContents),
  )

  invariant(isRight(elementHoldingProp.element), 'element must be a parsed element')
  invariant(
    isJSXElement(elementHoldingProp.element.value),
    'element must be a JSXElement because it must have props',
  )

  const propDeclaration = getJSXAttributesAtPath(
    elementHoldingProp.element.value.props,
    startFrom.propertyPath,
  )

  // for now we only support a simple JSIdentifier, and only if it was a full match
  if (propDeclaration.remainingPath != null) {
    return dataTracingFailed("We don't yet support propertyPaths pointing deeper into attributes")
  }
  if (propDeclaration.attribute.type === 'ATTRIBUTE_VALUE') {
    // bingo
    return dataTracingToLiteralAttribute(startFrom.elementPath, startFrom.propertyPath, [])
  }
  if (propDeclaration.attribute.type === 'JS_IDENTIFIER') {
    // for JSIdentifier we want to make sure the identifier points to a prop

    // let's try to match the name to the containing component's props!
    const foundPropSameName = componentHoldingElement.propsUsed.includes(
      propDeclaration.attribute.name,
    )

    if (foundPropSameName) {
      // ok, so let's now travel to the containing component's instance in the metadata and continue the lookup!
      const parentComponentInstance = EP.getContainingComponent(startFrom.elementPath)
      return traceDataFromProp(
        create(parentComponentInstance, PP.create(propDeclaration.attribute.name)),
        metadata,
        projectContents,
      )
    }
  }
  return dataTracingFailed('We only support simple JSIdentifiers')
}
