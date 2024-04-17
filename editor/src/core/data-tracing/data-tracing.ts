import type { ProjectContentTreeRoot } from '../../components/assets'
import { MetadataUtils } from '../model/element-metadata-utils'
import type {
  IdentifierOrAccess,
  JSElementAccess,
  JSExpression,
  JSIdentifier,
  JSPropertyAccess,
  Param,
  UtopiaJSXComponent,
} from '../shared/element-template'
import { isJSXElement, type ElementInstanceMetadataMap } from '../shared/element-template'
import { forceNotNull } from '../shared/optional-utils'
import type {
  ElementPath,
  ElementPropertyPath,
  PropertyPath,
  PropertyPathPart,
} from '../shared/project-file-types'
import type { Either } from '../shared/either'
import { isLeft, isRight, left, mapEither, right } from '../shared/either'
import * as EP from '../shared/element-path'
import * as PP from '../shared/property-path'
import invariant from '../../third-party/remix/invariant'
import {
  getJSExpressionAtPathParts,
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueInAttributeAtPathParts,
} from '../shared/jsx-attribute-utils'
import { withUnderlyingTarget } from '../../components/editor/store/editor-state'
import { findContainingComponentForPath } from '../model/element-template-utils'
import * as TPP from '../../components/template-property-path'
import { assertNever } from '../shared/utils'

export type DataPath = Array<string>

export type DataTracingToLiteralAttribute = {
  type: 'literal-attribute'
  elementPath: ElementPath
  property: PropertyPath
  dataPathIntoAttribute: Array<string | number>
}

export function dataTracingToLiteralAttribute(
  elementPath: ElementPath,
  property: PropertyPath,
  dataPathIntoAttribute: Array<string | number>,
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

function processJSPropertyAccessors(
  expression: JSExpression,
): Either<string, { originalIdentifier: JSIdentifier; path: DataPath }> {
  switch (expression.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'ATTRIBUTE_VALUE':
    case 'JSX_ELEMENT':
    case 'JSX_MAP_EXPRESSION':
      return left(`encountered unsupported expression type ${expression.type}`)
    case 'JS_IDENTIFIER': {
      return right({ path: [], originalIdentifier: expression })
    }
    case 'JS_ELEMENT_ACCESS': {
      const elementToSimpleValue = jsxSimpleAttributeToValue(expression.onValue)
      if (isLeft(elementToSimpleValue)) {
        return left(
          `Cannot convert JS_ELEMENT_ACCESS element into simple value, reason: ${elementToSimpleValue.value}`,
        )
      }

      const elementValue = elementToSimpleValue.value
      if (!(typeof elementValue === 'string' || typeof elementValue === 'number')) {
        return left(
          `Cannot convert JS_ELEMENT_ACCESS element into simple value, encountered: ${elementValue}`,
        )
      }

      return mapEither((resultSoFar) => {
        return {
          path: [...resultSoFar.path, `${elementValue}`],
          originalIdentifier: resultSoFar.originalIdentifier,
        }
      }, processJSPropertyAccessors(expression.onValue))
    }
    case 'JS_PROPERTY_ACCESS':
      return mapEither((resultSoFar) => {
        return {
          path: [...resultSoFar.path, expression.property],
          originalIdentifier: resultSoFar.originalIdentifier,
        }
      }, processJSPropertyAccessors(expression.onValue))

    default:
      assertNever(expression)
  }
}

function propUsedByIdentifierOrAccess(
  param: Param,
  access: { originalIdentifier: JSIdentifier; path: DataPath },
): Either<string, { propertyName: string; pathDrillInProperty: DataPath }> {
  switch (param.boundParam.type) {
    case 'REGULAR_PARAM': {
      // in case of a regular prop param, first we want to match the param name to the original identifier
      if (param.boundParam.paramName !== access.originalIdentifier.name) {
        return left('identifier does not match the prop name')
      }

      const path = access.path
      if (path.length === 0) {
        return left('identifier matches the prop name, but no path to drill in')
      }

      // the prop name we are looking for is going to be the first element of the path!
      return right({ propertyName: path[0], pathDrillInProperty: path.slice(1) })
    }
    case 'DESTRUCTURED_OBJECT': {
      for (const paramPart of param.boundParam.parts) {
        if (paramPart.param.boundParam.type === 'REGULAR_PARAM') {
          if (paramPart.param.boundParam.paramName === access.originalIdentifier.name) {
            const path = access.path
            return right({
              propertyName: paramPart.param.boundParam.paramName,
              pathDrillInProperty: path,
            })
          }
        }
      }
      return left('identifier does not match any of the destructured object properties')
    }
    case 'DESTRUCTURED_ARRAY': {
      return left('Destructured array properties are not yet supported')
    }
    default:
      assertNever(param.boundParam)
  }
}

export function traceDataFromProp(
  startFrom: ElementPropertyPath,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  pathDrillSoFar: Array<string>,
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
    return dataTracingToLiteralAttribute(startFrom.elementPath, startFrom.propertyPath, [
      ...pathDrillSoFar,
    ])
  }
  if (propDeclaration.attribute.type === 'ATTRIBUTE_NESTED_OBJECT') {
    const partOfObject = getJSExpressionAtPathParts(propDeclaration.attribute, pathDrillSoFar, 0)
    // TODO we may want to actually do something with this partOfObject, for validation?

    return dataTracingToLiteralAttribute(
      startFrom.elementPath,
      startFrom.propertyPath,
      pathDrillSoFar,
    )
  }
  if (
    propDeclaration.attribute.type === 'JS_IDENTIFIER' ||
    propDeclaration.attribute.type === 'JS_ELEMENT_ACCESS' ||
    propDeclaration.attribute.type === 'JS_PROPERTY_ACCESS'
  ) {
    // first, let's try to find the jsIdentifier at the root
    const dataPath = processJSPropertyAccessors(propDeclaration.attribute)

    if (isLeft(dataPath)) {
      return dataTracingFailed(dataPath.value)
    }

    const identifier = dataPath.value.originalIdentifier

    // let's try to match the name to the containing component's props!
    const foundPropSameName = propUsedByIdentifierOrAccess(
      componentHoldingElement.param!,
      dataPath.value,
    )

    if (isRight(foundPropSameName)) {
      // ok, so let's now travel to the containing component's instance in the metadata and continue the lookup!
      const parentComponentInstance = EP.getContainingComponent(startFrom.elementPath)
      return traceDataFromProp(
        TPP.create(parentComponentInstance, PP.create(foundPropSameName.value.propertyName)),
        metadata,
        projectContents,
        [...foundPropSameName.value.pathDrillInProperty, ...pathDrillSoFar],
      )
    }
  }
  return dataTracingFailed('We only support simple JSIdentifiers')
}
