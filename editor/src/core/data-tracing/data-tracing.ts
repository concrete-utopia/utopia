import type { ProjectContentTreeRoot } from '../../components/assets'
import { MetadataUtils } from '../model/element-metadata-utils'
import type {
  IdentifierOrAccess,
  JSAssignment,
  JSAssignmentStatement,
  JSElementAccess,
  JSExpression,
  JSExpressionFunctionCall,
  JSExpressionOtherJavaScript,
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
import { mapFirstApplicable } from '../shared/array-utils'

export type DataPath = Array<string>

export type DataTracingToLiteralAttribute = {
  type: 'literal-attribute'
  elementPath: ElementPath
  property: PropertyPath
  dataPathIntoAttribute: DataPath
}

export function dataTracingToLiteralAttribute(
  elementPath: ElementPath,
  property: PropertyPath,
  dataPathIntoAttribute: DataPath,
): DataTracingToLiteralAttribute {
  return {
    type: 'literal-attribute',
    elementPath: elementPath,
    property: property,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingToAHookCall = {
  type: 'hook-result'
  hookName: string
  elementPath: ElementPath
  dataPathIntoAttribute: DataPath
}

export function dataTracingToAHookCall(
  elementPath: ElementPath,
  hookName: string,
  dataPathIntoAttribute: DataPath,
): DataTracingToAHookCall {
  return {
    type: 'hook-result',
    hookName: hookName,
    elementPath: elementPath,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingToAComponentProp = {
  type: 'component-prop'
  elementPath: ElementPath
  propertyPath: PropertyPath
  dataPathIntoAttribute: DataPath
}

export function dataTracingToAComponentProp(
  elementPath: ElementPath,
  propertyPath: PropertyPath,
  dataPathIntoAttribute: DataPath,
): DataTracingToAComponentProp {
  return {
    type: 'component-prop',
    elementPath: elementPath,
    propertyPath: propertyPath,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingFailed = { type: 'failed'; reason: string }

export function dataTracingFailed(reason: string): DataTracingFailed {
  return { type: 'failed', reason: reason }
}

export type DataTracingResult =
  | DataTracingToLiteralAttribute
  | DataTracingToAHookCall
  | DataTracingToAComponentProp
  | DataTracingFailed

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
  originalIdentifier: JSIdentifier,
  pathDrillSoFar: DataPath,
): Either<string, { propertyName: string; modifiedPathDrillSoFar: DataPath }> {
  switch (param.boundParam.type) {
    case 'REGULAR_PARAM': {
      // in case of a regular prop param, first we want to match the param name to the original identifier
      if (param.boundParam.paramName !== originalIdentifier.name) {
        return left('identifier does not match the prop name')
      }

      return right({
        propertyName: pathDrillSoFar[0],
        modifiedPathDrillSoFar: pathDrillSoFar.slice(1),
      })
    }
    case 'DESTRUCTURED_OBJECT': {
      for (const paramPart of param.boundParam.parts) {
        if (paramPart.param.boundParam.type === 'REGULAR_PARAM') {
          if (paramPart.param.boundParam.paramName === originalIdentifier.name) {
            if (paramPart.param.dotDotDotToken) {
              return right({
                propertyName: pathDrillSoFar[0],
                modifiedPathDrillSoFar: pathDrillSoFar.slice(1),
              })
            } else {
              return right({
                propertyName: paramPart.param.boundParam.paramName,
                modifiedPathDrillSoFar: pathDrillSoFar,
              })
            }
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

    const resultInComponentScope: DataTracingResult = lookupInComponentScope(
      startFrom.elementPath,
      componentHoldingElement,
      identifier,
      [...dataPath.value.path, ...pathDrillSoFar],
    )
    if (resultInComponentScope.type === 'component-prop') {
      return traceDataFromProp(
        TPP.create(resultInComponentScope.elementPath, resultInComponentScope.propertyPath),
        metadata,
        projectContents,
        resultInComponentScope.dataPathIntoAttribute,
      )
    }
    if (resultInComponentScope.type !== 'failed') {
      return resultInComponentScope
    }
  }
  return dataTracingFailed('We only support simple JSIdentifiers')
}

function lookupInComponentScope(
  componentPath: ElementPath,
  componentHoldingElement: UtopiaJSXComponent,
  originalIdentifier: JSIdentifier,
  pathDrillSoFar: DataPath,
): DataTracingResult {
  const identifier = originalIdentifier

  if (componentHoldingElement.param != null) {
    // let's try to match the name to the containing component's props!
    const foundPropSameName = propUsedByIdentifierOrAccess(
      componentHoldingElement.param,
      identifier,
      [...pathDrillSoFar],
    )

    if (isRight(foundPropSameName)) {
      // ok, so let's now travel to the containing component's instance in the metadata and continue the lookup!
      const parentComponentInstance = EP.getContainingComponent(componentPath)
      return dataTracingToAComponentProp(
        parentComponentInstance,
        PP.create(foundPropSameName.value.propertyName),
        foundPropSameName.value.modifiedPathDrillSoFar,
      )
    }
  }

  const foundAssignmentOfIdentifier: JSAssignment<JSIdentifier, JSExpression> | null =
    mapFirstApplicable(componentHoldingElement.arbitraryJSBlock?.statements ?? [], (statement) => {
      if (statement.type !== 'JS_ASSIGNMENT_STATEMENT') {
        return null
      }

      return mapFirstApplicable(statement.assignments, (assignment) => {
        if (assignment.leftHandSide.name === identifier.name) {
          return assignment
        }
        return null
      })
    })

  if (foundAssignmentOfIdentifier != null) {
    if (
      foundAssignmentOfIdentifier.rightHandSide.type === 'JS_IDENTIFIER' ||
      foundAssignmentOfIdentifier.rightHandSide.type === 'JS_ELEMENT_ACCESS' ||
      foundAssignmentOfIdentifier.rightHandSide.type === 'JS_PROPERTY_ACCESS'
    ) {
      const dataPath = processJSPropertyAccessors(foundAssignmentOfIdentifier.rightHandSide)

      if (isRight(dataPath)) {
        return lookupInComponentScope(
          componentPath,
          componentHoldingElement,
          dataPath.value.originalIdentifier,
          [...dataPath.value.path, ...pathDrillSoFar],
        )
      }
    }

    if (
      foundAssignmentOfIdentifier.rightHandSide.type === 'ATTRIBUTE_OTHER_JAVASCRIPT' &&
      foundAssignmentOfIdentifier.rightHandSide.originalJavascript.startsWith('use') &&
      foundAssignmentOfIdentifier.rightHandSide.originalJavascript.endsWith('()') &&
      foundAssignmentOfIdentifier.rightHandSide.originalJavascript.match(/^use[A-Za-z]+\(\)$/) !=
        null
    ) {
      return dataTracingToAHookCall(
        componentPath,
        foundAssignmentOfIdentifier.rightHandSide.originalJavascript.split('()')[0],
        [...pathDrillSoFar],
      )
    }
  }

  return dataTracingFailed('Could not find a hook call')
}
