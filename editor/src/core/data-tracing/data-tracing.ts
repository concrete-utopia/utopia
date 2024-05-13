import type { ProjectContentTreeRoot } from '../../components/assets'
import { withUnderlyingTarget } from '../../components/editor/store/editor-state'
import * as TPP from '../../components/template-property-path'
import { MetadataUtils } from '../model/element-metadata-utils'
import { findContainingComponentForPath } from '../model/element-template-utils'
import { mapFirstApplicable } from '../shared/array-utils'
import type { Either } from '../shared/either'
import { isLeft, isRight, left, mapEither, maybeEitherToMaybe, right } from '../shared/either'
import * as EP from '../shared/element-path'
import type {
  BoundParam,
  JSAssignment,
  JSExpression,
  JSIdentifier,
  Param,
  UtopiaJSXComponent,
} from '../shared/element-template'
import {
  emptyComments,
  isJSXElement,
  jsIdentifier,
  type ElementInstanceMetadataMap,
  isRegularParam,
  isJSAssignmentStatement,
  isJSIdentifier,
} from '../shared/element-template'
import { getJSXAttributesAtPath, jsxSimpleAttributeToValue } from '../shared/jsx-attribute-utils'
import { forceNotNull, optionalMap } from '../shared/optional-utils'
import type { ElementPath, ElementPropertyPath, PropertyPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { assertNever } from '../shared/utils'

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
      const elementToSimpleValue = jsxSimpleAttributeToValue(expression.element)
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

function paramUsedByIdentifierOrAccess(
  param: Param,
  originalIdentifier: JSIdentifier,
  pathDrillSoFar: DataPath,
): Either<string, { modifiedPathDrillSoFar: DataPath }> {
  switch (param.boundParam.type) {
    case 'REGULAR_PARAM': {
      // in case of a regular prop param, first we want to match the param name to the original identifier
      if (param.boundParam.paramName !== originalIdentifier.name) {
        return left('identifier does not match the prop name')
      }

      return right({
        modifiedPathDrillSoFar: pathDrillSoFar,
      })
    }
    case 'DESTRUCTURED_OBJECT': {
      for (const paramPart of param.boundParam.parts) {
        if (paramPart.param.boundParam.type === 'REGULAR_PARAM') {
          if (paramPart.param.boundParam.paramName === originalIdentifier.name) {
            if (paramPart.param.dotDotDotToken) {
              return right({
                modifiedPathDrillSoFar: pathDrillSoFar,
              })
            } else {
              return right({
                modifiedPathDrillSoFar: [paramPart.param.boundParam.paramName, ...pathDrillSoFar],
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
  const elementHoldingProp = MetadataUtils.findElementByElementPath(metadata, startFrom.elementPath)
  if (elementHoldingProp == null) {
    return dataTracingFailed('traceDataFromProp did not find element at path')
  }
  const componentHoldingElement: UtopiaJSXComponent | null = findContainingComponentForElementPath(
    startFrom.elementPath,
    projectContents,
  )

  if (!isRight(elementHoldingProp.element)) {
    return dataTracingFailed('element must be a parsed element')
  }

  if (!isJSXElement(elementHoldingProp.element.value)) {
    return dataTracingFailed('element must be a JSXElement because it must have props')
  }

  const propDeclaration = getJSXAttributesAtPath(
    elementHoldingProp.element.value.props,
    startFrom.propertyPath,
  )

  // for now we only support a simple JSIdentifier, and only if it was a full match
  if (propDeclaration.remainingPath != null) {
    return dataTracingFailed("We don't yet support propertyPaths pointing deeper into attributes")
  }
  if (
    propDeclaration.attribute.type === 'ATTRIBUTE_VALUE' ||
    propDeclaration.attribute.type === 'ATTRIBUTE_NESTED_OBJECT' ||
    propDeclaration.attribute.type === 'ATTRIBUTE_NESTED_ARRAY'
  ) {
    // bingo
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

    if (componentHoldingElement != null) {
      const componentRootPath = EP.getPathOfComponentRoot(startFrom.elementPath)

      const resultInComponentScope: DataTracingResult = walkUpInnerScopesUntilReachingComponent(
        metadata,
        startFrom.elementPath,
        startFrom.elementPath,
        componentRootPath,
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
      return resultInComponentScope
    }
  }
  return dataTracingFailed(
    `We couldn\'t walk past ${EP.toString(startFrom.elementPath)} @ ${PP.toString(
      startFrom.propertyPath,
    )}`,
  )
}

function walkUpInnerScopesUntilReachingComponent(
  metadata: ElementInstanceMetadataMap,
  originalElementPath: ElementPath,
  currentElementPathOfWalk: ElementPath,
  containingComponentRootPath: ElementPath,
  componentHoldingElement: UtopiaJSXComponent,
  identifier: JSIdentifier,
  pathDrillSoFar: DataPath,
): DataTracingResult {
  if (EP.pathsEqual(currentElementPathOfWalk, containingComponentRootPath)) {
    return lookupInComponentScope(
      originalElementPath,
      containingComponentRootPath,
      componentHoldingElement,
      identifier,
      pathDrillSoFar,
    )
  }
  if (!EP.isDescendantOf(currentElementPathOfWalk, containingComponentRootPath)) {
    return dataTracingFailed('somehow walked beyond parent component')
  }

  // is the parent element of the element we are currently visiting a Map?
  // if so, we want to look at the mapFunction and see if the identifier's target is declared there
  {
    const parentElementMetadata = MetadataUtils.findElementByElementPath(
      metadata,
      EP.parentPath(currentElementPathOfWalk),
    )
    const parentElement = maybeEitherToMaybe(parentElementMetadata?.element)
    if (parentElement != null && parentElement.type === 'JSX_MAP_EXPRESSION') {
      const mapFunction = parentElement.mapFunction
      if (mapFunction.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
        // let's see if the identifier points to a component prop
        {
          const param = mapFunction.params[0] // the map function's first param is the mapped value
          if (param != null) {
            // let's try to match the name to the containing component's props!
            const foundPropSameName = paramUsedByIdentifierOrAccess(param, identifier, [
              ...pathDrillSoFar,
            ])

            if (isRight(foundPropSameName)) {
              // ok, so now we need to figure out what the map is mapping over
              const mapOver = parentElement.valueToMap
              // figure out the identifier of the mapOver
              if (
                mapOver.type === 'JS_IDENTIFIER' ||
                mapOver.type === 'JS_ELEMENT_ACCESS' ||
                mapOver.type === 'JS_PROPERTY_ACCESS'
              ) {
                const dataPath = processJSPropertyAccessors(mapOver)

                if (isRight(dataPath)) {
                  const mapIndexHack = optionalMap(
                    (i) => substractFromStringNumber(i, 1),
                    EP.extractIndexFromIndexedUid(EP.toUid(currentElementPathOfWalk)),
                  )
                  if (mapIndexHack == null) {
                    return dataTracingFailed(
                      `Could not extract index from map uid: ${EP.toString(
                        currentElementPathOfWalk,
                      )}`,
                    )
                  }
                  return walkUpInnerScopesUntilReachingComponent(
                    metadata,
                    originalElementPath,
                    EP.parentPath(currentElementPathOfWalk),
                    containingComponentRootPath,
                    componentHoldingElement,
                    dataPath.value.originalIdentifier,
                    [
                      ...dataPath.value.path,
                      mapIndexHack,
                      ...foundPropSameName.value.modifiedPathDrillSoFar,
                    ],
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  return walkUpInnerScopesUntilReachingComponent(
    metadata,
    originalElementPath,
    EP.parentPath(currentElementPathOfWalk),
    containingComponentRootPath,
    componentHoldingElement,
    identifier,
    pathDrillSoFar,
  )
}

const hookCallRegex = /^(use[A-Za-z0-9_]+)\(/

function getPossibleHookCall(expression: JSExpression): string | null {
  switch (expression.type) {
    case 'JS_IDENTIFIER':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_ELEMENT_ACCESS':
    case 'JSX_ELEMENT':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
      return null
    case 'ATTRIBUTE_FUNCTION_CALL':
      if (expression.functionName.startsWith('use')) {
        return expression.functionName
      } else {
        return null
      }
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      const matchingPart = expression.originalJavascript.match(hookCallRegex)
      if (matchingPart == null || matchingPart.length === 0) {
        return null
      } else {
        return matchingPart[1]
      }
    default:
      assertNever(expression)
  }
}

function findPathToIdentifier(param: BoundParam, identifier: string): DataPath | null {
  function innerFindPath(workingParam: BoundParam, currentPath: DataPath): DataPath | null {
    switch (workingParam.type) {
      case 'REGULAR_PARAM':
        if (workingParam.paramName === identifier) {
          return [...currentPath, workingParam.paramName]
        } else {
          return null
        }
      case 'DESTRUCTURED_OBJECT':
        for (const part of workingParam.parts) {
          // Prevent drilling down through spread values.
          if (part.param.dotDotDotToken) {
            return null
          } else if (part.propertyName == identifier) {
            return [...currentPath, part.propertyName]
          } else if (
            part.propertyName != null &&
            isRegularParam(part.param.boundParam) &&
            part.param.boundParam.paramName === identifier
          ) {
            return [...currentPath, part.propertyName]
          } else {
            const possibleResult = innerFindPath(part.param.boundParam, currentPath)
            if (possibleResult != null) {
              return possibleResult
            }
          }
        }
        break
      case 'DESTRUCTURED_ARRAY':
        let arrayIndex: number = 0
        for (const part of workingParam.parts) {
          switch (part.type) {
            case 'PARAM':
              // Prevent drilling down through spread values.
              if (part.dotDotDotToken) {
                return null
              }
              const possibleResult = innerFindPath(part.boundParam, [
                ...currentPath,
                `${arrayIndex}`,
              ])
              if (possibleResult != null) {
                return possibleResult
              }
              break
            case 'OMITTED_PARAM':
              return null
            default:
              assertNever(part)
          }
          arrayIndex++
        }
        return null
      default:
        assertNever(workingParam)
    }
    return null
  }
  return innerFindPath(param, [])
}

function lookupInComponentScope(
  elementPathInsideComponent: ElementPath,
  componentPath: ElementPath,
  componentHoldingElement: UtopiaJSXComponent,
  originalIdentifier: JSIdentifier,
  pathDrillSoFar: DataPath,
): DataTracingResult {
  const identifier = originalIdentifier

  // let's see if the identifier points to a component prop
  {
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
  }

  // let's see if the identifier points to a hook call or variable declaration
  {
    const foundAssignmentOfIdentifier: JSAssignment<JSExpression> | null = mapFirstApplicable(
      componentHoldingElement.arbitraryJSBlock?.statements ?? [],
      (statement) => {
        if (statement.type !== 'JS_ASSIGNMENT_STATEMENT') {
          return null
        }

        return mapFirstApplicable(statement.assignments, (assignment) => {
          if (
            isRegularParam(assignment.leftHandSide) &&
            assignment.leftHandSide.paramName === identifier.name
          ) {
            return assignment
          }
          return null
        })
      },
    )

    if (foundAssignmentOfIdentifier != null) {
      if (
        foundAssignmentOfIdentifier.rightHandSide.type === 'JS_IDENTIFIER' ||
        foundAssignmentOfIdentifier.rightHandSide.type === 'JS_ELEMENT_ACCESS' ||
        foundAssignmentOfIdentifier.rightHandSide.type === 'JS_PROPERTY_ACCESS'
      ) {
        const dataPath = processJSPropertyAccessors(foundAssignmentOfIdentifier.rightHandSide)

        if (isRight(dataPath)) {
          return lookupInComponentScope(
            elementPathInsideComponent,
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
  }

  // let's try to find "const { <identifier>, ...somethingElse } = <expression>" shaped statements
  {
    const foundResultFromStatements: DataTracingResult | null = mapFirstApplicable(
      componentHoldingElement.arbitraryJSBlock?.statements ?? [],
      (statement) => {
        if (isJSAssignmentStatement(statement)) {
          for (const assignment of statement.assignments) {
            const possiblePathToIdentifier = findPathToIdentifier(
              assignment.leftHandSide,
              identifier.name,
            )
            const originExpression = assignment.rightHandSide
            if (possiblePathToIdentifier == null) {
              return null
            } else {
              const possibleHookCall = getPossibleHookCall(assignment.rightHandSide)
              if (possibleHookCall == null && isJSIdentifier(originExpression)) {
                return lookupInComponentScope(
                  elementPathInsideComponent,
                  componentPath,
                  componentHoldingElement,
                  originExpression,
                  [...possiblePathToIdentifier, ...pathDrillSoFar],
                )
              } else if (possibleHookCall != null) {
                return dataTracingToAHookCall(componentPath, possibleHookCall, [
                  ...possiblePathToIdentifier,
                  ...pathDrillSoFar,
                ])
              }
            }
          }
        }
        return null
      },
    )

    if (foundResultFromStatements != null) {
      return foundResultFromStatements
    }
  }

  // end of temporary stopgap

  return dataTracingFailed('Could not find a hook call')
}

function substractFromStringNumber(n: string, minus: number): string {
  return `${parseInt(n) - minus}`
}
