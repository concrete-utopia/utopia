import type { ProjectContentTreeRoot } from '../../components/assets'
import type { FileRootPath } from '../../components/canvas/ui-jsx-canvas'
import { findUnderlyingTargetComponentImplementationFromImportInfo } from '../../components/custom-code/code-file'
import { withUnderlyingTarget } from '../../components/editor/store/editor-state'
import * as TPP from '../../components/template-property-path'
import { MetadataUtils } from '../model/element-metadata-utils'
import {
  findContainingComponentForPath,
  findContainingComponentForPathInProjectContents,
} from '../model/element-template-utils'
import { mapFirstApplicable } from '../shared/array-utils'
import type { Either } from '../shared/either'
import { isLeft, isRight, left, mapEither, maybeEitherToMaybe, right } from '../shared/either'
import * as EP from '../shared/element-path'
import type {
  BoundParam,
  IdentifierOrAccess,
  JSAssignment,
  JSExpression,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSIdentifier,
  JSXElementChild,
  Param,
  UtopiaJSXComponent,
} from '../shared/element-template'
import {
  isJSXElement,
  type ElementInstanceMetadataMap,
  isRegularParam,
  isJSAssignmentStatement,
  isJSIdentifier,
  jsIdentifier,
  emptyComments,
} from '../shared/element-template'
import { getJSXAttributesAtPath, jsxSimpleAttributeToValue } from '../shared/jsx-attribute-utils'
import { optionalMap } from '../shared/optional-utils'
import type { ElementPath, ElementPropertyPath, PropertyPath } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { assertNever } from '../shared/utils'

export type DataPath = Array<string>

export interface DataPathSuccess {
  type: 'DATA_PATH_SUCCESS'
  dataPath: DataPath
}

export function dataPathSuccess(dataPath: DataPath): DataPathSuccess {
  return {
    type: 'DATA_PATH_SUCCESS',
    dataPath: dataPath,
  }
}

export interface DataPathNotPossible {
  type: 'DATA_PATH_NOT_POSSIBLE'
}

export const dataPathNotPossible: DataPathNotPossible = {
  type: 'DATA_PATH_NOT_POSSIBLE',
}

export interface DataPathUnavailable {
  type: 'DATA_PATH_UNAVAILABLE'
}

export const dataPathUnavailable: DataPathUnavailable = {
  type: 'DATA_PATH_UNAVAILABLE',
}

export type DataPathPositiveResult = DataPathSuccess | DataPathNotPossible

export function combinePositiveResults(
  first: DataPathPositiveResult | DataPath,
  second: DataPathPositiveResult | DataPath,
): DataPathPositiveResult {
  if (Array.isArray(first)) {
    if (Array.isArray(second)) {
      return dataPathSuccess([...first, ...second])
    } else if (dataPathResultIsSuccess(second)) {
      return dataPathSuccess([...first, ...second.dataPath])
    }
  } else if (dataPathResultIsSuccess(first)) {
    if (Array.isArray(second)) {
      return dataPathSuccess([...first.dataPath, ...second])
    } else if (dataPathResultIsSuccess(second)) {
      return dataPathSuccess([...first.dataPath, ...second.dataPath])
    }
  }

  return dataPathNotPossible
}

export type DataPathResult = DataPathPositiveResult | DataPathUnavailable

export function dataPathResultIsSuccess(result: DataPathResult): result is DataPathSuccess {
  return result.type === 'DATA_PATH_SUCCESS'
}

export function dataPathResultIsNotPossible(result: DataPathResult): result is DataPathNotPossible {
  return result.type === 'DATA_PATH_NOT_POSSIBLE'
}

export function dataPathResultIsUnavailable(result: DataPathResult): result is DataPathUnavailable {
  return result.type === 'DATA_PATH_UNAVAILABLE'
}

export type DataTracingToLiteralAttribute = {
  type: 'literal-attribute'
  elementPath: ElementPath
  property: PropertyPath
  dataPathIntoAttribute: DataPathPositiveResult
}

export function dataTracingToLiteralAttribute(
  elementPath: ElementPath,
  property: PropertyPath,
  dataPathIntoAttribute: DataPathPositiveResult,
): DataTracingToLiteralAttribute {
  return {
    type: 'literal-attribute',
    elementPath: elementPath,
    property: property,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingToLiteralAssignment = {
  type: 'literal-assignment'
  elementPath: ElementPath
  dataPathIntoAttribute: DataPathPositiveResult
}

export function dataTracingToLiteralAssignment(
  elementPath: ElementPath,
  dataPathIntoAttribute: DataPathPositiveResult,
): DataTracingToLiteralAssignment {
  return {
    type: 'literal-assignment',
    elementPath: elementPath,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingToElementAtScope = {
  type: 'element-at-scope'
  scope: ElementPath
  element: JSXElementChild
  dataPathIntoAttribute: DataPathPositiveResult
}

export function dataTracingToElementAtScope(
  scope: ElementPath,
  element: JSXElementChild,
  dataPathIntoAttribute: DataPathPositiveResult,
): DataTracingToElementAtScope {
  return {
    type: 'element-at-scope',
    scope: scope,
    element: element,
    dataPathIntoAttribute: dataPathIntoAttribute,
  }
}

export type DataTracingToAHookCall = {
  type: 'hook-result'
  hookName: string
  elementPath: ElementPath
  dataPathIntoAttribute: DataPathPositiveResult
}

export function dataTracingToAHookCall(
  elementPath: ElementPath,
  hookName: string,
  dataPathIntoAttribute: DataPathPositiveResult,
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
  dataPathIntoAttribute: DataPathPositiveResult
}

export function dataTracingToAComponentProp(
  elementPath: ElementPath,
  propertyPath: PropertyPath,
  dataPathIntoAttribute: DataPathPositiveResult,
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
  | DataTracingToLiteralAssignment
  | DataTracingToAHookCall
  | DataTracingToAComponentProp
  | DataTracingToElementAtScope
  | DataTracingFailed

export function processJSPropertyAccessors(
  expression: JSExpression,
): Either<string, { originalIdentifier: JSIdentifier; path: Array<string | number> }> {
  switch (expression.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JSX_ELEMENT':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_VALUE':
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
      }, processJSPropertyAccessorsIntoDataTracingPath(expression.onValue))
    }
    case 'JS_PROPERTY_ACCESS':
      return mapEither((resultSoFar) => {
        return {
          path: [...resultSoFar.path, expression.property],
          originalIdentifier: resultSoFar.originalIdentifier,
        }
      }, processJSPropertyAccessorsIntoDataTracingPath(expression.onValue))

    default:
      assertNever(expression)
  }
}

function processJSPropertyAccessorsIntoDataTracingPath(
  expression: JSExpression,
): Either<string, { originalIdentifier: JSIdentifier; path: Array<string> }> {
  return mapEither((result) => {
    return {
      originalIdentifier: result.originalIdentifier,
      path: result.path.map((part) => `${part}`),
    }
  }, processJSPropertyAccessors(expression))
}

function propUsedByIdentifierOrAccess(
  param: Param,
  originalIdentifier: JSIdentifier,
  pathDrillSoFar: DataPathPositiveResult,
): Either<string, { propertyName: string; modifiedPathDrillSoFar: DataPathPositiveResult }> {
  function getPropertyNameFromPathSoFar(): Either<
    string,
    { propertyName: string; modifiedPathDrillSoFar: DataPathPositiveResult }
  > {
    switch (pathDrillSoFar.type) {
      case 'DATA_PATH_SUCCESS':
        const propertyName = pathDrillSoFar.dataPath.at(0)
        if (propertyName == null) {
          return left('Path so far is empty.')
        } else {
          return right({
            propertyName: propertyName,
            modifiedPathDrillSoFar: dataPathSuccess(pathDrillSoFar.dataPath.slice(1)),
          })
        }
      case 'DATA_PATH_NOT_POSSIBLE':
        return left('Path is not available.')
      default:
        assertNever(pathDrillSoFar)
    }
  }
  switch (param.boundParam.type) {
    case 'REGULAR_PARAM': {
      // in case of a regular prop param, first we want to match the param name to the original identifier
      if (param.boundParam.paramName !== originalIdentifier.name) {
        return left('identifier does not match the prop name')
      }

      return getPropertyNameFromPathSoFar()
    }
    case 'DESTRUCTURED_OBJECT': {
      for (const paramPart of param.boundParam.parts) {
        if (paramPart.param.boundParam.type === 'REGULAR_PARAM') {
          if (paramPart.param.boundParam.paramName === originalIdentifier.name) {
            if (paramPart.param.dotDotDotToken) {
              return getPropertyNameFromPathSoFar()
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
  pathDrillSoFar: DataPathPositiveResult,
): Either<string, { modifiedPathDrillSoFar: DataPathPositiveResult }> {
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
                modifiedPathDrillSoFar: combinePositiveResults(
                  [paramPart.param.boundParam.paramName],
                  pathDrillSoFar,
                ),
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

export function traceDataFromElement(
  startFromElement: JSXElementChild,
  enclosingScope: ElementPath, // <- the closest "parent" element path which points to the narrowest scope around the JSXElementChild. this is where we will start our upward scope walk
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  pathDrillSoFar: DataPathPositiveResult,
): DataTracingResult {
  switch (startFromElement.type) {
    case 'JSX_ELEMENT':
    case 'JSX_TEXT_BLOCK':
    case 'JSX_FRAGMENT':
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_MAP_EXPRESSION':
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
      return dataTracingToElementAtScope(enclosingScope, startFromElement, dataPathSuccess([]))
    case 'JS_IDENTIFIER':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
      return traceDataFromIdentifierOrAccess(
        startFromElement,
        enclosingScope,
        metadata,
        projectContents,
        pathDrillSoFar,
      )
    case 'ATTRIBUTE_FUNCTION_CALL': // TODO we should support this
    case 'ATTRIBUTE_OTHER_JAVASCRIPT': // by definition we can't support this, as this is the catch-all for unsupported expressions
      return dataTracingFailed(`Unsupported element type ${startFromElement.type}`)
    default:
      assertNever(startFromElement)
  }
}

export function traceDataFromVariableName(
  enclosingScope: ElementPath | FileRootPath,
  variableName: string,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  pathDrillSoFar: DataPathPositiveResult,
): DataTracingResult {
  if (enclosingScope.type === 'file-root') {
    return dataTracingFailed('Cannot trace data from variable name in file root')
  }
  const componentHoldingElement = findContainingComponentForPathInProjectContents(
    enclosingScope,
    projectContents,
  )

  if (componentHoldingElement == null || componentHoldingElement.arbitraryJSBlock == null) {
    return dataTracingFailed('Could not find containing component')
  }

  return walkUpInnerScopesUntilReachingComponent(
    metadata,
    projectContents,
    enclosingScope,
    enclosingScope,
    enclosingScope,
    componentHoldingElement,
    jsIdentifier(variableName, '', null, emptyComments),
    pathDrillSoFar,
  )
}

function traceDataFromIdentifierOrAccess(
  startFromElement: IdentifierOrAccess,
  enclosingScope: ElementPath,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  pathDrillSoFar: DataPathPositiveResult,
): DataTracingResult {
  const componentHoldingElement: UtopiaJSXComponent | null =
    findContainingComponentForPathInProjectContents(enclosingScope, projectContents)

  if (componentHoldingElement == null) {
    return dataTracingFailed('Could not find containing component')
  }

  const dataPath = processJSPropertyAccessorsIntoDataTracingPath(startFromElement)

  if (isLeft(dataPath)) {
    return dataTracingFailed(dataPath.value)
  }

  const identifier = dataPath.value.originalIdentifier

  return walkUpInnerScopesUntilReachingComponent(
    metadata,
    projectContents,
    enclosingScope,
    enclosingScope,
    EP.getPathOfComponentRoot(enclosingScope),
    componentHoldingElement,
    identifier,
    combinePositiveResults(dataPath.value.path, pathDrillSoFar),
  )
}

export function traceDataFromProp(
  startFrom: ElementPropertyPath,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  pathDrillSoFar: DataPathPositiveResult,
): DataTracingResult {
  const elementHoldingProp = MetadataUtils.findElementByElementPath(metadata, startFrom.elementPath)
  if (elementHoldingProp == null) {
    return dataTracingFailed('traceDataFromProp did not find element at path')
  }

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
  if (
    propDeclaration.remainingPath != null ||
    propDeclaration.attribute.type === 'PART_OF_ATTRIBUTE_VALUE'
  ) {
    return dataTracingFailed("We don't yet support propertyPaths pointing deeper into attributes")
  }
  if (propDeclaration.attribute.type === 'ATTRIBUTE_NOT_FOUND') {
    return dataTracingFailed('Could not find attribute at path')
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

  return traceDataFromElement(
    propDeclaration.attribute,
    startFrom.elementPath,
    metadata,
    projectContents,
    pathDrillSoFar,
  )
}

function walkUpInnerScopesUntilReachingComponent(
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  originalElementPath: ElementPath,
  currentElementPathOfWalk: ElementPath,
  containingComponentRootPath: ElementPath,
  componentHoldingElement: UtopiaJSXComponent,
  identifier: JSIdentifier,
  pathDrillSoFar: DataPathPositiveResult,
): DataTracingResult {
  if (EP.pathsEqual(currentElementPathOfWalk, containingComponentRootPath)) {
    const resultInComponentScope: DataTracingResult = lookupInComponentScope(
      originalElementPath,
      containingComponentRootPath,
      componentHoldingElement,
      identifier,
      pathDrillSoFar,
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
            const foundPropSameName = paramUsedByIdentifierOrAccess(
              param,
              identifier,
              pathDrillSoFar,
            )

            if (isRight(foundPropSameName)) {
              // ok, so now we need to figure out what the map is mapping over
              const mapOver = parentElement.valueToMap
              // figure out the identifier of the mapOver
              if (
                mapOver.type === 'JS_IDENTIFIER' ||
                mapOver.type === 'JS_ELEMENT_ACCESS' ||
                mapOver.type === 'JS_PROPERTY_ACCESS'
              ) {
                const dataPath = processJSPropertyAccessorsIntoDataTracingPath(mapOver)

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
                    projectContents,
                    originalElementPath,
                    EP.parentPath(currentElementPathOfWalk),
                    containingComponentRootPath,
                    componentHoldingElement,
                    dataPath.value.originalIdentifier,
                    combinePositiveResults(
                      combinePositiveResults(dataPath.value.path, [mapIndexHack]),
                      foundPropSameName.value.modifiedPathDrillSoFar,
                    ),
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
    projectContents,
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

function findPathToIdentifier(param: BoundParam, identifier: string): DataPathResult {
  function innerFindPath(workingParam: BoundParam, currentPath: DataPath): DataPathResult {
    switch (workingParam.type) {
      case 'REGULAR_PARAM':
        if (workingParam.paramName === identifier) {
          return dataPathSuccess([...currentPath, workingParam.paramName])
        } else {
          return dataPathUnavailable
        }
      case 'DESTRUCTURED_OBJECT':
        for (const part of workingParam.parts) {
          if (part.propertyName == identifier) {
            return dataPathSuccess([...currentPath, part.propertyName])
          } else if (
            isRegularParam(part.param.boundParam) &&
            part.param.boundParam.paramName === identifier &&
            part.param.dotDotDotToken
          ) {
            // Object spread here, we're jumping over the field name of the destructured object.
            return dataPathSuccess(currentPath)
          } else if (
            isRegularParam(part.param.boundParam) &&
            part.param.boundParam.paramName === identifier
          ) {
            return dataPathSuccess([...currentPath, part.propertyName ?? identifier])
          }
        }
        return dataPathUnavailable
      case 'DESTRUCTURED_ARRAY':
        let arrayIndex: number = 0
        for (const part of workingParam.parts) {
          switch (part.type) {
            case 'PARAM':
              // For a spread we can still locate the origin, but the path is
              // potentially gibberish.
              if (part.dotDotDotToken) {
                return dataPathNotPossible
              }
              const possibleResult = innerFindPath(part.boundParam, [
                ...currentPath,
                `${arrayIndex}`,
              ])
              if (dataPathResultIsSuccess(possibleResult)) {
                return possibleResult
              }
              break
            case 'OMITTED_PARAM':
              return dataPathUnavailable
            default:
              assertNever(part)
          }
          arrayIndex++
        }
        return dataPathUnavailable
      default:
        assertNever(workingParam)
    }
    return dataPathUnavailable
  }
  return innerFindPath(param, [])
}

function lookupInComponentScope(
  elementPathInsideComponent: ElementPath,
  componentPath: ElementPath,
  componentHoldingElement: UtopiaJSXComponent,
  originalIdentifier: JSIdentifier,
  pathDrillSoFar: DataPathPositiveResult,
): DataTracingResult {
  const identifier = originalIdentifier

  // let's see if the identifier points to a component prop
  {
    const firstParam = componentHoldingElement?.params?.at(0)
    if (firstParam != null) {
      // let's try to match the name to the containing component's props!
      const foundPropSameName = propUsedByIdentifierOrAccess(firstParam, identifier, pathDrillSoFar)

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
        const dataPath = processJSPropertyAccessorsIntoDataTracingPath(
          foundAssignmentOfIdentifier.rightHandSide,
        )

        if (isRight(dataPath)) {
          return lookupInComponentScope(
            elementPathInsideComponent,
            componentPath,
            componentHoldingElement,
            dataPath.value.originalIdentifier,
            combinePositiveResults(dataPath.value.path, pathDrillSoFar),
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
          pathDrillSoFar,
        )
      }

      if (isConsideredLiteralValue(foundAssignmentOfIdentifier.rightHandSide)) {
        return dataTracingToLiteralAssignment(componentPath, pathDrillSoFar)
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
            if (dataPathResultIsUnavailable(possiblePathToIdentifier)) {
              return null
            } else {
              const possibleHookCall = getPossibleHookCall(assignment.rightHandSide)
              if (possibleHookCall == null && isJSIdentifier(originExpression)) {
                return lookupInComponentScope(
                  elementPathInsideComponent,
                  componentPath,
                  componentHoldingElement,
                  originExpression,
                  combinePositiveResults(possiblePathToIdentifier, pathDrillSoFar),
                )
              } else if (possibleHookCall != null) {
                return dataTracingToAHookCall(
                  componentPath,
                  possibleHookCall,
                  combinePositiveResults(possiblePathToIdentifier, pathDrillSoFar),
                )
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

function isConsideredLiteralValue(value: JSExpression): boolean {
  switch (value.type) {
    case 'ATTRIBUTE_VALUE':
      return true
    case 'ATTRIBUTE_NESTED_ARRAY':
      return isArrayLiteral(value)
    case 'ATTRIBUTE_NESTED_OBJECT':
      return isObjectLiteral(value)
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JSX_ELEMENT':
    case 'JSX_MAP_EXPRESSION':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_IDENTIFIER':
    case 'JS_PROPERTY_ACCESS':
      return false
    default:
      assertNever(value)
  }
}

function isArrayLiteral(array: JSExpressionNestedArray): boolean {
  return array.content.every((c) => isConsideredLiteralValue(c.value))
}

function isObjectLiteral(value: JSExpressionNestedObject): boolean {
  return value.content.every((c) => isConsideredLiteralValue(c.value))
}
