import * as ObjectPath from 'object-path'
import type {
  ControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
} from '../../../custom-code/internal-property-controls'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import type { FileRootPath, VariableData, VariablesInScope } from '../../../canvas/ui-jsx-canvas'
import { useEditorState, Substores } from '../../../editor/store/store-hook'
import type { DataPickerOption } from './data-picker-utils'
import * as EP from '../../../../core/shared/element-path'
import React from 'react'
import { useGetPropertyControlsForSelectedComponents } from '../../common/property-controls-hooks'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { assertNever } from '../../../../core/shared/utils'
import { isValidReactNode } from '../../../../utils/react-utils'
import { emptySet } from '../../../../core/shared/set-utils'
import type { JSExpression, JSXElementChild } from '../../../../core/shared/element-template'
import type { Either } from '../../../../core/shared/either'
import { foldEither, isLeft, left, right } from '../../../../core/shared/either'
import { processJSPropertyAccessors } from '../../../../core/data-tracing/data-tracing'
import type { CartoucheDataType } from './cartouche-ui'
import { jsxSimpleAttributeToValue } from '../../../../core/shared/jsx-attribute-utils'
import type { ModifiableAttribute } from '../../../../core/shared/jsx-attributes'

function valuesFromObject(
  variable: ArrayInfo | ObjectInfo,
  insertionCeiling: ElementPath | FileRootPath,
  depth: number,
  originalObjectName: string,
  valuePath: Array<string | number>,
  isChildOfArray: boolean,
): Array<DataPickerOption> {
  const patchDefinedElsewhereInfo = (option: DataPickerOption): DataPickerOption => ({
    ...option,
    definedElsewhere: originalObjectName,
  })

  if (variable.type === 'array') {
    return [
      {
        type: 'array',
        variableInfo: variable,
        insertionCeiling: insertionCeiling,
        depth: depth,
        definedElsewhere: originalObjectName,
        children: variable.elements
          .flatMap((e, index) =>
            valuesFromVariable(
              e,
              insertionCeiling,
              depth + 1,
              originalObjectName,
              [...valuePath, index],
              true,
            ),
          )
          .map(patchDefinedElsewhereInfo),
        valuePath: valuePath,
        isChildOfArray: isChildOfArray,
      },
    ]
  } else if (variable.type === 'object') {
    return [
      {
        type: 'object',
        variableInfo: variable,
        insertionCeiling: insertionCeiling,
        depth: depth,
        definedElsewhere: originalObjectName,
        children: variable.props
          .flatMap((e) =>
            valuesFromVariable(
              e,
              insertionCeiling,
              depth + 1,
              originalObjectName,
              [...valuePath, e.expressionPathPart],
              false,
            ),
          )
          .map(patchDefinedElsewhereInfo),
        valuePath: valuePath,
        isChildOfArray: isChildOfArray,
      },
    ]
  } else {
    assertNever(variable)
  }
}

function valuesFromVariable(
  variable: VariableInfo,
  insertionCeiling: ElementPath | FileRootPath,
  depth: number,
  originalObjectName: string,
  valuePath: Array<string | number>,
  isChildOfArray: boolean,
): Array<DataPickerOption> {
  switch (variable.type) {
    case 'primitive':
      return [
        {
          type: 'primitive',
          variableInfo: variable,
          insertionCeiling: insertionCeiling,
          definedElsewhere: originalObjectName,
          depth: depth,
          valuePath: valuePath,
          isChildOfArray: isChildOfArray,
        },
      ]
    case 'array':
      return valuesFromObject(
        variable,
        insertionCeiling,
        depth,
        originalObjectName,
        valuePath,
        isChildOfArray,
      )
    case 'object':
      return valuesFromObject(
        variable,
        insertionCeiling,
        depth,
        originalObjectName,
        valuePath,
        isChildOfArray,
      )
    case 'jsx':
      return [
        {
          type: 'jsx',
          variableInfo: variable,
          insertionCeiling: insertionCeiling,
          definedElsewhere: originalObjectName,
          depth: depth,
          valuePath: valuePath,
          isChildOfArray: isChildOfArray,
        },
      ]
    default:
      assertNever(variable)
  }
}

export function usePropertyControlDescriptions(
  selectedProperty: PropertyPath | null,
): ControlDescription | null {
  const controls = useGetPropertyControlsForSelectedComponents()
  if (
    selectedProperty == null ||
    selectedProperty.propertyElements.length === 0 ||
    typeof selectedProperty.propertyElements[0] !== 'string'
  ) {
    // currently we only support picking data for props on components
    return null
  }
  const controlForProp = controls.flatMap(
    (c) => c.controls[selectedProperty.propertyElements[0]] ?? [],
  )
  return controlForProp[0] ?? null
}

export type VariableMatches = 'matches' | 'child-matches' | 'does-not-match'

export const variableMatches = (variable: VariableInfoBase) => {
  switch (variable.matches) {
    case 'matches':
      return true
    case 'child-matches':
    case 'does-not-match':
      return false
    default:
      assertNever(variable.matches)
  }
}

interface VariableInfoBase {
  type: string
  expression: string
  expressionPathPart: string | number
  value: unknown
  insertionCeiling: ElementPath | FileRootPath
  matches: VariableMatches
}

export interface PrimitiveInfo extends VariableInfoBase {
  type: 'primitive'
}

export interface ObjectInfo extends VariableInfoBase {
  type: 'object'
  props: Array<VariableInfo>
}

export interface ArrayInfo extends VariableInfoBase {
  type: 'array'
  elements: Array<VariableInfo>
}

export interface JSXInfo extends VariableInfoBase {
  type: 'jsx'
}

export type VariableInfo = PrimitiveInfo | ArrayInfo | ObjectInfo | JSXInfo

export function variableInfoFromValue(
  expression: string,
  expressionPathPart: string | number,
  value: unknown,
  insertionCeiling: ElementPath | FileRootPath,
  valueStackSoFar: Set<any>,
): VariableInfo | null {
  if ((typeof value === 'object' || typeof value === 'function') && valueStackSoFar.has(value)) {
    // prevent circular dependencies
    return null
  }
  // mutation
  valueStackSoFar.add(value)

  switch (typeof value) {
    case 'function':
    case 'symbol':
      return null
    case 'bigint':
    case 'boolean':
    case 'number':
    case 'string':
    case 'undefined':
      return {
        type: 'primitive',
        expression: expression,
        expressionPathPart: expressionPathPart,
        value: value,
        insertionCeiling: insertionCeiling,
        matches: 'does-not-match',
      }
    case 'object':
      if (value == null) {
        return {
          type: 'primitive',
          expression: expression,
          expressionPathPart: expressionPathPart,
          value: value,
          insertionCeiling: insertionCeiling,
          matches: 'does-not-match',
        }
      }
      if (Array.isArray(value)) {
        return {
          type: 'array',
          expression: expression,
          expressionPathPart: expressionPathPart,
          value: value,
          insertionCeiling: insertionCeiling,
          matches: 'does-not-match',
          elements: mapDropNulls(
            (e, idx) =>
              variableInfoFromValue(
                `${expression}[${idx}]`,
                idx,
                e,
                insertionCeiling,
                valueStackSoFar,
              ),
            value,
          ),
        }
      }
      if (React.isValidElement(value)) {
        return {
          type: 'jsx',
          expression: expression,
          expressionPathPart: expressionPathPart,
          value: value,
          insertionCeiling: insertionCeiling,
          matches: 'does-not-match',
        }
      }
      return {
        type: 'object',
        expression: expression,
        expressionPathPart: expressionPathPart,
        value: value,
        insertionCeiling: insertionCeiling,
        matches: 'does-not-match',
        props: mapDropNulls(([key, propValue]) => {
          return variableInfoFromValue(
            createPropertyAccessExpressionString(expression, key),
            key,
            propValue,
            insertionCeiling,
            valueStackSoFar,
          )
        }, Object.entries(value)),
      }
  }
}

function variableInfoFromVariableData(variableNamesInScope: VariableData): Array<VariableInfo> {
  const info = mapDropNulls(
    ([key, { spiedValue, insertionCeiling }]) =>
      variableInfoFromValue(key, key, spiedValue, insertionCeiling, emptySet()),
    Object.entries(variableNamesInScope),
  )

  return info
}

const varSafeStringRegex = /^[a-zA-Z_$][0-9a-zA-Z_$]*$/

function createPropertyAccessExpressionString(expressionSoFar: string, toAppend: string): string {
  const toAppendIsVarsafeString = varSafeStringRegex.test(toAppend)
  if (toAppendIsVarsafeString) {
    return `${expressionSoFar}.${toAppend}`
  } else {
    return `${expressionSoFar}['${toAppend}']`
  }
}

type VariableMatcher = (_: VariableInfo) => VariableMatches | null

interface MatchResult {
  bucket: number
  variable: VariableInfo
}

function getVariableMatch(
  variable: VariableInfo,
  matchers: Array<VariableMatcher>,
): MatchResult | null {
  for (const [idx, matcher] of matchers.entries()) {
    const matches = matcher(variable)
    if (matches != null) {
      return { bucket: idx, variable: { ...variable, matches: matches } }
    }
  }

  return null
}

function matchVariableDescendants(
  variable: VariableInfo,
  matchers: Array<VariableMatcher>,
): VariableInfo {
  switch (variable.type) {
    case 'array':
      return { ...variable, elements: matchVariables(variable.elements, matchers) }
    case 'object':
      return { ...variable, props: matchVariables(variable.props, matchers) }
    case 'jsx':
    case 'primitive':
      return variable
    default:
      assertNever(variable)
  }
}

function matchVariables(
  variableNamesInScope: Array<VariableInfo>,
  matchers: Array<VariableMatcher>,
): Array<VariableInfo> {
  let categories: Array<VariableInfo[]> = matchers.map(() => [])
  let rest: Array<VariableInfo> = []

  variableNamesInScope.forEach((variable) => {
    const withDescendantsMatched = matchVariableDescendants(variable, matchers)
    const match = getVariableMatch(withDescendantsMatched, matchers)

    if (match != null) {
      categories[match.bucket].push(match.variable)
    } else {
      rest.push(matchVariableDescendants(withDescendantsMatched, matchers))
    }
  })

  return [...categories.flat(), ...rest]
}

function predicateMatcher(cb: (_: VariableInfo) => boolean): VariableMatcher {
  return (variable: VariableInfo) => (cb(variable) ? 'matches' : null)
}

const valueElementMatches: VariableMatcher = (variable) => {
  const matches =
    (variable.type === 'array' && variable.elements.some((e) => e.matches)) ||
    (variable.type === 'object' && variable.props.some((e) => e.matches))
  return matches ? 'child-matches' : null
}

type Matcher = (_: Array<VariableInfo>) => Array<VariableInfo>

export const matchForPropertyValue =
  (
    controlDescription: ControlDescription | null,
    currentPropertyValue: PropertyValue,
    targetPropertyName: string | null,
  ): Matcher =>
  (variableNamesInScope: Array<VariableInfo>) => {
    const valuesExactlyMatchingPropertyDescription: VariableMatcher = predicateMatcher(
      (variable) => controlDescription?.control === 'jsx' && isValidReactNode(variable.value),
    )

    const targetControlDescription =
      targetPropertyName == null ||
      controlDescription == null ||
      controlDescription.control !== 'object'
        ? controlDescription
        : controlDescription.object[targetPropertyName] ?? null

    const valuesMatchingPropertyDescription: VariableMatcher = predicateMatcher(
      (variable) =>
        targetControlDescription != null &&
        variableMatchesControlDescription(variable.value, targetControlDescription),
    )

    const valuesMatchingPropertyShape: VariableMatcher = predicateMatcher(
      (variable) =>
        currentPropertyValue.type === 'existing' &&
        variableShapesMatch(currentPropertyValue.value, variable.value),
    )

    return matchVariables(variableNamesInScope, [
      valuesExactlyMatchingPropertyDescription,
      valuesMatchingPropertyDescription,
      valuesMatchingPropertyShape,
      valueElementMatches,
    ])
  }

export const matchForChildrenProp: Matcher = (variableNamesInScope: Array<VariableInfo>) => {
  return matchVariables(variableNamesInScope, [
    predicateMatcher((v) => isValidReactNode(v.value)),
    valueElementMatches,
  ])
}

export const matchForMappedValue: Matcher = (variableNamesInScope: Array<VariableInfo>) => {
  return matchVariables(variableNamesInScope, [
    predicateMatcher((v) => v.type === 'array'),
    valueElementMatches,
  ])
}

const filterKeyFromObject =
  (propName: string) =>
  <T extends Record<string, unknown>>(variablesInScope: T): T => {
    let next = { ...variablesInScope }
    delete next[propName]
    return next
  }

const filterObjectPropFromVariablesInScope =
  ({ prop, key }: { prop: string; key: string }) =>
  (variablesInScope: VariableData): VariableData => {
    const target = variablesInScope[prop]
    if (target == null || typeof target !== 'object') {
      return variablesInScope
    }
    let next = { ...variablesInScope }
    next[prop] = {
      insertionCeiling: next[prop].insertionCeiling,
      spiedValue: filterKeyFromObject(key)(target.spiedValue as Record<string, unknown>),
    }
    return next
  }

export function useVariablesInScopeForSelectedElement(
  elementPath: ElementPath | null,
  matcher: Matcher,
): Array<DataPickerOption> {
  const variablesInScope = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.variablesInScope,
    'useVariablesInScopeForSelectedElement variablesInScope',
  )

  const variableNamesInScope = React.useMemo((): Array<DataPickerOption> => {
    if (elementPath == null) {
      return []
    }

    let variablesInScopeForSelectedPath: VariableData | null = (() => {
      // if the selected element doesn't have an associacted variablesInScope, we assume it's safe to walk up the path within the same component
      const allPathsInsideScope = EP.allPathsInsideComponent(elementPath)
      for (const path of allPathsInsideScope) {
        const pathAsString = EP.toString(path)
        if (pathAsString in variablesInScope) {
          return variablesInScope[pathAsString]
        }
      }
      return null
    })()

    if (variablesInScopeForSelectedPath == null) {
      return []
    }

    variablesInScopeForSelectedPath = [
      filterKeyFromObject('className'),
      filterKeyFromObject('data-uid'),
      filterKeyFromObject('style'),
      filterKeyFromObject('css'),
      filterObjectPropFromVariablesInScope({ prop: 'props', key: 'className' }),
      filterObjectPropFromVariablesInScope({ prop: 'props', key: 'data-uid' }),
      filterObjectPropFromVariablesInScope({ prop: 'props', key: 'style' }),
      filterObjectPropFromVariablesInScope({ prop: 'props', key: 'css' }),
    ].reduce((vars, fn) => fn(vars), variablesInScopeForSelectedPath)

    const variableInfo = variableInfoFromVariableData(variablesInScopeForSelectedPath)

    const orderedVariablesInScope = matcher(variableInfo)

    return orderedVariablesInScope.flatMap((variable) =>
      valuesFromVariable(
        variable,
        variable.insertionCeiling,
        0,
        variable.expression,
        [variable.expressionPathPart],
        false,
      ),
    )
  }, [elementPath, matcher, variablesInScope])

  return variableNamesInScope
}

function arrayShapesMatch(l: Array<unknown>, r: Array<unknown>): boolean {
  if (l.length === 0 || r.length === 0) {
    return true
  }

  return variableShapesMatch(l[0], r[0])
}

function objectShapesMatch(l: object, r: object): boolean {
  const keysFromLeft = Object.keys(l)
  const keysFromRight = Object.keys(r)
  const keysMatch =
    keysFromLeft.length === keysFromRight.length &&
    keysFromLeft.every((key) => keysFromRight.includes(key))

  if (!keysMatch) {
    return false
  }

  return keysFromLeft.every((key) => variableShapesMatch((l as any)[key], (r as any)[key]))
}

function variableShapesMatch(current: unknown, other: unknown): boolean {
  if (React.isValidElement(current)) {
    return isValidReactNode(other) || typeof other === 'string' || typeof other === 'number'
  }

  if (Array.isArray(current) && Array.isArray(other)) {
    return arrayShapesMatch(current, other)
  }

  if (
    typeof current === 'object' &&
    typeof other === 'object' &&
    current != null &&
    other != null
  ) {
    return objectShapesMatch(current, other)
  }

  return typeof current === typeof other
}

function variableMatchesArrayControlDescription(
  variable: Array<unknown>,
  controlDescription: ArrayControlDescription,
): boolean {
  if (variable.length === 0) {
    return true
  }

  return variableMatchesControlDescription(variable[0], controlDescription.propertyControl)
}

function variableMatchesObjectControlDescription(
  variable: object,
  controlDescription: ObjectControlDescription,
): boolean {
  return Object.entries(controlDescription.object).every(([key, control]) =>
    variableMatchesControlDescription((variable as any)[key], control),
  )
}

function variableMatchesControlDescription(
  variable: unknown,
  controlDescription: ControlDescription,
): boolean {
  const matches =
    (isValidReactNode(variable) && controlDescription.control === 'jsx') ||
    (typeof variable === 'string' && controlDescription.control === 'string-input') ||
    (typeof variable === 'number' && controlDescription.control === 'number-input') ||
    (Array.isArray(variable) &&
      controlDescription.control === 'array' &&
      variableMatchesArrayControlDescription(variable, controlDescription)) ||
    (typeof variable === 'object' &&
      variable != null &&
      controlDescription.control === 'object' &&
      variableMatchesObjectControlDescription(variable, controlDescription))
  return matches
}

export type PropertyValue = { type: 'existing'; value: unknown } | { type: 'not-found' }

export function usePropertyValue(
  selectedView: ElementPath | null,
  propertyPath: PropertyPath | null,
): PropertyValue {
  const allElementProps = useEditorState(
    Substores.metadata,
    (store) => store.editor.allElementProps,
    'usePropertyValue allElementProps',
  )

  if (selectedView == null) {
    return { type: 'not-found' }
  }

  const propsForThisElement = allElementProps[EP.toString(selectedView)] ?? null
  if (propsForThisElement == null) {
    return { type: 'not-found' }
  }

  if (propertyPath == null) {
    return { type: 'not-found' }
  }

  const prop = propsForThisElement[propertyPath.propertyElements[0]] ?? null
  if (prop == null) {
    return { type: 'not-found' }
  }

  return { type: 'existing', value: prop }
}

export function getCartoucheDataTypeForExpression(
  enclosingScope: ElementPath,
  expression: JSXElementChild | ModifiableAttribute,
  variablesInScope: VariablesInScope,
): CartoucheDataType {
  switch (expression.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return 'unknown'
    case 'ATTRIBUTE_NESTED_OBJECT':
      return 'object'
    case 'ATTRIBUTE_NESTED_ARRAY':
      return 'array'
    case 'JSX_ELEMENT':
    case 'JSX_FRAGMENT':
    case 'JSX_MAP_EXPRESSION':
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_TEXT_BLOCK':
      return 'renderable'
    case 'ATTRIBUTE_VALUE':
    case 'PART_OF_ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NOT_FOUND':
      return foldEither(
        () => 'unknown',
        (value) => getCartoucheDataTypeFromJsValue(value),
        jsxSimpleAttributeToValue(expression),
      )
    case 'JS_IDENTIFIER':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_PROPERTY_ACCESS':
      return foldEither(
        () => 'unknown',
        (value) => getCartoucheDataTypeFromJsValue(value),
        getSpiedValueForIdentifierOrAccess(enclosingScope, expression, variablesInScope),
      )
    default:
      assertNever(expression)
  }
}

function getCartoucheDataTypeFromJsValue(value: unknown): CartoucheDataType {
  if (React.isValidElement(value) || typeof value === 'string' || typeof value === 'number') {
    return 'renderable'
  } else if (Array.isArray(value)) {
    return 'array'
  } else if (typeof value === 'object' && value != null) {
    return 'object'
  } else {
    return 'unknown'
  }
}

function getSpiedValueForIdentifierOrAccess(
  enclosingScope: ElementPath,
  expression: JSExpression,
  variablesInScope: VariablesInScope,
): Either<string, any> {
  const accessorPath = processJSPropertyAccessors(expression)
  if (isLeft(accessorPath)) {
    return accessorPath
  }
  const spiedValue = findClosestMatchingScope(variablesInScope, enclosingScope)?.[
    accessorPath.value.originalIdentifier.name
  ]?.spiedValue

  if (spiedValue == null) {
    return left('Variable not found in scope')
  }

  if (accessorPath.value.path.length === 0) {
    return right(spiedValue)
  }

  if (typeof spiedValue !== 'object') {
    return left('Cannot access properties of a non-object')
  }

  const value = ObjectPath.get(spiedValue, accessorPath.value.path)

  return right(value)
}

function findClosestMatchingScope(
  variablesInScope: VariablesInScope,
  targetScope: ElementPath,
): VariableData | null {
  if (targetScope.type === 'elementpath') {
    const allPaths = EP.allPathsInsideComponent(targetScope)
    for (const path of allPaths) {
      const variableData = variablesInScope[EP.toString(path)]
      if (variableData != null) {
        return variableData
      }
    }
  }

  return null
}
