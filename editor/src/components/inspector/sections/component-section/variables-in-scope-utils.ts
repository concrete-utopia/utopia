import type {
  ControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
} from '../../../custom-code/internal-property-controls'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import type { VariableData } from '../../../canvas/ui-jsx-canvas'
import { useEditorState, Substores } from '../../../editor/store/store-hook'
import type { DataPickerFilterOption, DataPickerOption } from './data-picker-utils'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import React from 'react'
import { useGetPropertyControlsForSelectedComponents } from '../../common/property-controls-hooks'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { assertNever, identity } from '../../../../core/shared/utils'
import { isValidReactNode } from '../../../../utils/react-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

function valuesFromObject(
  variable: ArrayInfo | ObjectInfo,
  depth: number,
  originalObjectName: string,
  valuePath: Array<string | number>,
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
        depth: depth,
        definedElsewhere: originalObjectName,
        children: variable.elements
          .flatMap((e, index) =>
            valuesFromVariable(e, depth + 1, originalObjectName, [...valuePath, index]),
          )
          .map(patchDefinedElsewhereInfo),
        valuePath: valuePath,
        disabled: false,
      },
    ]
  } else if (variable.type === 'object') {
    return [
      {
        type: 'object',
        variableInfo: variable,
        depth: depth,
        definedElsewhere: originalObjectName,
        children: variable.props
          .flatMap((e) =>
            valuesFromVariable(e, depth + 1, originalObjectName, [
              ...valuePath,
              e.expressionPathPart,
            ]),
          )
          .map(patchDefinedElsewhereInfo),
        valuePath: valuePath,
        disabled: false,
      },
    ]
  } else {
    assertNever(variable)
  }
}

function valuesFromVariable(
  variable: VariableInfo,
  depth: number,
  originalObjectName: string,
  valuePath: Array<string | number>,
): Array<DataPickerOption> {
  switch (variable.type) {
    case 'primitive':
      return [
        {
          type: 'primitive',
          variableInfo: variable,
          definedElsewhere: originalObjectName,
          depth: depth,
          valuePath: valuePath,
          disabled: false,
        },
      ]
    case 'array':
    case 'object':
      return valuesFromObject(variable, depth, originalObjectName, valuePath)
    case 'jsx':
      return [
        {
          type: 'jsx',
          variableInfo: variable,
          definedElsewhere: originalObjectName,
          depth: depth,
          valuePath: valuePath,
          disabled: false,
        },
      ]
    default:
      assertNever(variable)
  }
}

function usePropertyControlDescriptions(
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

export interface PrimitiveInfo {
  type: 'primitive'
  expression: string
  expressionPathPart: string | number
  value: unknown
  matches: boolean
}

export interface ObjectInfo {
  type: 'object'
  expression: string
  expressionPathPart: string | number
  value: unknown
  props: Array<VariableInfo>
  matches: boolean
}

export interface ArrayInfo {
  type: 'array'
  expression: string
  expressionPathPart: string | number
  value: unknown
  elements: Array<VariableInfo>
  matches: boolean
}

export interface JSXInfo {
  type: 'jsx'
  expression: string
  expressionPathPart: string | number
  value: unknown
  matches: boolean
}

export type VariableInfo = PrimitiveInfo | ArrayInfo | ObjectInfo | JSXInfo

export function variableInfoFromValue(
  expression: string,
  expressionPathPart: string | number,
  value: unknown,
): VariableInfo | null {
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
        matches: false,
      }
    case 'object':
      if (value == null) {
        return {
          type: 'primitive',
          expression: expression,
          expressionPathPart: expressionPathPart,
          value: value,
          matches: false,
        }
      }
      if (Array.isArray(value)) {
        return {
          type: 'array',
          expression: expression,
          expressionPathPart: expressionPathPart,
          value: value,
          matches: false,
          elements: mapDropNulls(
            (e, idx) => variableInfoFromValue(`${expression}[${idx}]`, idx, e),
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
          matches: false,
        }
      }
      return {
        type: 'object',
        expression: expression,
        expressionPathPart: expressionPathPart,
        value: value,
        matches: false,
        props: mapDropNulls(([key, propValue]) => {
          return variableInfoFromValue(`${expression}['${key}']`, key, propValue)
        }, Object.entries(value)),
      }
  }
}

function variableInfoFromVariableData(variableNamesInScope: VariableData): Array<VariableInfo> {
  const info = mapDropNulls(
    ([key, { spiedValue }]) => variableInfoFromValue(key, key, spiedValue),
    Object.entries(variableNamesInScope),
  )

  return info
}

function getTargetPropertyFromControlDescription(
  controlDescription: ControlDescription | null,
  targetPropertyName: string | null,
): ControlDescription | null {
  if (targetPropertyName == null || controlDescription == null) {
    return controlDescription
  } else {
    switch (controlDescription.control) {
      case 'object':
        return controlDescription.object[targetPropertyName] ?? null
      default:
        return controlDescription
    }
  }
}

export function orderVariablesForRelevance(
  variableNamesInScope_MUTABLE: Array<VariableInfo>,
  controlDescription: ControlDescription | null,
  currentPropertyValue: PropertyValue,
  targetPropertyName: string | null,
  mode: DataPickerFilterOption,
): Array<VariableInfo> {
  let valuesExactlyMatchingPropertyName: Array<VariableInfo> = []
  let valuesExactlyMatchingPropertyDescription: Array<VariableInfo> = []
  let valuesMatchingPropertyDescription: Array<VariableInfo> = []
  let valuesMatchingPropertyShape: Array<VariableInfo> = []
  let valueElementMatches: Array<VariableInfo> = []
  let restOfValues: Array<VariableInfo> = []

  for (let variable of variableNamesInScope_MUTABLE) {
    if (variable.type === 'array') {
      variable.elements = orderVariablesForRelevance(
        variable.elements,
        controlDescription,
        currentPropertyValue,
        targetPropertyName,
        mode,
      )
    } else if (variable.type === 'object') {
      variable.props = orderVariablesForRelevance(
        variable.props,
        controlDescription,
        currentPropertyValue,
        targetPropertyName,
        mode,
      )
    }

    const valueExactlyMatchesPropertyName = variable.expressionPathPart === targetPropertyName

    const variableCanBeMappedOver =
      variable.type === 'array' && currentPropertyValue.type === 'mapped-value'

    const valueExactlyMatchesControlDescription =
      controlDescription?.control === 'jsx' && React.isValidElement(variable.value)

    const targetControlDescription = getTargetPropertyFromControlDescription(
      controlDescription,
      targetPropertyName,
    )
    const valueMatchesControlDescription =
      targetControlDescription != null &&
      variableMatchesControlDescription(variable.value, targetControlDescription)

    const valueMatchesCurrentPropValue =
      currentPropertyValue.type === 'existing' &&
      variableShapesMatch(currentPropertyValue.value, variable.value)

    const arrayOrObjectChildMatches =
      (variable.type === 'array' && variable.elements.some((e) => e.matches)) ||
      (variable.type === 'object' && variable.props.some((e) => e.matches))

    if (valueExactlyMatchesPropertyName || variableCanBeMappedOver) {
      valuesExactlyMatchingPropertyName.push({ ...variable, matches: true })
    } else if (valueExactlyMatchesControlDescription) {
      valuesExactlyMatchingPropertyDescription.push({ ...variable, matches: true })
    } else if (valueMatchesControlDescription) {
      valuesMatchingPropertyDescription.push({ ...variable, matches: true })
    } else if (arrayOrObjectChildMatches) {
      valueElementMatches.push({ ...variable, matches: false })
    } else if (valueMatchesCurrentPropValue) {
      valuesMatchingPropertyShape.push({ ...variable, matches: true })
    } else {
      restOfValues.push(variable)
    }
  }

  return [
    ...valuesExactlyMatchingPropertyName,
    ...valuesExactlyMatchingPropertyDescription,
    ...valuesMatchingPropertyDescription,
    ...valuesMatchingPropertyShape,
    ...valueElementMatches,
    ...restOfValues,
  ]
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

const keepLocalestScope =
  () =>
  (variablesInScope: VariableData): VariableData => {
    let deepestInsertionCeiling = -Infinity
    Object.values(variablesInScope).forEach((variable) => {
      if (variable.insertionCeiling == null) {
        return
      }

      deepestInsertionCeiling = Math.max(
        deepestInsertionCeiling,
        EP.fullDepth(variable.insertionCeiling),
      )
    })

    const result: VariableData = {}
    Object.entries(variablesInScope).forEach(([key, variable]) => {
      if (variable.insertionCeiling == null) {
        result[key] = variable
        return
      }

      if (EP.fullDepth(variable.insertionCeiling) === deepestInsertionCeiling) {
        result[key] = variable
      }
    })

    return result
  }

export function useVariablesInScopeForSelectedElement(
  selectedView: ElementPath,
  propertyPath: PropertyPath | null,
  mode: DataPickerFilterOption,
): Array<DataPickerOption> {
  const selectedViewPath = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.at(0) ?? null,
    'useVariablesInScopeForSelectedElement selectedViewPath',
  )

  const variablesInScope = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.variablesInScope,
    'useVariablesInScopeForSelectedElement variablesInScope',
  )

  const controlDescriptions = usePropertyControlDescriptions(propertyPath)
  const currentPropertyValue = usePropertyValue(selectedView, propertyPath)

  const variableNamesInScope = React.useMemo((): Array<DataPickerOption> => {
    if (selectedViewPath == null) {
      return []
    }

    let variablesInScopeForSelectedPath: VariableData | null = (() => {
      // if the selected element doesn't have an associacted variablesInScope, we assume it's safe to walk up the path within the same component
      const allPathsInsideScope = EP.allPathsInsideComponent(selectedViewPath)
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
      mode === 'preferred' ? keepLocalestScope() : identity,
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

    const orderedVariablesInScope = orderVariablesForRelevance(
      variableInfo,
      controlDescriptions,
      currentPropertyValue,
      propertyPath == null ? null : '' + PP.lastPart(propertyPath),
      mode,
    )

    return orderedVariablesInScope.flatMap((variable) =>
      valuesFromVariable(variable, 0, variable.expression, [variable.expressionPathPart]),
    )
  }, [
    controlDescriptions,
    currentPropertyValue,
    mode,
    selectedViewPath,
    variablesInScope,
    propertyPath,
  ])

  return variableNamesInScope
}

function arrayShapesMatch(left: Array<unknown>, right: Array<unknown>): boolean {
  if (left.length === 0 || right.length === 0) {
    return true
  }

  return variableShapesMatch(left[0], right[0])
}

function objectShapesMatch(left: object, right: object): boolean {
  const keysFromLeft = Object.keys(left)
  const keysFromRight = Object.keys(right)
  const keysMatch =
    keysFromLeft.length === keysFromRight.length &&
    keysFromLeft.every((key) => keysFromRight.includes(key))

  if (!keysMatch) {
    return false
  }

  return keysFromLeft.every((key) => variableShapesMatch((left as any)[key], (right as any)[key]))
}

function variableShapesMatch(current: unknown, other: unknown): boolean {
  if (React.isValidElement(current)) {
    return isValidReactNode(other)
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

export type PropertyValue =
  | { type: 'existing'; value: unknown }
  | { type: 'mapped-value' }
  | { type: 'not-found' }

function usePropertyValue(
  selectedView: ElementPath,
  propertyPath: PropertyPath | null,
): PropertyValue {
  const allElementProps = useEditorState(
    Substores.metadata,
    (store) => store.editor.allElementProps,
    'usePropertyValue allElementProps',
  )

  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'usePropertyValue metadata',
  )

  if (MetadataUtils.isJSXMapExpression(selectedView, metadata)) {
    return { type: 'mapped-value' }
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
