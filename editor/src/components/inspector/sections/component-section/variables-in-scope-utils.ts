import type {
  ControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
} from 'utopia-api/core'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import type { VariableData, VariablesInScope } from '../../../canvas/ui-jsx-canvas'
import { useEditorState, Substores } from '../../../editor/store/store-hook'
import type { VariableOption } from './data-picker-popup'
import * as EP from '../../../../core/shared/element-path'
import React from 'react'
import { useGetPropertyControlsForSelectedComponents } from '../../common/property-controls-hooks'

function valuesFromObject(
  name: string,
  objectName: string,
  value: object | null,
): Array<VariableOption> {
  if (value == null) {
    return [
      {
        variableName: name,
        definedElsewhere: null,
        value: `null`,
      },
    ]
  }

  const patchDefinedElsewhereInfo = (variable: VariableOption) => ({
    variableName: variable.variableName,
    value: variable.value,
    definedElsewhere: objectName,
  })

  if (Array.isArray(value)) {
    return [
      patchDefinedElsewhereInfo({
        variableName: name,
        definedElsewhere: objectName,
        value: `[ ]`,
      }),
    ].concat(
      value.flatMap((v, idx) =>
        valuesFromVariable(`${name}[${idx}]`, v).map((variable) =>
          patchDefinedElsewhereInfo(variable),
        ),
      ),
    )
  }

  return [
    patchDefinedElsewhereInfo({
      variableName: name,
      definedElsewhere: objectName,
      value: `{ }`,
    }),
  ].concat(
    Object.entries(value).flatMap(([key, field]) =>
      valuesFromVariable(`${name}['${key}']`, field).map((variable) =>
        patchDefinedElsewhereInfo(variable),
      ),
    ),
  )
}

function valuesFromVariable(name: string, value: unknown): Array<VariableOption> {
  switch (typeof value) {
    case 'bigint':
    case 'boolean':
    case 'number':
    case 'string':
    case 'undefined':
      return [
        {
          variableName: name,
          definedElsewhere: name,
          value: `${value}`,
        },
      ]
    case 'object':
      return valuesFromObject(name, name, value)
    case 'function':
    case 'symbol':
      return []
  }
}

function usePropertyControlDescriptions(): Array<ControlDescription> {
  return useGetPropertyControlsForSelectedComponents().flatMap((controls) =>
    Object.values(controls.controls),
  )
}

function orderVariablesInScope<T>(
  variableNamesInScope: Array<T>,
  nameAndValueFn: (_: T) => [string, unknown],
  controlDescriptions: Array<ControlDescription>,
  currentPropertyValue: PropertyValue,
): Array<[string, unknown]> {
  let valuesMatchingPropertyDescription: [string, unknown][] = []
  let valuesMatchingPropertyShape: [string, unknown][] = []
  let restOfValues: [string, unknown][] = []

  for (const element of variableNamesInScope) {
    const [name, spiedValue] = nameAndValueFn(element)
    const valueMatchesControlDescription = controlDescriptions.some((d) =>
      variableMatchesControlDescription(spiedValue, d),
    )
    const valueMatchesCurrentPropValue =
      currentPropertyValue.type === 'existing' &&
      variableShapesMatch(currentPropertyValue.value, spiedValue)

    if (valueMatchesControlDescription) {
      valuesMatchingPropertyDescription.push([name, spiedValue])
    } else if (valueMatchesCurrentPropValue) {
      valuesMatchingPropertyShape.push([name, spiedValue])
    } else {
      restOfValues.push([name, spiedValue])
    }
  }

  return [...valuesMatchingPropertyDescription, ...valuesMatchingPropertyShape, ...restOfValues]
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
  selectedView: ElementPath,
  propertyPath: PropertyPath,
): Array<VariableOption> {
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

  const controlDescriptions = usePropertyControlDescriptions()
  const currentPropertyValue = usePropertyValue(selectedView, propertyPath)

  const variableNamesInScope = React.useMemo((): Array<VariableOption> => {
    if (selectedViewPath == null) {
      return []
    }

    let variablesInScopeForSelectedPath = variablesInScope[EP.toString(selectedViewPath)]

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

    const orderedVariablesInScope = orderVariablesInScope(
      Object.entries(variablesInScopeForSelectedPath),
      ([name, { spiedValue }]) => [name, spiedValue],
      controlDescriptions,
      currentPropertyValue,
    )

    return orderedVariablesInScope.flatMap(([name, variable]) =>
      valuesFromVariable(name, variable, name),
    )
  }, [controlDescriptions, currentPropertyValue, selectedViewPath, variablesInScope])

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

function variableShapesMatch(left: unknown, right: unknown): boolean {
  if (Array.isArray(left) && Array.isArray(right)) {
    return arrayShapesMatch(left, right)
  }

  if (typeof left === 'object' && typeof right === 'object' && left != null && right != null) {
    return objectShapesMatch(left, right)
  }

  return typeof left === typeof right
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

type PropertyValue = { type: 'existing'; value: unknown } | { type: 'not-found' }

function usePropertyValue(selectedView: ElementPath, propertyPath: PropertyPath): PropertyValue {
  const allElementProps = useEditorState(
    Substores.metadata,
    (store) => store.editor.allElementProps,
    'usePropertyValue allElementProps',
  )
  const propsForThisElement = allElementProps[EP.toString(selectedView)] ?? null
  if (propsForThisElement == null) {
    return { type: 'not-found' }
  }

  const prop = propsForThisElement[propertyPath.propertyElements[0]] ?? null
  if (prop == null) {
    return { type: 'not-found' }
  }

  return { type: 'existing', value: prop }
}
