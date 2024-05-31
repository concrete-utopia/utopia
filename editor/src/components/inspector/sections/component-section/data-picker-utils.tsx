import { atom } from 'jotai'
import {
  isJSExpression,
  type JSExpressionOtherJavaScript,
  type JSXElementChild,
} from '../../../../core/shared/element-template'
import { assertNever } from '../../../../core/shared/utils'
import type { ArrayInfo, JSXInfo, ObjectInfo, PrimitiveInfo } from './variables-in-scope-utils'
import { processJSPropertyAccessors } from '../../../../core/data-tracing/data-tracing'
import { foldEither } from '../../../../core/shared/either'

interface VariableOptionBase {
  depth: number
  definedElsewhere: string
  valuePath: Array<string | number>
  disabled: boolean
}

export interface PrimitiveOption extends VariableOptionBase {
  type: 'primitive'
  variableInfo: PrimitiveInfo
}

export interface ArrayOption extends VariableOptionBase {
  type: 'array'
  variableInfo: ArrayInfo
  children: Array<DataPickerOption>
}

export interface ObjectOption extends VariableOptionBase {
  type: 'object'
  variableInfo: ObjectInfo
  children: Array<DataPickerOption>
}

export interface JSXOption extends VariableOptionBase {
  type: 'jsx'
  variableInfo: JSXInfo
}

export type DataPickerOption = PrimitiveOption | ArrayOption | ObjectOption | JSXOption

const DataPickerFilterOptions = ['all', 'preferred'] as const
export type DataPickerFilterOption = (typeof DataPickerFilterOptions)[number]
export function dataPickerFilterOptionToString(mode: DataPickerFilterOption): string {
  switch (mode) {
    case 'all':
      return 'All Data'
    case 'preferred':
      return 'Preferred'
    default:
      assertNever(mode)
  }
}

export const DataPickerPreferredAllAtom = atom<DataPickerFilterOption>('preferred')

export type DataPickerCallback = (e: JSExpressionOtherJavaScript) => void

export type ObjectPath = Array<string | number>

export function jsxElementChildToValuePath(child: JSXElementChild | null): ObjectPath | null {
  if (child == null || !isJSExpression(child)) {
    return null
  }
  return foldEither(
    () => null,
    (result) => [result.originalIdentifier.name, ...result.path],
    processJSPropertyAccessors(child),
  )
}
