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

export interface PrimitiveOption {
  type: 'primitive'
  variableInfo: PrimitiveInfo
  definedElsewhere: string
  depth: number
  valuePath: Array<string | number>
  disabled: boolean
}

export interface ArrayOption {
  type: 'array'
  variableInfo: ArrayInfo
  depth: number
  definedElsewhere: string
  children: Array<DataPickerOption>
  valuePath: Array<string | number>
  disabled: boolean
}

export interface ObjectOption {
  type: 'object'
  variableInfo: ObjectInfo
  depth: number
  definedElsewhere: string
  children: Array<DataPickerOption>
  valuePath: Array<string | number>
  disabled: boolean
}

export interface JSXOption {
  type: 'jsx'
  variableInfo: JSXInfo
  definedElsewhere: string
  depth: number
  valuePath: Array<string | number>
  disabled: boolean
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

export function jsxElementChildToValuePath(child: JSXElementChild): ObjectPath | null {
  if (!isJSExpression(child)) {
    return null
  }
  return foldEither(
    () => null,
    (result) => [result.originalIdentifier.name, ...result.path],
    processJSPropertyAccessors(child),
  )
}
