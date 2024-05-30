import { atom } from 'jotai'
import type {
  JSExpressionOtherJavaScript,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { assertNever } from '../../../../core/shared/utils'
import type { ArrayInfo, JSXInfo, ObjectInfo, PrimitiveInfo } from './variables-in-scope-utils'

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
  return jsxElementChildToValuePathInner(child, false)
}

function jsxElementChildToValuePathInner(
  child: JSXElementChild,
  insideExpression: boolean,
): ObjectPath | null {
  switch (child.type) {
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_ELEMENT':
    case 'JSX_FRAGMENT':
    case 'JSX_MAP_EXPRESSION':
    case 'JSX_TEXT_BLOCK':
      return null
    case 'ATTRIBUTE_VALUE':
      return insideExpression ? [child.value] : null
    case 'JS_IDENTIFIER':
      return [child.name]
    case 'JS_ELEMENT_ACCESS':
      return [
        ...(jsxElementChildToValuePathInner(child.onValue, true) ?? []),
        ...(jsxElementChildToValuePathInner(child.element, true) ?? []),
      ]
    case 'JS_PROPERTY_ACCESS':
      return [...(jsxElementChildToValuePathInner(child.onValue, true) ?? []), child.property]
    default:
      assertNever(child)
  }
}
