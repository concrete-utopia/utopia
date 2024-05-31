import { atom } from 'jotai'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  isJSExpression,
  type JSExpressionOtherJavaScript,
  type JSXElementChild,
} from '../../../../core/shared/element-template'
import * as EP from '../../../../core/shared/element-path'
import { assertNever } from '../../../../core/shared/utils'
import type { ArrayInfo, JSXInfo, ObjectInfo, PrimitiveInfo } from './variables-in-scope-utils'
import { processJSPropertyAccessors } from '../../../../core/data-tracing/data-tracing'
import { foldEither } from '../../../../core/shared/either'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

interface VariableOptionBase {
  depth: number
  definedElsewhere: string
  valuePath: Array<string | number>
  disabled: boolean
  insertionCeiling: ElementPath | null
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

export const DataPickerPreferredAllAtom = atom<DataPickerFilterOption>('all')

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

export function getEnclosingScopes(
  leafScope: ElementPath,
  metadata: ElementInstanceMetadataMap,
): ElementPath[] {
  let result: ElementPath[] = [leafScope]
  let current = leafScope
  while (!EP.isEmptyPath(current)) {
    if (MetadataUtils.isJSXMapExpression(current, metadata)) {
      result.unshift(current)
    } else if (EP.isRootElementOfInstance(current)) {
      result.unshift(EP.parentPath(current))
    }
    current = EP.parentPath(current)
  }

  result.unshift(current)
  return result
}
