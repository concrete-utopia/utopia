import { atom } from 'jotai'
import { processJSPropertyAccessors } from '../../../../core/data-tracing/data-tracing'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  isJSExpression,
  type JSExpressionOtherJavaScript,
  type JSXElementChild,
} from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import type { AllElementProps } from '../../../editor/store/editor-state'
import type { ArrayInfo, JSXInfo, ObjectInfo, PrimitiveInfo } from './variables-in-scope-utils'

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
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  buckets: Array<ElementPath>,
  leafScope: ElementPath,
): Array<{
  insertionCeiling: ElementPath
  label: string
  hasContent: boolean
}> {
  let result: Array<{
    insertionCeiling: ElementPath
    label: string
    hasContent: boolean
  }> = []
  const pathsToCheck = [
    ...EP.allPathsInsideComponent(leafScope),
    EP.emptyElementPath, // empty element path is the file root, TODO make it a separate constant
  ]
  for (const current of pathsToCheck) {
    const parentOfCurrent = EP.parentPath(current)

    // we add maps and components even if they don't have content in scope, for the sake of breadcrumb readability
    if (
      MetadataUtils.isJSXMapExpression(parentOfCurrent, metadata) ||
      EP.isRootElementOfInstance(current)
    ) {
      result.unshift({
        insertionCeiling: current,
        label: MetadataUtils.getElementLabel(
          allElementProps,
          parentOfCurrent,
          elementPathTree,
          metadata,
        ),
        hasContent: buckets.includes(current),
      })
      continue
    }

    // the file root
    if (EP.pathsEqual(current, EP.emptyElementPath)) {
      result.unshift({
        insertionCeiling: current,
        label: 'File',
        hasContent: buckets.includes(current),
      })
      continue
    }

    // we also add anything that has content in scope even if it's not a component or map
    if (buckets.includes(current)) {
      result.unshift({
        insertionCeiling: current,
        label: MetadataUtils.getElementLabel(
          allElementProps,
          parentOfCurrent,
          elementPathTree,
          metadata,
        ),
        hasContent: true,
      })
      continue
    }
  }

  return result
}
