import { atom } from 'jotai'
import { processJSPropertyAccessors } from '../../../../core/data-tracing/data-tracing'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { findContainingComponentForPathInProjectContents } from '../../../../core/model/element-template-utils'
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
import type { ProjectContentTreeRoot } from '../../../assets'
import { insertionCeilingToString, type FileRootPath } from '../../../canvas/ui-jsx-canvas'
import type { AllElementProps } from '../../../editor/store/editor-state'
import type { ArrayInfo, JSXInfo, ObjectInfo, PrimitiveInfo } from './variables-in-scope-utils'

interface VariableOptionBase {
  depth: number
  definedElsewhere: string
  valuePath: Array<string | number>
  insertionCeiling: ElementPath | FileRootPath
  isChildOfArray: boolean
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
  projectContents: ProjectContentTreeRoot,
  buckets: Array<string>,
  lowestInsertionCeiling: ElementPath,
): Array<{
  insertionCeiling: ElementPath | FileRootPath
  label: string
  hasContent: boolean
}> {
  let result: Array<{
    insertionCeiling: ElementPath | FileRootPath
    label: string
    hasContent: boolean
  }> = []
  const pathsToCheck = EP.allPathsInsideComponent(lowestInsertionCeiling)
  for (const current of pathsToCheck) {
    const parentOfCurrent = EP.parentPath(current)

    // we add maps and components even if they don't have content in scope, for the sake of breadcrumb readability
    if (
      MetadataUtils.isJSXMapExpression(parentOfCurrent, metadata) ||
      EP.isRootElementOfInstance(current)
    ) {
      result.unshift({
        insertionCeiling: current,
        label: outletNameHack(metadata, allElementProps, elementPathTree, projectContents, current),
        hasContent: buckets.includes(insertionCeilingToString(current)),
      })
      continue
    }

    // we also add anything that has content in scope even if it's not a component or map
    if (buckets.includes(insertionCeilingToString(current))) {
      result.unshift({
        insertionCeiling: current,
        label: outletNameHack(metadata, allElementProps, elementPathTree, projectContents, current),
        hasContent: true,
      })
      continue
    }
  }

  // Add file root
  result.unshift({
    insertionCeiling: { type: 'file-root' },
    label: 'File',
    hasContent: buckets.includes(insertionCeilingToString({ type: 'file-root' })),
  })

  return result
}

function outletNameHack(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  target: ElementPath,
): string {
  const namePossiblyOutlet = MetadataUtils.getElementLabel(
    allElementProps,
    EP.parentPath(target),
    elementPathTree,
    metadata,
  )
  if (namePossiblyOutlet !== 'Outlet') {
    return namePossiblyOutlet
  }
  // if getElementLabel returned Outlet, we try to find the actual component name by hand – this is a hack and should be removed once the Navigator is capable of showing the correct name
  const component = findContainingComponentForPathInProjectContents(target, projectContents)
  return component?.name ?? namePossiblyOutlet
}
