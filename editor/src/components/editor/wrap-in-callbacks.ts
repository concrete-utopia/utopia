import React from 'react'
import {
  jsxElement,
  type ElementInstanceMetadataMap,
  jsxAttributesFromMap,
  jsExpressionValue,
  emptyComments,
} from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { CanvasCommand } from '../canvas/commands/commands'
import { assertNever } from '../../core/shared/utils'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { CanvasRectangle } from '../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangle,
  nullIfInfinity,
  zeroCanvasRect,
} from '../../core/shared/math-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { Either } from '../../core/shared/either'
import { isLeft, left, right } from '../../core/shared/either'
import { isNonEmptyArray } from '../../core/shared/array-utils'
import { autoLayoutParentAbsoluteOrStatic } from '../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-parent-lookup'
import type { AllElementProps } from './store/editor-state'
import * as EP from '../../core/shared/element-path'
import { addElement } from '../canvas/commands/add-element-command'
import type { InsertionPath } from './store/insertion-path'
import { childInsertionPath, getInsertionPath } from './store/insertion-path'
import type { ProjectContentTreeRoot } from '../assets'
import type { CSSProperties } from 'react'
import type { ReparentStrategy } from '../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-helpers'
import { setElementTopLeft, sizeToVisualDimensions } from '../inspector/inspector-common'
import { reparentElement } from '../canvas/commands/reparent-element-command'
import { useRefEditorState } from './store/store-hook'
import { useDispatch } from './store/dispatch-context'
import { applyCommandsAction } from './actions/action-creators'
import { generateConsistentUID } from '../../core/shared/uid-utils'
import { getAllUniqueUids, getAllUniqueUidsFromLookup } from '../../core/model/get-uid-mappings'
import { updateSelectedViews } from '../canvas/commands/update-selected-views-command'
import type { IndexPosition } from '../../utils/utils'
import { absolute } from '../../utils/utils'
import { setProperty } from '../canvas/commands/set-property-command'
import * as PP from '../../core/shared/property-path'
import type { InspectorStrategy } from '../inspector/inspector-strategies/inspector-strategy'
import type { PropertyControlsInfo } from '../custom-code/code-file'

type WrapInDivError =
  | 'No elements selected'
  | 'Cannot determine the bounding box of selected elements'
  | 'Cannot insert into parent of selected elements'

export const wrapInDivStrategy = (
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  elementPathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
  propertyControlsInfo: PropertyControlsInfo,
): InspectorStrategy => ({
  name: 'Wrap in div',
  strategy: () => {
    const result = wrapInDivCommands(
      metadata,
      elementPathTrees,
      allElementProps,
      projectContents,
      selectedViews,
      propertyControlsInfo,
    )
    if (isLeft(result)) {
      return null
    }
    return result.value
  },
})

function wrapInDivCommands(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
  selectedViews: ElementPath[],
  propertyControlsInfo: PropertyControlsInfo,
): Either<WrapInDivError, CanvasCommand[]> {
  if (!isNonEmptyArray(selectedViews)) {
    return left('No elements selected')
  }

  const childrenBoundingFrame = boundingRectangleArray(
    selectedViews.map((path) =>
      nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(path, metadata)),
    ),
  )

  if (childrenBoundingFrame == null) {
    return left('Cannot determine the bounding box of selected elements')
  }

  const allIds = new Set(
    getAllUniqueUidsFromLookup(getAllUniqueUids(projectContents).filePathToUids),
  )

  const wrapperUid = generateConsistentUID('wrapper', allIds)
  allIds.add(wrapperUid)

  const fragmentWrapperUid = generateConsistentUID('fragment-wrapper', allIds)

  const parentPath = EP.getCommonParentOfNonemptyPathArray(selectedViews)
  const insertionPath = getInsertionPath(
    parentPath,
    projectContents,
    metadata,
    elementPathTrees,
    fragmentWrapperUid,
    1,
    propertyControlsInfo,
  )
  if (insertionPath == null) {
    return left('Cannot insert into parent of selected elements')
  }

  const indexPosition =
    MetadataUtils.getIndexInParent(metadata, elementPathTrees, selectedViews[0]) + 1

  const parentCommands = getWrapperDivCommands(
    metadata,
    elementPathTrees,
    allElementProps,
    insertionPath,
    childrenBoundingFrame,
    wrapperUid,
    absolute(indexPosition),
  )

  const newParentPath = EP.appendToPath(insertionPath.intendedParentPath, wrapperUid)

  return right([
    ...parentCommands,
    ...selectedViews.flatMap((path) =>
      getCommandsForChild(metadata, elementPathTrees, childrenBoundingFrame, newParentPath, path),
    ),
    updateSelectedViews('always', [newParentPath]),
  ])
}

function getWrapperDivCommands(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  parentPath: InsertionPath,
  childrenBoundingFrame: CanvasRectangle,
  wrapperUid: string,
  indexPosition: IndexPosition,
): CanvasCommand[] {
  const parentType = autoLayoutParentAbsoluteOrStatic(
    metadata,
    allElementProps,
    elementPathTrees,
    parentPath.intendedParentPath,
    'prefer-absolute',
  )

  const parentFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(parentPath.intendedParentPath, metadata),
  )
  if (parentFrame == null) {
    return []
  }

  const desiredFrame: CanvasRectangle = canvasRectangle({
    width: childrenBoundingFrame.width,
    height: childrenBoundingFrame.height,
    x: childrenBoundingFrame.x - parentFrame.x,
    y: childrenBoundingFrame.y - parentFrame.y,
  })

  const props = jsxAttributesFromMap({
    style: jsExpressionValue(getWrapperStyle(parentType, desiredFrame), emptyComments),
  })
  return [
    addElement('always', parentPath, jsxElement('div', wrapperUid, props, []), {
      indexPosition: indexPosition,
    }),
  ]
}

function getCommandsForChild(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  childrenBoundingFrame: CanvasRectangle,
  newParentPath: ElementPath,
  path: ElementPath,
): CanvasCommand[] {
  const instance = MetadataUtils.findElementByElementPath(metadata, path)
  if (instance == null) {
    return []
  }

  const frame = nullIfInfinity(instance.globalFrame) ?? zeroCanvasRect

  return [
    ...sizeToVisualDimensions(metadata, elementPathTrees, path),
    ...setElementTopLeft(instance, {
      top: frame.y - childrenBoundingFrame.y,
      left: frame.x - childrenBoundingFrame.x,
    }),
    setProperty('always', path, PP.create('style', 'position'), 'absolute'),
    reparentElement('always', path, childInsertionPath(newParentPath)),
  ]
}

function getWrapperStyle(
  parentType: ReparentStrategy,
  desiredFrame: CanvasRectangle,
): CSSProperties {
  const style: CSSProperties = {
    contain: 'layout',
    width: desiredFrame.width,
    height: desiredFrame.height,
  }
  switch (parentType) {
    case 'REPARENT_AS_STATIC':
      return style
    case 'REPARENT_AS_ABSOLUTE':
      return {
        ...style,
        position: 'absolute',
        top: desiredFrame.y,
        left: desiredFrame.x,
      }
    default:
      assertNever(parentType)
  }
}

export function useWrapInDiv(): () => void {
  const dispatch = useDispatch()
  const editorRef = useRefEditorState((store) => store.editor)

  const wrapInDiv = React.useCallback(() => {
    const result = wrapInDivCommands(
      editorRef.current.jsxMetadata,
      editorRef.current.elementPathTree,
      editorRef.current.allElementProps,
      editorRef.current.projectContents,
      editorRef.current.selectedViews,
      editorRef.current.propertyControlsInfo,
    )

    if (isLeft(result)) {
      return
    }

    dispatch([applyCommandsAction(result.value)])
  }, [dispatch, editorRef])

  return wrapInDiv
}
