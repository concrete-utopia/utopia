import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isJSXElement,
} from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  canvasRectangle,
  CanvasRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  magnitude,
  MaybeInfinityCanvasRectangle,
  nullIfInfinity,
  offsetRect,
  rectanglesEqual,
  resizeCanvasRectangle,
  roundRectangleToNearestWhole,
  size,
  Size,
  transformFrameUsingBoundingBox,
  vectorDifference,
} from '../../../../core/shared/math-utils'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { editorState, EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { cssNumber } from '../../../inspector/common/css-utils'
import { CanvasFrameAndTarget, EdgePositionBottomRight } from '../../canvas-types'
import { adjustCssLengthProperty } from '../../commands/adjust-css-length-command'
import { CanvasCommand, foldAndApplyCommandsSimple } from '../../commands/commands'
import { PushIntendedBounds } from '../../commands/push-intended-bounds-command'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { PostStrategyFixupStep } from '../canvas-strategies'
import { createResizeCommandsFromFrame } from '../strategies/absolute-resize-bounding-box-strategy'
import { isElementMarkedAsGroup } from '../strategies/group-like-helpers'
import {
  isSizedContainerWithAbsoluteChildren,
  listAllGroupLikeAffectedAncestorsForTarget,
} from '../strategies/group-like-helpers'

function setElementTopLeftWidthHeight(
  instance: ElementInstanceMetadata,
  aabb: CanvasRectangle,
): Array<CanvasCommand> {
  return [
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.y, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.x, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'width'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.width, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'height'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.height, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
  ]
}

function getGlobalFrameOrIntendedBounds(
  path: ElementPath,
  startingMetadata: ElementInstanceMetadataMap,
  strategyIntendedBounds: Array<CanvasFrameAndTarget>,
): MaybeInfinityCanvasRectangle | null {
  const startingGlobalFrame =
    MetadataUtils.findElementByElementPath(startingMetadata, path)?.globalFrame ?? null

  const intendedBounds =
    strategyIntendedBounds.find((b) => EP.pathsEqual(b.target, path))?.frame ?? null

  return intendedBounds ?? startingGlobalFrame
}

export const groupSizingFixup: PostStrategyFixupStep = {
  name: 'Fix Group Sizes',
  fixup: (patchedStore: EditorState, startingMetadata: ElementInstanceMetadataMap): EditorState => {
    const currentMetadata = patchedStore.jsxMetadata
    const metadataKeys = Object.keys(currentMetadata)
    // we go top-down for this one
    metadataKeys.sort()

    return metadataKeys
      .map((k) => currentMetadata[k])
      .filter((instance) => isElementMarkedAsGroup(currentMetadata, instance.elementPath))
      .reduce((workingEditorState, instance) => {
        const startingChildren = MetadataUtils.getChildrenUnordered(
          startingMetadata,
          instance.elementPath,
        )
        const startingChildrenAABB = boundingRectangleArray(
          mapDropNulls((e) => {
            const childStartingGlobalFrameOrIntendedBounds = getGlobalFrameOrIntendedBounds(
              e.elementPath,
              startingMetadata,
              patchedStore.canvas.controls.strategyIntendedBounds,
            )
            return childStartingGlobalFrameOrIntendedBounds != null &&
              isFiniteRectangle(childStartingGlobalFrameOrIntendedBounds)
              ? childStartingGlobalFrameOrIntendedBounds
              : null
          }, startingChildren),
        )

        if (startingChildrenAABB == null) {
          return workingEditorState
        }

        if (instance.globalFrame == null || isInfinityRectangle(instance.globalFrame)) {
          return workingEditorState
        }

        const currentGlobalFrameRounded = roundRectangleToNearestWhole(instance.globalFrame)
        const childrenAABBRounded = roundRectangleToNearestWhole(startingChildrenAABB)

        if (
          instance.globalFrame != null &&
          isFiniteRectangle(instance.globalFrame) &&
          rectanglesEqual(currentGlobalFrameRounded, childrenAABBRounded)
        ) {
          return workingEditorState
        }

        // the children's AABB does not match the element's frame
        // let's fix the children!!
        return squishDescendantsToFitFrame(
          workingEditorState,
          startingMetadata,
          patchedStore.canvas.controls.strategyIntendedBounds,
          [
            {
              path: instance.elementPath,
              childrenCurrentAABB: startingChildrenAABB,
              desiredChildrenSize: instance.globalFrame,
            },
          ],
        )
      }, patchedStore)
  },
}

function squishDescendantsToFitFrame(
  editor: EditorState,
  startingMetadata: ElementInstanceMetadataMap,
  strategyIntendedBounds: Array<CanvasFrameAndTarget>,
  potentialGroupsWithUnfittingChildren: {
    path: ElementPath
    childrenCurrentAABB: CanvasRectangle
    desiredChildrenSize: Size
  }[],
): EditorState {
  const updatedEditor: EditorState = potentialGroupsWithUnfittingChildren.reduce(
    (workingEditor, maybeGroup) => {
      if (!isElementMarkedAsGroup(startingMetadata, maybeGroup.path)) {
        return workingEditor
      }

      const groupInstance = MetadataUtils.findElementByElementPath(
        startingMetadata,
        maybeGroup.path,
      )

      if (groupInstance == null) {
        return workingEditor
      }

      if (groupInstance.globalFrame == null || isInfinityRectangle(groupInstance.globalFrame)) {
        return workingEditor
      }

      const originalGroupFrame = maybeGroup.childrenCurrentAABB
      const newGroupFrame = canvasRectangle({
        x: originalGroupFrame.x,
        y: originalGroupFrame.y,
        width: maybeGroup.desiredChildrenSize.width,
        height: maybeGroup.desiredChildrenSize.height,
      })

      const childrenToResize = MetadataUtils.getChildrenPathsUnordered(
        startingMetadata,
        maybeGroup.path,
      )

      const resizeCommands = childrenToResize.flatMap((childPath) => {
        const childMetadata = MetadataUtils.findElementByElementPath(startingMetadata, childPath)

        const originalFrame = getGlobalFrameOrIntendedBounds(
          childPath,
          startingMetadata,
          strategyIntendedBounds,
        )

        if (
          childMetadata == null ||
          originalFrame == null ||
          isInfinityRectangle(originalFrame) ||
          isLeft(childMetadata.element) ||
          !isJSXElement(childMetadata.element.value)
        ) {
          return []
        }

        const newFrame = transformFrameUsingBoundingBox(
          newGroupFrame,
          originalGroupFrame,
          originalFrame,
        )

        if (
          rectanglesEqual(
            roundRectangleToNearestWhole(originalFrame),
            roundRectangleToNearestWhole(newFrame),
          )
        ) {
          return []
        }

        // console.log('setting frame', EP.toString(childPath), {
        //   newGroupFrame,
        //   originalGroupFrame,
        //   originalFrame,
        //   newFrame,
        // })

        return [
          ...createResizeCommandsFromFrame(
            childMetadata.element.value,
            childPath,
            newFrame,
            originalFrame,
            newGroupFrame,
            childMetadata.specialSizeMeasurements.parentFlexDirection,
            EdgePositionBottomRight,
          ),
          wildcardPatch('always', {
            jsxMetadata: { [EP.toString(childPath)]: { globalFrame: { $set: newFrame } } },
          }),
        ]
      })

      return foldAndApplyCommandsSimple(workingEditor, resizeCommands)
    },
    editor,
  )

  return updatedEditor
}
