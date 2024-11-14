import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { isJSXElement } from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  isInfinityRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { styleStringInArray } from '../../../../utils/common-constants'
import { gridItemIdentifier, trueUpGroupElementChanged } from '../../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { isFillOrStretchModeApplied } from '../../../inspector/inspector-common'
import type { EdgePosition } from '../../canvas-types'
import { oppositeEdgePosition } from '../../canvas-types'
import {
  isEdgePositionACorner,
  isEdgePositionAHorizontalEdge,
  isEdgePositionAVerticalEdge,
  pickPointOnRect,
} from '../../canvas-utils'
import type { LengthPropertyToAdjust } from '../../commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import { queueTrueUpElement } from '../../commands/queue-true-up-command'
import { setCursorCommand } from '../../commands/set-cursor-command'

import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import type {
  CanvasStrategy,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { honoursPropsSize } from './absolute-utils'
import { treatElementAsGroupLike } from './group-helpers'
import {
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
} from './resize-helpers'

export const BASIC_RESIZE_STRATEGY_ID = 'BASIC_RESIZE'

export function basicResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  if (selectedElements.length !== 1 || !honoursPropsSize(canvasState, selectedElements[0])) {
    return null
  }

  const selectedElement = selectedElements[0]

  const metadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  const elementDimensionsProps = metadata != null ? getElementDimensions(metadata) : null
  const elementParentBounds = metadata?.specialSizeMeasurements.immediateParentBounds ?? null

  const isGridCell = MetadataUtils.isGridItem(canvasState.startingMetadata, selectedElement)
  if (isGridCell && isFillOrStretchModeApplied(canvasState.startingMetadata, selectedElement)) {
    return null
  }

  return {
    id: BASIC_RESIZE_STRATEGY_ID,
    name: 'Resize (Basic)',
    descriptiveLabel: 'Resizing Elements',
    icon: {
      category: 'modalities',
      type: 'resize',
    },
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements, pathsWereReplaced: false },
        key: 'absolute-resize-control',
        show: 'always-visible',
        priority: 'top',
      }),
      controlWithProps({
        control: ZeroSizeResizeControlWrapper,
        props: { targets: selectedElements },
        key: 'zero-size-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: selectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
      ...(isGridCell ? [controlsForGridPlaceholders(gridItemIdentifier(selectedElement))] : []),
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE'
        ? 1
        : 0,
    apply: (_strategyLifecycle: InteractionLifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        // no multiselection support yet
        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBounds = MetadataUtils.getFrameInCanvasCoords(
            selectedElement,
            canvasState.startingMetadata,
          )

          if (originalBounds == null || isInfinityRectangle(originalBounds)) {
            return emptyStrategyApplicationResult
          }

          if (metadata == null) {
            return emptyStrategyApplicationResult
          }

          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            [selectedElement],
          )

          const elementIsGroup = treatElementAsGroupLike(
            canvasState.startingMetadata,
            selectedElement,
          )
          const groupChildren = elementIsGroup
            ? MetadataUtils.getChildrenUnordered(canvasState.startingMetadata, selectedElement)
            : []

          const resizedBounds = resizeBoundingBox(
            originalBounds,
            drag,
            edgePosition,
            getLockedAspectRatio(
              interactionSession,
              interactionSession.interactionData.modifiers,
              originalBounds,
              anySelectedElementAspectRatioLocked,
            ),
            'non-center-based',
          )

          let resizeProperties: Array<LengthPropertyToAdjust> = []
          function addResizeProperty(
            name: 'width' | 'height',
            elementDimension: number | null | undefined,
            original: number,
            resized: number,
            parent: number | undefined,
          ): void {
            if (original === resized) {
              return
            }
            resizeProperties.push(
              lengthPropertyToAdjust(
                stylePropPathMappingFn(name, styleStringInArray),
                elementDimension != null ? resized - original : resized,
                parent,
                'create-if-not-existing',
              ),
            )
          }

          addResizeProperty(
            'width',
            elementDimensionsProps?.width,
            originalBounds.width,
            resizedBounds.width,
            elementParentBounds?.width,
          )
          addResizeProperty(
            'height',
            elementDimensionsProps?.height,
            originalBounds.height,
            resizedBounds.height,
            elementParentBounds?.height,
          )

          const gridsToRerender = selectedElements
            .filter((element) => MetadataUtils.isGridItem(canvasState.startingMetadata, element))
            .map(EP.parentPath)

          const elementsToRerender = [...selectedElements, ...gridsToRerender]

          let commands: CanvasCommand[] = [
            adjustCssLengthProperties('always', selectedElement, null, resizeProperties),
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            pushIntendedBoundsAndUpdateGroups(
              [{ target: selectedElement, frame: resizedBounds }],
              'starting-metadata',
            ),
            ...groupChildren.map((c) =>
              queueTrueUpElement([trueUpGroupElementChanged(c.elementPath)]),
            ),
          ]

          if (isEdgePositionAHorizontalEdge(edgePosition)) {
            commands.push(
              deleteProperties('always', selectedElement, [PP.create('style', 'justifySelf')]),
            )
          }
          if (isEdgePositionAVerticalEdge(edgePosition)) {
            commands.push(
              deleteProperties('always', selectedElement, [PP.create('style', 'alignSelf')]),
            )
          }

          return strategyApplicationResult(commands, elementsToRerender)
        } else {
          return strategyApplicationResult(
            [
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            ],
            [],
          )
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

export function resizeWidthHeight(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
): CanvasRectangle {
  if (isEdgePositionACorner(edgePosition)) {
    const oppositeCornerPosition = {
      x: 1 - edgePosition.x,
      y: 1 - edgePosition.y,
    } as EdgePosition

    let oppositeCorner = pickPointOnRect(boundingBox, oppositeCornerPosition)
    const draggedCorner = pickPointOnRect(boundingBox, edgePosition)
    const newCorner = offsetPoint(draggedCorner, drag)

    const newWidth = Math.abs(oppositeCorner.x - newCorner.x)
    const newHeight = Math.abs(oppositeCorner.y - newCorner.y)

    return canvasRectangle({
      x: boundingBox.x,
      y: boundingBox.y,
      width: newWidth,
      height: newHeight,
    })
  } else {
    const isEdgeHorizontalSide = isEdgePositionAHorizontalEdge(edgePosition)

    const oppositeSideCenterPosition = oppositeEdgePosition(edgePosition)

    const oppositeSideCenter = pickPointOnRect(boundingBox, oppositeSideCenterPosition)
    const draggedSideCenter = pickPointOnRect(boundingBox, edgePosition)

    if (isEdgeHorizontalSide) {
      const newHeight = Math.abs(oppositeSideCenter.y - (draggedSideCenter.y + drag.y))
      return canvasRectangle({
        x: boundingBox.x,
        y: boundingBox.y,
        width: boundingBox.width,
        height: newHeight,
      })
    } else {
      const newWidth = Math.abs(oppositeSideCenter.x - (draggedSideCenter.x + drag.x))
      return canvasRectangle({
        x: boundingBox.x,
        y: boundingBox.y,
        width: newWidth,
        height: boundingBox.height,
      })
    }
  }
}

const getOffsetPropValue = (
  name: 'width' | 'height',
  attrs: PropsOrJSXAttributes,
): number | null => {
  return foldEither(
    (_) => null,
    (v) => v?.value ?? null,
    getLayoutProperty(name, attrs, styleStringInArray),
  )
}

type ElementDimensions = {
  width: number | null
  height: number | null
} | null

const getElementDimensions = (metadata: ElementInstanceMetadata): ElementDimensions => {
  if (isLeft(metadata.element)) {
    return null
  }
  const { value } = metadata.element
  if (!isJSXElement(value)) {
    return null
  }

  const attrs = right(value.props)

  return {
    width: getOffsetPropValue('width', attrs),
    height: getOffsetPropValue('height', attrs),
  }
}
