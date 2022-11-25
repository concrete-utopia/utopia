import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { EdgePosition, oppositeEdgePosition } from '../../canvas-types'
import {
  isEdgePositionACorner,
  isEdgePositionAHorizontalEdge,
  pickPointOnRect,
} from '../../canvas-utils'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../../commands/adjust-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionLifecycle,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { honoursPropsSize } from './absolute-utils'
import {
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
} from './resize-helpers'

export function flexResizeBasicStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  if (
    selectedElements.length !== 1 ||
    !MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedElements[0],
      canvasState.startingMetadata,
    ) ||
    !honoursPropsSize(canvasState, selectedElements[0])
  ) {
    return null
  }
  const metadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElements[0],
  )
  const elementDimensions = metadata != null ? getElementDimensions(metadata) : null
  const elementParentBounds = metadata?.specialSizeMeasurements.immediateParentBounds ?? null

  const hasDimensions =
    elementDimensions != null &&
    (elementDimensions.width != null || elementDimensions.height != null)
  const hasSizedParent =
    elementParentBounds != null &&
    (elementParentBounds.width !== 0 || elementParentBounds.height !== 0)

  return {
    id: 'FLEX_RESIZE_BASIC',
    name: 'Flex Resize (Basic)',
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements },
        key: 'absolute-resize-control',
        show: 'always-visible',
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
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
      (hasDimensions || !hasSizedParent)
        ? 1
        : 0,
    apply: (_strategyLifecycle: InteractionLifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        // no multiselection support yet
        const selectedElement = selectedElements[0]
        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBounds = MetadataUtils.getFrameInCanvasCoords(
            selectedElement,
            canvasState.startingMetadata,
          )

          if (originalBounds == null) {
            return emptyStrategyApplicationResult
          }

          if (metadata == null) {
            return emptyStrategyApplicationResult
          }

          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            [selectedElement],
          )

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

          const makeResizeCommand = (
            name: 'width' | 'height',
            elementDimension: number | null | undefined,
            original: number,
            resized: number,
            parent: number | undefined,
          ): AdjustCssLengthProperty[] => {
            if (elementDimension == null && (original === resized || hasSizedParent)) {
              return []
            }
            return [
              adjustCssLengthProperty(
                'always',
                selectedElement,
                stylePropPathMappingFn(name, ['style']),
                elementDimension != null ? resized - original : resized,
                parent,
                true,
              ),
            ]
          }

          const resizeCommands: Array<AdjustCssLengthProperty> = [
            ...makeResizeCommand(
              'width',
              elementDimensions?.width,
              originalBounds.width,
              resizedBounds.width,
              elementParentBounds?.width,
            ),
            ...makeResizeCommand(
              'height',
              elementDimensions?.height,
              originalBounds.height,
              resizedBounds.height,
              elementParentBounds?.height,
            ),
          ]

          return strategyApplicationResult([
            ...resizeCommands,
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            setElementsToRerenderCommand(selectedElements),
          ])
        } else {
          return strategyApplicationResult([
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
          ])
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
    const startingCornerPosition = {
      x: 1 - edgePosition.x,
      y: 1 - edgePosition.y,
    } as EdgePosition

    let oppositeCorner = pickPointOnRect(boundingBox, startingCornerPosition)
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

type ElementDimensions = {
  width: number | null
  height: number | null
} | null

const getElementDimensions = (metadata: ElementInstanceMetadata): ElementDimensions => {
  const getOffsetPropValue = (
    name: 'width' | 'height',
    attrs: PropsOrJSXAttributes,
  ): number | null => {
    return foldEither(
      (_) => null,
      (v) => v?.value ?? null,
      getLayoutProperty(name, attrs, ['style']),
    )
  }

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
