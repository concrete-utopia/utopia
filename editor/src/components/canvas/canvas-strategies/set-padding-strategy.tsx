import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import { CSSPadding } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CSSCursor, EdgePiece } from '../canvas-types'
import { adjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { isZeroSizedElement } from '../controls/outline-utils'
import { PaddingResizeControl } from '../controls/select-mode/padding-resize-control'
import { paddingForEdge, simplePaddingFromMetadata } from '../padding-utils'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getDragTargets, getMultiselectBounds } from './shared-move-strategies-helpers'

export const setPaddingStrategy: CanvasStrategy = {
  id: 'SET_PADDING_STRATEGY',
  name: () => 'Set Padding',
  controlsToRender: [
    {
      control: PaddingResizeControl,
      key: 'padding-resize-control',
      show: 'visible-except-when-other-strategy-is-active',
    },
  ],
  isApplicable: (canvasState, interactionState, metadata, allElementProps) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return false
    }

    return supportsPaddingControls(metadata, selectedElements[0])
  },
  fitness: (canvasState, interactionState, sessionState) => {
    return setPaddingStrategy.isApplicable(
      canvasState,
      interactionState,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'PADDING_RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type !== 'DRAG' ||
      interactionState.activeControl.type !== 'PADDING_RESIZE_HANDLE'
    ) {
      return emptyStrategyApplicationResult
    }

    const drag = interactionState.interactionData.drag
    if (drag == null) {
      return emptyStrategyApplicationResult
    }

    const edgePiece = interactionState.activeControl.edgePiece
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

    if (interactionState.interactionData.drag == null) {
      return emptyStrategyApplicationResult
    }

    const filteredSelectedElements = getDragTargets(selectedElements)
    const originalBoundingBox = getMultiselectBounds(
      canvasState.startingMetadata,
      filteredSelectedElements,
    )

    if (originalBoundingBox == null || filteredSelectedElements.length !== 1) {
      return strategyApplicationResult([
        setCursorCommand('mid-interaction', pickCursorFromEdge(edgePiece)),
        updateHighlightedViews('mid-interaction', []),
      ])
    }

    const selectedElement = filteredSelectedElements[0]

    const padding = simplePaddingFromMetadata(canvasState.startingMetadata, selectedElement)

    const startingPadding = paddingForEdge(edgePiece, padding)
    const delta = deltaFromEdge(drag, edgePiece)

    const newPadding = Math.max(-startingPadding, delta)

    const commandsForSelectedElements = [
      adjustCssLengthProperty(
        'always',
        selectedElement,
        stylePropPathMappingFn(paddingCursorFromEdge(edgePiece), ['style']),
        newPadding,
        0,
        true,
      ),
    ]

    return strategyApplicationResult([
      ...commandsForSelectedElements,
      updateHighlightedViews('mid-interaction', []),
      setCursorCommand('mid-interaction', pickCursorFromEdge(edgePiece)),
      setElementsToRerenderCommand(selectedElements),
    ])
  },
}

function pickCursorFromEdge(edgePiece: EdgePiece): CSSCursor {
  switch (edgePiece) {
    case 'top':
    case 'bottom':
      return CSSCursor.RowResize
    case 'left':
    case 'right':
      return CSSCursor.ColResize
    default:
      assertNever(edgePiece)
  }
}

function paddingCursorFromEdge(edgePiece: EdgePiece): keyof CSSPadding {
  switch (edgePiece) {
    case 'top':
      return 'paddingTop'
    case 'bottom':
      return 'paddingBottom'
    case 'left':
      return 'paddingLeft'
    case 'right':
      return 'paddingRight'
    default:
      assertNever(edgePiece)
  }
}

function deltaFromEdge(delta: CanvasVector, edgePiece: EdgePiece): number {
  switch (edgePiece) {
    case 'top':
      return delta.y
    case 'bottom':
      return -delta.y
    case 'left':
      return delta.x
    case 'right':
      return -delta.x
    default:
      assertNever(edgePiece)
  }
}

function supportsPaddingControls(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (element == null) {
    return false
  }

  if (element.globalFrame == null || isZeroSizedElement(element.globalFrame)) {
    return false
  }

  const childrenNotPositionAbsoluteOrSticky = MetadataUtils.getChildren(metadata, path).filter(
    (child) =>
      child.specialSizeMeasurements.position !== 'absolute' &&
      child.specialSizeMeasurements.position !== 'sticky',
  )
  if (childrenNotPositionAbsoluteOrSticky.length < 1) {
    return false
  }

  return true
}
