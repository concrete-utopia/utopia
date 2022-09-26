import { CanvasVector } from '../../../core/shared/math-utils'
import { assertNever } from '../../../core/shared/utils'
import { CSSPadding } from '../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CSSCursor, EdgePiece } from '../canvas-types'
import { adjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { PaddingResizeControl } from '../controls/select-mode/padding-resize-control'
import { paddingForEdge, simplePaddingFromMetadata } from '../padding-utils'
import { supportsAbsoluteResize } from './absolute-resize-helpers'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getDragTargets, getMultiselectBounds } from './shared-absolute-move-strategy-helpers'

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
    if (selectedElements.length > 0) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      return filteredSelectedElements.every((element) => {
        return supportsAbsoluteResize(metadata, element, canvasState) // TODO - add real predicate here
      })
    }
    return false
  },
  fitness: (canvasState, interactionState, sessionState) => {
    return setPaddingStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
      sessionState.startingAllElementProps,
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
      sessionState.startingMetadata,
      filteredSelectedElements,
    )

    if (originalBoundingBox == null || filteredSelectedElements.length !== 1) {
      return strategyApplicationResult([
        setCursorCommand('mid-interaction', pickCursorFromEdge(edgePiece)),
        updateHighlightedViews('mid-interaction', []),
      ])
    }

    const selectedElement = filteredSelectedElements[0]

    const padding = simplePaddingFromMetadata(sessionState.startingMetadata, selectedElement)

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
      return CSSCursor.ResizeNS
    case 'left':
    case 'right':
      return CSSCursor.ResizeEW
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
