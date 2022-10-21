import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { CanvasVector } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { CSSPadding, ParsedCSSProperties } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { CSSCursor, EdgePiece } from '../../canvas-types'
import { adjustCssLengthProperty } from '../../commands/adjust-css-length-command'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { isZeroSizedElement } from '../../controls/outline-utils'
import { PaddingResizeControl } from '../../controls/select-mode/padding-resize-control'
import {
  offsetPaddingByEdge,
  paddingForEdge,
  paddingToPaddingString,
  simplePaddingFromMetadata,
} from '../../padding-utils'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { getDragTargets, getMultiselectBounds } from './shared-move-strategies-helpers'

const StylePaddingProp = stylePropPathMappingFn('padding', ['style'])
const IndividualPaddingProps: Array<keyof ParsedCSSProperties> = [
  'paddingTop',
  'paddingBottom',
  'paddingLeft',
  'paddingRight',
]

export const setPaddingStrategy: CanvasStrategyFactory = (
  canvasState,
  interactionSession,
  customStrategyState,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length === 0) {
    return null
  }

  if (!supportsPaddingControls(canvasState.startingMetadata, selectedElements[0])) {
    return null
  }

  return {
    id: 'SET_PADDING_STRATEGY',
    name: 'Set Padding',
    controlsToRender: [
      controlWithProps({
        control: PaddingResizeControl,
        props: {},
        key: 'padding-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: 1,
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const drag = interactionSession.interactionData.drag
      if (drag == null) {
        return emptyStrategyApplicationResult
      }

      const edgePiece = interactionSession.activeControl.edgePiece

      if (interactionSession.interactionData.drag == null) {
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
      const currentPadding = paddingForEdge(edgePiece, padding)
      const delta = Math.max(-currentPadding, deltaFromEdge(drag, edgePiece))
      const newPadding = offsetPaddingByEdge(edgePiece, delta, padding)
      const paddingString = paddingToPaddingString(newPadding)

      return strategyApplicationResult([
        ...IndividualPaddingProps.map((p) =>
          deleteProperties('always', selectedElement, [stylePropPathMappingFn(p, ['style'])]),
        ),
        setProperty('always', selectedElement, StylePaddingProp, paddingString),
        updateHighlightedViews('mid-interaction', []),
        setCursorCommand('mid-interaction', pickCursorFromEdge(edgePiece)),
        setElementsToRerenderCommand(selectedElements),
      ])
    },
  }
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
