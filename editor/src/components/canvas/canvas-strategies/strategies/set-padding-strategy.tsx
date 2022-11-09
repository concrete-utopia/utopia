import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { ParsedCSSProperties } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { CSSCursor, EdgePiece } from '../../canvas-types'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { isZeroSizedElement } from '../../controls/outline-utils'
import { PaddingResizeControl } from '../../controls/select-mode/padding-resize-control'
import {
  FloatingCSSNumberIndicator,
  FloatingCSSNumberIndicatorProps,
} from '../../controls/select-mode/floating-number-indicator'
import {
  CSSPaddingMeasurements,
  deltaFromEdge,
  offsetPaddingByEdge,
  paddingForEdge,
  paddingKeyForEdge,
  paddingMeasurementForEdge,
  paddingToPaddingString,
  simplePaddingFromMetadata,
} from '../../padding-utils'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { getDragTargets, getMultiselectBounds } from './shared-move-strategies-helpers'
import { canvasPoint, CanvasPoint, CanvasVector } from '../../../../core/shared/math-utils'
import {
  offsetMeasurementByDelta,
  precisionFromModifiers,
} from '../../controls/select-mode/controls-common'

const StylePaddingProp = stylePropPathMappingFn('padding', ['style'])
const IndividualPaddingProps: Array<keyof ParsedCSSProperties> = [
  'paddingTop',
  'paddingBottom',
  'paddingLeft',
  'paddingRight',
]

export const SetPaddingStrategyName = 'Set Padding'

export const setPaddingStrategy: CanvasStrategyFactory = (canvasState, interactionSession) => {
  if (
    interactionSession != null &&
    !(
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'PADDING_RESIZE_HANDLE'
    )
  ) {
    // We don't want to include this in the strategy picker if any other interaction is active
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  if (!supportsPaddingControls(canvasState.startingMetadata, selectedElements[0])) {
    return null
  }

  const activeEdge = optionalMap(edgeFromInteractionSession, interactionSession)
  const measurements = updatedPaddingMeasurements(
    canvasState,
    interactionSession,
    selectedElements[0],
  )

  return {
    id: 'SET_PADDING_STRATEGY',
    name: SetPaddingStrategyName,
    controlsToRender: [
      controlWithProps({
        control: PaddingResizeControl,
        props: {
          targets: selectedElements,
          shouldShowIndicator: activeEdge,
          padding: measurements,
        },
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
          setCursorCommand(pickCursorFromEdge(edgePiece)),
          updateHighlightedViews('mid-interaction', []),
        ])
      }

      const selectedElement = filteredSelectedElements[0]

      const padding = simplePaddingFromMetadata(canvasState.startingMetadata, selectedElement)
      const currentPadding = paddingForEdge(edgePiece, padding)
      const delta = Math.max(-currentPadding, deltaFromEdge(drag, edgePiece))
      const newPadding = offsetPaddingByEdge(
        edgePiece,
        delta,
        padding,
        precisionFromModifiers(interactionSession.interactionData.modifiers),
      )
      const paddingString = paddingToPaddingString(newPadding)

      return strategyApplicationResult([
        ...IndividualPaddingProps.map((p) =>
          deleteProperties('always', selectedElement, [stylePropPathMappingFn(p, ['style'])]),
        ),
        setProperty('always', selectedElement, StylePaddingProp, paddingString),
        updateHighlightedViews('mid-interaction', []),
        setCursorCommand(pickCursorFromEdge(edgePiece)),
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

function supportsPaddingControls(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (element == null) {
    return false
  }

  if (element.globalFrame == null || isZeroSizedElement(element.globalFrame)) {
    return false
  }

  const children = MetadataUtils.getChildren(metadata, path)
  if (children.length === 0) {
    return true
  }

  const childrenNotPositionedAbsoluteOrSticky = MetadataUtils.getChildren(metadata, path).filter(
    (child) =>
      child.specialSizeMeasurements.position !== 'absolute' &&
      child.specialSizeMeasurements.position !== 'sticky',
  )

  if (childrenNotPositionedAbsoluteOrSticky.length < 1) {
    return false
  }

  const { top, right, bottom, left } = element.specialSizeMeasurements.padding
  const elementHasNonzeroPadding = [top, right, bottom, left].some((s) => s != null && s > 0)
  if (!elementHasNonzeroPadding) {
    return false
  }

  return true
}

function edgeFromInteractionSession(interactionSession: InteractionSession): EdgePiece | null {
  if (interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE') {
    return null
  }
  return interactionSession.activeControl.edgePiece
}

function updatedPaddingMeasurements(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  selectedElement: ElementPath,
): CSSPaddingMeasurements | null {
  const filteredSelectedElements = getDragTargets([selectedElement])

  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE' ||
    interactionSession.interactionData.drag == null
  ) {
    return null
  }

  const { drag } = interactionSession.interactionData

  const edgePiece = interactionSession.activeControl.edgePiece

  const padding = simplePaddingFromMetadata(
    canvasState.startingMetadata,
    filteredSelectedElements[0],
  )
  const currentPadding = paddingMeasurementForEdge(edgePiece, padding)

  const updatedPaddingMeasurement = offsetMeasurementByDelta(
    currentPadding,
    deltaFromEdge(drag, edgePiece),
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  return {
    ...padding,
    [paddingKeyForEdge(edgePiece)]: updatedPaddingMeasurement,
  }
}

function indicatorPosition(
  edge: EdgePiece,
  scale: number,
  dragStart: CanvasPoint,
  dragDelta: CanvasVector,
): CanvasPoint {
  const Offset = 4 / scale
  switch (edge) {
    case 'top':
    case 'bottom':
      return canvasPoint({ x: dragStart.x + Offset, y: dragStart.y + dragDelta.y + Offset })
    case 'left':
    case 'right':
      return canvasPoint({ x: dragStart.x + dragDelta.x + Offset, y: dragStart.y + Offset })
    default:
      assertNever(edge)
  }
}
