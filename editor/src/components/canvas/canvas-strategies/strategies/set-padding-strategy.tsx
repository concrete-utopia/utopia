import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  isIntrinsicElement,
  isJSXElement,
  jsxElementName,
  jsxElementNameEquals,
} from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
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
  FloatingIndicator,
  FloatingIndicatorProps,
} from '../../controls/select-mode/floating-number-indicator'
import {
  CSSPaddingKey,
  deltaFromEdge,
  maybeFullPadding,
  offsetPaddingByEdge,
  paddingPropForEdge,
  paddingToPaddingString,
  printCssNumberWithDefaultUnit,
  simplePaddingFromMetadata,
} from '../../padding-utils'
import { CanvasStrategyFactory, onlyFitWhenDraggingThisControl } from '../canvas-strategies'
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
  canShowCanvasPropControl,
  indicatorMessage,
  offsetMeasurementByDelta,
  precisionFromModifiers,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import { CanvasCommand } from '../../commands/commands'
import { foldEither } from '../../../../core/shared/either'

const StylePaddingProp = stylePropPathMappingFn('padding', ['style'])
const IndividualPaddingProps: Array<CSSPaddingKey> = [
  'paddingTop',
  'paddingBottom',
  'paddingLeft',
  'paddingRight',
]

export const PaddingTearThreshold: number = -25

export const SetPaddingStrategyName = 'Set Padding'

export const setPaddingStrategy: CanvasStrategyFactory = (canvasState, interactionSession) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const element = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElements[0],
  )
  if (element == null) {
    return null
  }

  const canShowPadding = canShowCanvasPropControl(
    canvasState.projectContents,
    canvasState.openFile ?? null,
    element,
    canvasState.scale,
  ).has('padding')
  if (!canShowPadding) {
    return null
  }

  if (!supportsPaddingControls(canvasState.startingMetadata, selectedElements[0])) {
    return null
  }

  const maybePaddingValueProps = paddingValueIndicatorProps(
    canvasState,
    interactionSession,
    selectedElements,
  )

  const resizeControl = controlWithProps({
    control: PaddingResizeControl,
    props: { targets: selectedElements },
    key: 'padding-resize-control',
    show: 'visible-except-when-other-strategy-is-active',
  })

  const controlsToRender = optionalMap(
    (props) => [
      resizeControl,
      controlWithProps({
        control: FloatingIndicator,
        props: props,
        key: 'padding-value-indicator-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    maybePaddingValueProps,
  ) ?? [resizeControl]

  return {
    id: 'SET_PADDING_STRATEGY',
    name: SetPaddingStrategyName,
    controlsToRender: controlsToRender,
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'PADDING_RESIZE_HANDLE', 1),
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
        return emptyStrategyApplicationResult
      }

      const selectedElement = filteredSelectedElements[0]

      const paddingPropInteractedWith = paddingPropForEdge(edgePiece)
      const precision = precisionFromModifiers(interactionSession.interactionData.modifiers)

      const padding = simplePaddingFromMetadata(canvasState.startingMetadata, selectedElement)
      const currentPadding = padding[paddingPropForEdge(edgePiece)]?.renderedValuePx ?? 0
      const rawDelta = deltaFromEdge(drag, edgePiece)
      const maxedDelta = Math.max(-currentPadding, rawDelta)
      const newPaddingEdge = offsetMeasurementByDelta(
        padding[paddingPropInteractedWith] ?? unitlessCSSNumberWithRenderedValue(maxedDelta),
        rawDelta,
        precision,
      )

      const delta = newPaddingEdge.renderedValuePx < PaddingTearThreshold ? rawDelta : maxedDelta

      const newPaddingMaxed = offsetPaddingByEdge(
        paddingPropInteractedWith,
        delta,
        padding,
        precision,
      )

      const basicCommands: CanvasCommand[] = [
        updateHighlightedViews('mid-interaction', []),
        setCursorCommand(pickCursorFromEdge(edgePiece)),
        setElementsToRerenderCommand(selectedElements),
      ]

      const nonZeroPropsToAdd = IndividualPaddingProps.flatMap(
        (p): Array<[CSSPaddingKey, string | number]> => {
          const value = newPaddingMaxed[p]
          if (value == null || value.renderedValuePx < 0) {
            return []
          }
          return [[p, printCssNumberWithDefaultUnit(value.value, 'px')]]
        },
      )

      if (newPaddingEdge.renderedValuePx < PaddingTearThreshold) {
        return strategyApplicationResult([
          ...basicCommands,
          deleteProperties('always', selectedElement, [
            StylePaddingProp,
            stylePropPathMappingFn(paddingPropInteractedWith, ['style']),
          ]),
          ...nonZeroPropsToAdd.map(([p, value]) =>
            setProperty('always', selectedElement, stylePropPathMappingFn(p, ['style']), value),
          ),
        ])
      }

      const allPaddingPropsDefined = maybeFullPadding(newPaddingMaxed)

      if (allPaddingPropsDefined != null) {
        const paddingString = paddingToPaddingString(allPaddingPropsDefined)
        return strategyApplicationResult([
          ...basicCommands,
          ...IndividualPaddingProps.map((p) =>
            deleteProperties('always', selectedElement, [stylePropPathMappingFn(p, ['style'])]),
          ),
          setProperty('always', selectedElement, StylePaddingProp, paddingString),
        ])
      }

      return strategyApplicationResult([
        ...basicCommands,
        deleteProperties('always', selectedElement, [
          StylePaddingProp,
          ...IndividualPaddingProps.map((p) => stylePropPathMappingFn(p, ['style'])),
        ]),
        ...nonZeroPropsToAdd.map(([p, value]) =>
          setProperty('always', selectedElement, stylePropPathMappingFn(p, ['style']), value),
        ),
      ])
    },
  }
}

function pickCursorFromEdge(edgePiece: EdgePiece): CSSCursor {
  switch (edgePiece) {
    case 'top':
      return CSSCursor.PaddingNorth
    case 'bottom':
      return CSSCursor.PaddingSouth
    case 'left':
      return CSSCursor.PaddingEast
    case 'right':
      return CSSCursor.PaddingWest
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

  const padding = simplePaddingFromMetadata(metadata, path)
  const { top, right, bottom, left } = element.specialSizeMeasurements.padding
  const elementHasNonzeroPaddingFromMeasurements = [top, right, bottom, left].some(
    (s) => s != null && s > 0,
  )
  const elementHasNonzeroPaddingFromProps = IndividualPaddingProps.some((s) => padding[s] != null)

  const elementIsScene = foldEither(
    () => false,
    (e) => isJSXElement(e) && jsxElementNameEquals(e.name, jsxElementName('Scene', [])),
    element.element,
  )

  if (elementIsScene) {
    return false
  }

  const elementIsIntrinsicElement = foldEither(
    () => false,
    (e) =>
      isJSXElement(e) &&
      (isIntrinsicElement(e.name) || jsxElementNameEquals(e.name, jsxElementName('Scene', []))),
    element.element,
  )

  if (
    !elementIsIntrinsicElement &&
    shouldShowControls({
      propAvailableFromStyle: elementHasNonzeroPaddingFromProps,
      measurementsNonZero: elementHasNonzeroPaddingFromMeasurements,
    })
  ) {
    return true
  }

  const children = MetadataUtils.getChildren(metadata, path)
  if (children.length === 0) {
    return false
  }

  const childrenNotPositionedAbsoluteOrSticky = MetadataUtils.getChildren(metadata, path).filter(
    (child) =>
      child.specialSizeMeasurements.position !== 'absolute' &&
      child.specialSizeMeasurements.position !== 'sticky',
  )

  if (childrenNotPositionedAbsoluteOrSticky.length > 0) {
    return true
  }

  return false
}

function paddingValueIndicatorProps(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  selectedElements: ElementPath[],
): FloatingIndicatorProps | null {
  const filteredSelectedElements = getDragTargets(selectedElements)

  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE' ||
    interactionSession.interactionData.drag == null ||
    filteredSelectedElements.length !== 1
  ) {
    return null
  }

  const { drag, dragStart } = interactionSession.interactionData

  const edgePiece = interactionSession.activeControl.edgePiece

  const padding = simplePaddingFromMetadata(
    canvasState.startingMetadata,
    filteredSelectedElements[0],
  )
  const currentPadding =
    padding[paddingPropForEdge(edgePiece)] ?? unitlessCSSNumberWithRenderedValue(0)

  const delta = deltaFromEdge(drag, edgePiece)

  const updatedPaddingMeasurement = offsetMeasurementByDelta(
    currentPadding,
    delta,
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  const maxedPaddingMeasurement = offsetMeasurementByDelta(
    currentPadding,
    Math.max(-currentPadding.renderedValuePx, delta),
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  return {
    value: indicatorMessage(
      updatedPaddingMeasurement.renderedValuePx > PaddingTearThreshold,
      maxedPaddingMeasurement,
    ),
    position: indicatorPosition(edgePiece, canvasState.scale, dragStart, drag),
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
