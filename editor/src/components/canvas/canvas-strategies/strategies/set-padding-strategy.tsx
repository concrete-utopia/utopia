import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
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
  CSSPaddingMappedValues,
  deltaFromEdge,
  maybeFullPadding,
  offsetPaddingByEdge,
  paddingFromSingleValue,
  paddingIsDefined,
  paddingPropForEdge,
  paddingToPaddingString,
  printCssNumberWithDefaultUnit,
  simplePaddingFromAllElementProps,
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
  CSSNumberWithRenderedValue,
  elementIsIntrinsicElementOrScene,
  indicatorMessage,
  offsetMeasurementByDelta,
  precisionFromModifiers,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import { CanvasCommand } from '../../commands/commands'
import { AllElementProps } from '../../../editor/store/editor-state'

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

  const paddingData = maybePaddingWithSource(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    selectedElements[0],
  )

  if (paddingData == null) {
    return null
  }

  const { padding, source } = paddingData

  const maybePaddingValueProps = paddingValueIndicatorProps(
    canvasState,
    interactionSession,
    selectedElements,
  )

  const resizeControl = controlWithProps({
    control: PaddingResizeControl,
    props: { targets: selectedElements, currentPadding: padding, disabled: source === 'code' },
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

      const currentPadding = padding[paddingPropInteractedWith]?.renderedValuePx ?? 0
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
    case 'bottom':
      return CSSCursor.RowResize
    case 'left':
    case 'right':
      return CSSCursor.ColResize
    default:
      assertNever(edgePiece)
  }
}

interface PaddingWithSource {
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>
  source: 'props' | 'code'
}

function maybePaddingWithSource(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  selectedElement: ElementPath,
): PaddingWithSource | null {
  const element = MetadataUtils.findElementByElementPath(metadata, selectedElement)
  if (element == null) {
    return null
  }

  if (element.globalFrame == null || isZeroSizedElement(element.globalFrame)) {
    return null
  }

  const paddingData = getPaddingData(metadata, allElementProps, selectedElement)

  if (
    !elementIsIntrinsicElementOrScene(element) &&
    shouldShowControls({
      propAvailableFromStyle: elementHasNonZeroPadding(paddingData.padding),
      measurementsNonZero: elementHasNonzeroPaddingFromMeasurements(element),
    })
  ) {
    return paddingData
  }

  const children = MetadataUtils.getChildren(metadata, selectedElement)
  if (children.length === 0) {
    return null
  }

  const childrenNotPositionedAbsoluteOrSticky = MetadataUtils.getChildren(
    metadata,
    selectedElement,
  ).filter(
    (child) =>
      child.specialSizeMeasurements.position !== 'absolute' &&
      child.specialSizeMeasurements.position !== 'sticky',
  )

  if (childrenNotPositionedAbsoluteOrSticky.length === 0) {
    return null
  }

  return paddingData
}

function getPaddingData(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  selectedElement: ElementPath,
): PaddingWithSource {
  const paddingFromMetadata = simplePaddingFromMetadata(metadata, selectedElement)

  const paddingFromAllElementProps = simplePaddingFromAllElementProps(
    allElementProps,
    metadata,
    selectedElement,
  )

  const paddingData: PaddingWithSource = paddingIsDefined(paddingFromMetadata)
    ? { padding: paddingFromMetadata, source: 'props' }
    : paddingIsDefined(paddingFromAllElementProps)
    ? { padding: paddingFromAllElementProps, source: 'code' }
    : { padding: paddingFromSingleValue(unitlessCSSNumberWithRenderedValue(0)), source: 'props' }
  return paddingData
}

const isConsideredNonZeroPaddingValue = (n: number | undefined): boolean => (n ?? 0) !== 0

function elementHasNonzeroPaddingFromMeasurements(element: ElementInstanceMetadata): boolean {
  const { top, right, bottom, left } = element.specialSizeMeasurements.padding
  return [top, right, bottom, left].some(isConsideredNonZeroPaddingValue)
}

function elementHasNonZeroPadding(
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): boolean {
  return IndividualPaddingProps.some((s) =>
    isConsideredNonZeroPaddingValue(padding[s]?.renderedValuePx),
  )
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
