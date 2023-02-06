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
import { CSSCursor, EdgePiece, isHorizontalEdgePiece, oppositeEdgePiece } from '../../canvas-types'
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
  paddingAdjustMode,
  PaddingAdjustMode,
  paddingForEdgeSimplePadding,
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
import {
  canvasPoint,
  CanvasPoint,
  canvasVector,
  CanvasVector,
  isInfinityRectangle,
  roundTo,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import {
  AdjustPrecision,
  canShowCanvasPropControl,
  CSSNumberWithRenderedValue,
  indicatorMessage,
  offsetMeasurementByDelta,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import { CanvasCommand } from '../../commands/commands'
import { foldEither } from '../../../../core/shared/either'
import { styleStringInArray } from '../../../../utils/common-constants'
import { elementHasOnlyTextChildren } from '../../canvas-utils'
import { Modifiers } from '../../../../utils/modifiers'
import { Axis, detectFillHugFixedState } from '../../../inspector/inspector-common'
import { adjustCssLengthProperty } from '../../commands/adjust-css-length-command'

const StylePaddingProp = stylePropPathMappingFn('padding', styleStringInArray)
const IndividualPaddingProps: Array<CSSPaddingKey> = [
  'paddingTop',
  'paddingBottom',
  'paddingLeft',
  'paddingRight',
]

export const PaddingTearThreshold: number = -25

export const SetPaddingStrategyName = 'Set Padding'

function precisionFromModifiers(modifiers: Modifiers): AdjustPrecision {
  return modifiers.cmd ? 'coarse' : 'precise'
}

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
    selectedElements[0],
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

      const filteredSelectedElements = getDragTargets(selectedElements)
      const originalBoundingBox = getMultiselectBounds(
        canvasState.startingMetadata,
        filteredSelectedElements,
      )

      if (originalBoundingBox == null || filteredSelectedElements.length !== 1) {
        return emptyStrategyApplicationResult
      }

      const selectedElement = filteredSelectedElements[0]

      const edgePiece = interactionSession.activeControl.edgePiece
      const drag = interactionSession.interactionData.drag ?? canvasVector({ x: 0, y: 0 })
      const padding = simplePaddingFromMetadata(canvasState.startingMetadata, selectedElement)
      const paddingPropInteractedWith = paddingPropForEdge(edgePiece)
      const currentPadding = padding[paddingPropForEdge(edgePiece)]?.renderedValuePx ?? 0
      const rawDelta = deltaFromEdge(drag, edgePiece)
      const maxedDelta = Math.max(-currentPadding, rawDelta)
      const precision = precisionFromModifiers(interactionSession.interactionData.modifiers)
      const newPaddingEdge = offsetMeasurementByDelta(
        padding[paddingPropInteractedWith] ?? unitlessCSSNumberWithRenderedValue(maxedDelta),
        rawDelta,
        precision,
      )

      const delta = calculateAdjustDelta(canvasState, interactionSession, selectedElement)
      if (delta == null) {
        return emptyStrategyApplicationResult
      }

      const newPaddingMaxed = adjustPaddingsWithAdjustMode(
        paddingAdjustMode(interactionSession.interactionData.modifiers),
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

      // Get size of parent
      const targetFrame = MetadataUtils.getFrameOrZeroRect(
        selectedElement,
        canvasState.startingMetadata,
      )

      // Get child content size
      const allChildPaths = MetadataUtils.getChildrenPaths(
        canvasState.startingMetadata,
        selectedElement,
      )

      const fixedSizeChildrenPaths = allChildPaths.filter(
        (childPath) =>
          detectFillHugFixedState('horizontal', canvasState.startingMetadata, childPath)?.type ===
          'fixed',
      )
      const fixedSizeNonAbsoluteChildrenPaths = fixedSizeChildrenPaths.filter((childPath) =>
        MetadataUtils.targetParticipatesInAutoLayout(canvasState.startingMetadata, childPath),
      )
      const childrenBoundingFrameMaybeInfinite = MetadataUtils.getBoundingRectangleInCanvasCoords(
        fixedSizeNonAbsoluteChildrenPaths,
        canvasState.startingMetadata,
      )
      const childrenBoundingFrame = zeroRectIfNullOrInfinity(childrenBoundingFrameMaybeInfinite)

      // FIXME include child offset
      // Check if child content size + padding exceeds parent size in that dimension
      const combinedPaddingInDimension =
        paddingForEdgeSimplePadding(edgePiece, padding) +
        paddingForEdgeSimplePadding(oppositeEdgePiece(edgePiece), padding)
      const dimensionKey = isHorizontalEdgePiece(edgePiece) ? 'width' : 'height'
      const combinedContentSizeInDimension =
        combinedPaddingInDimension + childrenBoundingFrame[dimensionKey] + delta

      const sizeDelta = combinedContentSizeInDimension - targetFrame[dimensionKey]
      if (sizeDelta > 0) {
        const elementMetadata = MetadataUtils.findElementByElementPath(
          canvasState.startingMetadata,
          selectedElement,
        )
        const elementParentBounds = elementMetadata?.specialSizeMeasurements.immediateParentBounds
        const parentDimensionPx = elementParentBounds?.[dimensionKey]
        const elementParentFlexDirection =
          elementMetadata?.specialSizeMeasurements.parentFlexDirection

        // TODO We need a way to call the correct resizing strategy here, but they are all assuming
        // the drag originates from a given edge, whereas we want to pass in the desired delta to a
        // dimension and receive the required commands to resize the element
        basicCommands.push(
          adjustCssLengthProperty(
            'always',
            selectedElement,
            stylePropPathMappingFn(dimensionKey, styleStringInArray),
            roundTo(sizeDelta, 0),
            parentDimensionPx,
            elementParentFlexDirection ?? null,
            'do-not-create-if-doesnt-exist',
          ),
        )
      }

      // "tearing off" padding
      if (newPaddingEdge.renderedValuePx < PaddingTearThreshold) {
        return strategyApplicationResult([
          ...basicCommands,
          deleteProperties('always', selectedElement, [
            StylePaddingProp,
            stylePropPathMappingFn(paddingPropInteractedWith, styleStringInArray),
          ]),
          ...nonZeroPropsToAdd.map(([p, value]) =>
            setProperty(
              'always',
              selectedElement,
              stylePropPathMappingFn(p, styleStringInArray),
              value,
            ),
          ),
        ])
      }

      const allPaddingPropsDefined = maybeFullPadding(newPaddingMaxed)

      // all 4 sides present - can be represented via the padding shorthand property
      if (allPaddingPropsDefined != null) {
        const paddingString = paddingToPaddingString(allPaddingPropsDefined)
        return strategyApplicationResult([
          ...basicCommands,
          ...IndividualPaddingProps.map((p) =>
            deleteProperties('always', selectedElement, [
              stylePropPathMappingFn(p, styleStringInArray),
            ]),
          ),
          setProperty('always', selectedElement, StylePaddingProp, paddingString),
        ])
      }

      // only some sides are present - longhand properties have to be used
      return strategyApplicationResult([
        ...basicCommands,
        deleteProperties('always', selectedElement, [
          StylePaddingProp,
          ...IndividualPaddingProps.map((p) => stylePropPathMappingFn(p, styleStringInArray)),
        ]),
        ...nonZeroPropsToAdd.map(([p, value]) =>
          setProperty(
            'always',
            selectedElement,
            stylePropPathMappingFn(p, styleStringInArray),
            value,
          ),
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

function supportsPaddingControls(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
  const element = MetadataUtils.findElementByElementPath(metadata, path)
  if (element == null) {
    return false
  }

  if (
    element.globalFrame == null ||
    isInfinityRectangle(element.globalFrame) ||
    isZeroSizedElement(element.globalFrame)
  ) {
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
    shouldShowControls(elementHasNonzeroPaddingFromProps, elementHasNonzeroPaddingFromMeasurements)
  ) {
    return true
  }

  if (elementHasOnlyTextChildren(element)) {
    return true
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
  selectedElement: ElementPath,
): FloatingIndicatorProps | null {
  const filteredSelectedElements = getDragTargets([selectedElement])

  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE'
  ) {
    return null
  }

  const drag = interactionSession.interactionData.drag ?? canvasVector({ x: 0, y: 0 })
  const dragStart = interactionSession.interactionData.dragStart

  const edgePiece = interactionSession.activeControl.edgePiece

  const padding = simplePaddingFromMetadata(
    canvasState.startingMetadata,
    filteredSelectedElements[0],
  )
  const currentPadding =
    padding[paddingPropForEdge(edgePiece)] ?? unitlessCSSNumberWithRenderedValue(0)

  const delta = calculateAdjustDelta(canvasState, interactionSession, selectedElement)
  if (delta == null) {
    return null
  }

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

function opposite(padding: CSSPaddingKey): CSSPaddingKey {
  switch (padding) {
    case 'paddingBottom':
      return 'paddingTop'
    case 'paddingTop':
      return 'paddingBottom'
    case 'paddingLeft':
      return 'paddingRight'
    case 'paddingRight':
      return 'paddingLeft'
    default:
      assertNever(padding)
  }
}

function adjustPaddingsWithAdjustMode(
  adjustMode: PaddingAdjustMode,
  paddingPropInteractedWith: CSSPaddingKey,
  delta: number,
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
  precision: AdjustPrecision,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined> {
  const newPadding = offsetPaddingByEdge(paddingPropInteractedWith, delta, padding, precision)
  switch (adjustMode) {
    case 'individual':
      return newPadding
    case 'all': {
      const newPaddingValue = newPadding[paddingPropInteractedWith]
      return {
        paddingTop: newPaddingValue,
        paddingBottom: newPaddingValue,
        paddingLeft: newPaddingValue,
        paddingRight: newPaddingValue,
      }
    }
    case 'cross-axis': {
      const newPaddingValue = newPadding[paddingPropInteractedWith]
      newPadding[opposite(paddingPropInteractedWith)] = newPaddingValue
      return newPadding
    }
    default:
      assertNever(adjustMode)
  }
}

function isElementSetToHugAlongAffectedAxis(
  paddingPropInteractedWith: CSSPaddingKey,
  metadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
): boolean {
  const axis: Axis =
    paddingPropInteractedWith === 'paddingBottom' || paddingPropInteractedWith === 'paddingTop'
      ? 'vertical'
      : 'horizontal'

  const isHug = detectFillHugFixedState(axis, metadata, selectedElement)?.type === 'hug'
  return isHug
}

function calculateAdjustDelta(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  selectedElement: ElementPath,
): number | null {
  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE'
  ) {
    return null
  }

  const edgePiece = interactionSession.activeControl.edgePiece
  const drag = interactionSession.interactionData.drag ?? canvasVector({ x: 0, y: 0 })
  const padding = simplePaddingFromMetadata(canvasState.startingMetadata, selectedElement)
  const paddingPropInteractedWith = paddingPropForEdge(edgePiece)
  const currentPadding = padding[paddingPropForEdge(edgePiece)]?.renderedValuePx ?? 0
  const rawDelta = deltaFromEdge(drag, edgePiece)
  const maxedDelta = Math.max(-currentPadding, rawDelta)
  const precision = precisionFromModifiers(interactionSession.interactionData.modifiers)
  const newPaddingEdge = offsetMeasurementByDelta(
    padding[paddingPropInteractedWith] ?? unitlessCSSNumberWithRenderedValue(maxedDelta),
    rawDelta,
    precision,
  )

  const delta = newPaddingEdge.renderedValuePx < PaddingTearThreshold ? rawDelta : maxedDelta

  const isHug = isElementSetToHugAlongAffectedAxis(
    paddingPropInteractedWith,
    canvasState.startingMetadata,
    selectedElement,
  )

  const deltaAdjusted =
    isHug &&
    (paddingPropInteractedWith === 'paddingRight' || paddingPropInteractedWith === 'paddingBottom')
      ? -delta
      : delta

  return deltaAdjusted
}
