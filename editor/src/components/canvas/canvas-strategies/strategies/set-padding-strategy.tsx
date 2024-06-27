import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  isIntrinsicElement,
  isJSXElement,
  jsxElementName,
  jsxElementNameEquals,
} from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import type { EdgePiece } from '../../canvas-types'
import { CSSCursor, isHorizontalEdgePiece, oppositeEdgePiece } from '../../canvas-types'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { isZeroSizedElement } from '../../controls/outline-utils'
import { PaddingResizeControl } from '../../controls/select-mode/padding-resize-control'
import type { FloatingIndicatorProps } from '../../controls/select-mode/floating-number-indicator'
import { FloatingIndicator } from '../../controls/select-mode/floating-number-indicator'
import type { CSSPaddingKey, CSSPaddingMappedValues, PaddingAdjustMode } from '../../padding-utils'
import {
  deltaFromEdge,
  getSizeUpdateCommandsForNewPadding,
  maybeFullPadding,
  offsetPaddingByEdge,
  paddingAdjustMode,
  paddingForEdgeSimplePadding,
  paddingPropForEdge,
  paddingToPaddingString,
  printCssNumberWithDefaultUnit,
  simplePaddingFromMetadata,
} from '../../padding-utils'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState, InteractionLifecycle } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { flattenSelection, getMultiselectBounds } from './shared-move-strategies-helpers'
import type { CanvasPoint, CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasVector,
  isInfinityRectangle,
  roundTo,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import type {
  AdjustPrecision,
  CSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import {
  canShowCanvasPropControl,
  indicatorMessage,
  offsetMeasurementByDelta,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import type { CanvasCommand } from '../../commands/commands'
import { eitherToMaybe, flatMapEither, foldEither } from '../../../../core/shared/either'
import { styleStringInArray } from '../../../../utils/common-constants'
import { elementHasOnlyTextChildren } from '../../canvas-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import type { Axis } from '../../../inspector/inspector-common'
import { detectFillHugFixedState, isHuggingFixedHugFill } from '../../../inspector/inspector-common'
import {
  AdjustCssLengthProperties,
  adjustCssLengthProperties,
} from '../../commands/adjust-css-length-command'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { activeFrameTargetPath, setActiveFrames } from '../../commands/set-active-frames-command'
import { convertPixelsToTailwindDimension } from '../../../../core/tailwind/tailwind-helpers'
import type { TailwindProp } from '../../../../core/tailwind/tailwind-helpers'
import { camelCaseToDashed } from '../../../../core/shared/string-utils'
import { create, fromString } from '../../../../core/shared/property-path'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../../../../core/shared/jsx-attribute-utils'
import { getClassNameAttribute } from '../../../../core/tailwind/tailwind-options'
import { ClassNameToAttributes } from '../../../../core/third-party/tailwind-defaults'
import type { CSSNumber } from '../../../inspector/common/css-utils'

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

  const canShowPadding = canShowCanvasPropControl(
    canvasState.projectContents,
    selectedElements[0],
    canvasState.scale,
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    canvasState.propertyControlsInfo,
  ).has('padding')
  if (!canShowPadding) {
    return null
  }

  if (
    !supportsPaddingControls(
      canvasState.startingMetadata,
      canvasState.startingElementPathTree,
      selectedElements[0],
    )
  ) {
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
    descriptiveLabel: 'Changing Padding',
    icon: { category: 'tools', type: 'pointer' },
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'PADDING_RESIZE_HANDLE', 1),
    apply: (lifecycle: InteractionLifecycle) => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.activeControl.type !== 'PADDING_RESIZE_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const filteredSelectedElements = flattenSelection(selectedElements)
      const originalBoundingBox = getMultiselectBounds(
        canvasState.startingMetadata,
        filteredSelectedElements,
      )

      if (originalBoundingBox == null || filteredSelectedElements.length !== 1) {
        return emptyStrategyApplicationResult
      }

      const selectedElement = filteredSelectedElements[0]
      const delta = calculateAdjustDelta(canvasState, interactionSession, selectedElement)

      if (delta == null) {
        return emptyStrategyApplicationResult
      }

      const edgePiece = interactionSession.activeControl.edgePiece
      const drag = interactionSession.interactionData.drag ?? canvasVector({ x: 0, y: 0 })
      const padding = simplePaddingFromMetadata(canvasState.startingMetadata, selectedElement)
      const paddingPropInteractedWith = paddingPropForEdge(edgePiece)
      const currentPadding = padding[paddingPropInteractedWith]?.renderedValuePx ?? 0
      const rawDelta = deltaFromEdge(drag, edgePiece)
      const maxedDelta = Math.max(-currentPadding, rawDelta)
      const precision = precisionFromModifiers(interactionSession.interactionData.modifiers)
      const newPaddingEdgeUnsnapped = offsetMeasurementByDelta(
        padding[paddingPropInteractedWith] ?? unitlessCSSNumberWithRenderedValue(maxedDelta),
        delta,
        precision,
      )

      const newPaddingEdgeSnapped: CSSNumberWithRenderedValue = (() => {
        const currentValuePx = newPaddingEdgeUnsnapped.renderedValuePx
        const currentValueOther = newPaddingEdgeUnsnapped.value.value

        const newValuePx = currentValuePx
        const newValueOther = (currentValueOther / currentValuePx) * newValuePx
        return {
          value: { value: newValueOther, unit: newPaddingEdgeUnsnapped.value.unit },
          renderedValuePx: newValuePx,
        }
      })()

      const newPaddingMaxed = adjustPaddingsWithAdjustMode(
        paddingAdjustMode(interactionSession.interactionData.modifiers),
        paddingPropInteractedWith,
        newPaddingEdgeSnapped,
        padding,
      )

      const basicCommands: CanvasCommand[] = [
        updateHighlightedViews('mid-interaction', []),
        setCursorCommand(pickCursorFromEdge(edgePiece)),
        setElementsToRerenderCommand(selectedElements),
      ]

      const nonZeroPropsToAddRaw = IndividualPaddingProps.flatMap(
        (p): Array<[CSSPaddingKey, CSSNumber]> => {
          const value = newPaddingMaxed[p]
          if (value == null || value.renderedValuePx < 0) {
            return []
          }
          return [[p, value.value]]
        },
      )

      const nonZeroPropsToAdd: Array<[CSSPaddingKey, string | number]> = nonZeroPropsToAddRaw.map(
        (p) => [p[0], printCssNumberWithDefaultUnit(p[1], 'px')],
      )

      const tailwindClasses = (() => {
        const element = MetadataUtils.getJsxElementChildFromMetadata(
          canvasState.startingMetadata,
          selectedElement,
        )
        const classNames =
          getClassNameAttribute(element)
            .value?.split(' ')
            .filter((c) => c.length > 0) ?? []

        const modifiedProps = nonZeroPropsToAddRaw.map((p) => p[0])

        const classNamesToKeep = classNames.filter((c) => {
          if (ClassNameToAttributes[c] == null) {
            return true
          }
          return modifiedProps.every(
            (p) => !ClassNameToAttributes[c].includes(camelCaseToDashed(p)),
          )
        })

        const newClassNames = nonZeroPropsToAddRaw.map((p) => {
          return convertPixelsToTailwindDimension(
            p[1].unit === 'rem' ? p[1].value * 16 : p[1].value, // very ugly
            camelCaseToDashed(p[0]) as TailwindProp,
          )
        })

        return [...classNamesToKeep, ...newClassNames].join(' ')
      })()

      const combinedXPadding =
        paddingForEdgeSimplePadding('left', newPaddingMaxed) +
        paddingForEdgeSimplePadding('right', newPaddingMaxed)

      const combinedYPadding =
        paddingForEdgeSimplePadding('top', newPaddingMaxed) +
        paddingForEdgeSimplePadding('bottom', newPaddingMaxed)

      const targetFrame = MetadataUtils.getFrameOrZeroRect(
        selectedElement,
        canvasState.startingMetadata,
      )

      const adjustSizeCommand = getSizeUpdateCommandsForNewPadding(
        combinedXPadding,
        combinedYPadding,
        targetFrame,
        filteredSelectedElements,
        canvasState.startingMetadata,
        canvasState.startingElementPathTree,
      )

      basicCommands.push(adjustSizeCommand)

      // "tearing off" padding
      if (newPaddingEdgeSnapped.renderedValuePx < PaddingTearThreshold) {
        return strategyApplicationResult([
          ...basicCommands,
          deleteProperties('always', selectedElement, [
            StylePaddingProp,
            stylePropPathMappingFn(paddingPropInteractedWith, styleStringInArray),
          ]),
          // ...nonZeroPropsToAdd.map(([p, value]) => {
          //   if (p === 'className') {
          //     return setProperty('always', selectedElement, fromString('className'), value)
          //   } else {
          //     return setProperty(
          //       'always',
          //       selectedElement,
          //       stylePropPathMappingFn(p, styleStringInArray),
          //       value,
          //     )
          //   }
          // }),
          setProperty('always', selectedElement, fromString('className'), tailwindClasses),
          setActiveFrames(
            selectedElements.map((path) => ({
              action: 'set-padding',
              target: activeFrameTargetPath(path),
              source: zeroRectIfNullOrInfinity(
                MetadataUtils.getFrameInCanvasCoords(path, canvasState.startingMetadata),
              ),
            })),
          ),
        ])
      }

      // const allPaddingPropsDefined = maybeFullPadding(newPaddingMaxed)

      // // all 4 sides present - can be represented via the padding shorthand property
      // if (allPaddingPropsDefined != null) {
      //   const paddingString = paddingToPaddingString(allPaddingPropsDefined)
      //   return strategyApplicationResult([
      //     ...basicCommands,
      //     ...IndividualPaddingProps.map((p) =>
      //       deleteProperties('always', selectedElement, [
      //         stylePropPathMappingFn(p, styleStringInArray),
      //       ]),
      //     ),
      //     setProperty('always', selectedElement, StylePaddingProp, paddingString),
      //     setActiveFrames(
      //       selectedElements.map((path) => ({
      //         action: 'set-padding',
      //         target: activeFrameTargetPath(path),
      //         source: zeroRectIfNullOrInfinity(
      //           MetadataUtils.getFrameInCanvasCoords(path, canvasState.startingMetadata),
      //         ),
      //       })),
      //     ),
      //   ])
      // }

      // only some sides are present - longhand properties have to be used
      return strategyApplicationResult([
        ...basicCommands,
        deleteProperties('always', selectedElement, [
          StylePaddingProp,
          ...IndividualPaddingProps.map((p) => stylePropPathMappingFn(p, styleStringInArray)),
        ]),
        // ...nonZeroPropsToAdd.map(([p, value]) => {
        //   if (p === 'className') {
        //     return setProperty('always', selectedElement, fromString('className'), value)
        //   } else {
        //     return setProperty(
        //       'always',
        //       selectedElement,
        //       stylePropPathMappingFn(p, styleStringInArray),
        //       value,
        //     )
        //   }
        // }),
        setProperty('always', selectedElement, fromString('className'), tailwindClasses),
        setActiveFrames(
          selectedElements.map((path) => ({
            action: 'set-padding',
            target: activeFrameTargetPath(path),
            source: zeroRectIfNullOrInfinity(
              MetadataUtils.getFrameInCanvasCoords(path, canvasState.startingMetadata),
            ),
          })),
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

function supportsPaddingControls(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): boolean {
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

  if (
    elementHasNonzeroPaddingFromProps &&
    shouldShowControls(elementHasNonzeroPaddingFromProps, elementHasNonzeroPaddingFromMeasurements)
  ) {
    return true
  }

  if (elementHasOnlyTextChildren(element)) {
    return true
  }

  const childrenNotPositionedAbsoluteOrSticky = MetadataUtils.getChildrenOrdered(
    metadata,
    pathTrees,
    path,
  ).filter(
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
  const filteredSelectedElements = flattenSelection([selectedElement])

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
  newPaddingValue: CSSNumberWithRenderedValue,
  padding: CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined>,
): CSSPaddingMappedValues<CSSNumberWithRenderedValue | undefined> {
  switch (adjustMode) {
    case 'individual':
      return {
        ...padding,
        [paddingPropInteractedWith]: newPaddingValue,
      }
    case 'all': {
      return {
        paddingTop: newPaddingValue,
        paddingBottom: newPaddingValue,
        paddingLeft: newPaddingValue,
        paddingRight: newPaddingValue,
      }
    }
    case 'cross-axis': {
      return {
        ...padding,
        [paddingPropInteractedWith]: newPaddingValue,
        [opposite(paddingPropInteractedWith)]: newPaddingValue,
      }
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

  const isHug = isHuggingFixedHugFill(
    detectFillHugFixedState(axis, metadata, selectedElement).fixedHugFill?.type,
  )
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

  const isHug = isElementSetToHugAlongAffectedAxis(
    paddingPropInteractedWith,
    canvasState.startingMetadata,
    selectedElement,
  )

  const isHugOppositeSide =
    (paddingPropInteractedWith === 'paddingRight' ||
      paddingPropInteractedWith === 'paddingBottom') &&
    isHug

  const isInDeadZone = isHugOppositeSide
    ? newPaddingEdge.renderedValuePx < -PaddingTearThreshold
    : newPaddingEdge.renderedValuePx < PaddingTearThreshold

  const delta = isInDeadZone ? rawDelta : maxedDelta

  const deltaAdjusted =
    isHug &&
    (paddingPropInteractedWith === 'paddingRight' || paddingPropInteractedWith === 'paddingBottom')
      ? -delta
      : delta

  return deltaAdjusted
}
