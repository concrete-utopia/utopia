import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp, LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  canvasPoint,
  CanvasRectangle,
  CanvasVector,
  pointDifference,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'

import {
  getElementFromProjectContents,
  withUnderlyingTarget,
} from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  adjustCssLengthProperty,
  AdjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { runLegacyAbsoluteMoveSnapping } from '../controls/guideline-helpers'
import { ConstrainedDragAxis, GuidelineWithSnappingVector } from '../guideline'
import { AbsolutePin } from './absolute-resize-helpers'
import { InteractionCanvasState } from './canvas-strategy-types'
import { StrategyState } from './interaction-state'

export function getAbsoluteMoveCommandsForSelectedElement(
  selectedElement: ElementPath,
  drag: CanvasVector,
  canvasState: InteractionCanvasState,
  sessionState: StrategyState,
): Array<AdjustCssLengthProperty> {
  const element: JSXElement | null = getElementFromProjectContents(
    selectedElement,
    canvasState.projectContents,
    canvasState.openFile,
  )
  const elementParentBounds =
    MetadataUtils.findElementByElementPath(
      sessionState.startingMetadata, // TODO should this be using the current metadata?
      selectedElement,
    )?.specialSizeMeasurements.immediateParentBounds ?? null // TODO this should probably be coordinateSystemBounds

  if (element == null) {
    return []
  }

  return createMoveCommandsForElement(element, selectedElement, drag, elementParentBounds)
}

function createMoveCommandsForElement(
  element: JSXElement,
  selectedElement: ElementPath,
  drag: CanvasVector,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  const pins = ensureAtLeastOnePinPerDimension(right(element.props))

  return mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const negative = pin === 'right' || pin === 'bottom'

    const updatedPropValue = (horizontal ? drag.x : drag.y) * (negative ? -1 : 1)
    const parentDimension = horizontal ? elementParentBounds?.width : elementParentBounds?.height

    return adjustCssLengthProperty(
      'permanent',
      selectedElement,
      stylePropPathMappingFn(pin, ['style']),
      updatedPropValue,
      parentDimension,
      true,
    )
  }, pins)
}

export function getAbsoluteOffsetCommandsForSelectedElement(
  target: ElementPath,
  newParent: ElementPath,
  strategyState: StrategyState,
  canvasState: InteractionCanvasState,
): Array<AdjustCssLengthProperty> {
  const element: JSXElement | null = getElementFromProjectContents(
    target,
    canvasState.projectContents,
    canvasState.openFile,
  )

  if (element == null) {
    return []
  }

  const currentParentContentBox =
    MetadataUtils.findElementByElementPath(strategyState.startingMetadata, EP.parentPath(target))
      ?.specialSizeMeasurements.globalContentBox ?? zeroCanvasRect

  const newParentContentBox =
    MetadataUtils.findElementByElementPath(strategyState.startingMetadata, newParent)
      ?.specialSizeMeasurements.globalContentBox ?? zeroCanvasRect

  const offsetTL = pointDifference(newParentContentBox, currentParentContentBox)
  const offsetBR = pointDifference(
    canvasPoint({
      x: currentParentContentBox.x + currentParentContentBox.width,
      y: currentParentContentBox.y + currentParentContentBox.height,
    }),
    canvasPoint({
      x: newParentContentBox.x + newParentContentBox.width,
      y: newParentContentBox.y + newParentContentBox.height,
    }),
  )

  const createAdjustCssLengthProperty = (
    pin: LayoutPinnedProp,
    newValue: number,
    parentDimension: number | undefined,
  ): AdjustCssLengthProperty | null => {
    const value = getLayoutProperty(pin, right(element.props), ['style'])
    if (isRight(value) && value.value != null) {
      // TODO what to do about missing properties?
      return adjustCssLengthProperty(
        'permanent',
        target,
        stylePropPathMappingFn(pin, ['style']),
        newValue,
        parentDimension,
        true,
      )
    } else {
      return null
    }
  }

  const newParentFrame = MetadataUtils.getFrameInCanvasCoords(
    newParent,
    strategyState.startingMetadata,
  )

  return [
    ...mapDropNulls(
      (pin) => {
        const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
        return createAdjustCssLengthProperty(
          pin,
          horizontal ? offsetTL.x : offsetTL.y,
          horizontal ? newParentFrame?.width : newParentFrame?.height,
        )
      },
      ['top', 'left'] as const,
    ),
    ...mapDropNulls(
      (pin) => {
        const horizontal = isHorizontalPoint(framePointForPinnedProp(pin))
        return createAdjustCssLengthProperty(
          pin,
          horizontal ? offsetBR.x : offsetBR.y,
          horizontal ? newParentFrame?.width : newParentFrame?.height,
        )
      },
      ['bottom', 'right'] as const,
    ),
  ]
}

export function getMultiselectBounds(
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): CanvasRectangle | null {
  const frames = mapDropNulls((element) => {
    return MetadataUtils.getFrameInCanvasCoords(element, jsxMetadata)
  }, selectedElements)

  return boundingRectangleArray(frames)
}

export function getFileOfElement(
  target: ElementPath | null,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
): string | null {
  return withUnderlyingTarget(
    target,
    projectContents,
    {},
    openFile,
    null,
    (_success, _element, _underlyingTarget, underlyingFilePath) => underlyingFilePath,
  )
}

// No need to include descendants in multiselection when dragging
// Note: this maybe slow when there are lot of selected views
export function getDragTargets(selectedViews: Array<ElementPath>): Array<ElementPath> {
  return selectedViews.filter((view) =>
    selectedViews.every((otherView) => !EP.isDescendantOf(view, otherView)),
  )
}

export function snapDrag(
  drag: CanvasPoint | null,
  constrainedDragAxis: ConstrainedDragAxis | null,
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  canvasScale: number,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
} {
  if (drag == null) {
    return { snappedDragVector: zeroCanvasPoint, guidelinesWithSnappingVector: [] }
  }
  const multiselectBounds = getMultiselectBounds(jsxMetadata, selectedElements)

  // This is the entry point to extend the list of snapping strategies, if we want to add more

  const { snappedDragVector, guidelinesWithSnappingVector } = runLegacyAbsoluteMoveSnapping(
    drag,
    constrainedDragAxis,
    jsxMetadata,
    selectedElements,
    canvasScale,
    multiselectBounds,
  )

  return { snappedDragVector, guidelinesWithSnappingVector }
}

const horizontalPins: Array<AbsolutePin> = ['left', 'right']
const verticalPins: Array<AbsolutePin> = ['top', 'bottom']

function ensureAtLeastOnePinPerDimension(props: PropsOrJSXAttributes): Array<AbsolutePin> {
  const existingHorizontalPins = horizontalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, ['style'])
    return isRight(prop) && prop.value != null
  })
  const existingVerticalPins = verticalPins.filter((p) => {
    const prop = getLayoutProperty(p, props, ['style'])
    return isRight(prop) && prop.value != null
  })

  const horizontalPinsToAdd: Array<AbsolutePin> = [...existingHorizontalPins]
  if (existingHorizontalPins.length === 0) {
    horizontalPinsToAdd.push('left')
  }

  const verticalPinsToAdd: Array<AbsolutePin> = [...existingVerticalPins]
  if (existingVerticalPins.length === 0) {
    verticalPinsToAdd.push('top')
  }

  return [...horizontalPinsToAdd, ...verticalPinsToAdd]
}
