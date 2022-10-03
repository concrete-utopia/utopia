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
  canvasRectangle,
  CanvasRectangle,
  CanvasVector,
  localRectangle,
  LocalRectangle,
  offsetRect,
  pointDifference,
  rectangleDifference,
  zeroCanvasPoint,
  zeroCanvasRect,
  canvasRectangleToLocalRectangle,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'

import {
  getElementFromProjectContents,
  withUnderlyingTarget,
} from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CanvasFrameAndTarget } from '../canvas-types'
import {
  adjustCssLengthProperty,
  AdjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { runLegacyAbsoluteMoveSnapping } from '../controls/guideline-helpers'
import {
  ConstrainedDragAxis,
  Guideline,
  GuidelineWithRelevantPoints,
  GuidelineWithSnappingVectorAndPointsOfRelevance,
} from '../guideline'
import { AbsolutePin } from './absolute-resize-helpers'
import { InteractionCanvasState } from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'

export function getAbsoluteMoveCommandsForSelectedElement(
  selectedElement: ElementPath,
  drag: CanvasVector,
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
): {
  commands: Array<AdjustCssLengthProperty>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const element: JSXElement | null = getElementFromProjectContents(
    selectedElement,
    canvasState.projectContents,
    canvasState.openFile,
  )

  const elementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata, // TODO should this be using the current metadata?
    selectedElement,
  )

  const elementParentBounds =
    elementMetadata?.specialSizeMeasurements.coordinateSystemBounds ?? null

  const localFrame = MetadataUtils.getLocalFrameFromSpecialSizeMeasurements(
    selectedElement,
    canvasState.startingMetadata,
  )

  const globalFrame = MetadataUtils.getFrameInCanvasCoords(
    selectedElement,
    canvasState.startingMetadata,
  )

  if (element == null) {
    return { commands: [], intendedBounds: [] }
  }

  const mappedPath =
    interactionState.updatedTargetPaths[EP.toString(selectedElement)] ?? selectedElement

  return createMoveCommandsForElement(
    element,
    selectedElement,
    mappedPath,
    drag,
    localFrame,
    globalFrame,
    elementParentBounds,
  )
}

function createMoveCommandsForElement(
  element: JSXElement,
  selectedElement: ElementPath,
  mappedPath: ElementPath,
  drag: CanvasVector,
  localFrame: LocalRectangle | null,
  globalFrame: CanvasRectangle | null,
  elementParentBounds: CanvasRectangle | null,
): {
  commands: Array<AdjustCssLengthProperty>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const { existingPins, extendedPins } = ensureAtLeastOnePinPerDimension(right(element.props))

  const adjustPinCommands = mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const negative = pin === 'right' || pin === 'bottom'

    // if this is a new pin which was missing, we offset the drag value with the initial value, which is
    // coming from the localFrame from metadata
    const isNewPin = !existingPins.includes(pin)

    const offsetX = isNewPin && pin === 'left' ? localFrame?.x ?? 0 : 0
    const offsetY = isNewPin && pin === 'top' ? localFrame?.y ?? 0 : 0

    const updatedPropValue =
      (horizontal ? offsetX + drag.x : offsetY + drag.y) * (negative ? -1 : 1)
    const parentDimension = horizontal ? elementParentBounds?.width : elementParentBounds?.height

    return adjustCssLengthProperty(
      'always',
      selectedElement,
      stylePropPathMappingFn(pin, ['style']),
      updatedPropValue,
      parentDimension,
      true,
    )
  }, extendedPins)

  const intendedBounds = (() => {
    if (globalFrame == null) {
      return []
    } else {
      const intendedGlobalFrame = offsetRect(globalFrame, drag)
      return [{ target: mappedPath, frame: intendedGlobalFrame }]
    }
  })()

  return { commands: adjustPinCommands, intendedBounds: intendedBounds }
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
  moveGuidelines: Array<GuidelineWithRelevantPoints>,
  canvasScale: number,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  if (drag == null) {
    return {
      snappedDragVector: zeroCanvasPoint,
      guidelinesWithSnappingVector: [],
    }
  }

  const multiselectBounds = getMultiselectBounds(jsxMetadata, selectedElements)

  // This is the entry point to extend the list of snapping strategies, if we want to add more

  const { snappedDragVector, guidelinesWithSnappingVector } = runLegacyAbsoluteMoveSnapping(
    drag,
    constrainedDragAxis,
    moveGuidelines,
    canvasScale,
    multiselectBounds,
  )

  return { snappedDragVector, guidelinesWithSnappingVector }
}

const horizontalPins: Array<AbsolutePin> = ['left', 'right']
const verticalPins: Array<AbsolutePin> = ['top', 'bottom']

function ensureAtLeastOnePinPerDimension(props: PropsOrJSXAttributes): {
  existingPins: Array<AbsolutePin>
  extendedPins: Array<AbsolutePin>
} {
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

  return {
    existingPins: [...existingHorizontalPins, ...existingVerticalPins],
    extendedPins: [...horizontalPinsToAdd, ...verticalPinsToAdd],
  }
}

export function areAllSelectedElementsNonAbsolute(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): boolean {
  if (selectedElements.length > 0) {
    return selectedElements.every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
      return !MetadataUtils.isPositionAbsolute(elementMetadata)
    })
  } else {
    return false
  }
}
