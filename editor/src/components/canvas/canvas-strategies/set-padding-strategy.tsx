import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { JSXElement } from '../../../core/shared/element-template'
import { CanvasRectangle, rectangleDifference, roundTo } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { EdgePosition } from '../canvas-types'
import {
  adjustCssLengthProperty,
  AdjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { setCssLengthProperty, SetCssLengthProperty } from '../commands/set-css-length-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { PaddingResizeControl } from '../controls/select-mode/padding-resize-control'
import {
  AbsolutePin,
  ensureAtLeastTwoPinsForEdgePosition,
  supportsAbsoluteResize,
} from './absolute-resize-helpers'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getDragTargets, getMultiselectBounds } from './shared-absolute-move-strategy-helpers'
import { pickCursorFromEdgePosition } from './shared-absolute-resize-strategy-helpers'

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
        return supportsAbsoluteResize(metadata, element, canvasState)
      })
    } else {
      return false
    }
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

    const edgePosition = interactionState.activeControl.edgePosition
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
        setCursorCommand('mid-interaction', pickCursorFromEdgePosition(edgePosition)),
        updateHighlightedViews('mid-interaction', []),
      ])
    }

    const selectedElement = filteredSelectedElements[0]

    const element = getElementFromProjectContents(
      selectedElement,
      canvasState.projectContents,
      canvasState.openFile,
    )

    const originalFrame = MetadataUtils.getFrameInCanvasCoords(
      selectedElement,
      sessionState.startingMetadata,
    )

    if (element == null || originalFrame == null) {
      return strategyApplicationResult([])
    }

    const elementParentBounds =
      MetadataUtils.findElementByElementPath(sessionState.startingMetadata, selectedElement)
        ?.specialSizeMeasurements.immediateParentBounds ?? null

    const commandsForSelectedElements = [
      adjustCssLengthProperty(
        'always',
        selectedElement,
        stylePropPathMappingFn('paddingTop', ['style']),
        drag.y,
        0,
        true,
      ),
    ]

    return strategyApplicationResult([
      ...commandsForSelectedElements,
      updateHighlightedViews('mid-interaction', []),
      setCursorCommand('mid-interaction', pickCursorFromEdgePosition(edgePosition)),
      setElementsToRerenderCommand(selectedElements),
    ])
  },
}

function createPaddingResizeCommandsFromFrame(
  element: JSXElement,
  selectedElement: ElementPath,
  newFrame: CanvasRectangle,
  originalFrame: CanvasRectangle,
  elementParentBounds: CanvasRectangle | null,
  edgePosition: EdgePosition,
): (AdjustCssLengthProperty | SetCssLengthProperty)[] {
  const pins: Array<AbsolutePin> = ensureAtLeastTwoPinsForEdgePosition(
    right(element.props),
    edgePosition,
  )
  return mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const value = getLayoutProperty(pin, right(element.props), ['style'])
    const rectangleDiff = rectangleDifference(originalFrame, newFrame)
    const delta = allPinsFromFrame(rectangleDiff)[pin]
    const roundedDelta = roundTo(delta, 0)
    const pinDirection = pin === 'right' || pin === 'bottom' ? -1 : 1
    if (roundedDelta !== 0) {
      if (isRight(value) && value.value != null) {
        return adjustCssLengthProperty(
          'always',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          roundedDelta * pinDirection,
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
          true,
        )
      } else {
        const valueToSet = allPinsFromFrame(newFrame)[pin]
        return setCssLengthProperty(
          'always',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          roundTo(valueToSet, 0),
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
        )
      }
    } else {
      return null
    }
  }, pins)
}

function allPinsFromFrame(frame: CanvasRectangle): { [key: string]: number } {
  return {
    left: frame.x,
    top: frame.y,
    width: frame.width,
    height: frame.height,
    right: frame.x + frame.width,
    bottom: frame.y + frame.height,
  }
}
