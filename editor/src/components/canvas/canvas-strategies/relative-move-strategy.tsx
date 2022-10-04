import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../../../core/shared/element-template'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from './canvas-strategy-types'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  getDragTargets,
} from './shared-move-strategies-helpers'

export const relativeMoveStrategy: CanvasStrategy = {
  id: 'RELATIVE_MOVE',

  name: () => 'Move (Relative)',

  isApplicable: (canvasState, _interactionState, instanceMetadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return false
    }

    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const metadata = MetadataUtils.findElementByElementPath(instanceMetadata, last)
    if (!metadata) {
      return false
    }

    // should we also support absolute elements, for which we'll do something like `CONVERT_TO_RELATIVE`?
    return metadata.specialSizeMeasurements.position === 'relative'
  },

  controlsToRender: [
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ],

  fitness: (canvasState, interactionState, _sessionState) => {
    const { interactionData, activeControl } = interactionState
    if (!(interactionData.type === 'DRAG' && interactionData.drag != null)) {
      return 0
    }
    if (activeControl.type !== 'BOUNDING_AREA') {
      return 0
    }

    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return 0
    }
    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const metadata = MetadataUtils.findElementByElementPath(interactionState.latestMetadata, last)
    if (!metadata) {
      return 0
    }
    const offsets = getStyleOffsets(metadata)
    if (!offsets) {
      return 0
    }

    const hasOffsets =
      offsets.left != null || offsets.top != null || offsets.right != null || offsets.bottom != null

    return hasOffsets
      ? 4 // +1 than reorder flow
      : 1 // there should be a more structured way to define priorities (:
  },

  apply: (canvasState, interactionState, sessionState) => {
    const isFitting = relativeMoveStrategy.fitness(canvasState, interactionState, sessionState) > 0
    if (!isFitting) {
      return emptyStrategyApplicationResult
    }

    return applyMoveCommon(
      canvasState,
      interactionState,
      sessionState,
      getAdjustMoveCommands(canvasState, interactionState, sessionState),
    )
  },
}

const getStyleOffsets = (metadata: ElementInstanceMetadata) => {
  const getOffsetPropValue = (
    name: 'left' | 'top' | 'right' | 'bottom',
    attrs: PropsOrJSXAttributes,
  ): number | null => {
    return foldEither(
      (_) => null,
      (v) => (v != null ? v.value : null),
      getLayoutProperty(name, attrs, ['style']),
    )
  }

  if (isLeft(metadata.element)) {
    return null
  }
  const { value } = metadata.element
  if (!isJSXElement(value)) {
    return null
  }

  const attrs = right(value.props)

  return {
    top: getOffsetPropValue('top', attrs),
    left: getOffsetPropValue('left', attrs),
    bottom: getOffsetPropValue('bottom', attrs),
    right: getOffsetPropValue('right', attrs),
  }
}
