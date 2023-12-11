import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../../core/shared/either'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { isJSXElement } from '../../../../core/shared/element-template'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import type { InteractionCanvasState, MoveStrategy } from '../canvas-strategy-types'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  flattenSelection,
} from './shared-move-strategies-helpers'
import { styleStringInArray } from '../../../../utils/common-constants'

export function relativeMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): MoveStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget) // TODO eventually make this handle fragmentLike elements
  if (selectedElements.length === 0) {
    return null
  }
  const filteredSelectedElements = flattenSelection(selectedElements)
  const last = filteredSelectedElements[filteredSelectedElements.length - 1]
  const metadata = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, last)
  if (
    metadata == null ||
    metadata.specialSizeMeasurements.position !== 'relative' // I think this check should be filteredSelectedElements.every
  ) {
    return null
  }
  // should we also support absolute elements, for which we'll do something like `CONVERT_TO_RELATIVE`?

  const offsets = getStyleOffsets(metadata)
  const hasOffsets =
    offsets != null &&
    (offsets.left != null || offsets.top != null || offsets.right != null || offsets.bottom != null)

  const WeightWithExistingOffset = 4
  const WeightWithoutExistingOffset = 1.3 // this needs to be lower than DragConversionWeight in convert-to-absolute-and-move-strategy.tsx and FlexReorderFitness in flex-reorder-strategy.tsx

  const fitness = (() => {
    if (
      interactionSession == null ||
      interactionSession.interactionData.type !== 'DRAG' ||
      interactionSession.activeControl.type !== 'BOUNDING_AREA'
    ) {
      return 0
    }
    return hasOffsets ? WeightWithExistingOffset : WeightWithoutExistingOffset
  })()

  return {
    strategy: {
      id: 'RELATIVE_MOVE',
      name: 'Move (Relative)',
      descriptiveLabel: 'Moving Relative Elements',
      icon: {
        category: 'modalities',
        type: 'moveabs-large',
      },
      controlsToRender: [
        controlWithProps({
          control: ImmediateParentOutlines,
          props: { targets: filteredSelectedElements },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ImmediateParentBounds,
          props: { targets: filteredSelectedElements },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
      ],
      fitness: fitness,
      apply: () => {
        if (
          interactionSession != null &&
          interactionSession.interactionData.type === 'DRAG' &&
          interactionSession.interactionData.drag != null &&
          interactionSession.activeControl.type === 'BOUNDING_AREA'
        ) {
          return applyMoveCommon(
            filteredSelectedElements,
            filteredSelectedElements,
            canvasState,
            interactionSession,
            getAdjustMoveCommands(filteredSelectedElements, canvasState, interactionSession, {
              ignoreLocalFrame: true,
            }),
            'move',
          )
        } else {
          return emptyStrategyApplicationResult
        }
      },
    },
    dragType: 'absolute', // is this a third type?
  }
}

const getStyleOffsets = (metadata: ElementInstanceMetadata) => {
  const getOffsetPropValue = (
    name: 'left' | 'top' | 'right' | 'bottom',
    attrs: PropsOrJSXAttributes,
  ): number | null => {
    return foldEither(
      (_) => null,
      (v) => (v != null ? v.value : null),
      getLayoutProperty(name, attrs, styleStringInArray),
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
