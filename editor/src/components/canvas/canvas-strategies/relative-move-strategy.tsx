import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { foldEither, isLeft, right } from '../../../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../../../core/shared/element-template'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import {
  applyMoveCommon,
  getAdjustMoveCommands,
  getDragTargets,
} from './shared-move-strategies-helpers'

export function relativeMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length > 0) {
    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const metadata = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, last)
    if (metadata != null && metadata.specialSizeMeasurements.position === 'relative') {
      // should we also support absolute elements, for which we'll do something like `CONVERT_TO_RELATIVE`?

      const offsets = getStyleOffsets(metadata)
      const hasOffsets =
        offsets != null &&
        (offsets.left != null ||
          offsets.top != null ||
          offsets.right != null ||
          offsets.bottom != null)

      return {
        id: 'RELATIVE_MOVE',
        name: 'Move (Relative)',
        controlsToRender: [
          controlWithProps({
            control: ParentOutlines,
            props: {},
            key: 'parent-outlines-control',
            show: 'visible-only-while-active',
          }),
          controlWithProps({
            control: ParentBounds,
            props: {},
            key: 'parent-bounds-control',
            show: 'visible-only-while-active',
          }),
        ],
        fitness:
          interactionSession != null &&
          interactionSession.interactionData.type === 'DRAG' &&
          interactionSession.interactionData.drag != null &&
          interactionSession.activeControl.type === 'BOUNDING_AREA'
            ? hasOffsets
              ? 4 // +1 than reorder flow
              : 1
            : 0,

        apply: () => {
          if (
            interactionSession != null &&
            interactionSession.interactionData.type === 'DRAG' &&
            interactionSession.interactionData.drag != null &&
            interactionSession.activeControl.type === 'BOUNDING_AREA'
          ) {
            return applyMoveCommon(
              canvasState,
              interactionSession,
              getAdjustMoveCommands(canvasState, interactionSession, {
                ignoreLocalFrame: true,
              }),
            )
          } else {
            return emptyStrategyApplicationResult
          }
        },
      }
    }
  }
  return null
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
