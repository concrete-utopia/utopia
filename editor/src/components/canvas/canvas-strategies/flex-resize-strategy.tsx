import { stylePropPathMappingFn } from '../../../components/inspector/common/property-path-hooks'
import { CanvasVector } from '../../../core/shared/math-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { last, safeIndex } from '../../../core/shared/array-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  FlexDirection,
} from '../../../core/shared/element-template'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { adjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { FlexResizeControl } from '../controls/select-mode/flex-resize-control'
import { CanvasStrategy } from './canvas-strategy-types'
import { setCssLengthProperty } from '../commands/set-css-length-command'

// TODO Move to a separate flex file
function isHorizontal(flexDirection: FlexDirection): boolean {
  return flexDirection === 'row' || flexDirection === 'row-reverse'
}

function isReverse(flexDirection: FlexDirection): boolean {
  return flexDirection === 'row-reverse' || flexDirection === 'column-reverse'
}

function getSiblingSpacingFromEdge(
  metadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
  parentMetadata: ElementInstanceMetadata | null,
  flexDirection: FlexDirection,
): number | null {
  // While the furthest sibling is not snapping or about to snap?
  if (parentMetadata == null || parentMetadata.globalFrame == null) {
    return null
  }
  const siblings = MetadataUtils.getSiblings(metadata, selectedElement)
  const targetSibling = isReverse(flexDirection) ? siblings[0] : last(siblings)

  if (targetSibling == null || targetSibling.globalFrame == null) {
    return null
  } else {
    switch (flexDirection) {
      case 'row':
        const parentRight = parentMetadata.globalFrame.x + parentMetadata.globalFrame.width
        const siblingRight = targetSibling.globalFrame.x + targetSibling.globalFrame.width
        return parentRight - siblingRight
      case 'row-reverse':
        const parentLeft = parentMetadata.globalFrame.x
        const siblingLeft = targetSibling.globalFrame.x
        return siblingLeft - parentLeft
      case 'column':
        const parentBottom = parentMetadata.globalFrame.y + parentMetadata.globalFrame.height
        const siblingBottom = targetSibling.globalFrame.y + targetSibling.globalFrame.height
        return parentBottom - siblingBottom
      case 'column-reverse':
        const parentTop = parentMetadata.globalFrame.y
        const siblingTop = targetSibling.globalFrame.y
        return parentTop - siblingTop
      default:
        const _exhaustiveCheck: never = flexDirection
        throw new Error(`Unknown flex direction ${flexDirection}`)
    }
  }
}

function getDragDeltaForFlexDirection(
  drag: CanvasVector | null,
  flexDirection: FlexDirection,
): number {
  if (drag == null) {
    return 0
  } else {
    const rawDelta = isHorizontal(flexDirection) ? drag.x : drag.y
    return isReverse(flexDirection) ? -rawDelta : rawDelta
  }
}

export const flexBasisResizeStrategy: CanvasStrategy = {
  id: 'FLEX_BASIS_RESIZE',
  name: 'Flex Basis Resize',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        canvasState.selectedElements[0],
        metadata,
      )
    } else {
      return false
    }
  },
  controlsToRender: [
    { control: FlexResizeControl, key: 'flex-resize-control', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    const applicable = flexBasisResizeStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )
    if (
      applicable &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const selectedElement = forceNotNull(
        'isApplicable should prevent null.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const parentMetadata = MetadataUtils.getParent(
        strategyState.startingMetadata,
        selectedElement,
      )
      const flexDirection = MetadataUtils.getFlexDirection(parentMetadata)

      const originalSiblingSpacing = getSiblingSpacingFromEdge(
        strategyState.startingMetadata,
        selectedElement,
        parentMetadata,
        flexDirection,
      )
      if (originalSiblingSpacing != null) {
        const drag = interactionState.interactionData.drag
        const spacing = originalSiblingSpacing - getDragDeltaForFlexDirection(drag, flexDirection)
        if (spacing > 5) {
          return 10
        }
      }
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    const draggedElement = safeIndex(canvasState.selectedElements, 0)
    const parentMetadata = MetadataUtils.getParent(
      strategyState.startingMetadata,
      draggedElement ?? null,
    )
    const originalMetadata = MetadataUtils.findElementByElementPath(
      strategyState.startingMetadata,
      draggedElement ?? null,
    )
    if (
      draggedElement == null ||
      parentMetadata == null ||
      originalMetadata == null ||
      interactionState.interactionData.type !== 'DRAG' ||
      interactionState.interactionData.drag == null
    ) {
      return {
        commands: [],
        customState: null,
      }
    } else {
      const flexDirection = MetadataUtils.getFlexDirection(parentMetadata)
      const originalFrame = originalMetadata.localFrame
      const parentBounds = originalMetadata.specialSizeMeasurements.immediateParentBounds
      const horizontal = isHorizontal(flexDirection)

      const newFlexBasis = horizontal
        ? (originalFrame?.width ?? 0) + interactionState.interactionData.drag.x
        : (originalFrame?.height ?? 0) + interactionState.interactionData.drag.y

      const resizeCursor = horizontal ? CSSCursor.ResizeEW : CSSCursor.ResizeNS

      return {
        commands: [
          setCssLengthProperty(
            'permanent',
            draggedElement,
            stylePropPathMappingFn('flexBasis', ['style']),
            newFlexBasis,
            horizontal ? parentBounds?.width : parentBounds?.height,
          ),
          setCursorCommand('transient', resizeCursor),
        ],
        customState: strategyState.customStrategyState,
      }
    }
  },
}
