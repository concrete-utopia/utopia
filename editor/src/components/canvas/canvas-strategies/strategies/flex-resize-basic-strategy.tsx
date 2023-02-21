import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../../commands/adjust-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionLifecycle,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { honoursPropsSize } from './absolute-utils'
import {
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
} from './resize-helpers'
import { FlexDirection } from '../../../inspector/common/css-utils'
import { getElementDimensions } from './flex-resize-helpers'

export function flexResizeBasicStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  if (
    selectedElements.length !== 1 ||
    !MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedElements[0],
      canvasState.startingMetadata,
    ) ||
    !honoursPropsSize(canvasState, selectedElements[0])
  ) {
    return null
  }
  const metadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElements[0],
  )
  const elementDimensionsProps = metadata != null ? getElementDimensions(metadata) : null
  const elementParentBounds = metadata?.specialSizeMeasurements.immediateParentBounds ?? null
  const elementParentFlexDirection = metadata?.specialSizeMeasurements.parentFlexDirection ?? null

  const widthPropToUse =
    (elementParentFlexDirection === 'row' || elementParentFlexDirection === 'row-reverse') &&
    elementDimensionsProps?.flexBasis != null
      ? 'flexBasis'
      : 'width'
  const heightPropToUse =
    (elementParentFlexDirection === 'column' || elementParentFlexDirection === 'column-reverse') &&
    elementDimensionsProps?.flexBasis != null
      ? 'flexBasis'
      : 'height'

  const elementDimensions =
    elementDimensionsProps == null
      ? null
      : {
          width: elementDimensionsProps[widthPropToUse],
          height: elementDimensionsProps[heightPropToUse],
        }

  const hasDimensions =
    elementDimensions != null &&
    (elementDimensions.width != null || elementDimensions.height != null)
  const hasSizedParent =
    elementParentBounds != null &&
    (elementParentBounds.width !== 0 || elementParentBounds.height !== 0)

  return {
    id: 'FLEX_RESIZE_BASIC',
    name: 'Flex Resize (Basic)',
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements },
        key: 'absolute-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ZeroSizeResizeControlWrapper,
        props: { targets: selectedElements },
        key: 'zero-size-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: selectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
      (hasDimensions || !hasSizedParent)
        ? 1
        : 0,
    apply: (_strategyLifecycle: InteractionLifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        // no multiselection support yet
        const selectedElement = selectedElements[0]
        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBounds = MetadataUtils.getFrameInCanvasCoords(
            selectedElement,
            canvasState.startingMetadata,
          )

          if (originalBounds == null || isInfinityRectangle(originalBounds)) {
            return emptyStrategyApplicationResult
          }

          if (metadata == null) {
            return emptyStrategyApplicationResult
          }

          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            [selectedElement],
          )

          const resizedBounds = resizeBoundingBox(
            originalBounds,
            drag,
            edgePosition,
            getLockedAspectRatio(
              interactionSession,
              interactionSession.interactionData.modifiers,
              originalBounds,
              anySelectedElementAspectRatioLocked,
            ),
            'non-center-based',
          )

          const makeResizeCommand = (
            name: 'width' | 'height' | 'flexBasis',
            elementDimension: number | null | undefined,
            original: number,
            resized: number,
            parent: number | undefined,
            parentFlexDirection: FlexDirection | null,
          ): AdjustCssLengthProperty[] => {
            if (elementDimension == null && (original === resized || hasSizedParent)) {
              return []
            }
            return [
              adjustCssLengthProperty(
                'always',
                selectedElement,
                stylePropPathMappingFn(name, styleStringInArray),
                elementDimension != null ? resized - original : resized,
                parent,
                parentFlexDirection,
                'create-if-not-existing',
              ),
            ]
          }

          const resizeCommands: Array<AdjustCssLengthProperty> = [
            ...makeResizeCommand(
              widthPropToUse,
              elementDimensionsProps?.[widthPropToUse],
              originalBounds.width,
              resizedBounds.width,
              elementParentBounds?.width,
              elementParentFlexDirection,
            ),
            ...makeResizeCommand(
              heightPropToUse,
              elementDimensionsProps?.[heightPropToUse],
              originalBounds.height,
              resizedBounds.height,
              elementParentBounds?.height,
              elementParentFlexDirection,
            ),
          ]

          return strategyApplicationResult([
            ...resizeCommands,
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            setElementsToRerenderCommand(selectedElements),
          ])
        } else {
          return strategyApplicationResult([
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
          ])
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}
