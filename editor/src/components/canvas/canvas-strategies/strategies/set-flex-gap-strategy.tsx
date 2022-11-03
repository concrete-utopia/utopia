import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { CanvasVector, canvasVector } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { FlexGapControl } from '../../controls/select-mode/flex-gap-control'
import {
  SimpleFlexDirection,
  simpleFlexDirectionFromString,
  updateGapValue,
} from '../../drag-utils'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const SetFlexGapStrategyId = 'SET_FLEX_GAP_STRATEGY'

const StyleGapProp = stylePropPathMappingFn('gap', ['style'])

export const setFlexGapStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customtrategyState: CustomStrategyState,
) => {
  if (
    interactionSession != null &&
    !(
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'FLEX_GAP_HANDLE'
    )
  ) {
    // We don't want to include this in the strategy picker if any other interaction is active
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]

  const flexGap = maybeFlexGapFromElement(canvasState.startingMetadata, selectedElements[0])
  if (flexGap == null) {
    return null
  }

  const drag = dragFromInteractionSession(interactionSession) ?? canvasVector({ x: 0, y: 0 })

  const updatedDragValue = Math.max(0, updateGapValue(flexGap.direction, flexGap.value, drag))

  return {
    id: SetFlexGapStrategyId,
    name: 'Set flex gap',
    controlsToRender: [
      controlWithProps({
        control: FlexGapControl,
        props: {
          selectedElement: selectedElement,
          gap: updatedDragValue,
          flexDirection: flexGap.direction,
        },
        key: 'flex-gap-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: 1,
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.activeControl.type !== 'FLEX_GAP_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult([
        setProperty('always', selectedElement, StyleGapProp, updatedDragValue + 'px'),
        setCursorCommand('always', cursorFromFlexDirection(flexGap.direction)),
        updateHighlightedViews('mid-interaction', []),
        setElementsToRerenderCommand(selectedElements),
      ])
    },
  }
}

interface FlexGapData {
  value: number
  direction: SimpleFlexDirection
}

function maybeFlexGapFromElement(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexGapData | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (elementMetadata == null || elementMetadata.specialSizeMeasurements.display !== 'flex') {
    return null
  }

  const children = MetadataUtils.getChildren(metadata, elementPath)
  if (children.length < 2) {
    return null
  }

  const flexGap = children[0].specialSizeMeasurements.parentFlexGap
  if (flexGap === 0) {
    return null
  }

  const flexDirection =
    optionalMap(
      simpleFlexDirectionFromString,
      children[0].specialSizeMeasurements.parentFlexDirection,
    ) ?? 'row'

  return { value: flexGap, direction: flexDirection }
}

function dragFromInteractionSession(
  interactionSession: InteractionSession | null,
): CanvasVector | null {
  if (interactionSession != null && interactionSession.interactionData.type === 'DRAG') {
    return interactionSession.interactionData.drag
  }
  return null
}

function cursorFromFlexDirection(direction: SimpleFlexDirection): CSSCursor {
  switch (direction) {
    case 'column':
    case 'column-reverse':
      return CSSCursor.RowResize
    case 'row':
    case 'row-reverse':
      return CSSCursor.ColResize
    default:
      assertNever(direction)
  }
}
