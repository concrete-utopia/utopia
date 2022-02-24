import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  emptyComments,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasRectangle,
  magnitude,
  offsetPoint,
  rectContainsPoint,
} from '../../../core/shared/math-utils'
import * as PP from '../../../core/shared/property-path'
import { CanvasStrategy } from '../../../interactions_proposal'
import { setProperty, SetProperty, wildcardPatch } from '../commands/commands'
import { FlexAlignControls } from '../controls/select-mode/flex-align-controls'
import { FlexAlignControlRectProps } from './canvas-strategy-types'

export const flexAlignParentStrategy: CanvasStrategy = {
  name: "Change Parent's Flex Align and Justify",
  strategyGroups: new Set(),
  isApplicable: (canvasState, interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedElement = canvasState.selectedElements[0]

      const isFlexLayouted = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        MetadataUtils.findElementByElementPath(metadata, selectedElement),
      )
      const hasNoSiblings = MetadataUtils.getSiblings(metadata, selectedElement).length === 1

      if (isFlexLayouted && hasNoSiblings) {
        return true
      }
    }
    return false
  },
  controlsToRender: [
    { control: FlexAlignControls, key: 'FlexAlignControls', show: 'visible-only-while-active' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return flexAlignParentStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    )
      ? 10
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    // only apply after a certain treshold IF we hadn't already passed that treshold once
    const draggedElement = safeIndex(canvasState.selectedElements, 0)
    if (draggedElement == null || interactionState.interactionData.type !== 'DRAG') {
      return []
    }

    const targetParent = MetadataUtils.getParent(sessionState.startingMetadata, draggedElement)
    const mousePosition =
      interactionState.interactionData.drag == null
        ? interactionState.interactionData.dragStart
        : offsetPoint(
            interactionState.interactionData.dragStart,
            interactionState.interactionData.drag,
          )
    const indicatorBoxes = calcualteFlexAlignIndicatorBoxes(targetParent, mousePosition)

    // if any indicator box is highlighted, we also want to change the parent's style too
    const higlightedIndicator = indicatorBoxes.find((b) => b.highlighted === true)
    if (higlightedIndicator == null || targetParent == null) {
      return [
        wildcardPatch('transient', {
          canvas: {
            controls: {
              flexAlignDropTargets: { $set: indicatorBoxes },
              animatedPlaceholderTargetUids: {
                $set: [EP.toUid(draggedElement)],
              },
            },
          },
        }),
      ]
    } else {
      const flexPropToChange: AssociatedFlexProp = higlightedIndicator.associatedFlexProp

      // Change Parent Props
      const parentPropsToUpdate: Array<SetProperty> = Object.entries(flexPropToChange).map(
        ([key, value]) => {
          return setProperty(
            'permanent',
            targetParent.elementPath,
            PP.create(['style', key]),
            jsxAttributeValue(value, emptyComments),
          )
        },
      )

      // Make child invisible
      const childOpacity0: SetProperty = setProperty(
        'transient',
        draggedElement,
        PP.create(['style', 'opacity']),
        jsxAttributeValue(0, emptyComments),
      )

      return [
        ...parentPropsToUpdate,
        childOpacity0,
        wildcardPatch('transient', {
          canvas: {
            controls: {
              flexAlignDropTargets: { $set: indicatorBoxes },
              animatedPlaceholderTargetUids: {
                $set: [EP.toUid(draggedElement)],
              },
            },
          },
        }),
      ]
    }
  },
}

interface AssociatedFlexProp {
  justifyContent?: React.CSSProperties['justifyContent']
  alignItems?: React.CSSProperties['alignItems']
}

function flexIndicatorBox(
  mousePosition: CanvasPoint,
  canvasFrame: CanvasRectangle,
  associatedFlexProp: AssociatedFlexProp,
): FlexAlignControlRectProps {
  const mouseInRect = rectContainsPoint(canvasFrame, mousePosition)
  return {
    x: canvasFrame.x,
    y: canvasFrame.y,
    width: canvasFrame.width,
    height: canvasFrame.height,
    highlighted: mouseInRect,
    associatedFlexProp: associatedFlexProp,
  }
}

function calcualteFlexAlignIndicatorBoxes(
  targetMetadata: ElementInstanceMetadata | null,
  mousePosition: CanvasPoint,
): Array<FlexAlignControlRectProps> {
  const BoxHeight = 20

  if (targetMetadata?.globalFrame == null) {
    return []
  }

  return [
    flexIndicatorBox(
      mousePosition,
      {
        x: targetMetadata.globalFrame.x,
        y: targetMetadata.globalFrame.y,
        width: targetMetadata.globalFrame.width,
        height: BoxHeight,
      } as CanvasRectangle,
      { alignItems: 'flex-start' },
    ),
    flexIndicatorBox(
      mousePosition,
      {
        x: targetMetadata.globalFrame?.x,
        y: targetMetadata.globalFrame?.y + targetMetadata.globalFrame.height - BoxHeight,
        width: targetMetadata.globalFrame?.width,
        height: BoxHeight,
      } as CanvasRectangle,
      { alignItems: 'flex-end' },
    ),
  ]
}
