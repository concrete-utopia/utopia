import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { flatMapArray, safeIndex } from '../../../core/shared/array-utils'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { CanvasStrategy } from './canvas-strategy-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { deleteProperty, setProperty } from '../commands/commands'
import { ApplyOnMouseUp } from '../controls/select-mode/apply-on-mouse-up'

const timeThreshold: number = 3 * 1000

export const alignToParentStrategy: CanvasStrategy = {
  name: 'Align To Parent',
  strategyGroups: new Set(),
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionState != null &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      !interactionState.interactionData.dragThresholdPassed &&
      interactionState.startedAt + timeThreshold < interactionState.globalTime
    ) {
      return canvasState.selectedElements.every((element) => {
        return (
          MetadataUtils.findElementByElementPath(metadata, element)?.specialSizeMeasurements
            .parentLayoutSystem === 'flow'
        )
      })
    }
    return false
  },
  controlsToRender: [
    { control: ApplyOnMouseUp, key: 'ApplyOnMouseUp', show: 'visible-only-while-active' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    const applicable = alignToParentStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    )
    return applicable ? 10 : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    const selectedElement = safeIndex(canvasState.selectedElements, 0)
    if (selectedElement == null) {
      return []
    } else {
      const parentMetadata = MetadataUtils.getParent(sessionState.startingMetadata, selectedElement)
      if (parentMetadata == null || parentMetadata.globalFrame == null) {
        return []
      } else {
        const parentDimensions = parentMetadata.globalFrame
        const siblings = MetadataUtils.getSiblings(sessionState.startingMetadata, selectedElement)
        const newHeight = Math.floor(parentDimensions.height / siblings.length)
        return flatMapArray((sibling, index) => {
          let heightToSet: number
          if (index === siblings.length - 1) {
            // Fill the remainder of the height.
            heightToSet = parentDimensions.height - newHeight * (siblings.length - 1)
          } else {
            heightToSet = newHeight
          }
          return [
            setProperty(
              'permanent',
              sibling.elementPath,
              stylePropPathMappingFn('position', ['style']),
              jsxAttributeValue('relative', emptyComments),
            ),
            setProperty(
              'permanent',
              sibling.elementPath,
              stylePropPathMappingFn('left', ['style']),
              jsxAttributeValue(0, emptyComments),
            ),
            deleteProperty(
              'permanent',
              sibling.elementPath,
              stylePropPathMappingFn('top', ['style']),
            ),
            setProperty(
              'permanent',
              sibling.elementPath,
              stylePropPathMappingFn('width', ['style']),
              jsxAttributeValue('100%', emptyComments),
            ),
            setProperty(
              'permanent',
              sibling.elementPath,
              stylePropPathMappingFn('height', ['style']),
              jsxAttributeValue(heightToSet, emptyComments),
            ),
          ]
        }, siblings)
      }
    }
  },
}
