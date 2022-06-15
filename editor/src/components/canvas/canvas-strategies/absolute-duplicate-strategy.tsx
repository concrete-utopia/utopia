import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { DuplicateElement, duplicateElement } from '../commands/duplicate-element-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { CanvasStrategy } from './canvas-strategy-types'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

export const absoluteDuplicateStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_DUPLICATE',
  name: 'Duplicate Absolute Elements',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState != null &&
      interactionState.interactionData.modifiers.alt
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      })
    }
    return false
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
  fitness: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState.interactionData.modifiers.alt &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      return 2
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    const { selectedElements } = canvasState
    const filteredSelectedElements = getDragTargets(selectedElements)

    let duplicatedElementNewUids = { ...strategyState.customStrategyState.duplicatedElementNewUids }
    let withDuplicatedMetadata: ElementInstanceMetadataMap = { ...strategyState.startingMetadata }
    let duplicateCommands: Array<{ newPath: ElementPath; commands: DuplicateElement[] }> = []

    filteredSelectedElements.forEach((selectedElement) => {
      const selectedElementString = EP.toString(selectedElement)
      const newUid =
        duplicatedElementNewUids[selectedElementString] ??
        generateUidWithExistingComponents(canvasState.projectContents)
      const newPath = EP.appendToPath(EP.parentPath(selectedElement), newUid)
      const newPathString = EP.toString(newPath)

      duplicatedElementNewUids[selectedElementString] = newUid
      withDuplicatedMetadata[newPathString] = {
        ...withDuplicatedMetadata[EP.toString(selectedElement)],
        elementPath: newPath,
      }

      duplicateCommands.push({
        newPath: newPath,
        commands: [duplicateElement('permanent', selectedElement, newUid)],
      })
    })

    const moveCommands = absoluteMoveStrategy.apply(canvasState, interactionState, {
      ...strategyState,
      startingMetadata: withDuplicatedMetadata,
    })

    return {
      commands: [...duplicateCommands.flatMap((c) => c.commands), ...moveCommands.commands],
      customState: {
        ...strategyState.customStrategyState,
        duplicatedElementNewUids: duplicatedElementNewUids,
      },
    }
  },
}
