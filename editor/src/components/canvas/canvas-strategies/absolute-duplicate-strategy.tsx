import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { duplicateElement } from '../commands/duplicate-element-command'
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
      interactionState.interactionData.dragThresholdPassed
    ) {
      return 2
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    const { selectedElements } = canvasState
    const filteredSelectedElements = getDragTargets(selectedElements)

    const duplicateCommands = filteredSelectedElements.map((selectedElement) => {
      const newUid = generateUidWithExistingComponents(canvasState.projectContents)
      const newPath = EP.appendToPath(EP.parentPath(selectedElement), newUid)
      return {
        newPath: newPath,
        commands: [duplicateElement('permanent', selectedElement, newUid)],
      }
    })

    const newSelectedElements = duplicateCommands.map((c) => c.newPath)

    const moveCommands = absoluteMoveStrategy.apply(
      { ...canvasState, selectedElements: newSelectedElements },
      interactionState,
      strategyState,
    )

    return {
      commands: [
        ...duplicateCommands.flatMap((c) => c.commands),
        ...moveCommands.commands,
        updateSelectedViews('permanent', newSelectedElements),
      ],
      customState: null,
    }
  },
}
