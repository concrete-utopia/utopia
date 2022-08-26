import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getInsertionSubjectsFromInteractionTarget,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import { InsertionSubject } from '../../editor/editor-modes'
import { LayoutHelpers } from '../../../core/layout/layout-helpers'
import { isLeft } from '../../../core/shared/either'
import { insertElement } from '../commands/insert-element-command'

export const absoluteInsertStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_INSERT',
  name: 'Absolute Insert (Delta-based)',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    const insertionElementSubjects = insertionSubjects.filter((s) => s.type === 'Element')
    return insertionElementSubjects.length > 0
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
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteInsertStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
      sessionState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    const insertionSubjects = getInsertionSubjectsFromInteractionTarget(
      canvasState.interactionTarget,
    )
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const insertionCommands = insertionSubjects.flatMap((s) =>
        getInsertionCommands(s, interactionState),
      )

      return {
        commands: insertionCommands,
        customState: null,
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

function getInsertionCommands(subject: InsertionSubject, interactionState: InteractionSession) {
  if (subject.type !== 'Element') {
    // non-element subjects are not supported
    return []
  }
  if (
    interactionState.interactionData.type === 'DRAG' &&
    interactionState.interactionData.drag != null
  ) {
    const pointOnCanvas = interactionState.interactionData.dragStart
    const updatedAttributes = LayoutHelpers.updateLayoutPropsWithFrame(
      false,
      null,
      subject.element.props,
      {
        left: pointOnCanvas.x,
        top: pointOnCanvas.y,
        width: 100,
        height: 100,
      },
      ['style'],
    )

    if (isLeft(updatedAttributes)) {
      throw new Error(`Problem setting drag frame on an element we just created.`)
    }

    const updatedInsertionSubject = {
      ...subject,
      element: {
        ...subject.element,
        props: updatedAttributes.value,
      },
    }

    return insertElement('always', updatedInsertionSubject)
  }
  return []
}
