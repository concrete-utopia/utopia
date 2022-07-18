import { EditorState } from '../../editor/store/editor-state'
import { elementPath } from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { getEditorState, makeTestProjectCodeWithSnippet } from '../ui-jsx.test-utils'
import { absoluteDuplicateStrategy } from './absolute-duplicate-strategy'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import {
  canvasPoint,
  canvasRectangle,
  CanvasVector,
  localRectangle,
} from '../../../core/shared/math-utils'
import { altModifier, Modifiers } from '../../../utils/modifiers'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { InteractionSession } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

const defaultMetadata: ElementInstanceMetadataMap = {
  'scene-aaa': {
    elementPath: elementPath([['scene-aaa']]),
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity': {
    elementPath: elementPath([['scene-aaa', 'app-entity']]),
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa': {
    elementPath: elementPath([['scene-aaa', 'app-entity'], ['aaa']]),
    specialSizeMeasurements: {
      position: 'absolute',
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ]),
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
    globalFrame: canvasRectangle({ x: 50, y: 50, width: 250, height: 300 }),
    localFrame: localRectangle({ x: 50, y: 50, width: 250, height: 300 }),
  } as ElementInstanceMetadata,
}

function dragByPixelsIsApplicable(
  editorState: EditorState,
  vector: CanvasVector,
  modifiers: Modifiers,
  metadata: ElementInstanceMetadataMap,
): boolean {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      null as any, // the strategy does not use this
      modifiers,
      null as any, // the strategy does not use this
      vector,
    ),
    metadata: null as any, // the strategy does not use this
    allElementProps: null as any, // the strategy does not use this
  }

  return absoluteDuplicateStrategy.isApplicable(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    metadata,
    editorState.allElementProps,
  )
}

describe('absoluteDuplicateStrategy', () => {
  it('does not apply for elements that are the root element of an instance', () => {
    const targetElement = elementPath([['scene-aaa', 'app-entity'], ['aaa']])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const actualResult = dragByPixelsIsApplicable(
      initialEditor,
      canvasPoint({ x: 1, y: 1 }),
      altModifier,
      defaultMetadata,
    )
    expect(actualResult).toEqual(false)
  })
})
