import { elementPath } from '../../../core/shared/element-path'
import { canvasPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { emptyModifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import {
  createInteractionViaMouse,
  DragInteractionData,
  InteractionSession,
} from './interaction-state'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

function dragBy15Pixels(editorState: EditorState): EditorState {
  const interactionSession: InteractionSession = {
    ...createInteractionViaMouse(
      null as any,
      emptyModifiers,
      null as any, // the strategy does not use this
      canvasPoint({ x: 15, y: 15 }),
    ),
    metadata: null as any, // the strategy does not use this
  }

  const strategyResult = absoluteMoveStrategy.apply(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    null as any, // the strategy does not use this
  )

  const finalEditor = foldAndApplyCommands(editorState, editorState, strategyResult, 'permanent')
    .editorState

  return finalEditor
}

describe('Absolute Move Strategy', () => {
  it('works with a TL pinned absolute element', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

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

    const finalEditor = dragBy15Pixels(initialEditor)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})
