import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { emptyModifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { absoluteResizeStrategy } from './absolute-resize-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

function resizeElement(editor: EditorState, edgePosition: EdgePosition): EditorState {
  const interactionSessionWithoutMetadata = createMouseInteractionForTests(
    null as any, // the strategy does not use this
    emptyModifiers,
    { type: 'RESIZE_HANDLE', edgePosition: edgePosition },
    canvasPoint({ x: 15, y: 25 }),
  )

  const strategyResult = absoluteResizeStrategy.apply(
    pickCanvasStateFromEditorState(editor),
    { ...interactionSessionWithoutMetadata, metadata: {} },
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedCommands: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: {
        'scene-aaa/app-entity:aaa/bbb': {
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
      },
    } as StrategyState,
  )
  return foldAndApplyCommands(editor, editor, strategyResult, 'permanent').editorState
}

describe('Absolute Resize Strategy', () => {
  it('works with a TLWH pinned absolute element resized from BR corner', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      ),
      [targetElement],
    )

    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const updatedEditor = resizeElement(initialEditor, edgePosition)

    expect(testPrintCodeFromEditorState(updatedEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, width: 265, height: 325 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a TLWH pinned absolute element resized from TL corner', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      ),
      [targetElement],
    )

    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const updatedEditor = resizeElement(initialEditor, edgePosition)

    expect(testPrintCodeFromEditorState(updatedEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '65px', top: 75, width: 235, height: 275 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a TLBR pinned absolute element resized from BR corner', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, bottom: 85, right: 110 }}
        data-uid='bbb'
      />
    </View>
    `,
      ),
      [targetElement],
    )

    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const updatedEditor = resizeElement(initialEditor, edgePosition)

    expect(testPrintCodeFromEditorState(updatedEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, bottom: 60, right: 95 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a TLBR pinned absolute element resized from TL corner', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, bottom: 85, right: 110 }}
        data-uid='bbb'
      />
    </View>
    `,
      ),
      [targetElement],
    )

    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const updatedEditor = resizeElement(initialEditor, edgePosition)

    expect(testPrintCodeFromEditorState(updatedEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '65px', top: 75, bottom: 85, right: 110 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a WHBR pinned absolute element resized from BR corner', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 250, height: 300, bottom: '50px', right: 40, }}
        data-uid='bbb'
      />
    </View>
    `,
      ),
      [targetElement],
    )

    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const updatedEditor = resizeElement(initialEditor, edgePosition)

    expect(testPrintCodeFromEditorState(updatedEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 265, height: 325, bottom: '25px', right: 25 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a WHBR pinned absolute element resized from TL corner', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 250, height: 300, bottom: '50px', right: 40, }}
        data-uid='bbb'
      />
    </View>
    `,
      ),
      [targetElement],
    )

    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const updatedEditor = resizeElement(initialEditor, edgePosition)

    expect(testPrintCodeFromEditorState(updatedEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 235, height: 275, bottom: '50px', right: 40 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})
