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
import { absoluteResizeDeltaStrategy } from './absolute-resize-delta-strategy'
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

  const strategyResult = absoluteResizeDeltaStrategy.apply(
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

function createTestEditorAndResizeElement(
  edgePosition: EdgePosition,
  snippet: string,
): EditorState {
  const targetElement = elementPath([
    ['scene-aaa', 'app-entity'],
    ['aaa', 'bbb'],
  ])

  const initialEditor = getEditorStateWithSelectedViews(makeTestProjectCodeWithSnippet(snippet), [
    targetElement,
  ])

  return resizeElement(initialEditor, edgePosition)
}

function resizeTestWithTLWH(edgePosition: EdgePosition): EditorState {
  const snippet = `
  <View style={{ ...(props.style || {}) }} data-uid='aaa'>
    <View
      style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, width: 250, height: 300 }}
      data-uid='bbb'
    />
  </View>
  `
  return createTestEditorAndResizeElement(edgePosition, snippet)
}

describe('Absolute Delta Resize Strategy TLWH', () => {
  it('works with element resized from TL corner', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const editorAfterStrategy = resizeTestWithTLWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
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
  it('works with element resized from BR corner', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const editorAfterStrategy = resizeTestWithTLWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
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
  it('works with element resized from R edge', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 0.5 }
    const editorAfterStrategy = resizeTestWithTLWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, width: 265, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with element resized from T edge', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 0 }
    const editorAfterStrategy = resizeTestWithTLWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 75, width: 250, height: 275 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

function resizeTestWithTLBR(edgePosition: EdgePosition): EditorState {
  const snippet = `
  <View style={{ ...(props.style || {}) }} data-uid='aaa'>
    <View
      style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, bottom: 85, right: 110 }}
      data-uid='bbb'
    />
  </View>
  `
  return createTestEditorAndResizeElement(edgePosition, snippet)
}
describe('Absolute Delta Resize Strategy TLBR', () => {
  it('works with element resized from TL corner', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const editorAfterStrategy = resizeTestWithTLBR(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
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
  it('works with element resized from BR corner', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const editorAfterStrategy = resizeTestWithTLBR(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
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
  it('works with element resized from R edge', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 0.5 }
    const editorAfterStrategy = resizeTestWithTLBR(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 50, bottom: 85, right: 95 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with element resized from T edge', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 0 }
    const editorAfterStrategy = resizeTestWithTLBR(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: 75, bottom: 85, right: 110 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

function resizeTestWithBRWH(edgePosition: EdgePosition): EditorState {
  const snippet = `
  <View style={{ ...(props.style || {}) }} data-uid='aaa'>
    <View
      style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 250, height: 300, bottom: '50px', right: 40 }}
      data-uid='bbb'
    />
  </View>
  `
  return createTestEditorAndResizeElement(edgePosition, snippet)
}
describe('Absolute Delta Resize Strategy BRWH', () => {
  it('works with element resized from TL corner', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const editorAfterStrategy = resizeTestWithBRWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
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
  it('works with element resized from BR corner', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const editorAfterStrategy = resizeTestWithBRWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
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
  it('works with element resized from R edge', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 0.5 }
    const editorAfterStrategy = resizeTestWithBRWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 265, height: 300, bottom: '50px', right: 25 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with element resized from T edge', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 0 }
    const editorAfterStrategy = resizeTestWithBRWH(edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: 250, height: 275, bottom: '50px', right: 40 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

describe('Absolute Delta Resize Strategy TLBRWH', () => {
  it('works with element resized from TL corner with too many pins', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const snippet = `
  <View style={{ ...(props.style || {}) }} data-uid='aaa'>
    <View
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        width: 100,
        height: 120,
        top: 50,
        left: 30,
        bottom: 230,
        right: 270
       }}
      data-uid='bbb'
    />
  </View>
  `
    const editorAfterStrategy = createTestEditorAndResizeElement(edgePosition, snippet)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            width: 85,
            height: 95,
            top: 75,
            left: 45,
            bottom: 230,
            right: 270
          }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})
