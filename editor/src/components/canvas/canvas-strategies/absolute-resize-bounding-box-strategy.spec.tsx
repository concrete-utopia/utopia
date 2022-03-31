import { left } from '../../../core/shared/either'
import { elementPath, fromString } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { emptyModifiers, Modifiers, shiftModifier } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import { absoluteResizeBoundingBoxStrategy } from './absolute-resize-bounding-box-strategy'

function multiselectResizeElements(
  snippet: string,
  targetElements: Array<ElementPath>,
  edgePosition: EdgePosition,
  modifiers: Modifiers = emptyModifiers,
): EditorState {
  const initialEditor = getEditorStateWithSelectedViews(
    makeTestProjectCodeWithSnippet(snippet),
    targetElements,
  )

  const interactionSessionWithoutMetadata = createMouseInteractionForTests(
    null as any, // the strategy does not use this
    modifiers,
    { type: 'RESIZE_HANDLE', edgePosition: edgePosition },
    canvasPoint({ x: 15, y: 25 }),
  )

  const strategyResult = absoluteResizeBoundingBoxStrategy.apply(
    pickCanvasStateFromEditorState(initialEditor),
    { ...interactionSessionWithoutMetadata, metadata: {} },
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: {
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb'),
          element: left('div'),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
          globalFrame: { height: 120, width: 100, x: 30, y: 50 },
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:aaa/ccc': {
          elementPath: fromString('scene-aaa/app-entity:aaa/ccc'),
          element: left('div'),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
          globalFrame: { height: 110, width: 100, x: 90, y: 40 },
        } as ElementInstanceMetadata,
      },
    } as StrategyState,
  )
  return foldAndApplyCommands(initialEditor, initialEditor, [], [], strategyResult, 'permanent')
    .editorState
}

describe('Absolute Resize Bounding Box Strategy', () => {
  it('works with element resized from TL corner', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(snippet, selectedElements, edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 73,
              left: 45,
              width: 91,
              height: 97,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 65,
              left: 99,
              bottom: 246,
              right: 210
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element aspect ratio locked resized from TL corner', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      shiftModifier,
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 66,
              left: 51,
              width: 87,
              height: 104,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 57,
              left: 103,
              bottom: 247,
              right: 210
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element resized from BR corner', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(snippet, selectedElements, edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 52,
              left: 30,
              width: 109,
              height: 143,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 40,
              left: 96,
              bottom: 229,
              right: 195
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element aspect ratio locked resized from BR corner', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      shiftModifier,
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 51,
              left: 30,
              width: 113,
              height: 136,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 40,
              left: 98,
              bottom: 235,
              right: 189
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element resized from RIGHT edge', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 0.5 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(snippet, selectedElements, edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 50,
              left: 30,
              width: 109,
              height: 120,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 40,
              left: 96,
              bottom: 250,
              right: 195
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element aspect ratio locked resized from RIGHT edge', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 0.5 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      shiftModifier,
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 45,
              left: 30,
              width: 109,
              height: 131,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 34,
              left: 96,
              bottom: 246,
              right: 195
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element resized from TOP edge', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 0 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(snippet, selectedElements, edgePosition)
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 73,
              left: 30,
              width: 100,
              height: 97,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 65,
              left: 90,
              bottom: 246,
              right: 210
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element aspect ratio locked resized from TOP edge', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 0 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      shiftModifier,
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 73,
              left: 45,
              width: 81,
              height: 97,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 65,
              left: 93,
              bottom: 246,
              right: 226
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
})

describe('Center based resize strategy', () => {
  it('works with element resized from TL corner', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const modifiers: Modifiers = {
      alt: true,
      cmd: false,
      ctrl: false,
      shift: false,
    }
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      modifiers,
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 71,
              left: 45,
              width: 81,
              height: 74,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 65,
              left: 94,
              bottom: 267,
              right: 225
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
  it('works with element resized from Bottom edge', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 1 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 50,
        left: 30,
        width: 100,
        height: 120,
       }}
      data-uid='bbb'
    />
    <div
      style={{ 
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        top: 40,
        left: 90,
        bottom: 250,
        right: 210
       }}
      data-uid='ccc'
    />
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
    ]
    const modifiers: Modifiers = {
      alt: true,
      cmd: false,
      ctrl: false,
      shift: false,
    }
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      modifiers,
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 29,
              left: 30,
              width: 100,
              height: 166,
            }}
            data-uid='bbb'
          />
          <div
            style={{ 
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              top: 15,
              left: 90,
              bottom: 233,
              right: 210
            }}
            data-uid='ccc'
          />
        </div>`,
      ),
    )
  })
})
