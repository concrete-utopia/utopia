import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { emptyModifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { escapeHatchStrategy } from './escape-hatch-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

const simpleMetadata = {
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ]),
    localFrame: { x: 0, y: 0, width: 250, height: 300 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
}

const complexMetadata = {
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ]),
    localFrame: { x: 0, y: 0, width: 250, height: 100 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ccc': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ]),
    localFrame: { x: 15, y: 115, width: 125, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ddd': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ddd'],
    ]),
    localFrame: { x: 0, y: 280, width: 100, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
}

function dragBy15Pixels(
  editorState: EditorState,
  metadata: ElementInstanceMetadataMap,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      null as any, // the strategy does not use this
      emptyModifiers,
      null as any, // the strategy does not use this
      canvasPoint({ x: 15, y: 15 }),
    ),
    metadata: null as any, // the strategy does not use this
  }

  const strategyResult = escapeHatchStrategy.apply(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: metadata,
    } as StrategyState,
  )

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    strategyResult.commands,
    'permanent',
  ).editorState

  return finalEditor
}

describe('Escape Hatch Strategy', () => {
  it('works on a flow element without siblings', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor, simpleMetadata)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', width: 250, height: 300, position: 'absolute', left: 15, top: 15  }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('works on a flow element with lots of siblings', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{ backgroundColor: '#0091FFAA', width: 250, height: 100 }}
        data-uid='bbb'
      />
      <div
        style={{ backgroundColor: '#0091FFAA', width: 125, height: 150, margin: 15 }}
        data-uid='ccc'
      />
      <div
        style={{ backgroundColor: '#0091FFAA', width: 100, height: 50 }}
        data-uid='ddd'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor, complexMetadata)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <div
          style={{
            backgroundColor: '#0091FFAA',
            width: 250,
            height: 100,
            position: 'absolute',
            left: 15,
            top: 15,
          }}
          data-uid='bbb'
        />
        <div
          style={{
            backgroundColor: '#0091FFAA',
            width: 125,
            height: 50,
            margin: 15,
            position: 'absolute',
            left: 15,
            top: 115,
          }}
          data-uid='ccc'
          />
        <div
          style={{
            backgroundColor: '#0091FFAA',
            width: 100,
            height: 50,
            position: 'absolute',
            left: 0,
            top: 280,
          }}
          data-uid='ddd'
        />
      </View>`,
      ),
    )
  })
})
