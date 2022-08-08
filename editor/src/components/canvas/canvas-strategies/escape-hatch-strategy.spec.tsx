import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { CanvasPoint, canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
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
    globalFrame: { x: 0, y: 0, width: 250, height: 300 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
}
const simpleMetadataPercentValue = {
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ]),
    localFrame: { x: 0, y: 0, width: 200, height: 80 },
    globalFrame: { x: 0, y: 0, width: 200, height: 80 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
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
    globalFrame: { x: 0, y: 0, width: 250, height: 100 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ccc': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ]),
    localFrame: { x: 15, y: 115, width: 125, height: 50 },
    globalFrame: { x: 15, y: 115, width: 125, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ddd': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ddd'],
    ]),
    localFrame: { x: 0, y: 280, width: 100, height: 50 },
    globalFrame: { x: 0, y: 280, width: 100, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
}

const mixedPinsMetadata = {
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ]),
    localFrame: { x: 0, y: 0, width: 400, height: 19 },
    globalFrame: { x: 0, y: 0, width: 400, height: 19 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ccc': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ]),
    localFrame: { x: 0, y: 19, width: 65, height: 50 },
    globalFrame: { x: 0, y: 19, width: 65, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ddd': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ddd'],
    ]),
    localFrame: { x: 0, y: 69, width: 65, height: 50 },
    globalFrame: { x: 0, y: 69, width: 65, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/eee': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'eee'],
    ]),
    localFrame: { x: 0, y: 119, width: 65, height: 50 },
    globalFrame: { x: 0, y: 119, width: 65, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/fff': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'fff'],
    ]),
    localFrame: { x: 0, y: 169, width: 65, height: 50 },
    globalFrame: { x: 0, y: 169, width: 65, height: 50 },
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
  } as ElementInstanceMetadata,
}

function dragBy15Pixels(editorState: EditorState, metadata: ElementInstanceMetadataMap) {
  return dragByPixels(editorState, metadata, canvasPoint({ x: 15, y: 15 }))
}

function dragByPixels(
  editorState: EditorState,
  metadata: ElementInstanceMetadataMap,
  dragVector: CanvasPoint,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      null as any, // the strategy does not use this
      emptyModifiers,
      null as any, // the strategy does not use this
      dragVector,
    ),
    metadata: null as any, // the strategy does not use this
    allElementProps: null as any, // the strategy does not use this
  }

  const strategyResult = escapeHatchStrategy.apply(
    pickCanvasStateFromEditorState(editorState, createBuiltInDependenciesList(null)),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: metadata,
      customStrategyState: {
        escapeHatchActivated: true,
      },
    } as StrategyState,
  )

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    [],
    strategyResult.commands,
    'permanent',
  ).editorState

  return finalEditor
}

describe('Escape Hatch Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
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

    const finalEditor = dragByPixels(initialEditor, simpleMetadata, canvasPoint({ x: 1, y: 1 }))

    expect(finalEditor).toEqual(initialEditor)
  })
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
  it('works on a flow element without siblings where width and height is percentage', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ position: 'relative', width: 400, height: 400 }} data-uid='aaa'>
      <div
        style={{ backgroundColor: '#0091FFAA', width: '50%', height: '20%' }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor, simpleMetadataPercentValue)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ position: 'relative', width: 400, height: 400 }} data-uid='aaa'>
          <div
            style={{
              backgroundColor: '#0091FFAA',
              width: '50%',
              height: '20%',
              position: 'absolute',
              left: 15,
              top: 15,
            }}
            data-uid='bbb'
          />
      </View>`,
      ),
    )
  })
  it('works on a flow element with all pins', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{
          backgroundColor: '#0091FFAA',
          width: '50%',
          height: '20%',
          right: 200,
          bottom: 320,
          top: 0,
          left: 0
        }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor, simpleMetadataPercentValue)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', width: '50%', height: '20%', right: 185, bottom: 305, top: 15, left: 15, position: 'absolute', }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  xit('works on a flow element with lots of siblings', async () => {
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
  xit('works on a flow element with lots of siblings and mixed frame pins', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
        <div data-uid='bbb' style={{}}><div data-uid='sad' style={{ width: 400, height: 19 }} /></div>
        <div
          style={{ bottom: 0, height: 50, width: 65, right: 0 }}
          data-uid='ccc'
        />
        <div
          style={{ top: 10, height: 50, width: 65, right: 335 }}
          data-uid='ddd'
        />
        <div
          style={{ top: 20, height: 50, width: 65, left: 0 }}
          data-uid='eee'
        />
        <div
          style={{ top: 10, height: 50, width: 65, left: 0, bottom: 60, right: 10 }}
          data-uid='fff'
        />
    </div>
      `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor, mixedPinsMetadata)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            left: 15,
            width: 400,
            top: 15,
            height: 19,
          }}
        >
          <div
            data-uid='sad'
            style={{ width: 400, height: 19 }}
          />
        </div>
        <div
          style={{
            bottom: 331,
            height: 50,
            width: 65,
            right: 335,
            position: 'absolute',
          }}
          data-uid='ccc'
        />
        <div
          style={{
            top: 69,
            height: 50,
            width: 65,
            right: 335,
            position: 'absolute',
          }}
          data-uid='ddd'
        />
        <div
          style={{
            top: 119,
            height: 50,
            width: 65,
            left: 0,
            position: 'absolute',
          }}
          data-uid='eee'
        />
        <div
          style={{
            top: 169,
            height: 50,
            width: 65,
            left: 0,
            bottom: 181,
            right: 335,
            position: 'absolute',
          }}
          data-uid='fff'
        />
      </div>`,
      ),
    )
  })
})
