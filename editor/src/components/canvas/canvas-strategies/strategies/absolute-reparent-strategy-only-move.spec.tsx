import { createBuiltInDependenciesList } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { elementPath } from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import { canvasPoint, canvasRectangle } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import type { AllElementProps, EditorState } from '../../../editor/store/editor-state'
import { foldAndApplyCommands } from '../../commands/commands'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../../ui-jsx.test-utils'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { pickCanvasStateFromEditorStateWithMetadata } from '../canvas-strategies'
import { defaultCustomStrategyState } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { boundingArea } from '../interaction-state'
import { createMouseInteractionForTests } from '../interaction-state.test-utils'

jest.mock('../../canvas-utils', () => ({
  ...jest.requireActual('../../canvas-utils'),
  getReparentTarget: () => ({
    shouldReparent: false,
    newParent: null,
  }),
}))

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

function dragBy15Pixels(editorState: EditorState): EditorState {
  return dragByPixels(editorState, canvasPoint({ x: 15, y: 15 }), cmdModifier)
}

function dragByPixels(
  editorState: EditorState,
  vector: CanvasVector,
  modifiers: Modifiers,
): EditorState {
  const startingMetadata = {
    'scene-aaa/app-entity:aaa/bbb': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      specialSizeMeasurements: {
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'scene-aaa/app-entity:aaa/ccc': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
      specialSizeMeasurements: {
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }
  const startingAllElementProps: AllElementProps = {
    'scene-aaa/app-entity:aaa/bbb': {
      style: {
        width: 400,
        height: 400,
      },
    },
    'scene-aaa/app-entity:aaa/ccc': {
      style: {
        width: 400,
        height: 400,
      },
    },
  }
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      canvasPoint({ x: 0, y: 0 }),
      modifiers,
      boundingArea(),
      vector,
    ),
    latestMetadata: null as any, // the strategy does not use this
    latestAllElementProps: null as any, // the strategy does not use this
    latestElementPathTree: null as any, // the strategy does not use this
  }

  const strategyResult = absoluteMoveStrategy(
    pickCanvasStateFromEditorStateWithMetadata(
      editorState,
      createBuiltInDependenciesList(null),
      startingMetadata,
      startingAllElementProps,
    ),
    interactionSession,
    defaultCustomStrategyState(),
  )!.strategy.apply('end-interaction')

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    strategyResult.commands,
    'end-interaction',
  ).editorState

  return finalEditor
}

describe('Absolute Reparent Strategy without new parent', () => {
  it('does not activate when drag threshold is not reached', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 1, y: 1 }), cmdModifier)

    expect(finalEditor).toEqual(initialEditor)
  })
  it('works with a TL pinned absolute element', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('works with a TL pinned absolute element with px values', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: 50, width: 250, height: 300 }}
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '65px', top: 65, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('works with a RB pinned absolute element', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 50, bottom: 50, width: 250, height: 300 }}
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 35, bottom: 35, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('works with a TLRB pinned absolute element', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, right: 50, bottom: 50, width: 250, height: 300 }}
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, right: 35, bottom: 35, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  // TODO needs design review
  it('keeps expressions intact', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50 + 5, top: 50 + props.top, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      testPrintCodeFromEditorState(initialEditor),
    )
  })

  it('works with percentages', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '25%', top: '0%', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor)

    expect(testPrintCodeFromEditorState(finalEditor)).not.toEqual(
      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '28.75%', top: '3.75%', width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
        ),
      ),
    )
  })
})

describe('Axis locked move', () => {
  it('works with a TL pinned absolute element', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const modifiers: Modifiers = {
      alt: false,
      cmd: false,
      ctrl: false,
      shift: true,
    }

    const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 10, y: 20 }), modifiers)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 70, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a TLBR pinned absolute element', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, bottom: 250, right: 200 }}
        data-uid='bbb'
      />
    </View>
    `,
      [targetElement],
    )

    const modifiers: Modifiers = {
      alt: false,
      cmd: false,
      ctrl: false,
      shift: true,
    }

    const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 25, y: 10 }), modifiers)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 75, top: 50, bottom: 250, right: 175 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('works with a TL pinned absolute element with child', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      >
        <View
          style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
          data-uid='ccc'
        />
      </View>
    </View>
    `,
      [targetElement],
    )

    const finalEditor = dragBy15Pixels(initialEditor)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
          data-uid='bbb'
        >
          <View
            style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
            data-uid='ccc'
          />
        </View>
      </View>`,
      ),
    )
  })
  it('works with TL pinned absolute elements in multiselection', async () => {
    const targetElement1 = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])
    const targetElement2 = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
      <View
        style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
        data-uid='ccc'
      />
    </View>
    `,
      [targetElement1, targetElement2],
    )

    const finalEditor = dragBy15Pixels(initialEditor)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
          data-uid='bbb'
        />
        <View
          style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 115, top: 115, width: 250, height: 300 }}
          data-uid='ccc'
        />
      </View>`,
      ),
    )
  })
  it('works with TL pinned absolute elements in multiselection with descendant', async () => {
    const targetElement1 = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])
    const targetElement2 = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb', 'ccc'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      >
        <View
          style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
          data-uid='ccc'
        />
      </View>
    </View>
    `,
      [targetElement1, targetElement2],
    )

    const finalEditor = dragBy15Pixels(initialEditor)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
          data-uid='bbb'
        >
          <View
            style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
            data-uid='ccc'
          />
        </View>
      </View>`,
      ),
    )
  })
})
