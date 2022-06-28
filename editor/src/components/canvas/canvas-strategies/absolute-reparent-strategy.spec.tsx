import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { CanvasPoint, canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { defaultCustomStrategyState } from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import * as EP from '../../../core/shared/element-path'

jest.mock('../canvas-utils', () => ({
  ...jest.requireActual('../canvas-utils'),
  getReparentTarget: () => ({
    shouldReparent: true,
    newParent: {
      type: 'elementpath',
      parts: [
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ],
    },
  }),
}))

// KEEP THIS IN SYNC WITH THE MOCK ABOVE
const newParent = EP.elementPath([
  ['scene-aaa', 'app-entity'],
  ['aaa', 'bbb'],
])

function reparentElement(
  editorState: EditorState,
  targetParentWithSpecialContentBox: boolean,
  dragVector: CanvasPoint = canvasPoint({ x: 15, y: 15 }),
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      null as any, // the strategy does not use this
      { cmd: true, alt: false, shift: false, ctrl: false },
      null as any, // the strategy does not use this
      dragVector,
    ),
    metadata: null as any, // the strategy does not use this
    allElementProps: null as any, // the strategy does not use this
  }

  const strategyResult = absoluteReparentStrategy.apply(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: {
        'scene-aaa/app-entity:aaa': {
          elementPath: EP.elementPath([['scene-aaa', 'app-entity'], ['aaa']]),
          globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            providesBoundsForChildren: true,
            globalContentBox: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: EP.elementPath([
            ['scene-aaa', 'app-entity'],
            ['aaa', 'bbb'],
          ]),
          globalFrame: canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            providesBoundsForChildren: true,
            globalContentBox: targetParentWithSpecialContentBox
              ? canvasRectangle({ x: 90, y: 100, width: 170, height: 120 })
              : canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
      },
      startingAllElementProps: {},
      customStrategyState: defaultCustomStrategyState(),
    } as StrategyState,
  )

  expect(strategyResult.customState).toBeNull()

  // Check if there are set SetElementsToRerenderCommands with the new parent path
  expect(
    strategyResult.commands.find(
      (c) =>
        c.type === 'SET_ELEMENTS_TO_RERENDER_COMMAND' &&
        c.value !== 'rerender-all-elements' &&
        c.value.every((p) => EP.pathsEqual(EP.parentPath(p), newParent)),
    ),
  ).not.toBeNull()

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

describe('Absolute Reparent Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
    const targetElement = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            width: 20,
            height: 30,
            top: 75,
            left: 90,
          }}
        />
      </div>
      `),
      [targetElement],
    )

    const finalEditor = reparentElement(initialEditor, false, canvasPoint({ x: 1, y: 1 }))

    expect(finalEditor).toEqual(initialEditor)
  })
  it('works with a TL pinned absolute element', async () => {
    const targetElement = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            width: 20,
            height: 30,
            top: 75,
            left: 90,
          }}
        />
      </div>
      `),
      [targetElement],
    )

    const finalEditor = reparentElement(initialEditor, false)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `
        <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
        >
          <div
            data-uid='bbb'
            style={{
              position: 'absolute',
              width: 250,
              height: 200,
              top: 60,
              left: 50,
            }}
          >
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                width: 20,
                height: 30,
                top: 30,
                left: 55,
              }}
            />
          </div>
        </div>
        `,
      ),
    )
  })

  it('works with a TLBR pinned absolute element', async () => {
    const targetElement = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            top: 75,
            left: 90,
            bottom: 295,
            right: 290
          }}
        />
      </div>
      `),
      [targetElement],
    )

    const finalEditor = reparentElement(initialEditor, false)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            position: 'relative',
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
          }}
        >
          <div
            data-uid='bbb'
            style={{
              position: 'absolute',
              width: 250,
              height: 200,
              top: 60,
              left: 50,
            }}
          >
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                top: 30,
                left: 55,
                bottom: 140,
                right: 175
              }}
            />
          </div>
        </div>
        `),
    )
  })
  it('works with a TLBR pinned absolute element when the parent has padding and border', async () => {
    const targetElement = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            border: '40px solid grey',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            top: 105,
            left: 120,
            bottom: 240,
            right: 240,
          }}
        />
      </div>
      `),
      [targetElement],
    )

    const finalEditor = reparentElement(initialEditor, true)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            position: 'relative',
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
          }}
        >
          <div
            data-uid='bbb'
            style={{
              position: 'absolute',
              border: '40px solid grey',
              width: 250,
              height: 200,
              top: 60,
              left: 50,
            }}
          >
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                top: 20,
                left: 45,
                bottom: 45,
                right: 85,
              }}
            />
          </div>
        </div>
        `),
    )
  })

  it('works with a TL pinned absolute element with child', async () => {
    const targetElement = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            width: 20,
            height: 30,
            top: 75,
            left: 90,
          }}
        >
          <div
            data-uid='ddd'
            style={{
              position: 'absolute',
              width: 10,
              height: 10,
              top: 30,
              left: 40,
            }}
          />
        </div>
      </div>
      `),
      [targetElement],
    )

    const finalEditor = reparentElement(initialEditor, false)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `
        <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
        >
          <div
            data-uid='bbb'
            style={{
              position: 'absolute',
              width: 250,
              height: 200,
              top: 60,
              left: 50,
            }}
          >
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                width: 20,
                height: 30,
                top: 30,
                left: 55,
              }}
            >
              <div
                data-uid='ddd'
                style={{
                  position: 'absolute',
                  width: 10,
                  height: 10,
                  top: 30,
                  left: 40,
                }}
              />
            </div>
          </div>
        </div>
        `,
      ),
    )
  })

  it('works with TL pinned absolute elements in multiselection', async () => {
    const targetElement1 = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const targetElement2 = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ddd'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            width: 20,
            height: 30,
            top: 75,
            left: 90,
          }}
        />
        <div
          data-uid='ddd'
          style={{
            position: 'absolute',
            width: 10,
            height: 10,
            top: 30,
            left: 40,
          }}
        />
      </div>
      `),
      [targetElement1, targetElement2],
    )

    const finalEditor = reparentElement(initialEditor, false)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `
        <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
        >
          <div
            data-uid='bbb'
            style={{
              position: 'absolute',
              width: 250,
              height: 200,
              top: 60,
              left: 50,
            }}
          >
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                width: 20,
                height: 30,
                top: 30,
                left: 55,
              }}
            />
            <div
              data-uid='ddd'
              style={{
                position: 'absolute',
                width: 10,
                height: 10,
                top: -15,
                left: 5,
              }}
            />
          </div>
        </div>
        `,
      ),
    )
  })

  it('works with a TL pinned absolute elements in multiselection with descendant', async () => {
    const targetElement = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])
    const targetElement2 = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc', 'ddd'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            width: 250,
            height: 200,
            top: 60,
            left: 50,
          }}
        />
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            width: 20,
            height: 30,
            top: 75,
            left: 90,
          }}
        >
          <div
            data-uid='ddd'
            style={{
              position: 'absolute',
              width: 10,
              height: 10,
              top: 30,
              left: 40,
            }}
          />
        </div>
      </div>
      `),
      [targetElement],
    )

    const finalEditor = reparentElement(initialEditor, false)

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(
        `
        <div
        data-uid='aaa'
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
        >
          <div
            data-uid='bbb'
            style={{
              position: 'absolute',
              width: 250,
              height: 200,
              top: 60,
              left: 50,
            }}
          >
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                width: 20,
                height: 30,
                top: 30,
                left: 55,
              }}
            >
              <div
                data-uid='ddd'
                style={{
                  position: 'absolute',
                  width: 10,
                  height: 10,
                  top: 30,
                  left: 40,
                }}
              />
            </div>
          </div>
        </div>
        `,
      ),
    )
  })
})
