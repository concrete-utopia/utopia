import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { WindowMousePositionRaw } from '../../../utils/global-positions'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

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

function reparentElement(
  editorState: EditorState,
  targetParentWithSpecialContentBox: boolean,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      null as any, // the strategy does not use this
      { cmd: true, alt: false, shift: false, ctrl: false },
      null as any, // the strategy does not use this
      canvasPoint({ x: 0, y: 0 }),
    ),
    metadata: null as any, // the strategy does not use this
  }

  const strategyResult = absoluteReparentStrategy.apply(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: {
        'scene-aaa/app-entity:aaa': {
          elementPath: elementPath([['scene-aaa', 'app-entity'], ['aaa']]),
          globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            providesBoundsForChildren: true,
            globalContentBox: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: elementPath([
            ['scene-aaa', 'app-entity'],
            ['aaa', 'bbb'],
          ]),
          globalFrame: canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            providesBoundsForChildren: true,
            globalContentBox: targetParentWithSpecialContentBox
              ? canvasRectangle({ x: 90, y: 100, width: 170, height: 120 })
              : canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
      },
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

describe('Absolute Reparent Strategy', () => {
  it('works with a TL pinned absolute element', async () => {
    const targetElement = elementPath([
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
                top: 15,
                left: 40,
              }}
            />
          </div>
        </div>
        `,
      ),
    )
  })

  it('works with a TLBR pinned absolute element', async () => {
    const targetElement = elementPath([
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
                top: 15,
                left: 40,
                bottom: 155,
                right: 190
              }}
            />
          </div>
        </div>
        `),
    )
  })
  it('works with a TLBR pinned absolute element when the parent has padding and border', async () => {
    const targetElement = elementPath([
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
                top: 5,
                left: 30,
                bottom: 60,
                right: 100,
              }}
            />
          </div>
        </div>
        `),
    )
  })

  it('works with a TL pinned absolute element with child', async () => {
    const targetElement = elementPath([
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
                top: 15,
                left: 40,
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
    const targetElement1 = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const targetElement2 = elementPath([
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
                top: 15,
                left: 40,
              }}
            />
            <div
              data-uid='ddd'
              style={{
                position: 'absolute',
                width: 10,
                height: 10,
                top: -30,
                left: -10,
              }}
            />
          </div>
        </div>
        `,
      ),
    )
  })

  it('works with a TL pinned absolute elements in multiselection with descendant', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])
    const targetElement2 = elementPath([
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
                top: 15,
                left: 40,
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
