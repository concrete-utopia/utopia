import { createBuiltInDependenciesList } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { left, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import {
  emptyComments,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxElement,
  jsxElementName,
} from '../../../../core/shared/element-template'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import { canvasPoint, canvasRectangle } from '../../../../core/shared/math-utils'
import type { EditorState } from '../../../editor/store/editor-state'
import { deriveState } from '../../../editor/store/editor-state'
import { patchedCreateRemixDerivedDataMemo } from '../../../editor/store/remix-derived-data'
import { foldAndApplyCommands } from '../../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../../ui-jsx.test-utils'
import { pickCanvasStateFromEditorStateWithMetadata } from '../canvas-strategies'
import { defaultCustomStrategyState } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { boundingArea } from '../interaction-state'
import { createMouseInteractionForTests } from '../interaction-state.test-utils'
import { reparentMetaStrategy } from './reparent-metastrategy'

jest.mock('../../canvas-utils', () => ({
  ...jest.requireActual('../../canvas-utils'),
  getReparentTarget: () => ({
    shouldReparent: true,
    newParent: {
      type: 'elementpath',
      parts: [
        ['sb', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ],
    },
  }),
}))

// KEEP THIS IN SYNC WITH THE MOCK ABOVE
const newParent = EP.elementPath([
  ['sb', 'scene-aaa', 'app-entity'],
  ['aaa', 'bbb'],
])

function reparentElement(
  editorState: EditorState,
  targetParentWithSpecialContentBox: boolean,
  dragVector: CanvasPoint = canvasPoint({ x: 15, y: 15 }),
): EditorState {
  const startingMetadata = {
    sb: {
      elementPath: EP.elementPath([['sb']]),
      element: left('storyboard'),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'sb/scene-aaa': {
      elementPath: EP.elementPath([['sb', 'scene-aaa']]),
      element: left('div'),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'sb/scene-aaa/app-entity': {
      elementPath: EP.elementPath([['sb', 'scene-aaa', 'app-entity']]),
      element: left('div'),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'sb/scene-aaa/app-entity:aaa': {
      elementPath: EP.elementPath([['sb', 'scene-aaa', 'app-entity'], ['aaa']]),
      element: left('div'),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'sb/scene-aaa/app-entity:aaa/bbb': {
      elementPath: EP.elementPath([
        ['sb', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      element: left('div'),
      globalFrame: canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: targetParentWithSpecialContentBox
          ? canvasRectangle({ x: 90, y: 100, width: 170, height: 120 })
          : canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'sb/scene-aaa/app-entity:aaa/ccc': {
      elementPath: EP.elementPath([
        ['sb', 'scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
      element: right(
        jsxElement(
          jsxElementName('div', []),
          'ccc',
          jsxAttributesFromMap({
            style: jsExpressionValue(
              {
                position: 'absolute',
                width: 20,
                height: 30,
                top: 75,
                left: 90,
              },
              emptyComments,
            ),
            'data-uid': jsExpressionValue('ccc', emptyComments),
          }),
          [],
        ),
      ),
      globalFrame: canvasRectangle({ x: 150, y: 160, width: 250, height: 200 }),
      specialSizeMeasurements: {
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 150, y: 160, width: 250, height: 200 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'sb/scene-aaa/app-entity:aaa/ddd': {
      elementPath: EP.elementPath([
        ['sb', 'scene-aaa', 'app-entity'],
        ['aaa', 'ddd'],
      ]),
      element: right(
        jsxElement(
          jsxElementName('div', []),
          'ddd',
          jsxAttributesFromMap({
            style: jsExpressionValue(
              {
                position: 'absolute',
                width: 20,
                height: 30,
                top: 75,
                left: 90,
              },
              emptyComments,
            ),
            'data-uid': jsExpressionValue('ddd', emptyComments),
          }),
          [],
        ),
      ),
      globalFrame: canvasRectangle({ x: 150, y: 160, width: 250, height: 200 }),
      specialSizeMeasurements: {
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 150, y: 160, width: 250, height: 200 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }

  const startPoint = canvasPoint({ x: 95, y: 80 })

  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      startPoint,
      { cmd: true, alt: false, shift: false, ctrl: false },
      boundingArea(),
      dragVector,
    ),
    latestMetadata: null as any, // the strategy does not use this
    latestAllElementProps: null as any, // the strategy does not use this
    latestElementPathTree: null as any, // the strategy does not use this
    latestVariablesInScope: null as any, // the strategy does not use this
  }

  const canvasState = pickCanvasStateFromEditorStateWithMetadata(
    editorState,
    createBuiltInDependenciesList(null),
    startingMetadata,
  )

  const reparentStrategies = reparentMetaStrategy(
    canvasState,
    interactionSession,
    defaultCustomStrategyState(),
  )

  if (reparentStrategies.length === 0) {
    return editorState
  } else {
    const defaultReparentStrategy = reparentStrategies[0]
    const strategyResult = defaultReparentStrategy.apply('end-interaction')

    expect(strategyResult.customStatePatch).toEqual({})
    expect(strategyResult.status).toEqual('success')

    expect(
      strategyResult.elementsToRerender === 'rerender-all-elements'
        ? []
        : strategyResult.elementsToRerender.map(EP.parentPath),
    ).toEqual([newParent])

    return foldAndApplyCommands(
      editorState,
      editorState,
      [],
      strategyResult.commands,
      'end-interaction',
    ).editorState
  }
}

xdescribe('Absolute Reparent Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
    const targetElement = EP.elementPath([
      ['sb', 'scene-aaa', 'app-entity'],
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
      ['sb', 'scene-aaa', 'app-entity'],
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
      ['sb', 'scene-aaa', 'app-entity'],
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
      ['sb', 'scene-aaa', 'app-entity'],
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
      ['sb', 'scene-aaa', 'app-entity'],
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
      ['sb', 'scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])

    const targetElement2 = EP.elementPath([
      ['sb', 'scene-aaa', 'app-entity'],
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
      ['sb', 'scene-aaa', 'app-entity'],
      ['aaa', 'ccc'],
    ])
    const targetElement2 = EP.elementPath([
      ['sb', 'scene-aaa', 'app-entity'],
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
