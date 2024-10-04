import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { jsxElement } from '../../../core/shared/element-template'
import { canvasPoint } from '../../../core/shared/math-utils'
import { NO_OP } from '../../../core/shared/utils'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../../core/workers/workers'
import { emptyModifiers } from '../../../utils/modifiers'
import CanvasActions from '../../canvas/canvas-actions'
import type { MetaCanvasStrategy } from '../../canvas/canvas-strategies/canvas-strategies'
import { RegisteredCanvasStrategies } from '../../canvas/canvas-strategies/canvas-strategies'
import type {
  InteractionCanvasState,
  StrategyApplicationResult,
} from '../../canvas/canvas-strategies/canvas-strategy-types'
import { strategyApplicationResult } from '../../canvas/canvas-strategies/canvas-strategy-types'
import type {
  InteractionSession,
  InteractionSessionWithoutMetadata,
} from '../../canvas/canvas-strategies/interaction-state'
import {
  boundingArea,
  createEmptyStrategyState,
  createInteractionViaKeyboard,
  createInteractionViaMouse,
  updateInteractionViaMouse,
} from '../../canvas/canvas-strategies/interaction-state'
import { runCanvasCommand } from '../../canvas/commands/commands'
import { wildcardPatch } from '../../canvas/commands/wildcard-patch-command'
import { emptyUiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { toggleBackgroundLayers, toggleStylePropPaths } from '../../inspector/common/css-utils'
import { EditorDispatch, notLoggedIn } from '../action-types'
import { saveDOMReport, selectComponents, toggleProperty } from '../actions/action-creators'
import * as History from '../history'
import { InitialOnlineState } from '../online-status'
import { DummyPersistenceMachine } from '../persistence/persistence.test-utils'
import type { DispatchResult } from './dispatch'
import {
  handleStrategies,
  interactionCancel,
  interactionHardReset,
  interactionStart,
  interactionUpdate,
} from './dispatch-strategies'
import type { EditorStoreFull } from './editor-state'
import { createEditorState, deriveState, emptyCollaborativeEditingSupport } from './editor-state'
import { emptyProjectServerState } from './project-server-state'
import { unpatchedCreateRemixDerivedDataMemo } from './remix-derived-data'

function createEditorStore(
  interactionSession: InteractionSessionWithoutMetadata | null,
): EditorStoreFull {
  let emptyEditorState = createEditorState(NO_OP)
  let interactionSessionWithMetadata: InteractionSession | null = null
  if (interactionSession != null) {
    interactionSessionWithMetadata = {
      ...interactionSession,
      latestMetadata: {},
      latestAllElementProps: {},
      latestElementPathTree: {},
      latestVariablesInScope: {},
    }
  }

  emptyEditorState.canvas.interactionSession = interactionSessionWithMetadata
  const derivedState = deriveState(
    emptyEditorState,
    null,
    'unpatched',
    unpatchedCreateRemixDerivedDataMemo,
  )

  const history = History.init(emptyEditorState, derivedState)

  const initialEditorStore: EditorStoreFull = {
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    unpatchedDerived: derivedState,
    patchedDerived: derivedState,
    strategyState: createEmptyStrategyState({}, {}, {}),
    history: history,
    userState: {
      loginState: notLoggedIn,
      shortcutConfig: {},
      themeConfig: 'light',
      githubState: {
        authenticated: false,
        gitRepoToLoad: null,
      },
    },
    workers: new UtopiaTsWorkersImplementation(
      new FakeParserPrinterWorker(),
      new FakeLinterWorker(),
      new FakeWatchdogWorker(),
    ),
    persistence: DummyPersistenceMachine,
    saveCountThisSession: 0,
    builtInDependencies: createBuiltInDependenciesList(null),
    elementMetadata: {},
    postActionInteractionSession: null,
    projectServerState: emptyProjectServerState(),
    collaborativeEditingSupport: emptyCollaborativeEditingSupport(),
    onlineState: InitialOnlineState,
  }

  return initialEditorStore
}

function dispatchResultFromEditorStore(editorStore: EditorStoreFull): DispatchResult {
  return {
    ...editorStore,
    nothingChanged: true,
    entireUpdateFinished: Promise.resolve(),
  }
}

describe('interactionCancel', () => {
  it('returns a clean state', () => {
    let editorStore = createEditorStore(
      createInteractionViaMouse(
        canvasPoint({ x: 100, y: 200 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        boundingArea(),
        'zero-drag-not-permitted',
      ),
    )
    const actualResult = interactionCancel(editorStore, dispatchResultFromEditorStore(editorStore))
    expect(actualResult.newStrategyState.commandDescriptions).toHaveLength(0)
    expect(actualResult.newStrategyState.currentStrategyCommands).toHaveLength(0)
    expect(actualResult.newStrategyState.currentStrategy).toBeNull()
  })
})

const testStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => [
  {
    id: 'TEST_STRATEGY',
    name: 'Test Strategy',
    controlsToRender: [],
    fitness: 10,
    apply: function (): StrategyApplicationResult {
      return strategyApplicationResult(
        [wildcardPatch('always', { canvas: { scale: { $set: 100 } } })],
        'rerender-all-elements',
      )
    },
    descriptiveLabel: 'A Test Strategy',
    icon: {
      category: 'modalities',
      type: 'magic-large',
    },
  },
]

describe('interactionStart', () => {
  it('creates the initial state with a simple test strategy', () => {
    const editorStore = createEditorStore(
      createInteractionViaMouse(
        canvasPoint({ x: 100, y: 200 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        boundingArea(),
        'zero-drag-not-permitted',
      ),
    )
    const actualResult = interactionStart(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyDescriptiveLabel": null,
        "currentStrategyFitness": 0,
        "currentStrategyIcon": null,
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": null,
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "_accumulatedMovement": Object {
          "x": 0,
          "y": 0,
        },
        "drag": null,
        "dragStart": Object {
          "x": 100,
          "y": 200,
        },
        "hasMouseMoved": false,
        "modifiers": Object {
          "alt": false,
          "cmd": false,
          "ctrl": false,
          "shift": false,
        },
        "originalDragStart": Object {
          "x": 100,
          "y": 200,
        },
        "prevDrag": null,
        "spacePressed": false,
        "type": "DRAG",
        "zeroDragPermitted": "zero-drag-not-permitted",
      }
    `)
  })
  it('potentially process a start with no interaction session', () => {
    const editorStore = createEditorStore(null)
    const actualResult = interactionStart(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyDescriptiveLabel": null,
        "currentStrategyFitness": 0,
        "currentStrategyIcon": null,
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": null,
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('interactionUpdate', () => {
  it('steps an interaction session correctly', () => {
    const startInteraction = createInteractionViaMouse(
      canvasPoint({ x: 100, y: 200 }),
      { alt: false, shift: false, ctrl: false, cmd: false },
      boundingArea(),
      'zero-drag-not-permitted',
    )

    const editorStore = createEditorStore(
      updateInteractionViaMouse(
        startInteraction,
        'DRAG',
        canvasPoint({ x: 10, y: 10 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        null,
      ),
    )
    editorStore.strategyState.currentStrategy = 'TEST_STRATEGY'
    const actualResult = interactionUpdate(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
      'non-interaction',
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [
          Object {
            "description": "Wildcard Patch: {
        \\"canvas\\": {
          \\"scale\\": {
            \\"$set\\": 100
          }
        }
      }",
            "transient": false,
          },
        ],
        "currentStrategy": "TEST_STRATEGY",
        "currentStrategyCommands": Array [
          Object {
            "patch": Object {
              "canvas": Object {
                "scale": Object {
                  "$set": 100,
                },
              },
            },
            "type": "WILDCARD_PATCH",
            "whenToRun": "always",
          },
        ],
        "currentStrategyDescriptiveLabel": "A Test Strategy",
        "currentStrategyFitness": 10,
        "currentStrategyIcon": Object {
          "category": "modalities",
          "type": "magic-large",
        },
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": Array [
          Object {
            "name": "Test Strategy",
            "strategy": Object {
              "apply": [Function],
              "controlsToRender": Array [],
              "descriptiveLabel": "A Test Strategy",
              "fitness": 10,
              "icon": Object {
                "category": "modalities",
                "type": "magic-large",
              },
              "id": "TEST_STRATEGY",
              "name": "Test Strategy",
            },
          },
        ],
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "_accumulatedMovement": Object {
          "x": 0,
          "y": 0,
        },
        "drag": Object {
          "x": 10,
          "y": 10,
        },
        "dragStart": Object {
          "x": 100,
          "y": 200,
        },
        "hasMouseMoved": true,
        "modifiers": Object {
          "alt": false,
          "cmd": false,
          "ctrl": false,
          "shift": false,
        },
        "originalDragStart": Object {
          "x": 100,
          "y": 200,
        },
        "prevDrag": null,
        "spacePressed": false,
        "type": "DRAG",
        "zeroDragPermitted": "zero-drag-not-permitted",
      }
    `)
  })
  it('potentially process an update with no interaction session', () => {
    const editorStore = createEditorStore(null)
    const actualResult = interactionUpdate(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
      'non-interaction',
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyDescriptiveLabel": null,
        "currentStrategyFitness": 0,
        "currentStrategyIcon": null,
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": null,
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('interactionHardReset', () => {
  it('steps an interaction session correctly', () => {
    let interactionSession = createInteractionViaMouse(
      canvasPoint({ x: 100, y: 200 }),
      { alt: false, shift: false, ctrl: false, cmd: false },
      boundingArea(),
      'zero-drag-not-permitted',
    )
    if (interactionSession.interactionData.type === 'DRAG') {
      interactionSession.interactionData.dragStart = canvasPoint({ x: 110, y: 210 })
      interactionSession.interactionData.drag = canvasPoint({ x: 50, y: 140 })
      interactionSession.interactionData.prevDrag = canvasPoint({ x: 30, y: 120 })
    }
    const editorStore = createEditorStore(interactionSession)
    const actualResult = interactionHardReset(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [
          Object {
            "description": "Wildcard Patch: {
        \\"canvas\\": {
          \\"scale\\": {
            \\"$set\\": 100
          }
        }
      }",
            "transient": false,
          },
        ],
        "currentStrategy": "TEST_STRATEGY",
        "currentStrategyCommands": Array [
          Object {
            "patch": Object {
              "canvas": Object {
                "scale": Object {
                  "$set": 100,
                },
              },
            },
            "type": "WILDCARD_PATCH",
            "whenToRun": "always",
          },
        ],
        "currentStrategyDescriptiveLabel": "A Test Strategy",
        "currentStrategyFitness": 10,
        "currentStrategyIcon": Object {
          "category": "modalities",
          "type": "magic-large",
        },
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": Array [
          Object {
            "name": "Test Strategy",
            "strategy": Object {
              "apply": [Function],
              "controlsToRender": Array [],
              "descriptiveLabel": "A Test Strategy",
              "fitness": 10,
              "icon": Object {
                "category": "modalities",
                "type": "magic-large",
              },
              "id": "TEST_STRATEGY",
              "name": "Test Strategy",
            },
          },
        ],
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "_accumulatedMovement": Object {
          "x": 0,
          "y": 0,
        },
        "drag": Object {
          "x": 50,
          "y": 140,
        },
        "dragStart": Object {
          "x": 110,
          "y": 210,
        },
        "hasMouseMoved": false,
        "modifiers": Object {
          "alt": false,
          "cmd": false,
          "ctrl": false,
          "shift": false,
        },
        "originalDragStart": Object {
          "x": 100,
          "y": 200,
        },
        "prevDrag": Object {
          "x": 30,
          "y": 120,
        },
        "spacePressed": false,
        "type": "DRAG",
        "zeroDragPermitted": "zero-drag-not-permitted",
      }
    `)
  })
  it('potentially process an update with no interaction session', () => {
    const editorStore = createEditorStore(null)
    const actualResult = interactionHardReset(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyDescriptiveLabel": null,
        "currentStrategyFitness": 0,
        "currentStrategyIcon": null,
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": null,
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('interactionUpdate with user changed strategy', () => {
  it('steps an interaction session correctly', () => {
    let interactionSession = createInteractionViaMouse(
      canvasPoint({ x: 100, y: 200 }),
      { alt: false, shift: false, ctrl: false, cmd: false },
      boundingArea(),
      'zero-drag-not-permitted',
    )
    if (interactionSession.interactionData.type === 'DRAG') {
      interactionSession.interactionData.dragStart = canvasPoint({ x: 110, y: 210 })
      interactionSession.interactionData.drag = canvasPoint({ x: 50, y: 140 })
      interactionSession.interactionData.prevDrag = canvasPoint({ x: 30, y: 120 })
      interactionSession.userPreferredStrategy = 'EMPTY_TEST_STRATEGY'
    }
    const editorStore = createEditorStore(interactionSession)

    const result = dispatchResultFromEditorStore(editorStore)
    result.unpatchedEditor = {
      ...result.unpatchedEditor,
      canvas: {
        ...result.unpatchedEditor.canvas,
        interactionSession: {
          ...result.unpatchedEditor.canvas.interactionSession!,
          userPreferredStrategy: 'TEST_STRATEGY',
        },
      },
    }

    const actualResult = interactionUpdate([testStrategy], editorStore, result, 'non-interaction')
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [
          Object {
            "description": "Strategy switched to Test Strategy by user input. Interaction data reset.",
            "transient": true,
          },
          Object {
            "description": "Wildcard Patch: {
        \\"canvas\\": {
          \\"scale\\": {
            \\"$set\\": 100
          }
        }
      }",
            "transient": false,
          },
        ],
        "currentStrategy": "TEST_STRATEGY",
        "currentStrategyCommands": Array [
          Object {
            "patch": Object {
              "canvas": Object {
                "scale": Object {
                  "$set": 100,
                },
              },
            },
            "type": "WILDCARD_PATCH",
            "whenToRun": "always",
          },
        ],
        "currentStrategyDescriptiveLabel": "A Test Strategy",
        "currentStrategyFitness": 10,
        "currentStrategyIcon": Object {
          "category": "modalities",
          "type": "magic-large",
        },
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": Array [
          Object {
            "name": "Test Strategy",
            "strategy": Object {
              "apply": [Function],
              "controlsToRender": Array [],
              "descriptiveLabel": "A Test Strategy",
              "fitness": 10,
              "icon": Object {
                "category": "modalities",
                "type": "magic-large",
              },
              "id": "TEST_STRATEGY",
              "name": "Test Strategy",
            },
          },
        ],
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "_accumulatedMovement": Object {
          "x": 0,
          "y": 0,
        },
        "drag": Object {
          "x": 50,
          "y": 140,
        },
        "dragStart": Object {
          "x": 110,
          "y": 210,
        },
        "hasMouseMoved": false,
        "modifiers": Object {
          "alt": false,
          "cmd": false,
          "ctrl": false,
          "shift": false,
        },
        "originalDragStart": Object {
          "x": 100,
          "y": 200,
        },
        "prevDrag": Object {
          "x": 30,
          "y": 120,
        },
        "spacePressed": false,
        "type": "DRAG",
        "zeroDragPermitted": "zero-drag-not-permitted",
      }
    `)
  })
  it('potentially process an update with no interaction session', () => {
    const editorStore = createEditorStore(null)
    const actualResult = interactionUpdate(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
      'non-interaction',
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyDescriptiveLabel": null,
        "currentStrategyFitness": 0,
        "currentStrategyIcon": null,
        "customStrategyState": Object {
          "action": null,
          "duplicatedElementNewUids": Object {},
          "elementsToRerender": Array [],
          "escapeHatchActivated": false,
          "grid": Object {
            "currentRootCell": null,
            "metadataCacheForGrids": Object {},
            "targetCellData": null,
          },
          "lastReorderIdx": null,
          "strategyGeneratedUidsCache": Object {},
        },
        "sortedApplicableStrategies": null,
        "startingAllElementProps": Object {},
        "startingElementPathTree": Object {},
        "startingMetadata": Object {},
        "status": "success",
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('only update metadata on SAVE_DOM_REPORT', () => {
  // eslint-disable-next-line jest/expect-expect
  it('no canvas.interactionSession', () => {
    const oldEditorStore = createEditorStore(null)

    const newMetadata: ElementInstanceMetadataMap = {
      'new-entry': {
        elementPath: EP.fromString('new-entry'),
        specialSizeMeasurements: { position: 'absolute' },
        element: right(jsxElement('div', 'aaa', [], [])),
      } as ElementInstanceMetadata,
    }

    const newEditorStore: EditorStoreFull = {
      ...oldEditorStore,
      unpatchedEditor: { ...oldEditorStore.unpatchedEditor, jsxMetadata: newMetadata },
      patchedEditor: oldEditorStore.patchedEditor,
    }

    // when new metadata is dispatched in SAVE_DOM_REPORT, only the unpatchedEditor is updated
    // we see that newEditorState's unpatchedEditor has the new metadata
    expect(newEditorStore.unpatchedEditor.jsxMetadata).not.toBe(
      oldEditorStore.unpatchedEditor.jsxMetadata,
    )
    // but newEditorState's patchedEditor has the old metadata
    expect(newEditorStore.patchedEditor.jsxMetadata).toBe(oldEditorStore.patchedEditor.jsxMetadata)

    // the job of handleStrategies in this case is to update the metadata of patchedEditor, without running any strategies
    const actualResult = handleStrategies(
      RegisteredCanvasStrategies,
      [saveDOMReport(newMetadata, [], [])],
      oldEditorStore,
      dispatchResultFromEditorStore(newEditorStore),
      oldEditorStore.patchedDerived,
    )

    expect(actualResult.patchedEditorState.jsxMetadata).toBe(
      newEditorStore.unpatchedEditor.jsxMetadata,
    )
  })

  it('has non-null canvas.interactionSession', () => {
    const oldEditorStore = createEditorStore(
      createInteractionViaMouse(
        canvasPoint({ x: 100, y: 200 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        boundingArea(),
        'zero-drag-not-permitted',
      ),
    )

    const newMetadata: ElementInstanceMetadataMap = {
      'new-entry': {
        elementPath: EP.fromString('new-entry'),
        specialSizeMeasurements: { position: 'absolute' },
        element: right(jsxElement('div', 'aaa', [], [])),
      } as ElementInstanceMetadata,
    }

    if (oldEditorStore.unpatchedEditor.canvas.interactionSession == null) {
      throw new Error('interactionSession cannot be null')
    }

    const newEditorStore: EditorStoreFull = {
      ...oldEditorStore,
      unpatchedEditor: {
        ...oldEditorStore.unpatchedEditor,
        canvas: {
          ...oldEditorStore.unpatchedEditor.canvas,
          interactionSession: {
            ...oldEditorStore.unpatchedEditor.canvas.interactionSession,
            latestMetadata: newMetadata,
          },
        },
      },
      patchedEditor: oldEditorStore.patchedEditor,
    }

    // the job of handleStrategies in this case is to update the jsxMetadata of patchedEditor using unpatchedEditor.canvas.interactionSession.metadata, without running any strategies
    const actualResult = handleStrategies(
      RegisteredCanvasStrategies,
      [saveDOMReport(newMetadata, [], [])],
      oldEditorStore,
      dispatchResultFromEditorStore(newEditorStore),
      oldEditorStore.patchedDerived,
    )

    expect(actualResult.patchedEditorState.jsxMetadata).toBe(
      newEditorStore.unpatchedEditor.canvas.interactionSession?.latestMetadata,
    )
  })

  it('InteractionSession.metadata is the latest metadata', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div data-uid="element-of-interest" style={{}}>hello!</div>`),
      'await-first-dom-report',
    )

    const targetElement = EP.elementPath([
      [BakedInStoryboardUID, TestSceneUID, TestAppUID],
      ['element-of-interest'],
    ])

    await renderResult.dispatch([selectComponents([targetElement], false)], true)

    // FIXME We need a working setup here where the TEST_STRATEGY isn't active at first, but then later becomes active,
    // however for some reason triggering multiple `updateInteractionViaMouse` calls here was resulting in `latestMetadata`
    // becoming undefined for the target element
    await renderResult.dispatch(
      [
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            canvasPoint({ x: 0, y: 0 }),
            emptyModifiers,
            boundingArea(),
            'zero-drag-not-permitted',
          ),
        ),
      ],
      true,
      [],
    )

    // toggling the backgroundColor to update the metadata
    await renderResult.dispatch(
      [toggleProperty(targetElement, toggleStylePropPaths(toggleBackgroundLayers))],
      true,
    )

    // dispatching a no-op change to the interaction session to trigger the strategies

    let testStrategyRan = false

    await renderResult.dispatch(
      [
        CanvasActions.updateInteractionSession(
          updateInteractionViaMouse(
            renderResult.getEditorState().editor.canvas.interactionSession!,
            'DRAG',
            canvasPoint({ x: 10, y: 10 }),
            { alt: false, shift: false, ctrl: false, cmd: false },
            null,
          ),
        ),
      ],
      true,
      [
        (canvasState: InteractionCanvasState, interactionSession: InteractionSession | null) => [
          {
            id: 'TEST_STRATEGY',
            name: 'Test Strategy',
            controlsToRender: [],
            fitness: 10,
            descriptiveLabel: 'A Test Strategy',
            icon: {
              category: 'modalities',
              type: 'magic-large',
            },
            apply: function (): StrategyApplicationResult {
              if (interactionSession == null) {
                return strategyApplicationResult([], 'rerender-all-elements')
              }
              expect(canvasState.startingMetadata).not.toBe(interactionSession.latestMetadata)
              expect(canvasState.startingAllElementProps).not.toBe(
                interactionSession.latestAllElementProps,
              )

              // first we make sure the _starting_ metadata and startingAllElementProps have the original undefined backgroundColor
              expect(
                canvasState.startingMetadata[EP.toString(targetElement)].computedStyle
                  ?.backgroundColor,
              ).toBeUndefined()
              expect(
                canvasState.startingAllElementProps[EP.toString(targetElement)].style
                  .backgroundColor,
              ).toBeUndefined()

              // then we check that the latestMetadata and latestAllElementProps have a backgroundColor defined, as a result of the previous toggleProperty dispatch
              expect(
                interactionSession.latestMetadata[EP.toString(targetElement)].computedStyle
                  ?.backgroundColor,
              ).toBeDefined()
              expect(
                interactionSession.latestAllElementProps[EP.toString(targetElement)].style
                  .backgroundColor,
              ).toBeDefined()
              testStrategyRan = true
              return strategyApplicationResult([], 'rerender-all-elements')
            },
          },
        ],
      ],
    )

    expect(testStrategyRan).toEqual(true)
  })
})
