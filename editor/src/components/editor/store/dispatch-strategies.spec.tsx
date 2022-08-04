import create from 'zustand'
import { subscribeWithSelector } from 'zustand/middleware'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { NO_OP } from '../../../core/shared/utils'
import {
  FakeLinterWorker,
  FakeParserPrinterWorker,
  FakeWatchdogWorker,
} from '../../../core/workers/test-workers'
import { UtopiaTsWorkersImplementation } from '../../../core/workers/workers'
import { emptyUiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import { EditorDispatch, notLoggedIn } from '../action-types'
import * as History from '../history'
import { DummyPersistenceMachine } from '../persistence/persistence.test-utils'
import { DispatchResult, editorDispatch } from './dispatch'
import {
  handleStrategies,
  interactionCancel,
  interactionHardReset,
  interactionStart,
  interactionUpdate,
} from './dispatch-strategies'
import { createEditorState, deriveState, EditorStoreFull } from './editor-state'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  ElementInstanceMetadata,
  elementInstanceMetadata,
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import {
  createEmptyStrategyState,
  createInteractionViaKeyboard,
  createInteractionViaMouse,
  InteractionSession,
  InteractionSessionWithoutMetadata,
  StrategyState,
} from '../../canvas/canvas-strategies/interaction-state'
import {
  CanvasStrategy,
  CanvasStrategyId,
  defaultCustomStrategyState,
  InteractionCanvasState,
  StrategyApplicationResult,
} from '../../canvas/canvas-strategies/canvas-strategy-types'
import { canvasPoint } from '../../../core/shared/math-utils'
import { wildcardPatch } from '../../canvas/commands/wildcard-patch-command'
import { runCanvasCommand } from '../../canvas/commands/commands'
import { saveDOMReport } from '../actions/action-creators'
import { RegisteredCanvasStrategies } from '../../canvas/canvas-strategies/canvas-strategies'

beforeAll(() => {
  return jest.spyOn(Date, 'now').mockReturnValue(new Date(1000).getTime())
})

afterAll(() => {
  return jest.clearAllMocks()
})

function createEditorStore(
  interactionSession: InteractionSessionWithoutMetadata | null,
  strategyState?: StrategyState,
): EditorStoreFull {
  let emptyEditorState = createEditorState(NO_OP)
  let interactionSessionWithMetadata: InteractionSession | null = null
  if (interactionSession != null) {
    interactionSessionWithMetadata = {
      ...interactionSession,
      metadata: {},
      allElementProps: {},
    }
  }

  emptyEditorState.canvas.interactionSession = interactionSessionWithMetadata
  const derivedState = deriveState(emptyEditorState, null)

  const history = History.init(emptyEditorState, derivedState)
  const spyCollector = emptyUiJsxCanvasContextData()

  const dispatch: EditorDispatch = (actions) => {
    const result = editorDispatch(dispatch, actions, storeHook.getState(), spyCollector)
    storeHook.setState(result)
  }

  const initialEditorStore: EditorStoreFull = {
    unpatchedEditor: emptyEditorState,
    patchedEditor: emptyEditorState,
    unpatchedDerived: derivedState,
    patchedDerived: derivedState,
    strategyState: strategyState ?? createEmptyStrategyState({}, {}),
    history: history,
    userState: {
      loginState: notLoggedIn,
      shortcutConfig: {},
    },
    workers: new UtopiaTsWorkersImplementation(
      new FakeParserPrinterWorker(),
      new FakeLinterWorker(),
      new FakeWatchdogWorker(),
    ),
    persistence: DummyPersistenceMachine,
    dispatch: dispatch,
    alreadySaved: false,
    builtInDependencies: createBuiltInDependenciesList(null),
  }

  const storeHook = create<EditorStoreFull>(subscribeWithSelector((set) => initialEditorStore))

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
        { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
      ),
    )
    editorStore.strategyState.accumulatedPatches = runCanvasCommand(
      editorStore.unpatchedEditor,
      wildcardPatch('permanent', { selectedViews: { $set: [] } }),
      'permanent',
    ).editorStatePatches
    const actualResult = interactionCancel(editorStore, dispatchResultFromEditorStore(editorStore))
    expect(actualResult.newStrategyState.accumulatedPatches).toHaveLength(0)
    expect(actualResult.newStrategyState.commandDescriptions).toHaveLength(0)
    expect(actualResult.newStrategyState.currentStrategyCommands).toHaveLength(0)
    expect(actualResult.newStrategyState.currentStrategy).toBeNull()
  })
})

const testStrategy: CanvasStrategy = {
  id: 'TEST_STRATEGY' as CanvasStrategyId,
  name: 'Test Strategy',
  isApplicable: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
  ): boolean {
    return true
  },
  controlsToRender: [],
  fitness: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyState,
  ): number {
    return 10
  },
  apply: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyState,
  ): StrategyApplicationResult {
    return {
      commands: [wildcardPatch('permanent', { canvas: { scale: { $set: 100 } } })],
      customState: defaultCustomStrategyState,
    }
  },
}

describe('interactionStart', () => {
  it('creates the initial state with a simple test strategy', () => {
    const editorStore = createEditorStore(
      createInteractionViaMouse(
        canvasPoint({ x: 100, y: 200 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
      ),
    )
    const actualResult = interactionStart(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedPatches": Array [],
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
            "transient": "permanent",
            "type": "WILDCARD_PATCH",
          },
        ],
        "currentStrategyFitness": 10,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [
          Object {
            "apply": [Function],
            "controlsToRender": Array [],
            "fitness": [Function],
            "id": "TEST_STRATEGY",
            "isApplicable": [Function],
            "name": "Test Strategy",
          },
        ],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "drag": null,
        "dragStart": Object {
          "x": 100,
          "y": 200,
        },
        "globalTime": 1000,
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
        "type": "DRAG",
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
        "accumulatedPatches": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('interactionUpdatex', () => {
  it('steps an interaction session correctly', () => {
    const editorStore = createEditorStore(
      createInteractionViaMouse(
        canvasPoint({ x: 100, y: 200 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
      ),
    )
    editorStore.strategyState.currentStrategy = 'TEST_STRATEGY' as CanvasStrategyId
    const actualResult = interactionUpdate(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
      'non-interaction',
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedPatches": Array [],
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
            "transient": "permanent",
            "type": "WILDCARD_PATCH",
          },
        ],
        "currentStrategyFitness": 10,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [
          Object {
            "apply": [Function],
            "controlsToRender": Array [],
            "fitness": [Function],
            "id": "TEST_STRATEGY",
            "isApplicable": [Function],
            "name": "Test Strategy",
          },
        ],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "drag": null,
        "dragStart": Object {
          "x": 100,
          "y": 200,
        },
        "globalTime": 1000,
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
        "type": "DRAG",
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
        "accumulatedPatches": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('interactionUpdate without strategy', () => {
  it('processes the accumulated commands', () => {
    const editorStore = createEditorStore(
      createInteractionViaMouse(
        canvasPoint({ x: 100, y: 200 }),
        { alt: false, shift: false, ctrl: false, cmd: false },
        { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
      ),
    )
    editorStore.strategyState.currentStrategy = null
    editorStore.strategyState.accumulatedPatches = runCanvasCommand(
      editorStore.unpatchedEditor,
      wildcardPatch('permanent', { canvas: { scale: { $set: 100 } } }),
      'permanent',
    ).editorStatePatches
    const actualResult = interactionUpdate(
      [],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
      'non-interaction',
    )
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
  })
})

describe('interactionHardReset', () => {
  it('steps an interaction session correctly', () => {
    let interactionSession = createInteractionViaMouse(
      canvasPoint({ x: 100, y: 200 }),
      { alt: false, shift: false, ctrl: false, cmd: false },
      { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
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
        "accumulatedPatches": Array [],
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
            "transient": "permanent",
            "type": "WILDCARD_PATCH",
          },
        ],
        "currentStrategyFitness": 10,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [
          Object {
            "apply": [Function],
            "controlsToRender": Array [],
            "fitness": [Function],
            "id": "TEST_STRATEGY",
            "isApplicable": [Function],
            "name": "Test Strategy",
          },
        ],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "drag": Object {
          "x": 50,
          "y": 140,
        },
        "dragStart": Object {
          "x": 110,
          "y": 210,
        },
        "globalTime": 1000,
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
        "type": "DRAG",
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
        "accumulatedPatches": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(
      actualResult.patchedEditorState.canvas.interactionSession?.interactionData,
    ).toMatchInlineSnapshot(`undefined`)
  })
})

describe('interactionUpdate with accumulating keypresses', () => {
  it('steps an interaction session correctly', () => {
    let interactionSession = createInteractionViaKeyboard(
      ['left'],
      { alt: false, shift: false, ctrl: false, cmd: false },
      { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
    )

    const editorStore = createEditorStore(interactionSession)
    editorStore.strategyState.currentStrategy = 'PREVIOUS_STRATEGY' as CanvasStrategyId
    // the currentStrategyCommands should be added to accumulatedCommands
    editorStore.strategyState.currentStrategyCommands = [
      wildcardPatch('permanent', { selectedViews: { $set: [EP.elementPath([['aaa']])] } }),
    ]
    editorStore.strategyState.accumulatedPatches = runCanvasCommand(
      editorStore.unpatchedEditor,
      wildcardPatch('permanent', { focusedPanel: { $set: 'codeEditor' } }),
      'permanent',
    ).editorStatePatches

    const actualResult = interactionUpdate(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
      'interaction-create-or-update',
    )

    // accumulatedCommands should have the currentStrategyCommands added
    expect(actualResult.newStrategyState.accumulatedPatches).toMatchInlineSnapshot(`
      Array [
        Object {
          "focusedPanel": Object {
            "$set": "codeEditor",
          },
        },
        Object {
          "selectedViews": Object {
            "$set": Array [
              Object {
                "parts": Array [
                  Array [
                    "aaa",
                  ],
                ],
                "type": "elementpath",
              },
            ],
          },
        },
      ]
    `)

    // accumulatedCommands + currentStrategyCommands + the command coming from the strategy should all be applied to the patch
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.selectedViews).toMatchInlineSnapshot(`
      Array [
        Object {
          "parts": Array [
            Array [
              "aaa",
            ],
          ],
          "type": "elementpath",
        },
      ]
    `)
    expect(actualResult.unpatchedEditorState.selectedViews).toHaveLength(0)
    expect(actualResult.patchedEditorState.focusedPanel).toEqual('codeEditor')
    expect(actualResult.unpatchedEditorState.focusedPanel).toEqual('canvas')
  })
})

describe('interactionUpdate with user changed strategy', () => {
  it('steps an interaction session correctly', () => {
    let interactionSession = createInteractionViaMouse(
      canvasPoint({ x: 100, y: 200 }),
      { alt: false, shift: false, ctrl: false, cmd: false },
      { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
    )
    if (interactionSession.interactionData.type === 'DRAG') {
      interactionSession.interactionData.dragStart = canvasPoint({ x: 110, y: 210 })
      interactionSession.interactionData.drag = canvasPoint({ x: 50, y: 140 })
      interactionSession.interactionData.prevDrag = canvasPoint({ x: 30, y: 120 })
      interactionSession.userPreferredStrategy = 'EMPTY_TEST_STRATEGY' as CanvasStrategyId
    }
    const editorStore = createEditorStore(interactionSession)

    const result = dispatchResultFromEditorStore(editorStore)
    result.unpatchedEditor = {
      ...result.unpatchedEditor,
      canvas: {
        ...result.unpatchedEditor.canvas,
        interactionSession: {
          ...result.unpatchedEditor.canvas.interactionSession!,
          userPreferredStrategy: 'TEST_STRATEGY' as CanvasStrategyId,
        },
      },
    }

    const actualResult = interactionUpdate([testStrategy], editorStore, result, 'non-interaction')
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedPatches": Array [],
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
            "transient": "permanent",
            "type": "WILDCARD_PATCH",
          },
        ],
        "currentStrategyFitness": 10,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [
          Object {
            "apply": [Function],
            "controlsToRender": Array [],
            "fitness": [Function],
            "id": "TEST_STRATEGY",
            "isApplicable": [Function],
            "name": "Test Strategy",
          },
        ],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(100)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "drag": Object {
          "x": 50,
          "y": 140,
        },
        "dragStart": Object {
          "x": 110,
          "y": 210,
        },
        "globalTime": 1000,
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
        "type": "DRAG",
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
        "accumulatedPatches": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "customStrategyState": Object {
          "duplicatedElementNewUids": Object {},
          "escapeHatchActivated": false,
          "lastReorderIdx": null,
          "reparentedToPaths": Array [],
        },
        "sortedApplicableStrategies": Array [],
        "startingAllElementProps": Object {},
        "startingMetadata": Object {},
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
        { type: 'BOUNDING_AREA', target: EP.elementPath([['aaa']]) },
      ),
    )

    const newMetadata: ElementInstanceMetadataMap = {
      'new-entry': {
        elementPath: EP.fromString('new-entry'),
        specialSizeMeasurements: { position: 'absolute' },
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
            metadata: newMetadata,
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
      newEditorStore.unpatchedEditor.canvas.interactionSession?.metadata,
    )
  })
})
