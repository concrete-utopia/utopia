import create from 'zustand'
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
  interactionCancel,
  interactionHardReset,
  interactionStart,
  handleStrategyChangeStacked,
  interactionUpdate,
  handleUserChangedStrategy,
} from './dispatch-strategies'
import { createEditorState, deriveState, EditorStoreFull } from './editor-state'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import {
  createEmptyStrategyState,
  createInteractionViaMouse,
  InteractionSession,
  InteractionSessionWithoutMetadata,
  StrategyState,
} from '../../canvas/canvas-strategies/interaction-state'
import {
  CanvasStrategy,
  CanvasStrategyId,
  InteractionCanvasState,
  StrategyApplicationResult,
} from '../../canvas/canvas-strategies/canvas-strategy-types'
import { canvasPoint } from '../../../core/shared/math-utils'
import { wildcardPatch } from '../../canvas/commands/wildcard-patch-command'

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
    strategyState: strategyState ?? createEmptyStrategyState(),
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

  const storeHook = create<EditorStoreFull>((set) => initialEditorStore)

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
    editorStore.strategyState.accumulatedCommands = [
      {
        commands: [wildcardPatch('permanent', { selectedViews: { $set: [] } })],
        strategy: null,
      },
    ]
    const actualResult = interactionCancel(editorStore, dispatchResultFromEditorStore(editorStore))
    expect(actualResult.newStrategyState.accumulatedCommands).toHaveLength(0)
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
    return [wildcardPatch('permanent', { canvas: { scale: { $set: 100 } } })]
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
        "accumulatedCommands": Array [],
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
        "dragThresholdPassed": false,
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
        "accumulatedCommands": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "sortedApplicableStrategies": Array [],
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

describe('interactionUpdate', () => {
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
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedCommands": Array [],
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
        "dragThresholdPassed": false,
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
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedCommands": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "sortedApplicableStrategies": Array [],
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
        "accumulatedCommands": Array [],
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
        "dragThresholdPassed": false,
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
        "accumulatedCommands": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "sortedApplicableStrategies": Array [],
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

describe('interactionUpdate with stacked strategy change', () => {
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
    editorStore.strategyState.currentStrategy = 'EMPTY_TEST_STRATEGY' as CanvasStrategyId
    const actualResult = interactionUpdate(
      [testStrategy],
      editorStore,
      dispatchResultFromEditorStore(editorStore),
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedCommands": Array [
          Object {
            "commands": Array [],
            "strategy": "EMPTY_TEST_STRATEGY",
          },
          Object {
            "commands": Array [
              Object {
                "dataReset": true,
                "newFitness": 10,
                "newStrategy": "Test Strategy",
                "previousFitness": NaN,
                "reason": "user-input",
                "transient": "transient",
                "type": "STRATEGY_SWITCHED",
              },
            ],
            "strategy": null,
          },
        ],
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
        "startingMetadata": Object {},
      }
    `)
    expect(actualResult.patchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.unpatchedEditorState.canvas.scale).toEqual(1)
    expect(actualResult.patchedEditorState.canvas.interactionSession?.interactionData)
      .toMatchInlineSnapshot(`
      Object {
        "drag": Object {
          "x": 20,
          "y": 20,
        },
        "dragStart": Object {
          "x": 140,
          "y": 330,
        },
        "dragThresholdPassed": false,
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
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedCommands": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "sortedApplicableStrategies": Array [],
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

    const actualResult = interactionUpdate([testStrategy], editorStore, result)
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedCommands": Array [
          Object {
            "commands": Array [
              Object {
                "dataReset": true,
                "newFitness": 10,
                "newStrategy": "Test Strategy",
                "previousFitness": NaN,
                "reason": "user-input",
                "transient": "transient",
                "type": "STRATEGY_SWITCHED",
              },
            ],
            "strategy": null,
          },
        ],
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
        "dragThresholdPassed": false,
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
    )
    expect(actualResult.newStrategyState).toMatchInlineSnapshot(`
      Object {
        "accumulatedCommands": Array [],
        "commandDescriptions": Array [],
        "currentStrategy": null,
        "currentStrategyCommands": Array [],
        "currentStrategyFitness": 0,
        "sortedApplicableStrategies": Array [],
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
