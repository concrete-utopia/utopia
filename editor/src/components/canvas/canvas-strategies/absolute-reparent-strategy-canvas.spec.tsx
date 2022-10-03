import {
  ElementInstanceMetadata,
  emptyComments,
  jsxAttributesFromMap,
  jsxAttributeValue,
  jsxElement,
  jsxElementName,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { CanvasPoint, canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  TestAppUID,
  testPrintCodeFromEditorState,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import {
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
} from './canvas-strategies'
import { defaultCustomStrategyState } from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import { right } from '../../../core/shared/either'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'

jest.mock('../canvas-utils', () => ({
  ...jest.requireActual('../canvas-utils'),
  getReparentTarget: () => ({
    shouldReparent: true,
    newParent: {
      type: 'elementpath',
      parts: [['utopia-storyboard-uid']],
    },
  }),
}))

// KEEP THIS IN SYNC WITH THE MOCK ABOVE
const newParent = EP.elementPath([[BakedInStoryboardUID]])

function reparentElement(
  editorState: EditorState,
  targetParentWithSpecialContentBox: boolean,
  dragVector: CanvasPoint = canvasPoint({ x: 15, y: 15 }),
): EditorState {
  const startingMetadata = {
    'scene-aaa/app-entity:aaa': {
      elementPath: EP.elementPath([['scene-aaa', 'app-entity'], ['aaa']]),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
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
        providesBoundsForAbsoluteChildren: true,
        globalContentBox: targetParentWithSpecialContentBox
          ? canvasRectangle({ x: 90, y: 100, width: 170, height: 120 })
          : canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'scene-aaa/app-entity:aaa/ccc': {
      elementPath: EP.elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'ccc'],
      ]),
      element: right(
        jsxElement(
          jsxElementName('div', []),
          'ccc',
          jsxAttributesFromMap({
            style: jsxAttributeValue(
              {
                position: 'absolute',
                width: 20,
                height: 30,
                top: 75,
                left: 90,
              },
              emptyComments,
            ),
            'data-uid': jsxAttributeValue('ccc', emptyComments),
          }),
          [],
        ),
      ),
      globalFrame: canvasRectangle({ x: 150, y: 160, width: 250, height: 200 }),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBox: canvasRectangle({ x: 150, y: 160, width: 250, height: 200 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      canvasPoint({ x: 0, y: 0 }),
      { cmd: true, alt: false, shift: false, ctrl: false },
      null as any, // the strategy does not use this
      dragVector,
    ),
    latestMetadata: null as any, // the strategy does not use this
    latestAllElementProps: null as any, // the strategy does not use this
    startingTargetParentsToFilterOut: null,
  }

  const strategyResult = absoluteReparentStrategy.apply(
    pickCanvasStateFromEditorStateWithMetadata(
      editorState,
      createBuiltInDependenciesList(null),
      startingMetadata,
    ),
    interactionSession,
    defaultCustomStrategyState(),
    'end-interaction',
  )

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

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
    'end-interaction',
  ).editorState

  return finalEditor
}

describe('Absolute Reparent Strategy', () => {
  it('reparents an element to the canvas', async () => {
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

    const finalEditor = reparentElement(initialEditor, false, canvasPoint({ x: -1000, y: -1000 }))

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
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
      </div>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
        <div
          data-uid='ccc'
          style={{
            position: 'absolute',
            width: 20,
            height: 30,
            top: -925,
            left: -910,
          }}
        />
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
})
