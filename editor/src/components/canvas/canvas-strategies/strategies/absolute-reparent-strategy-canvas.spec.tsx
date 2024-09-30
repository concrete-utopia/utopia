import * as Prettier from 'prettier/standalone'
import { PrettierConfig } from 'utopia-vscode-common'
import { createBuiltInDependenciesList } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import { right } from '../../../../core/shared/either'
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
import { foldAndApplyCommands } from '../../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  TestAppUID,
  testPrintCodeFromEditorState,
  TestSceneUID,
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
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'scene-aaa/app-entity:aaa/bbb': {
      elementPath: EP.elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      globalFrame: canvasRectangle({ x: 50, y: 60, width: 250, height: 200 }),
      specialSizeMeasurements: {
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        providesBoundsForAbsoluteChildren: true,
        globalContentBoxForChildren: targetParentWithSpecialContentBox
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
  }
  const startPoint = canvasPoint({ x: 0, y: 0 })
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

  expect(reparentStrategies.length).toBeGreaterThan(0)

  const strategyResult = reparentStrategies[0].apply('end-interaction')

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

  expect(
    strategyResult.elementsToRerender === 'rerender-all-elements'
      ? []
      : strategyResult.elementsToRerender.map(EP.parentPath),
  ).toEqual([newParent])

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    strategyResult.commands,
    'end-interaction',
  ).editorState

  return finalEditor
}

xdescribe('Absolute Reparent Strategy', () => {
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
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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
