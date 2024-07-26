import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'

import * as Prettier from 'prettier/standalone'
import { PrettierConfig } from 'utopia-vscode-common'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  canvasPoint,
  localRectangle,
  LocalRectangle,
  nullIfInfinity,
  offsetPoint,
  offsetRect,
} from '../../../../core/shared/math-utils'
import { fastForEach } from '../../../../core/shared/utils'
import {
  runEscapeHatch,
  selectComponents,
  setFocusedElement,
} from '../../../editor/actions/action-creators'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { cartesianProduct, mapArrayToDictionary } from '../../../../core/shared/array-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  keyDown,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
  mouseUpAtPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import type { FragmentLikeType } from './fragment-like-helpers'
import {
  AllFragmentLikeNonDomElementTypes,
  AllFragmentLikeTypes,
  treatElementAsFragmentLike,
} from './fragment-like-helpers'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
} from './fragment-like-helpers.test-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { ConvertToAbsoluteAndMoveStrategyID } from './convert-to-absolute-and-move-strategy'
import CanvasActions from '../../canvas-actions'
import { ctrlModifier } from '../../../../utils/modifiers'

const complexProject = () => {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export const Card = (props) => {
    return (
      <div
        data-uid='card-inner'
        style={{
          ...props.style,
          display: 'flex',
          flexDirection: 'column',
        }}
      >
        <div
          data-uid='card-title'
          style={{ alignSelf: 'center' }}
        >
          Card Title
        </div>
        <div
          data-uid='card-content'
          style={{ flexGrow: 1, backgroundColor: 'orange' }}
        />
        <div
          data-uid='card-close-X'
          style={{
            position: 'absolute',
            left: 340,
            top: 108,
          }}
        >
          X
        </div>
      </div>
    )
  }
  
  export var App = (props) => {
    return (
      <div
        style={{
          position: 'absolute',
          width: 380,
          left: 10,
          top: 20,
        }}
        data-uid='app-inner'
      >
        <div
          style={{
            backgroundColor: 'hotpink',
            width: '100%',
            height: 80,
            marginBottom: 20,
          }}
          data-uid='pink-static-div'
        />
        <div
          style={{
            display: 'flex',
            padding: 8,
            flexDirection: 'column',
          }}
          data-uid='flex-column'
        >
          <div
            style={{ display: 'flex' }}
            data-uid='flex-row'
          >
            <div
              data-uid='absolute-div-in-flex'
              style={{
                position: 'absolute',
                width: 100,
                top: 82,
                height: 20,
                left: 140,
              }}
            >
              Absolute Text
            </div>
            <div
              style={{
                position: 'relative',
                flexBasis: 25,
                height: 150,
                backgroundColor: '#6CF8C0',
              }}
              data-uid='relative-container-green'
            >
              <div data-uid='static-wrapper-div'>
                <span
                  data-uid='absolute-div'
                  style={{
                    position: 'absolute',
                    width: 19,
                    top: 58,
                    height: 22,
                    left: 6,
                  }}
                >
                  {'<'}
                </span>
              </div>
            </div>
            <Card
              style={{ flexGrow: 1, paddingRight: 20 }}
              data-uid='card-component'
            />
          </div>
        </div>
        <div
          style={{
            display: 'flex',
            padding: 8,
            flexDirection: 'column',
          }}
          data-uid='flex-column-2'
        >
          <div
            style={{
              backgroundColor: 'rebeccapurple',
              width: '100%',
              height: 80,
              marginBottom: 20,
              display: 'flex',
              flexDirection: 'row',
              gap: 15,
              paddingLeft: 15,
            }}
            data-uid='purple-flex-row'
          >
            <div
              style={{
                backgroundColor: 'plum',
                width: 50,
                marginBottom: 20,
              }}
              data-uid='plum-static-div-1'
            />
            <div
              style={{
                backgroundColor: 'plum',
                width: 50,
              }}
              data-uid='plum-static-div-2'
            />
            <React.Fragment data-uid="fragment-1">
              <div
                style={{
                  backgroundColor: 'plum',
                  width: 75,
                  height: 40,
                  alignSelf: 'flex-end',
                }}
                data-uid='plum-static-div-fragment-child-1'
              />
              <div
                style={{
                  backgroundColor: 'plum',
                  width: 50,
                }}
                data-uid='plum-static-div-fragment-child-2'
              />
            </React.Fragment>
            <div
              style={{
                backgroundColor: 'plum',
                width: 50,
                marginTop: 20,
              }}
              data-uid='plum-static-div-5'
            />
          </div>
        </div>
      </div>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <style>{\`
div,
span,
img,
ul,
li,
label {
  box-sizing: border-box !important;
}\`
        }</style>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }`

  return Prettier.format(code, PrettierConfig)
}

const AppRoot = EP.appendNewElementPath(TestScenePath, ['app-inner'])
const FlexContainer = EP.appendNewElementPath(TestScenePath, ['app-inner', 'flex-column'])
const FlexRow = EP.appendNewElementPath(TestScenePath, ['app-inner', 'flex-column', 'flex-row'])
const AbsoluteDivInFlex = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column',
  'flex-row',
  'absolute-div-in-flex',
])
const CardInstance = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column',
  'flex-row',
  'card-component',
])
const CardInner = EP.appendNewElementPath(CardInstance, ['card-inner'])
const CardCloseX = EP.appendNewElementPath(CardInstance, ['card-inner', 'card-close-X'])
const CardContent = EP.appendNewElementPath(CardInstance, ['card-inner', 'card-content'])
const CardTitle = EP.appendNewElementPath(CardInstance, ['card-inner', 'card-title'])

const RelativeContainer = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column',
  'flex-row',
  'relative-container-green',
])
const AbsoluteDivInRelative = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column',
  'flex-row',
  'relative-container-green',
  'static-wrapper-div',
  'absolute-div-in-relative',
])

const FlexChild1 = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column-2',
  'purple-flex-row',
  'plum-static-div-1',
])

const FragmentInFlex = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column-2',
  'purple-flex-row',
  'fragment-1',
])

const FragmentChild1 = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column-2',
  'purple-flex-row',
  'fragment-1',
  'plum-static-div-fragment-child-1',
])

const FragmentChild2 = EP.appendNewElementPath(TestScenePath, [
  'app-inner',
  'flex-column-2',
  'purple-flex-row',
  'fragment-1',
  'plum-static-div-fragment-child-2',
])

interface EscapeHatchTestCases {
  targetsToConvert: Array<ElementPath>
  focusedElement: ElementPath | null
  globalFrameUnchangedForElements: Array<{
    path: ElementPath
    pathAfter?: ElementPath
  }> // check resulting frame shifts, maybe new path after a possible reparent in multiselect
  expectedAbsoluteElements: Array<ElementPath> // new path after a possible reparent in multiselect
}

const escapeHatchTestCases: Array<EscapeHatchTestCases> = [
  {
    targetsToConvert: [FlexContainer],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      { path: FlexContainer },
      { path: FlexRow },
      { path: AbsoluteDivInFlex },
      { path: RelativeContainer },
      { path: AbsoluteDivInRelative },
      { path: CardInstance },
    ],
    expectedAbsoluteElements: [FlexContainer],
  },
  {
    targetsToConvert: [RelativeContainer],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      { path: AbsoluteDivInFlex },
      { path: RelativeContainer },
      { path: AbsoluteDivInRelative },
    ],
    expectedAbsoluteElements: [RelativeContainer],
  },
  {
    targetsToConvert: [RelativeContainer, CardInstance],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      { path: RelativeContainer },
      { path: CardInstance },
      { path: AbsoluteDivInFlex },
      { path: AbsoluteDivInRelative },
    ],
    expectedAbsoluteElements: [RelativeContainer, CardInstance],
  },
  // selecting an element and its child results in only the parent being absolute converted (drag-to-move rules)
  {
    targetsToConvert: [FlexContainer, FlexRow],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      {
        path: AbsoluteDivInFlex,
      },
      {
        path: RelativeContainer,
      },
      {
        path: AbsoluteDivInRelative,
      },
      {
        path: FlexRow,
      },
    ],
    expectedAbsoluteElements: [FlexContainer],
  },
  {
    targetsToConvert: [CardInner],
    focusedElement: CardInstance,
    globalFrameUnchangedForElements: [
      { path: CardCloseX },
      { path: CardContent },
      { path: CardTitle },
    ],
    expectedAbsoluteElements: [CardInner],
  },
  // Multiselection across parents will reparent the elements to a common ancestor!!!!
  {
    targetsToConvert: [RelativeContainer, FlexChild1],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      {
        path: RelativeContainer,
        pathAfter: EP.appendNewElementPath(TestScenePath, [
          'app-inner',
          'relative-container-green',
        ]),
      },
      {
        path: FlexChild1,
        pathAfter: EP.appendNewElementPath(TestScenePath, ['app-inner', 'plum-static-div-1']),
      },
    ],
    expectedAbsoluteElements: [
      EP.appendNewElementPath(TestScenePath, ['app-inner', 'relative-container-green']),
      EP.appendNewElementPath(TestScenePath, ['app-inner', 'plum-static-div-1']),
    ],
  },
  {
    targetsToConvert: [FragmentInFlex],
    focusedElement: null,
    globalFrameUnchangedForElements: [{ path: FragmentChild1 }, { path: FragmentChild2 }],
    expectedAbsoluteElements: [FragmentChild1, FragmentChild2],
  },
  // for a sibling of the fragment, there's no reparent involved
  {
    targetsToConvert: [FlexChild1, FragmentInFlex],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      { path: FlexChild1 },
      { path: FragmentChild1 },
      { path: FragmentChild2 },
    ],
    expectedAbsoluteElements: [FlexChild1, FragmentChild1, FragmentChild2],
  },
  // For fragment in a multiselect, we reparent the fragment, but absolutize the children of the fragment.
  {
    targetsToConvert: [RelativeContainer, FragmentInFlex],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      {
        path: RelativeContainer,
        pathAfter: EP.appendNewElementPath(TestScenePath, [
          'app-inner',
          'relative-container-green',
        ]),
      },
      {
        path: FragmentChild1,
        pathAfter: EP.appendNewElementPath(TestScenePath, [
          'app-inner',
          'fragment-1',
          'plum-static-div-fragment-child-1',
        ]),
      },
      {
        path: FragmentChild2,
        pathAfter: EP.appendNewElementPath(TestScenePath, [
          'app-inner',
          'fragment-1',
          'plum-static-div-fragment-child-2',
        ]),
      },
    ],
    expectedAbsoluteElements: [
      EP.appendNewElementPath(TestScenePath, ['app-inner', 'relative-container-green']),
      EP.appendNewElementPath(TestScenePath, [
        'app-inner',
        'fragment-1',
        'plum-static-div-fragment-child-1',
      ]),
      EP.appendNewElementPath(TestScenePath, [
        'app-inner',
        'fragment-1',
        'plum-static-div-fragment-child-2',
      ]),
    ],
  },
]

describe('Convert to Absolute/runEscapeHatch action', () => {
  fastForEach(
    escapeHatchTestCases,
    ({
      targetsToConvert,
      focusedElement,
      globalFrameUnchangedForElements,
      expectedAbsoluteElements,
    }) => {
      const focusedElementInTitle =
        focusedElement != null ? `, focused element is${EP.toUid(focusedElement)}` : ''
      it(`Converts ${targetsToConvert
        .map(EP.toUid)
        .join(', ')} element(s) to absolute ${focusedElementInTitle}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          complexProject(),
          'await-first-dom-report',
        )

        if (focusedElement != null) {
          await renderResult.dispatch([setFocusedElement(focusedElement)], true)
        }

        const canvasFramesBeforeConversion = mapArrayToDictionary(
          globalFrameUnchangedForElements,
          ({ path, pathAfter }) => EP.toString(pathAfter ?? path),
          ({ path, pathAfter }) => {
            return MetadataUtils.getFrameInCanvasCoords(
              path,
              renderResult.getEditorState().editor.jsxMetadata,
            )
          },
        )

        // CONVERT TO ABSOLUTE
        await renderResult.dispatch(
          [runEscapeHatch(targetsToConvert, 'set-hugging-parent-to-fixed')],
          true,
        )

        fastForEach(expectedAbsoluteElements, (target) => {
          const result = MetadataUtils.isPositionAbsolute(
            MetadataUtils.findElementByElementPath(
              renderResult.getEditorState().editor.jsxMetadata,
              target,
            ),
          )
          expect(result).toEqual(true)
        })
        fastForEach(Object.keys(canvasFramesBeforeConversion), (target) => {
          const resultFrame = MetadataUtils.getFrameInCanvasCoords(
            EP.fromString(target),
            renderResult.getEditorState().editor.jsxMetadata,
          )
          expect(resultFrame).toEqual(canvasFramesBeforeConversion[target])
        })
      })
    },
  )
})

describe('Convert to Absolute', () => {
  it('Correctly uses the captured closestOffsetParentPath to determine which elements to update', async () => {
    function getCodeForTestProject(childTag: string): string {
      return formatTestProjectCode(`
        import * as React from 'react'
        import { Scene, Storyboard } from 'utopia-api'

        export var App = (props) => {
          return (
            <div data-uid='app-root'>
              <div
                data-uid='inner-div'
                style={{ position: 'absolute', top: 100 }}
              >
                <div data-uid='immediate-parent'>
                  {props.children}
                </div>
              </div>
            </div>
          )
        }

        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              data-uid='scene'
              style={{
                position: 'absolute',
                width: 375,
                height: 812,
              }}
            >
              <App data-uid='app'>
                ${childTag}
              </App>
            </Scene>
          </Storyboard>
        )
      `)
    }

    const childTagBefore = `
    <div
      data-uid='child'
      style={{
        width: 200,
        height: 200,
        backgroundColor: '#d3d3d3',
      }}
    />
    `
    const childTagAfter = `
    <div
      data-uid='child'
      style={{
        width: 200,
        height: 200,
        backgroundColor: '#d3d3d3',
        position: 'absolute',
        left: 0,
        top: 100,
      }}
    />
    `

    const renderResult = await renderTestEditorWithCode(
      getCodeForTestProject(childTagBefore),
      'await-first-dom-report',
    )

    const targetToConvert = EP.fromString('sb/scene/app/child')
    await renderResult.dispatch(
      [runEscapeHatch([targetToConvert], 'set-hugging-parent-to-fixed')],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      getCodeForTestProject(childTagAfter),
    )
  })
  it('snaps to parent and sibling after being converted', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 20,
      top: 26,
      width: 150,
      height: 150,
    }}
    data-uid='container'
  >
    <div
      style={{
        backgroundColor: '#0075ff',
        width: 50,
        height: 50,
        contain: 'layout',
        position: 'absolute',
        left: 0,
        top: 0,
      }}
      data-uid='child'
      data-testid='child'
    />
    <div
      style={{
        backgroundColor: '#0075ff',
        width: 50,
        height: 65,
        contain: 'layout',
        position: 'absolute',
        left: 100,
        top: 86,
      }}
    />
  </div>`),
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const element = renderResult.renderedDOM.getByTestId('child')
    const elementBounds = element.getBoundingClientRect()

    await mouseDownAtPoint(
      canvasControlsLayer,
      {
        x: elementBounds.x + elementBounds.width / 2,
        y: elementBounds.y + elementBounds.width / 2,
      },
      {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      },
    )

    // move so that the bottom right corner snaps to the center of the parent
    await mouseMoveToPoint(canvasControlsLayer, {
      x: elementBounds.x + elementBounds.width / 2 + 25,
      y: elementBounds.y + elementBounds.width / 2 + 25,
    })

    {
      const activeStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(activeStrategy).not.toBeNull()
      expect(activeStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)
      expect(renderResult.getEditorState().editor.canvas.controls.snappingGuidelines).toHaveLength(
        2,
      )
    }

    // move so that the bottom edge snaps to the top edge of the sibling
    await mouseMoveToPoint(canvasControlsLayer, {
      x: elementBounds.x + elementBounds.width / 2 + 5,
      y: elementBounds.y + elementBounds.width / 2 + 35,
    })

    {
      const activeStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(activeStrategy).not.toBeNull()
      expect(activeStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)
      expect(renderResult.getEditorState().editor.canvas.controls.snappingGuidelines).toHaveLength(
        1,
      )
    }
  })
})

describe('Convert to absolute/escape hatch', () => {
  it('becomes the active strategy while space is pressed', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ position: 'absolute', width: '100%', height: '100%', display: 'flex'}} data-uid='flex-container'>
          <div
            style={{ backgroundColor: '#DDDDDD', width: 100, height: 100 }}
            data-uid='child1'
            data-testid='child1'
          />
          <div
            style={{ backgroundColor: '#EEEEEE', width: 50, height: 100 }}
            data-uid='child2'
            data-testid='child2'
          />
        </div>
  `),
      'await-first-dom-report',
    )
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const element = renderResult.renderedDOM.getByTestId('child1')
    const elementBounds = element.getBoundingClientRect()

    await mouseDownAtPoint(
      canvasControlsLayer,
      {
        x: elementBounds.x + 10,
        y: elementBounds.y + 10,
      },
      {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      },
    )
    await mouseMoveToPoint(
      canvasControlsLayer,
      {
        x: elementBounds.x + 50,
        y: elementBounds.y + 50,
      },
      {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      },
    )

    const strategyBeforeSpacePressed = renderResult.getEditorState().strategyState.currentStrategy
    expect(strategyBeforeSpacePressed).toEqual('FLEX_REORDER')

    keyDown('Space')

    const currentStrategy = renderResult.getEditorState().strategyState.currentStrategy
    expect(currentStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)
  })

  it('becomes the active strategy while space is pressed rather than ancestor metastrategy', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 100,
              top: 100,
              width: 300,
              height: 300,
            }}
            data-uid='grandparent'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: '100%',
                height: '100%',
              }}
              data-uid='parent'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: '100%',
                  height: '100%',
                }}
                data-uid='child'
                data-testid='child'
              />
            </div>
          </div>
        </div>
      `),
      'await-first-dom-report',
    )
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const element = renderResult.renderedDOM.getByTestId('child')
    const elementBounds = element.getBoundingClientRect()

    await mouseDownAtPoint(
      canvasControlsLayer,
      {
        x: elementBounds.x + 10,
        y: elementBounds.y + 10,
      },
      {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      },
    )
    await mouseMoveToPoint(canvasControlsLayer, {
      x: elementBounds.x + 50,
      y: elementBounds.y + 50,
    })

    const strategyBeforeSpacePressed = renderResult.getEditorState().strategyState.currentStrategy
    expect(strategyBeforeSpacePressed).toEqual('ABSOLUTE_MOVE_ANCESTOR_2')

    keyDown('Space')

    const currentStrategy = renderResult.getEditorState().strategyState.currentStrategy
    expect(currentStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)
  })
  ;(['flex', 'flow'] as const).forEach((parentLayoutSystem) =>
    it(`DOES NOT BECOME the active strategy when dragging for multiselection across the hierarchy for ${parentLayoutSystem} parent`, async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(codeForDragToEscapeHatchProject(parentLayoutSystem)),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [
          selectComponents(
            [
              EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:container/child1`),
              EP.fromString(
                `utopia-storyboard-uid/scene-aaa/app-entity:container/child3/grandchild`,
              ),
            ],
            false,
          ),
        ],
        true,
      )

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('child1')
      const elementBounds = element.getBoundingClientRect()

      await mouseDownAtPoint(
        canvasControlsLayer,
        {
          x: elementBounds.x + 10,
          y: elementBounds.y + 10,
        },
        {
          modifiers: {
            alt: false,
            cmd: true,
            ctrl: true,
            shift: false,
          },
        },
      )

      // Drag without going outside the sibling bounds
      await mouseMoveToPoint(canvasControlsLayer, {
        x: elementBounds.x + 50,
        y: elementBounds.y + 10,
      })

      const midDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(midDragStrategy).not.toBeNull()
      expect(midDragStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)

      // Now drag until we have passed the sibling bounds
      await mouseMoveToPoint(canvasControlsLayer, {
        x: elementBounds.x + 110,
        y: elementBounds.y + 10,
      })

      const endDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(endDragStrategy).not.toBeNull()
      expect(endDragStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)

      // HOWEVER!!!! pressing space now makes the strategy active!!!!!!
      keyDown('Space')
      const keydownStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(keydownStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)
    }),
  )
  ;(['flex', 'flow'] as const).forEach((parentLayoutSystem) =>
    it(`does become the active strategy when dragging for single selection in ${parentLayoutSystem} parent`, async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(codeForDragToEscapeHatchProject(parentLayoutSystem)),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [
          selectComponents(
            [
              EP.fromString(
                `utopia-storyboard-uid/scene-aaa/app-entity:container/child3/grandchild`,
              ),
            ],
            false,
          ),
        ],
        true,
      )

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('child1')
      const elementBounds = element.getBoundingClientRect()

      await mouseDownAtPoint(
        canvasControlsLayer,
        {
          x: elementBounds.x + 10,
          y: elementBounds.y + 10,
        },
        {
          modifiers: {
            alt: false,
            cmd: true,
            ctrl: true,
            shift: false,
          },
        },
      )

      // Drag without going outside the sibling bounds
      await mouseMoveToPoint(
        canvasControlsLayer,
        {
          x: elementBounds.x + 50,
          y: elementBounds.y + 10,
        },
        {
          modifiers: {
            alt: false,
            cmd: true,
            ctrl: true,
            shift: false,
          },
        },
      )

      const midDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(midDragStrategy).not.toBeNull()
      expect(midDragStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)

      // Now drag until we have passed the sibling bounds
      await mouseMoveToPoint(
        canvasControlsLayer,
        {
          x: elementBounds.x + 110,
          y: elementBounds.y + 10,
        },
        {
          modifiers: {
            alt: false,
            cmd: true,
            ctrl: true,
            shift: false,
          },
        },
      )

      const endDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(endDragStrategy).not.toBeNull()
      expect(endDragStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)

      // pressing space keeps the strategy active
      keyDown('Space')
      const keydownStrategy = renderResult.getEditorState().strategyState.currentStrategy
      expect(keydownStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)
    }),
  )

  cartesianProduct(['flex', 'flow'] as const, ['single-select', 'multiselect'] as const).forEach(
    ([parentLayoutSystem, multiselect]) => {
      it(`dragging ${parentLayoutSystem} elements out of sibling bounds`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(codeForDragToEscapeHatchProject('flow')),
          'await-first-dom-report',
        )

        await renderResult.dispatch(
          [
            selectComponents(
              multiselect === 'multiselect'
                ? [
                    EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:container/child1`),
                    EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:container/child2`),
                  ]
                : [EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:container/child1`)],
              false,
            ),
          ],
          true,
        )

        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
        const element = renderResult.renderedDOM.getByTestId('child1')
        const elementBounds = element.getBoundingClientRect()

        await mouseDownAtPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 10,
            y: elementBounds.y + 10,
          },
          {
            modifiers: {
              alt: false,
              cmd: true,
              ctrl: true,
              shift: false,
            },
          },
        )

        // Drag without going outside the sibling bounds
        await mouseMoveToPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 50,
            y: elementBounds.y + 10,
          },
          {
            modifiers: {
              alt: false,
              cmd: true,
              ctrl: true,
              shift: false,
            },
          },
        )

        const midDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
        expect(midDragStrategy).not.toBeNull()
        expect(midDragStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)

        // Now drag until we have passed the sibling bounds
        await mouseMoveToPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 110,
            y: elementBounds.y + 10,
          },
          {
            modifiers: {
              alt: false,
              cmd: true,
              ctrl: true,
              shift: false,
            },
          },
        )

        const endDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
        expect(endDragStrategy).not.toBeNull()
        expect(endDragStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)
      })
    },
  )

  cartesianProduct(['flex', 'flow'] as const, AllFragmentLikeNonDomElementTypes).forEach(
    ([parentLayoutSystem, type]) => {
      it(`dragging group-like element ${type} out of sibling bounds in ${parentLayoutSystem} context`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(
            codeWithFragmentLikeELementForEscapeHatch(parentLayoutSystem, type),
          ),
          'await-first-dom-report',
        )

        const groupElementPath = EP.fromString(
          `utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-like`,
        )
        const child2 = EP.appendPartToPath(groupElementPath, ['inner-fragment', 'child2'])
        const child3 = EP.appendPartToPath(groupElementPath, ['inner-fragment', 'child3'])

        const jsxMetadataBefore = renderResult.getEditorState().editor.jsxMetadata
        const child2OriginalBounds = nullIfInfinity(
          MetadataUtils.findElementByElementPath(jsxMetadataBefore, child2)?.globalFrame,
        )
        const child3OriginalBounds = nullIfInfinity(
          MetadataUtils.findElementByElementPath(jsxMetadataBefore, child3)?.globalFrame,
        )

        await renderResult.dispatch([selectComponents([groupElementPath], false)], true)

        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
        const element = renderResult.renderedDOM.getByTestId('child2')
        const elementBounds = element.getBoundingClientRect()

        await mouseDownAtPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 10,
            y: elementBounds.y + 10,
          },
          {
            modifiers: ctrlModifier,
          },
        )

        // Drag without going outside the sibling bounds
        await mouseMoveToPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 50,
            y: elementBounds.y + 10,
          },
          {
            modifiers: {
              alt: false,
              cmd: true,
              ctrl: true,
              shift: false,
            },
          },
        )

        const midDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
        expect(midDragStrategy).not.toBeNull()
        expect(midDragStrategy).not.toEqual(ConvertToAbsoluteAndMoveStrategyID)

        // Now drag until we have passed the sibling bounds
        await mouseMoveToPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 110,
            y: elementBounds.y + 10,
          },
          {
            modifiers: {
              alt: false,
              cmd: true,
              ctrl: true,
              shift: false,
            },
          },
        )

        const endDragStrategy = renderResult.getEditorState().strategyState.currentStrategy
        expect(endDragStrategy).not.toBeNull()
        expect(endDragStrategy).toEqual(ConvertToAbsoluteAndMoveStrategyID)

        await mouseUpAtPoint(
          canvasControlsLayer,
          {
            x: elementBounds.x + 110,
            y: elementBounds.y + 10,
          },
          {
            modifiers: {
              alt: false,
              cmd: true,
              ctrl: true,
              shift: false,
            },
          },
        )

        const jsxMetadataAfter = renderResult.getEditorState().editor.jsxMetadata
        const allElementPropsAfter = renderResult.getEditorState().editor.allElementProps
        const pathTreesAfter = renderResult.getEditorState().editor.elementPathTree

        expect(
          treatElementAsFragmentLike(
            jsxMetadataAfter,
            allElementPropsAfter,
            pathTreesAfter,
            groupElementPath,
          ),
        ).toEqual(true) // make sure the original group-like element remained group-like

        // check that the children became absolute
        expect(
          MetadataUtils.findElementByElementPath(jsxMetadataAfter, child2)?.specialSizeMeasurements
            .position,
        ).toEqual('absolute')
        expect(
          MetadataUtils.findElementByElementPath(jsxMetadataAfter, child3)?.specialSizeMeasurements
            .position,
        ).toEqual('absolute')

        const child2ResultBounds = MetadataUtils.findElementByElementPath(
          jsxMetadataAfter,
          child2,
        )?.globalFrame
        const child3ResultBounds = MetadataUtils.findElementByElementPath(
          jsxMetadataAfter,
          child3,
        )?.globalFrame

        expect(offsetRect(child2OriginalBounds!, canvasPoint({ x: 100, y: 0 }))).toEqual(
          child2ResultBounds,
        )
        expect(offsetRect(child3OriginalBounds!, canvasPoint({ x: 100, y: 0 }))).toEqual(
          child3ResultBounds,
        )
      })
    },
  )

  describe('Escape Hatch Strategy', () => {
    it('does not activate when drag threshold is not reached', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
      `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const viewBounds = initialEditor.renderedDOM.getByTestId('bbb').getBoundingClientRect()

      const canvas = initialEditor.renderedDOM.getByTestId(CanvasControlsContainerID)

      const viewCenter = canvasPoint({
        x: viewBounds.left + viewBounds.width / 2,
        y: viewBounds.top + viewBounds.height / 2,
      })

      await mouseDragFromPointToPoint(
        canvas,
        viewCenter,
        offsetPoint(viewCenter, canvasPoint({ x: 1, y: 1 })),
      )

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
      `),
      )
    })

    it('Runs the escape hatch strategy and sets hugging parent to fixed size', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
             <View
              style={{
                ...(props.style || {}),
                width: 'max-content',
                height: 'max-content',
              }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
                data-uid='bbb'
              />
            </View>
        `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const action = runEscapeHatch([targetElement], 'set-hugging-parent-to-fixed')

      await initialEditor.dispatch([action], true)
      await initialEditor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View
            style={{
              ...(props.style || {}),
              width: 250,
              height: 300,
            }}
            data-uid='aaa'
          >
          <View
            style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300, position: 'absolute', left: 0, top: 0  }}
            data-uid='bbb'
          />
        </View>`,
        ),
      )
    })

    it('works on a flow element without siblings where width and height is percentage', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <View
        style={{
          ...(props.style || {}),
        }}
        data-uid='aaa'
      >
        <View
          style={{
            backgroundColor: '#aaaaaa33',
            width: '50%',
            height: '20%',
            right: 200,
            bottom: 320,
            top: 0,
            left: 0
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
      `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const viewBounds = initialEditor.renderedDOM.getByTestId('bbb').getBoundingClientRect()

      const canvas = initialEditor.renderedDOM.getByTestId(CanvasControlsContainerID)

      const viewCenter = canvasPoint({
        x: viewBounds.left + viewBounds.width / 2,
        y: viewBounds.top + viewBounds.height / 2,
      })

      await mouseDragFromPointToPoint(
        canvas,
        viewCenter,
        offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })),
        {
          modifiers: {
            alt: false,
            cmd: true,
            ctrl: true,
            shift: false,
          },
        },
      )

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{
              backgroundColor: '#aaaaaa33',
              width: 200,
              height: 80,
              top: 15,
              left: 15,
              position: 'absolute',
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </View>`,
        ),
      )
    })
  })

  describe('Choosing strategy to not set hugging parent to fixed size', () => {
    it('Runs the escape hatch strategy and does not set hugging parent to fixed size', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
              <View
              style={{
                ...(props.style || {}),
                width: 'max-content',
                height: 'max-content',
              }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
                data-uid='bbb'
              />
            </View>
        `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const action = runEscapeHatch([targetElement], 'dont-set-hugging-parent-to-fixed')

      await initialEditor.dispatch([action], true)
      await initialEditor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View
            style={{
              ...(props.style || {}),
              width: 'max-content',
              height: 'max-content',
            }}
            data-uid='aaa'
          >
          <View
            style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300, position: 'absolute', left: 0, top: 0  }}
            data-uid='bbb'
          />
        </View>`,
        ),
      )
    })

    it('works on a flow element without siblings', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
      `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const viewBounds = initialEditor.renderedDOM.getByTestId('bbb').getBoundingClientRect()

      const canvas = initialEditor.renderedDOM.getByTestId(CanvasControlsContainerID)

      const viewCenter = canvasPoint({
        x: viewBounds.left + viewBounds.width / 2,
        y: viewBounds.top + viewBounds.height / 2,
      })

      await mouseDownAtPoint(canvas, viewCenter, {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })
      await mouseMoveToPoint(canvas, offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })), {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })

      await initialEditor.dispatch(
        [CanvasActions.setUsersPreferredStrategy(ConvertToAbsoluteAndMoveStrategyID)],
        true,
      )

      await mouseUpAtPoint(canvas, offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })), {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300, position: 'absolute', left: 15, top: 15  }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </View>`,
        ),
      )
    })

    it('works on a flow element with all pins', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{
          backgroundColor: '#aaaaaa33',
          width: '50%',
          height: '20%',
          right: 200,
          bottom: 320,
          top: 0,
          left: 0
        }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </View>
    `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const viewBounds = initialEditor.renderedDOM.getByTestId('bbb').getBoundingClientRect()

      const canvas = initialEditor.renderedDOM.getByTestId(CanvasControlsContainerID)

      const viewCenter = canvasPoint({
        x: viewBounds.left + viewBounds.width / 2,
        y: viewBounds.top + viewBounds.height / 2,
      })

      await mouseDownAtPoint(canvas, viewCenter, {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })
      await mouseMoveToPoint(canvas, offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })), {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })

      await initialEditor.dispatch(
        [CanvasActions.setUsersPreferredStrategy(ConvertToAbsoluteAndMoveStrategyID)],
        true,
      )

      await mouseUpAtPoint(canvas, offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })), {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{
            backgroundColor: '#aaaaaa33',
            width: 200,
            height: 80,
            top: 15,
            left: 15,
            position: 'absolute',
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>`,
        ),
      )
    })

    it('works on a flow element without siblings where width and height is percentage', async () => {
      const initialEditor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{
            backgroundColor: '#aaaaaa33',
            width: '50%',
            height: '20%',
            right: 200,
            bottom: 320,
            top: 0,
            left: 0
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
      `),
        'await-first-dom-report',
      )

      const targetElement = EP.elementPath([
        [BakedInStoryboardUID, 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      await selectComponentsForTest(initialEditor, [targetElement])

      const viewBounds = initialEditor.renderedDOM.getByTestId('bbb').getBoundingClientRect()

      const canvas = initialEditor.renderedDOM.getByTestId(CanvasControlsContainerID)

      const viewCenter = canvasPoint({
        x: viewBounds.left + viewBounds.width / 2,
        y: viewBounds.top + viewBounds.height / 2,
      })

      await mouseDownAtPoint(canvas, viewCenter, {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })
      await mouseMoveToPoint(canvas, offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })), {
        modifiers: {
          alt: false,
          cmd: true,
          ctrl: true,
          shift: false,
        },
      })

      await initialEditor.dispatch(
        [CanvasActions.setUsersPreferredStrategy(ConvertToAbsoluteAndMoveStrategyID)],
        true,
      )

      await mouseUpAtPoint(canvas, offsetPoint(viewCenter, canvasPoint({ x: 15, y: 15 })))

      expect(getPrintedUiJsCode(initialEditor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{
              backgroundColor: '#aaaaaa33',
              width: 200,
              height: 80,
              top: 15,
              left: 15,
              position: 'absolute',
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </View>`,
        ),
      )
    })
  })
})

describe('Escape hatch strategy on awkward project', () => {
  it('Fixes the size of the dragged element to stop it growing out of control', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
        import * as React from 'react'
        import { Scene, Storyboard } from 'utopia-api'
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 400,
                height: 400,
                display: 'flex',
                flexDirection: 'column',
                gap: 60,
              }}
              data-uid='root'
            >
              <div
                style={{
                  display: 'flex',
                  flexDirection: 'row',
                  width: '100%',
                  justifyContent: 'space-between',
                }}
                data-uid='container'
              >
                <div
                  style={{
                    height: '100%',
                    overflow: 'hidden',
                    display: 'flex',
                    flexDirection: 'column',
                    backgroundColor: '#FFEE00',
                  }}
                  data-testid='drag-me'
                  data-uid='drag-me'
                >
                  <div
                    style={{
                      height: 100,
                      width: 100,
                      backgroundColor: '#FD1919',
                    }}
                    data-uid='child'
                  />
                </div>
              </div>
            </div>
          </Storyboard>
        )
      `,
      'await-first-dom-report',
    )

    const targetElement = EP.elementPath([['sb', 'root', 'container', 'drag-me']])

    await selectComponentsForTest(renderResult, [targetElement])

    const targetBounds = renderResult.renderedDOM.getByTestId('drag-me').getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await mouseDragFromPointWithDelta(
      canvasControlsLayer,
      {
        x: targetBounds.left + targetBounds.width / 2,
        y: targetBounds.top + targetBounds.height / 2,
      },
      {
        x: 15,
        y: 15,
      },
      {
        modifiers: ctrlModifier,
      },
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 400,
                  height: 400,
                  display: 'flex',
                  flexDirection: 'column',
                  gap: 60,
                }}
                data-uid='root'
              >
                <div
                  style={{
                    display: 'flex',
                    flexDirection: 'row',
                    width: 400,
                    justifyContent: 'space-between',
                    height: 100,
                  }}
                  data-uid='container'
                >
                  <div
                    style={{
                      height: 100,
                      overflow: 'hidden',
                      display: 'flex',
                      flexDirection: 'column',
                      backgroundColor: '#FFEE00',
                      width: 100,
                      position: 'absolute',
                      left: 15,
                      top: 15,
                    }}
                    data-testid='drag-me'
                    data-uid='drag-me'
                  >
                    <div
                      style={{
                        height: 100,
                        width: 100,
                        backgroundColor: '#FD1919',
                      }}
                      data-uid='child'
                    />
                  </div>
                </div>
              </div>
            </Storyboard>
          )
      `),
    )
  })
})

function codeForDragToEscapeHatchProject(flowOrFlex: 'flow' | 'flex'): string {
  return `
    <div
      style={{
        width: 400,
        height: 400,
        ${flowOrFlex === 'flex' ? "display: 'flex'," : ''}
        ${flowOrFlex === 'flex' ? "flexDirection: 'column'," : ''}
      }}
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#FF0000',
          width: 100,
          height: 100,
          contain: 'layout',
        }}
        data-uid='child1'
        data-testid='child1'
      />
      <div
        style={{
          backgroundColor: '#00FF00',
          width: 100,
          height: 100,
          contain: 'layout',
        }}
        data-uid='child2'
      />
      <div
        style={{
          backgroundColor: '#0000FF',
          width: 100,
          height: 100,
          contain: 'layout',
        }}
        data-uid='child3'
      >
        <div
          style={{
            backgroundColor: '#000088',
            width: 50,
            height: 50,
            contain: 'layout',
          }}
          data-uid='grandchild'
        />
      </div>
    </div>
  `
}

function codeWithFragmentLikeELementForEscapeHatch(
  flowOrFlex: 'flow' | 'flex',
  type: FragmentLikeType,
): string {
  return `
    <div
      style={{
        width: 400,
        height: 400,
        ${flowOrFlex === 'flex' ? "display: 'flex'," : ''}
        ${flowOrFlex === 'flex' ? "flexDirection: 'column'," : ''}
      }}
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#FF0000',
          width: 100,
          height: 100,
          contain: 'layout',
        }}
        data-uid='child1'
        data-testid='child1'
      />
      ${getOpeningFragmentLikeTag(type)}
        <div
          style={{
            backgroundColor: '#00FF00',
            width: 100,
            height: 100,
            contain: 'layout',
          }}
          data-uid='child2'
          data-testid='child2'
        />
        <div
          style={{
            backgroundColor: '#0000FF',
            width: 100,
            height: 100,
            contain: 'layout',
          }}
          data-uid='child3'
        />
      ${getClosingFragmentLikeTag(type)}
    </div>
  `
}
