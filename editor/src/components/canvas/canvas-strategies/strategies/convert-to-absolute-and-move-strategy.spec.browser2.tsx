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
import { localRectangle, LocalRectangle } from '../../../../core/shared/math-utils'
import { fastForEach } from '../../../../core/shared/utils'
import { runEscapeHatch, setFocusedElement } from '../../../editor/actions/action-creators'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { mapArrayToDictionary } from '../../../../core/shared/array-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { keyDown, mouseDownAtPoint, mouseMoveToPoint } from '../../event-helpers.test-utils'
import { cmdModifier } from '../../../../utils/modifiers'

const complexProject = () => {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

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
  {
    targetsToConvert: [FlexContainer, FlexRow],
    focusedElement: null,
    globalFrameUnchangedForElements: [
      {
        path: AbsoluteDivInFlex,
        pathAfter: EP.appendToPath(
          EP.appendToPath(AppRoot, EP.toUid(FlexRow)),
          EP.toUid(AbsoluteDivInFlex),
        ),
      },
      {
        path: RelativeContainer,
        pathAfter: EP.appendToPath(
          EP.appendToPath(AppRoot, EP.toUid(FlexRow)),
          EP.toUid(RelativeContainer),
        ),
      },
      {
        path: AbsoluteDivInRelative,
        pathAfter: EP.appendToPath(
          EP.appendToPath(AppRoot, EP.toUid(FlexRow)),
          EP.toUid(AbsoluteDivInRelative),
        ),
      },
    ],
    expectedAbsoluteElements: [FlexContainer, EP.appendToPath(AppRoot, EP.toUid(FlexRow))],
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
        await renderResult.dispatch([runEscapeHatch(targetsToConvert)], true)

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
    function getCodeForTestProject(appOpeningTag: string): string {
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
              ${appOpeningTag}
                <div
                  data-uid='child'
                  style={{
                    position: 'absolute',
                    width: 200,
                    height: 200,
                    backgroundColor: '#d3d3d3',
                  }}
                />
              </App>
            </Scene>
          </Storyboard>
        )
      `)
    }

    const appOpeningTagBefore = `<App data-uid='app'>`
    const appOpeningTagAfter = `
      <App
        data-uid='app'
        style={{
          position: 'absolute',
          left: 0,
          width: 375,
          top: 0,
          height: 300,
        }}
      >`

    const renderResult = await renderTestEditorWithCode(
      getCodeForTestProject(appOpeningTagBefore),
      'await-first-dom-report',
    )

    const targetToConvert = EP.fromString('sb/scene/app')
    // Converting App should not result in any changes to `sb/scene/app/child`
    await renderResult.dispatch([runEscapeHatch([targetToConvert])], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      getCodeForTestProject(appOpeningTagAfter),
    )
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
      { modifiers: cmdModifier },
    )
    await mouseMoveToPoint(canvasControlsLayer, {
      x: elementBounds.x + 50,
      y: elementBounds.y + 50,
    })

    const strategyBeforeSpacePressed = renderResult.getEditorState().strategyState.currentStrategy
    expect(strategyBeforeSpacePressed).toEqual('FLEX_REORDER')

    keyDown('Space')

    const currentStrategy = renderResult.getEditorState().strategyState.currentStrategy
    expect(currentStrategy).toEqual('CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY')
  })
})
