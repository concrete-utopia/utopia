/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkFocusedPath", "checkSelectedPaths", "selectInjectedDivAndCheckAllPaths"] }] */
/// <reference types="karma-viewport" />
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import {
  clearSelection,
  selectComponents,
  setCursorOverlay,
  setFocusedElement,
  toggleSelectionLock,
} from '../../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../new-canvas-controls'
import { SceneLabelTestID } from './scene-label'
import { CSSCursor } from '../../../../uuiui-deps'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDownAtPoint,
  mouseMoveToPoint,
  mouseUpAtPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier, emptyModifiers, shiftCmdModifier } from '../../../../utils/modifiers'
import { FOR_TESTS_setNextGeneratedUids } from '../../../../core/model/element-template-utils.test-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { wait } from '../../../../utils/utils.test-utils'

async function fireSingleClickEvents(
  target: HTMLElement,
  clientX: number,
  clientY: number,
  modifiers: Modifiers = emptyModifiers,
): Promise<void> {
  await mouseMoveToPoint(target, { x: clientX, y: clientY })
  await mouseClickAtPoint(target, { x: clientX, y: clientY }, { modifiers: modifiers })
}

function createDoubleClicker(
  target: HTMLElement,
  clientX: number,
  clientY: number,
): () => Promise<void> {
  let clickCount = 0

  return async () => {
    await mouseMoveToPoint(target, { x: clientX, y: clientY })
    await mouseDoubleClickAtPoint(
      target,
      { x: clientX, y: clientY },
      {
        initialClickCount: clickCount,
      },
    )
    clickCount += 2
  }
}

describe('Select Mode Selection', () => {
  it('keep double clicking on a children eventually selects it – even if it is out of bounds of the parents', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
          <div data-uid='a' style={{ ...props.style }}>
            <div
              data-uid='b'
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                overflow: 'visible',
                left: 50,
                top: 50,
                height: 120,
                width: 120,
              }}
            >
              <div
              data-uid='c'
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 30,
                  top: 30,
                  height: 120,
                  width: 120,
                }}
              >
                <div
                  data-uid='d'
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    height: 120,
                    width: 120,
                    left: 30,
                    top: 30,
                  }}
                >
                  <div
                    data-uid='e'
                    style={{
                      backgroundColor: '#aaaaaa33',
                      position: 'absolute',
                      left: 30,
                      top: 30,
                      height: 120,
                      width: 120,
                    }}
                  >
                    <div
                      data-uid='targetdiv'
                      data-testid='targetdiv'
                      style={{
                        backgroundColor: '#aaaaaa33',
                        position: 'absolute',
                        left: 30,
                        top: 30,
                        height: 120,
                        width: 120,
                      }}
                    />
                  </div>
                </div>
              </div>
            </div>
          </div>
      `),
      'await-first-dom-report',
    )

    const areaControl = renderResult.renderedDOM.getByTestId('targetdiv')
    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      areaControlBounds.left + 20,
      areaControlBounds.top + 20,
    )

    const getSelectedViews = () => renderResult.getEditorState().editor.selectedViews

    expect(getSelectedViews()).toEqual([])

    await fireSingleClickEvents(
      canvasControlsLayer,
      areaControlBounds.left + 20,
      areaControlBounds.top + 20,
    )

    expect(getSelectedViews()).toEqual([EP.appendNewElementPath(TestScenePath, ['a', 'b'])])

    await doubleClick()

    expect(getSelectedViews()).toEqual([EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c'])])

    await doubleClick()

    expect(getSelectedViews()).toEqual([
      EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c', 'd']),
    ])

    await doubleClick()

    expect(getSelectedViews()).toEqual([
      EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c', 'd', 'e']),
    ])

    await doubleClick()

    // after 5 "double clicks", the `targetdiv` div should be selected
    expect(getSelectedViews()).toEqual([
      EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c', 'd', 'e', 'targetdiv']),
    ])
  })

  it('Clicking a scene label selects the scene', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
        import * as React from 'react'
        import {
          Scene,
          Storyboard,
        } from 'utopia-api'
        export var App = (props) => {
          return (
            <div />
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid={'${BakedInStoryboardUID}'}>
              <Scene
                style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
                data-uid={'${TestSceneUID}'}
              >
                <App
                  data-uid='${TestAppUID}' 
                  style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
                />
              </Scene>
            </Storyboard>
          )
        }
      `,
      'await-first-dom-report',
    )

    const sceneLabel = renderResult.renderedDOM.getByTestId(SceneLabelTestID)
    const sceneLabelBounds = sceneLabel.getBoundingClientRect()

    await fireSingleClickEvents(sceneLabel, sceneLabelBounds.left + 5, sceneLabelBounds.top + 5)

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}`),
    ])
  })
  it('Doubleclick on a child element selects it, with custom cursors set', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa' data-testid='aaa' style={{ position: 'absolute', width: 200, height: 200 }}>
          <div data-uid='bbb' data-testid='bbb' style={{ width: 100, height: 100 }}></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const appElementPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID]])
    await renderResult.dispatch(
      [setCursorOverlay(CSSCursor.Move), selectComponents([appElementPath], false)],
      true,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('aaa')
    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      areaControlBounds.left + 20,
      areaControlBounds.top + 20,
    )
    await doubleClick()

    const selectedViews = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews).toEqual([EP.appendNewElementPath(appElementPath, ['aaa', 'bbb'])])
  })
})

describe('Select Mode Advanced Cases', () => {
  it('Can cmd-click to select Button on a Card Scene Root', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await mouseClickAtPoint(
      canvasControlsLayer,
      {
        x: cardSceneRootBounds.left + 130,
        y: cardSceneRootBounds.top + 220,
      },
      { modifiers: cmdModifier },
    )

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString('sb/scene-2/Card-instance:Card-Root/Card-Row-Buttons/Card-Button-3'),
    ])
  })
})

describe('Select Mode Clicking', () => {
  // The below tests are to ensure we're not inadvertently handling clicks too many times,
  // which could either mean focusing something that wasn't meant to be focused, or skipping
  // over the target element and selecting something further down the hierarchy.
  // Each double click should _either_ select the next element down the hierarchy, _or_ focus
  // the currently selected element. Also, we specifically skip over Scenes, component children
  // of those, and root elements, meaning if nothing is selected a single click can skip right
  // to the child of the root element of a component

  it('One click to select child of Card Scene Root', async () => {
    // prettier-ignore
    const desiredPath = EP.fromString(
      'sb' +                // Skipped as it's the storyboard
      '/scene-2' +          // Skipped because we skip over Scenes
      '/Card-instance' +    // Skipped because we skip component children of Scenes
      ':Card-Root' +        // Skipped because we skip over root elements
      '/Card-Row-Buttons',  // <- Single click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })

  it('Single click and then double click to select Button on a Card Scene Root', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                // Skipped as it's the storyboard
      '/scene-2' +          // Skipped because we skip over Scenes
      '/Card-instance' +    // Skipped because we skip component children of Scenes
      ':Card-Root' +        // Skipped because we skip over root elements
      '/Card-Row-Buttons',  // <- Single click
      '/Card-Button-3',     // <- Double click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])
  })

  it('Single click and three double clicks will focus a generated Card', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
    'sb' +                 // Skipped as it's the storyboard
    '/scene-CardList' +    // Skipped because we skip over Scenes
    '/CardList-instance' + // Skipped because we skip component children of Scenes
    ':CardList-Root' +     // Skipped because we skip over root elements
    '/CardList-Col',       // <- Single click
    '/cardlist-expr',      // <- First double click selects the expression item
    '/CardList-Card~~~1',  // <- Second *and* third double click, as the third is required to focus it
  )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('generated-card-1')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[2]])
  })
  it('Single click and five double clicks will focus a generated Card and select the Button inside', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                 // Skipped as it's the storyboard
      '/scene-CardList' +    // Skipped because we skip over Scenes
      '/CardList-instance' + // Skipped because we skip component children of Scenes
      ':CardList-Root' +     // Skipped because we skip over root elements
      '/CardList-Col',       // <- Single click
      '/cardlist-expr',      // <- First double click selects the expression item
      '/CardList-Card~~~1',  // <- Second *and* third double click, as the third is required to focus it
      ':Card-Root' +         // Skipped because we skip over root elements
      '/Card-Row-Buttons',   // <- Fourth double click
      '/Card-Button-3',      // <- Fifth double click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('generated-card-1')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[3]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[4]])
  })

  it('Need to unlock the playground root to select it', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +      // Skipped as it's the storyboard
      '/sc' +     // Skipped because we skip over Scenes
      '/pg',      // <- Cmd click when locked
      ':pg-root', // <- Cmd click when unlocked
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectPlayground,
      'await-first-dom-report',
    )

    const playgroundRoot = renderResult.renderedDOM.getByTestId('pg-root')
    const playgroundRootBounds = playgroundRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    // Click before unlocking
    await fireSingleClickEvents(
      canvasControlsLayer,
      playgroundRootBounds.left + 10,
      playgroundRootBounds.top + 10,
      cmdModifier,
    )

    // Ensure the playground root isn't selected
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    // Clear the selection, then remove the lock and try again
    await renderResult.dispatch([clearSelection()], true)

    await renderResult.dispatch(
      [
        toggleSelectionLock(
          renderResult.getEditorState().editor.lockedElements.simpleLock,
          'selectable',
        ),
      ],
      true,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      playgroundRootBounds.left + 10,
      playgroundRootBounds.top + 10,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])
  })

  it('Unlocked playground root can be single-click selected', async () => {
    // prettier-ignore
    const desiredPath = EP.fromString(
      'sb' +      // Skipped as it's the storyboard
      '/sc' +     // Skipped because we skip over Scenes
      '/pg' +     // Skipped because we skip component children of Scenes
      ':pg-root', // <- Single click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectPlayground,
      'await-first-dom-report',
    )

    const playgroundRoot = renderResult.renderedDOM.getByTestId('pg-root')
    const playgroundRootBounds = playgroundRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await renderResult.dispatch(
      [
        toggleSelectionLock(
          renderResult.getEditorState().editor.lockedElements.simpleLock,
          'selectable',
        ),
      ],
      true,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      playgroundRootBounds.left + 10,
      playgroundRootBounds.top + 10,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })

  it('Selecting a locked element and then clicking again keeps it selected', async () => {
    // prettier-ignore
    const desiredPath = EP.fromString(
      'sb' +      // Skipped as it's the storyboard
      '/sc' +     // Skipped because we skip over Scenes
      '/pg' +     // Skipped because we skip component children of Scenes
      ':pg-root', // <- Cmd click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectPlayground,
      'await-first-dom-report',
    )

    const playgroundRoot = renderResult.renderedDOM.getByTestId('pg-root')
    const playgroundRootBounds = playgroundRoot.getBoundingClientRect()

    // Unlock the playground root so that we can click to select it
    await renderResult.dispatch(
      [
        toggleSelectionLock(
          renderResult.getEditorState().editor.lockedElements.simpleLock,
          'selectable',
        ),
      ],
      true,
    )
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      playgroundRootBounds.left + 10,
      playgroundRootBounds.top + 10,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])

    // Now lock it again to check that we can still click it without losing the selection
    await renderResult.dispatch(
      [
        toggleSelectionLock(
          renderResult.getEditorState().editor.lockedElements.simpleLock,
          'locked',
        ),
      ],
      true,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      playgroundRootBounds.left + 10,
      playgroundRootBounds.top + 10,
      emptyModifiers,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })

  it('The defaulted in simple locks are maintained across changes', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    expect(renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString)).toEqual(
      ['sb/sc-app/app:app-root'],
    )

    await renderResult.dispatch([setFocusedElement(EP.fromString('sb/sbchild'))], true)

    expect(renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString)).toEqual(
      ['sb/sc-app/app:app-root', 'sb/sbchild:sbchild-root'],
    )

    // Remove the lock from one element.
    await renderResult.dispatch(
      [toggleSelectionLock([EP.fromString('sb/sc-app/app:app-root')], 'selectable')],
      true,
    )

    expect(renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString)).toEqual(
      ['sb/sbchild:sbchild-root'],
    )
  })
})

describe('Select Mode Double Clicking With Fragments', () => {
  // The below tests are similar to tests in 'Select Mode Double Clicking', but
  // they use a test project which contain components which renders fragment root
  // elements. These are special cases because these fragments and their components
  // do not appear in the dom.

  it('One click to select child of Card Scene Root', async () => {
    FOR_TESTS_setNextGeneratedUids([
      'cardlistfragment',
      'manuallistfragment',
      'cardlistfragment',
      'manuallistfragment',
    ])
    // prettier-ignore
    const desiredPath = EP.fromString(
        'sb' +                // Skipped as it's the storyboard
        '/scene-2' +          // Skipped because we skip over Scenes
        '/Card-instance' +    // Skipped because we skip component children of Scenes
        ':Card-Root' +        // Skipped because we skip over root elements
        '/Card-Row-Buttons',  // <- Single click
      )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithFragments,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })

  it('Single click and then double click to select Button on a Card Scene Root', async () => {
    FOR_TESTS_setNextGeneratedUids([
      'cardlistfragment',
      'manuallistfragment',
      'cardlistfragment',
      'manuallistfragment',
    ])
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                // Skipped as it's the storyboard
      '/scene-2' +          // Skipped because we skip over Scenes
      '/Card-instance' +    // Skipped because we skip component children of Scenes
      ':Card-Root' +        // Skipped because we skip over root elements
      '/Card-Row-Buttons',  // <- Single click
      '/Card-Button-3',     // <- Double click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithFragments,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])
  })

  it('Single click and three double clicks will focus a generated Card', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                  // Skipped as it's the storyboard
      '/scene-CardList' +     // Skipped because we skip over Scenes
      '/CardList-instance' +  // Skipped because we skip component children of Scenes
      ':979' +                // Skipped because we skip over root elements
      '/CardList-Col',        // <- Single click
      '/cardlist-expr',       // <- First double click selects the expression item
      '/CardList-Card~~~1',   // <- Second *and* Third double click, as the Third is required to focus it
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithFragments,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('generated-card-1')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 50,
      cardSceneRootBounds.top + 50,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 50,
      cardSceneRootBounds.top + 50,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[2]])
  })
  it('Single click and five double clicks will focus a generated Card and select the Button inside', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                  // Skipped as it's the storyboard
      '/scene-CardList' +     // Skipped because we skip over Scenes
      '/CardList-instance' +  // Skipped because we skip component children of Scenes
      ':979' +                // Skipped because we skip over root elements
      '/CardList-Col',        // <- Single click
      '/cardlist-expr',       // <- First double click selects the expression item
      '/CardList-Card~~~1',   // <- Second *and* Third double click, as the Third is required to focus it
      ':Card-Root' +          // Skipped because we skip over root elements
      '/Card-Row-Buttons',    // <- Fourth double click
      '/Card-Button-3',       // <- Fifth double click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithFragments,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('generated-card-1')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[3]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[2])
    checkSelectedPaths(renderResult, [desiredPaths[4]])
  })
  // This test fails because there is a generated component there with a root fragment, which case is not supported
  // See comments in function firstAncestorOrItselfWithValidElementPath
  xit('Single click and four double clicks will focus a generated Card with a root fragment and select the Button inside', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                  // Skipped as it's the storyboard
      '/scene-CardList' +     // Skipped because we skip over Scenes
      '/CardList-instance' +  // Skipped because we skip component children of Scenes
      ':dbc' +                // Skipped because we skip over root elements
      '/CardList-Col',        // <- Single click
      '/CardList-Card~~~1',   // <- First *and* Second double click, as the Second is required to focus it
      ':Card-Fragment-Root' +          // Skipped because we skip over root elements
      '/Card-Root',           // <- Third double click
      '/Card-Row-Buttons',    // <- Fourth double click
      '/Card-Button-3',       // <- Fifth double clic
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithCardWithRootFragment,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('generated-card-1')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[1])
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[1])
    checkSelectedPaths(renderResult, [desiredPaths[2]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[1])
    checkSelectedPaths(renderResult, [desiredPaths[3]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[1])
    checkSelectedPaths(renderResult, [desiredPaths[4]])
  })
})

describe('Select Mode Double Clicking With conditionals', () => {
  it('Double click can dive into single conditional inside element with an expression in the active branch', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                // Skipped as it's the storyboard
      '/scene-2' +          // Skipped because we skip over Scenes
      '/Card-instance' +    // Skipped because we skip component children of Scenes
      ':Card-Root' +        // Skipped because we skip over root elements
      '/Card-Row-Buttons',  // <- Single click
      '/Card-Button-3',     // <- Double click
      '/cond'               // <- Double click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithConditional,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[2]])
  })

  it('Double click can not dive into conditional inside element when the conditional has siblings', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                // Skipped as it's the storyboard
      '/scene-2' +          // Skipped because we skip over Scenes
      '/Card-instance' +    // Skipped because we skip component children of Scenes
      ':Card-Root' +        // Skipped because we skip over root elements
      '/Card-Row-Buttons',  // <- Single click
      '/Card-Button-3',     // <- Double click
    )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithConditionalWithSiblings,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    // can not dive deeper into the conditional because it has a sibling
    checkSelectedPaths(renderResult, [desiredPaths[1]])
  })
})

describe('Selection with locked elements', () => {
  it('Click selection skips locked elements', async () => {
    // prettier-ignore
    const desiredPath = EP.fromString(
        'sb' +                // Skipped as it's the storyboard
        '/scene-2' +          // Skipped because we skip over Scenes
        '/Card-instance' +    // Skipped because we skip component children of Scenes
        ':Card-Root' +        // Skipped because we skip over root elements
        '/Card-Row-Buttons' + // <- Locked element is skipped
        '/Card-Button-3',     // <- Single click
      )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithFragments,
      'await-first-dom-report',
    )

    // Lock element
    await renderResult.dispatch(
      [
        toggleSelectionLock(
          [EP.fromString('sb/scene-2/Card-instance:Card-Root/Card-Row-Buttons')],
          'locked',
        ),
      ],
      true,
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })
  it('Double click selection stops when reaching hierarchy locked elements', async () => {
    // prettier-ignore
    const desiredPath = EP.fromString(
        'sb' +                // Skipped as it's the storyboard
        '/scene-2' +          // Skipped because we skip over Scenes
        '/Card-instance' +    // Skipped because we skip component children of Scenes
        ':Card-Root' +        // Skipped because we skip over root elements
        '/Card-Row-Buttons'   // <- Single click
      )

    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimbWithFragments,
      'await-first-dom-report',
    )

    // Lock element
    await renderResult.dispatch(
      [
        toggleSelectionLock(
          [EP.fromString('sb/scene-2/Card-instance:Card-Root/Card-Row-Buttons/Card-Button-3')],
          'locked-hierarchy',
        ),
      ],
      true,
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSceneRootBounds.left + 130,
      cardSceneRootBounds.top + 220,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])

    // We can't descend any further
    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })
})

describe('Storyboard auto-focusing', () => {
  it('Scene with a single child will auto-focus the child', async () => {
    // We expect the App component to be focused, meaning we can directly select the span within it
    const desiredPath = EP.fromString('sb/sc-app/app:app-root/app-div/app-span')

    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const appSpan = renderResult.renderedDOM.getByTestId('app-span')
    const appSpanBounds = appSpan.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      appSpanBounds.left + 2,
      appSpanBounds.top + 2,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })

  it('Scene with multiple children will not auto-focus those children', async () => {
    // We expect neither of the Card components to be focused, meaning we can only directly select the instances
    const desiredPaths = [EP.fromString('sb/sc-cards/card1'), EP.fromString('sb/sc-cards/card2')]

    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    const cardSpan2 = renderResult.renderedDOM.getByTestId('card-span-2')
    const cardSpan2Bounds = cardSpan2.getBoundingClientRect()

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSpan2Bounds.left + 2,
      cardSpan2Bounds.top + 2,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])
  })

  it('Scene with multiple generated children will not auto-focus those children', async () => {
    // We expect neither of the Card components to be focused, meaning we can only directly select the instances
    const desiredPaths = [
      EP.fromString('sb/sc-generated/5f0/generated~~~1'),
      EP.fromString('sb/sc-generated/5f0/generated~~~2'),
    ]

    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const generatedSpan1 = renderResult.renderedDOM.getByTestId('generated-span-1')
    const generatedSpan1Bounds = generatedSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      generatedSpan1Bounds.left + 2,
      generatedSpan1Bounds.top + 2,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    const generatedSpan2 = renderResult.renderedDOM.getByTestId('generated-span-2')
    const generatedSpan2Bounds = generatedSpan2.getBoundingClientRect()

    await fireSingleClickEvents(
      canvasControlsLayer,
      generatedSpan2Bounds.left + 2,
      generatedSpan2Bounds.top + 2,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])
  })

  it('A child of the storyboard is not auto-focused', async () => {
    // We expect the SBChild component not to be focused, meaning we can only directly select the instance
    const desiredPath = EP.fromString('sb/sbchild')

    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const sbChildSpan = renderResult.renderedDOM.getByTestId('sbchild-span')
    const sbChildSpanBounds = sbChildSpan.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      sbChildSpanBounds.left + 2,
      sbChildSpanBounds.top + 2,
      cmdModifier,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPath])
  })
})

describe('Select mode focusing and un-focusing', () => {
  it('Double clicking an unselected component will focus it', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Check that double clicking focused the element
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])
  })

  it('Double clicking a selected component will focus it', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
      cmdModifier,
    )

    // Ensure that the selected element is neither auto-focused nor explicitly focused (so we can only select the instance)
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1')])

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Check that double clicking focused the element
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])
  })

  it('Clearing the selection or selecting a different element will not clear the focused path', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Ensure the component was selected and focused
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])

    // Select a different element
    const cardSpan2 = renderResult.renderedDOM.getByTestId('card-span-2')
    const cardSpan2Bounds = cardSpan2.getBoundingClientRect()

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSpan2Bounds.left + 2,
      cardSpan2Bounds.top + 2,
      cmdModifier,
    )

    // Check that a different element was selected without clearing the focused path
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card2')])

    // Click the empty space on the canvas
    await fireSingleClickEvents(canvasControlsLayer, -10, -10)

    // Check that the selection was cleared without clearing the focused path
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [])
  })

  it('Clearing the selection and clicking the empty canvas space will clear the focused path', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Ensure the component was selected and focused
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])

    // Click the empty space on the canvas
    await fireSingleClickEvents(canvasControlsLayer, -10, -10)

    // Check that the selection was cleared without clearing the focused path
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [])

    // Click the empty space on the canvas again
    await fireSingleClickEvents(canvasControlsLayer, -10, -10)

    // Check that the focused path has now been cleared
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [])
  })

  it('Pressing esc when nothing is selected will clear the focused path', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Ensure the component was selected and focused
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])

    // Click the empty space on the canvas
    await fireSingleClickEvents(canvasControlsLayer, -10, -10)

    // Check that the selection was cleared without clearing the focused path
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [])

    // Press the escape key
    await pressKey('Escape')

    // Check that the focused path has now been cleared
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [])
  })

  it('Pressing esc when a different element is selected will not clear the focused path', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Ensure the component was selected and focused
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])

    // Select a different element
    const cardSpan2 = renderResult.renderedDOM.getByTestId('card-span-2')
    const cardSpan2Bounds = cardSpan2.getBoundingClientRect()

    await fireSingleClickEvents(
      canvasControlsLayer,
      cardSpan2Bounds.left + 2,
      cardSpan2Bounds.top + 2,
      cmdModifier,
    )

    // Check that a different element was selected without clearing the focused path
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card2')])

    // Press the escape key
    await pressKey('Escape')

    // Check that the selection was updated without clearing the focused path
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards')])
  })

  it('Pressing esc when a focused element is selected will clear the focused path', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectScene2Children,
      'await-first-dom-report',
    )

    const cardSpan1 = renderResult.renderedDOM.getByTestId('card-span-1')
    const cardSpan1Bounds = cardSpan1.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await createDoubleClicker(
      canvasControlsLayer,
      cardSpan1Bounds.left + 2,
      cardSpan1Bounds.top + 2,
    )()

    // Ensure the component was selected and focused
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1:card1-root/card1-div')])

    // Keep pressing the esc key until we have worked our way up the hierarchy to the focused path
    await pressKey('Escape')
    checkFocusedPath(renderResult, EP.fromString('sb/sc-cards/card1'))
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1')])

    // Now pressing it again should clear the focused path without changing the selection
    await pressKey('Escape')
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [EP.fromString('sb/sc-cards/card1')])
  })
})

describe('Locking and selection', () => {
  const LockingTestProject = makeTestProjectCodeWithSnippet(`
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 59,
          top: 69,
          width: 304,
          height: 295,
        }}
        data-uid='locked'
        data-testid='locked'
      />
    </div>
  `)
  const LockedPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'locked'],
  ])

  const LockingTestProjectWithOverlappingSiblings = makeTestProjectCodeWithSnippet(`
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 59,
          top: 69,
          width: 304,
          height: 295,
        }}
        data-uid='unlocked'
        data-testid='unlocked'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 76,
          top: 85,
          width: 304,
          height: 295,
        }}
        data-uid='locked'
        data-testid='locked'
      />
    </div>
  `)
  const UnLockedPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'unlocked'],
  ])

  const LockingTestProjectWithOverlappingSiblingsAndParent = makeTestProjectCodeWithSnippet(`
  <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 59,
          top: 69,
          width: 304,
          height: 295,
        }}
        data-uid='unlocked'
        data-testid='unlocked'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 76,
          top: 85,
          width: 304,
          height: 295,
        }}
        data-uid='parent-of-locked'
        data-testid='parent-of-locked'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 18,
            top: 15,
            width: 247,
            height: 250,
          }}
          data-uid='locked-child'
          data-testid='locked-child'
        />
      </div>
    </div>
    `)

  const ParentOfLockedPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'parent-of-locked'],
  ])
  const LockedChildPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'parent-of-locked', 'locked-child'],
  ])

  it('Cant select locked element', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProject,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const lockedCenter = await getElementCentre('locked', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, lockedCenter)

    // can select when it is not locked yet
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([LockedPath])

    await renderResult.dispatch([selectComponents([], false)], true)
    await renderResult.dispatch([toggleSelectionLock([LockedPath], 'locked')], true)

    await mouseClickAtPoint(canvasControlsLayer, lockedCenter)

    // can't select now because it is locked
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([])
  })
  it('Can select element behind locked element', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithOverlappingSiblings,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const lockedCenter = await getElementCentre('locked', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, lockedCenter)

    // can select the top element when it is not locked yet
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([LockedPath])

    await renderResult.dispatch([selectComponents([], false)], true)
    await renderResult.dispatch([toggleSelectionLock([LockedPath], 'locked')], true)

    await mouseClickAtPoint(canvasControlsLayer, lockedCenter)

    // the bottom element is selected, because we can click through the locked element
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([UnLockedPath])
  })
  it('Unlocked parent of locked element is selected and not the element behind it', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithOverlappingSiblingsAndParent,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const lockedCenter = await getElementCentre('locked-child', renderResult)

    await renderResult.dispatch([toggleSelectionLock([LockedChildPath], 'locked')], true)

    await mouseClickAtPoint(canvasControlsLayer, lockedCenter)

    // the parent of the locked element is selected, because that still covers the unlocked element behind
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([ParentOfLockedPath])
  })
  it('Can select behind hierarchy locked parent and its child', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithOverlappingSiblingsAndParent,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const lockedCenter = await getElementCentre('locked-child', renderResult)

    await renderResult.dispatch(
      [toggleSelectionLock([LockedChildPath, ParentOfLockedPath], 'locked')],
      true,
    )

    await mouseClickAtPoint(canvasControlsLayer, lockedCenter)

    // the parent is hierachy locked (so its child is locked too), so we can select the unlocked element behind the locked parent and its child
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([UnLockedPath])
  })
})

describe('Locking and selection in components', () => {
  const LockingTestProjectWithComponent = `
    import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='${TestSceneUID}'
    >
      <Playground
        style={{}}
        data-uid='${TestAppUID}'
      />
    </Scene>
  </Storyboard>
)

export var Playground = ({ style }) => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
        ...style,
      }}
      data-uid='root'
      data-label='root'
    >
      <Foo data-uid='Foo'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 65,
            top: 58,
            width: 329,
            height: 266,
          }}
          data-label='Foo_props_child'
          data-uid='Foo_props_child'
          data-testid='Foo_props_child'
        />
      </Foo>
    </div>
  )
}

const Foo = (props) => {
  return (
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 91,
        top: 81,
        width: 484,
        height: 390,
      }}
      data-label='Foo_root_div'
      data-uid='Foo_root_div'
    >
      {props.children}
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 44,
          top: 38,
          width: 390,
          height: 303,
        }}
        data-label='Foo_internal_child'
        data-uid='Foo_internal_child'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 118,
            top: 27,
            width: 149,
            height: 162,
          }}
            data-label='Foo_internal_grandchild'
          data-uid='Foo_internal_grandchild'
        />
      </div>
    </div>
  )
}
  `

  const FooPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['root', 'Foo'],
  ])

  const PropsChildPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['root', 'Foo', 'Foo_props_child'],
  ])

  const InternalChild = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['root', 'Foo'],
    ['Foo_root_div', 'Foo_internal_child'],
  ])

  const InternalGrandChild = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['root', 'Foo'],
    ['Foo_root_div', 'Foo_internal_child', 'Foo_internal_grandchild'],
  ])

  it('In not locked case overlapping internal child is selected instead of the props child', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithComponent,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await renderResult.dispatch([setFocusedElement(FooPath)], true)

    const propsChildCenter = await getElementCentre('Foo_props_child', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, propsChildCenter, { modifiers: cmdModifier })

    // can't select props child because it is covered by the internal child and grandchild
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([InternalGrandChild])
  })

  it('When the internal grandchild is locked, its internal parent is selected instead of the props child, because it is unlocked and covers props child', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithComponent,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await renderResult.dispatch([setFocusedElement(FooPath)], true)
    await renderResult.dispatch([toggleSelectionLock([InternalGrandChild], 'locked')], true)

    const propsChildCenter = await getElementCentre('Foo_props_child', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, propsChildCenter, { modifiers: cmdModifier })

    // can't select props child because it is covered by the internal child and grandchild, grandchild is locked so child is selected
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([InternalChild])
  })

  it('When the internal child is locked, the unlocked granchild is selected instead of the props child, because it covers it', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithComponent,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await renderResult.dispatch([setFocusedElement(FooPath)], true)
    await renderResult.dispatch([toggleSelectionLock([InternalChild], 'locked')], true)
    const propsChildCenter = await getElementCentre('Foo_props_child', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, propsChildCenter, { modifiers: cmdModifier })

    // can't select props child because it is covered by the internal child and grandchild, internal child is locked, but grandchild is not
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([InternalGrandChild])
  })
  it('When the internal child is hierarchy locked, props child can be selected, because all internal covering elements are locked', async () => {
    const renderResult = await renderTestEditorWithCode(
      LockingTestProjectWithComponent,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await renderResult.dispatch([setFocusedElement(FooPath)], true)
    await renderResult.dispatch([toggleSelectionLock([InternalChild], 'locked-hierarchy')], true)
    const propsChildCenter = await getElementCentre('Foo_props_child', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, propsChildCenter, { modifiers: cmdModifier })

    // can select props child because even though it is covered by the internal child and grandchild, they are both locked by the hierarchy lock of the internal child
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([PropsChildPath])
  })
})

async function getElementRect(testId: string, renderResult: EditorRenderResult) {
  const targetElement = await renderResult.renderedDOM.findByTestId(testId)
  return targetElement.getBoundingClientRect()
}

function getDOMRectCentre(domRect: DOMRect) {
  return {
    x: domRect.x + domRect.width / 2,
    y: domRect.y + domRect.height / 2,
  }
}

async function getElementCentre(testId: string, renderResult: EditorRenderResult) {
  const elementRect = await getElementRect(testId, renderResult)
  return getDOMRectCentre(elementRect)
}

describe('mouseup selection', () => {
  const MouseupTestProject = makeTestProjectCodeWithSnippet(`
      <div
        style={{ width: '100%', height: '100%' }}
        data-uid='app-root'
      >
        <div
          data-uid='red'
          data-testid='red'
          style={{
            position: 'absolute',
            left: 0,
            width: 50,
            top: 0,
            height: 50,
            backgroundColor: 'red',
          }}
        />
        <div
          data-uid='blue'
          data-testid='blue'
          style={{
            position: 'absolute',
            left: 75,
            width: 50,
            top: 0,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
        <div
          data-uid='green'
          data-testid='green'
          style={{
            position: 'absolute',
            left: 150,
            width: 50,
            top: 0,
            height: 50,
            backgroundColor: 'green',
          }}
        />
      </div>
    `)

  const ScenePath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID]])
  const RedPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'red'],
  ])
  const BluePath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'blue'],
  ])
  const GreenPath = EP.elementPath([
    [BakedInStoryboardUID, TestSceneUID, TestAppUID],
    ['app-root', 'green'],
  ])

  describe('Interactions in selected scene', () => {
    it('Can select element in selected scene by clicking on it', async () => {
      const renderResult = await renderTestEditorWithCode(
        MouseupTestProject,
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([ScenePath], false)], true)

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const redCentre = await getElementCentre('red', renderResult)
      await mouseDownAtPoint(canvasControlsLayer, redCentre)

      // selection should not change on mouse down
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([ScenePath])
      await mouseUpAtPoint(canvasControlsLayer, redCentre)

      // selection should change on mouse up
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

      // Check nothing has changed in the project
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(MouseupTestProject)
    })

    it('Can select element in selected scene by dragging it under the drag threshold', async () => {
      const renderResult = await renderTestEditorWithCode(
        MouseupTestProject,
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([ScenePath], false)], true)

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const redCentre = await getElementCentre('red', renderResult)
      await mouseMoveToPoint(canvasControlsLayer, redCentre)

      await mouseDownAtPoint(canvasControlsLayer, redCentre)
      // selection should not change on mouse down
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([ScenePath])

      await mouseMoveToPoint(canvasControlsLayer, { x: redCentre.x + 1, y: redCentre.y + 1 })

      await mouseUpAtPoint(canvasControlsLayer, { x: redCentre.x + 1, y: redCentre.y + 1 })

      // selection should change on mouse up because dragging was below threshold, so this interaction was something like a click
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

      // Check nothing has changed in the project
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(MouseupTestProject)
    })

    it('When the scene is selected, dragging an element inside it drags the scene and the selection doesnt change', async () => {
      const renderResult = await renderTestEditorWithCode(
        MouseupTestProject,
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([ScenePath], false)], true)

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const redCentre = await getElementCentre('red', renderResult)
      await mouseMoveToPoint(canvasControlsLayer, redCentre)
      await mouseDownAtPoint(canvasControlsLayer, redCentre)

      // selection should not change on mouse down
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([ScenePath])

      await mouseMoveToPoint(canvasControlsLayer, { x: redCentre.x + 100, y: redCentre.y + 100 })
      await mouseUpAtPoint(canvasControlsLayer, { x: redCentre.x + 100, y: redCentre.y + 100 })

      // selection should not change on mouse up because dragging of the scene was successful
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([ScenePath])

      // Dragging was successful so the project has changed
      expect(getPrintedUiJsCode(renderResult.getEditorState())).not.toEqual(MouseupTestProject)
    })

    it('When the scene is selected, cmd-mousedown selects the element inside it', async () => {
      const renderResult = await renderTestEditorWithCode(
        MouseupTestProject,
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([ScenePath], false)], true)

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const redCentre = await getElementCentre('red', renderResult)
      await mouseMoveToPoint(canvasControlsLayer, redCentre)

      await mouseDownAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })

      // selection should change on mouse down because cmd was down
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])
    })

    it('When the scene is selected, cmd-drag drags the element inside it', async () => {
      const renderResult = await renderTestEditorWithCode(
        MouseupTestProject,
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([ScenePath], false)], true)

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const redCentre = await getElementCentre('red', renderResult)
      await mouseMoveToPoint(canvasControlsLayer, redCentre)

      await mouseDownAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })

      // selection should change on mouse down because cmd was down. This is necessary, the following drag will move the element inside
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

      await mouseMoveToPoint(
        canvasControlsLayer,
        { x: redCentre.x + 100, y: redCentre.y + 100 },
        { modifiers: cmdModifier },
      )
      await mouseUpAtPoint(canvasControlsLayer, { x: redCentre.x + 100, y: redCentre.y + 100 })

      // selection is still on dragged the element
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

      // Dragging was successful so the project has changed
      expect(getPrintedUiJsCode(renderResult.getEditorState())).not.toEqual(MouseupTestProject)
    })

    it('When the scene is selected, cmd-dragging it below the drag threshold selects the element inside it', async () => {
      const renderResult = await renderTestEditorWithCode(
        MouseupTestProject,
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([ScenePath], false)], true)

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const redCentre = await getElementCentre('red', renderResult)
      await mouseMoveToPoint(canvasControlsLayer, redCentre)

      await mouseDownAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })

      // selection should change on mouse down because cmd was down
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

      await mouseMoveToPoint(
        canvasControlsLayer,
        { x: redCentre.x + 100, y: redCentre.y + 100 },
        { modifiers: cmdModifier },
      )
      await mouseUpAtPoint(canvasControlsLayer, { x: redCentre.x + 1, y: redCentre.y + 1 })

      // selection is still on the element which was already selected on mousedown
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

      // Dragging was not successful so the project hasn't changed
      expect(getPrintedUiJsCode(renderResult.getEditorState())).not.toEqual(MouseupTestProject)
    })
  })

  it('mouseup in the gap between a multi-selection will not select the element behind if no drag happens', async () => {
    const renderResult = await renderTestEditorWithCode(
      MouseupTestProject,
      'await-first-dom-report',
      { strategiesToUse: [] },
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const redCentre = await getElementCentre('red', renderResult)
    const blueCentre = await getElementCentre('blue', renderResult)
    const greenCentre = await getElementCentre('green', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })
    await mouseClickAtPoint(canvasControlsLayer, greenCentre, { modifiers: shiftCmdModifier })

    // Check we have multi-selected the red and green elements
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath, GreenPath])

    // mousedown over the blue element to check that doesn't select it, as it is within the multiselect bounds
    await mouseMoveToPoint(canvasControlsLayer, blueCentre)
    await mouseDownAtPoint(canvasControlsLayer, blueCentre)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath, GreenPath])

    // now mouseup over that blue element to check that it _does_ select it
    await mouseUpAtPoint(canvasControlsLayer, blueCentre)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([BluePath])

    // Check nothing has changed in the project
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(MouseupTestProject)
  })

  it('mouseup in the gap between a multi-selection will not select the element behind if a drag happens', async () => {
    const renderResult = await renderTestEditorWithCode(
      MouseupTestProject,
      'await-first-dom-report',
      { strategiesToUse: [] },
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const redCentre = await getElementCentre('red', renderResult)
    const blueCentre = await getElementCentre('blue', renderResult)
    const greenCentre = await getElementCentre('green', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })
    await mouseClickAtPoint(canvasControlsLayer, greenCentre, { modifiers: shiftCmdModifier })

    // Check we have multi-selected the red and green elements
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath, GreenPath])

    // mousedown over the blue element to check that doesn't select it, as it is within the multiselect bounds
    await mouseMoveToPoint(canvasControlsLayer, blueCentre)
    await mouseDownAtPoint(canvasControlsLayer, blueCentre)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath, GreenPath])

    // trigger enough of a drag to surpass the dragging threshold, but move the element back
    await mouseMoveToPoint(canvasControlsLayer, redCentre, { eventOptions: { buttons: 1 } })
    await mouseMoveToPoint(canvasControlsLayer, blueCentre, { eventOptions: { buttons: 1 } })

    // now mouseup over that blue element to check that the selection doesn't change
    await mouseUpAtPoint(canvasControlsLayer, blueCentre)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath, GreenPath])

    // Check nothing has changed in the project
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(MouseupTestProject)
  })

  it('mouseup does not change selection after a cancelled interaction', async () => {
    const renderResult = await renderTestEditorWithCode(
      MouseupTestProject,
      'await-first-dom-report',
      { strategiesToUse: [] },
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const redCentre = await getElementCentre('red', renderResult)
    const blueCentre = await getElementCentre('blue', renderResult)

    await mouseClickAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

    // Drag the red element directly over the blue element
    await mouseDownAtPoint(canvasControlsLayer, redCentre)
    await mouseMoveToPoint(canvasControlsLayer, blueCentre, { eventOptions: { buttons: 1 } })

    // Cancel the interaction, then mouseup and check that the selection wasn't changed
    await pressKey('Escape')
    await mouseUpAtPoint(canvasControlsLayer, blueCentre)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

    // Check nothing has changed in the project
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(MouseupTestProject)
  })
  describe('overflown text', () => {
    it('Clicking on overflown text selects the parent element', async () => {
      // prettier-ignore
      const desiredPath = EP.fromString(
      'sb' +      // Skipped as it's the storyboard
      '/sc' +     // Skipped because we skip over Scenes
      '/pg' +     // Skipped because we skip component children of Scenes
      ':pg-root' + // Skipped because we skip root element of root component of Scenes
      '/pg-div'  // Single click
    )

      const renderResult = await renderTestEditorWithCode(
        TestProjectOverflownText,
        'await-first-dom-report',
      )

      const overflownText = renderResult.renderedDOM.getByTestId('pg-div')
      const overflownTextBounds = overflownText.getBoundingClientRect()

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      // Clicking outside of bounds on the overflown text
      await fireSingleClickEvents(
        canvasControlsLayer,
        overflownTextBounds.left + overflownTextBounds.width + 10,
        overflownTextBounds.top + 10,
      )

      checkSelectedPaths(renderResult, [desiredPath])
    })
  })

  xit('clicking on the catchment area of a control but over another element does not change the selection', async () => {
    // FIXME for some reason the absolute resize controls don't capture the mousedown event here
    // even though they definitely should
    const renderResult = await renderTestEditorWithCode(
      MouseupTestProject,
      'await-first-dom-report',
      { strategiesToUse: [] },
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const redElementRect = await getElementRect('red', renderResult)
    const redCentre = getDOMRectCentre(redElementRect)

    // Far enough to the left to be outside of the element, but still inside the resize control
    // catchment area
    const justToTheLeftOfRedElement = {
      x: redElementRect.x - 2,
      y: redElementRect.y + redElementRect.height / 2,
    }

    // Miles away
    const farToTheLeftOfRedElement = {
      x: redElementRect.x - 200,
      y: redElementRect.y + redElementRect.height / 2,
    }

    await mouseClickAtPoint(canvasControlsLayer, redCentre, { modifiers: cmdModifier })
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

    await renderResult.getDispatchFollowUpActionsFinished()

    // Click just to the left of the red element
    await mouseMoveToPoint(canvasControlsLayer, justToTheLeftOfRedElement)
    await mouseClickAtPoint(canvasControlsLayer, justToTheLeftOfRedElement)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([RedPath])

    // Just to be sure, if we move further left it clears the selection
    await mouseMoveToPoint(canvasControlsLayer, farToTheLeftOfRedElement)
    await mouseClickAtPoint(canvasControlsLayer, farToTheLeftOfRedElement)
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([])

    // Check nothing has changed in the project
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(MouseupTestProject)
  })
})

describe('Problematic selection', () => {
  // This was a real world bug caused by the unholy combination of these things

  async function selectInjectedDivAndCheckAllPaths(
    renderResult: EditorRenderResult,
    injectedDiv: HTMLElement,
    desiredPaths: Array<ElementPath>,
  ) {
    const injectedDivBounds = injectedDiv.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const doubleClick = createDoubleClicker(
      canvasControlsLayer,
      injectedDivBounds.left + injectedDivBounds.width / 2,
      injectedDivBounds.top + injectedDivBounds.height / 2,
    )

    await fireSingleClickEvents(
      canvasControlsLayer,
      injectedDivBounds.left + injectedDivBounds.width / 2,
      injectedDivBounds.top + injectedDivBounds.height / 2,
    )

    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[0]])

    await doubleClick()
    checkFocusedPath(renderResult, null)
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[1])
    checkSelectedPaths(renderResult, [desiredPaths[1]])

    await doubleClick()
    checkFocusedPath(renderResult, desiredPaths[1])
    checkSelectedPaths(renderResult, [desiredPaths[2]])
  }

  it('Can keep clicking to select the parent of a dangerouslySetInnerHTML element, inside a component with a fragment', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +                   // Skipped as it's the storyboard
      '/div-a',                // Single click to select
      '/FragmentAtRoot',       // Double click to select, double click again to focus
      ':FragmentAtRoot-root' + // Skipped as it's locked
      '/FragmentAtRoot-target' // Third double click to select
    )

    const renderResult = await renderTestEditorWithCode(
      DangerouslySetInnerHTMLProject,
      'await-first-dom-report',
    )

    const injectedDiv = renderResult.renderedDOM.getAllByTestId('injected-div')[0]

    await selectInjectedDivAndCheckAllPaths(renderResult, injectedDiv, desiredPaths)
  })

  it('Can keep clicking to select the parent of a dangerouslySetInnerHTML element, inside a component with a div', async () => {
    // prettier-ignore
    const desiredPaths = createConsecutivePaths(
      'sb' +              // Skipped as it's the storyboard
      '/div-b',           // Single click to select
      '/DivAtRoot',       // Double click to select, double click again to focus
      ':DivAtRoot-root' + // Skipped as it's locked
      '/DivAtRoot-target' // Third double click
    )

    const renderResult = await renderTestEditorWithCode(
      DangerouslySetInnerHTMLProject,
      'await-first-dom-report',
    )

    const injectedDiv = renderResult.renderedDOM.getAllByTestId('injected-div')[1]

    await selectInjectedDivAndCheckAllPaths(renderResult, injectedDiv, desiredPaths)
  })
})

function createConsecutivePaths(...partialPathStrings: Array<string>): Array<ElementPath> {
  return partialPathStrings.map((_value, index, arr) => {
    const joinedParts = arr.slice(0, index + 1).join('')
    return EP.fromString(joinedParts)
  })
}

function checkFocusedPath(renderResult: EditorRenderResult, expected: ElementPath | null) {
  checkWithKey('focusedPath', renderResult.getEditorState().editor.focusedElementPath, expected)
}

function checkSelectedPaths(renderResult: EditorRenderResult, expected: Array<ElementPath>) {
  checkWithKey('selectedPaths', renderResult.getEditorState().editor.selectedViews, expected)
}

function checkWithKey<T>(key: string, actual: T, expected: T) {
  expect({
    check: key,
    value: actual,
  }).toEqual({
    check: key,
    value: expected,
  })
}

const DangerouslySetInnerHTMLProject = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const content =
  '<div ' +
  ' style=" ' +
  ' background-color: lightblue;' +
  ' width: 150px;' +
  ' height: 150px;' +
  ' " ' +
  ' data-testid="injected-div" ' +
  ' >Click me if you can!</div> '

const UnSelectable = () => {
  return (
    <React.Fragment data-uid='FragmentAtRoot-root'>
      You can't select this
      <div
        dangerouslySetInnerHTML={{ __html: content }}
        data-uid='FragmentAtRoot-target'
      />
    </React.Fragment>
  )
}

const Selectable = () => {
  return (
    <div data-uid='DivAtRoot-root'>
      You can select this
      <div
        dangerouslySetInnerHTML={{ __html: content }}
        data-uid='DivAtRoot-target'
      />
    </div>
  )
}

const Card = (props) => {
  return <div style={props.style}>{props.children}</div>
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 400,
        top: 50,
        width: 300,
        height: 300,
      }}
      data-uid='div-a'
    >
      <UnSelectable data-uid='FragmentAtRoot' />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 400,
        top: 364,
        width: 300,
        height: 300,
      }}
      data-uid='div-b'
    >
      <Selectable data-uid='DivAtRoot' />
    </div>
  </Storyboard>
)
`

const generateTestProjectAlpineClimb = (conditional: boolean, conditionalSiblings: boolean) => `
import * as React from "react";
import { Scene, Storyboard } from "utopia-api";
import styled from "@emotion/styled";

export const Col = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      gap: 34,
      padding: 12,
      alignItems: "stretch",
      backgroundColor: "yellow",
    }}
    data-uid="Col-Root"
  >
    {props.children}
  </div>
);

export const LabelRow = (props) => (
  <div
    style={{
      color: "white",
      display: "flex",
      alignItems: "center",
      fontFamily: "sans-serif",
      backdropFilter: "blur(6px) brightness(120%)",
      paddingLeft: 12,
      paddingRight: 12,
      position: "absolute",
      bottom: 0,
      left: 0,
      height: 34,
      right: 0,
    }}
    data-uid="LabelRow-Root"
  >
    <div style={{ flex: "1 0 150px" }} data-uid="LabelRow-div">
      Beautiful Hackney Street Arrrrt
    </div>
    <Button data-uid="LabelRow-Button">Hello</Button>
  </div>
);

export const Button = styled.button({
  minWidth: 40,
  minHeight: 22,
  boxShadow: "1px 1px 0px 1px black",
  backgroundColor: "#00FFAA",
  color: "black",
});

export const CardList = (props) => {
  cards = [1, 2, 3, 4, 5];

  return (
    <div data-uid="CardList-Root">
      <h2 data-uid="CardList-h2">List of available street art</h2>
      <Col data-uid="CardList-Col">
        {
          // @utopia/uid=cardlist-expr
          cards.map((card) => (
            <Card data-uid="CardList-Card" testid={'generated-card-'+ card} />
          ))
        }
      </Col>
    </div>
  );
};

export const ManualCardList = (props) => {
  return (
    <div data-uid="ManualCardList-root">
      <h2 data-uid="ManualCardList-h2">List of available street art</h2>
      <Col data-uid="ManualCardList-Col">
        <Card data-uid="ManualCardList-Card-1" />
        <Card data-uid="ManualCardList-Card-2" />
      </Col>
    </div>
  );
};

export const Card = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      width: 364,
      height: 250,
      backgroundColor: "hsl(0,0%,95%)",
      boxShadow: "0px 0px 0px 1px black",
    }}
    data-testid={props.testid}
    data-uid="Card-Root"
  >
    <Row
      style={{ minHeight: 200, position: "relative", overflow: "hidden" }}
      data-uid="Card-Row"
    >
      <img
        src="https://www.hackneycitizen.co.uk/wp-content/uploads/nerone-1-620.jpg"
        data-uid="Card-img"
      />
      <LabelRow data-uid="Card-LabelRow" />
    </Row>
    <Row style={{ minHeight: 40, gap: 12 }} data-uid="Card-Row-Buttons">
      <Button data-uid="Card-Button-1">Hello</Button>
      <Button data-uid="Card-Button-2">Button</Button>
      <Button data-uid="Card-Button-3">${
        conditional
          ? `{
        // @utopia/uid=cond 
        true ? 'Button' : <div />
      }
        `
          : 'Button'
      }
      ${conditionalSiblings ? '<div />' : ''}</Button>
    </Row>
  </div>
);

export const FlexRow = styled.div({
  display: "flex",
  alignItems: "center",
});

export const Row = styled(FlexRow)({});
export const UIRow = styled.div({
  minHeight: 34,
  paddingLeft: 8,
  paddingRight: 8,
  display: "flex",
  alignItems: "center",
});

export const UIListRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});
export const UIListGridRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "28px 1fr",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const UIContextMenuRow = styled(UIRow)({
  height: 22,
  borderRadius: 2,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "1fr auto",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const ContextMenu = (props) => {
  return (
    <div
      style={{
        borderRadius: 4,
        padding: 4,
        backgroundColor: "hsl(0,0%,95%)",
        paddingTop: 4,
        paddingBottom: 4,
        width: 202,
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        boxShadow:
          "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
      }}
      data-uid="ContextMenu-Root"
    >
      <UIContextMenuRow data-uid="ContextMenu-Copy">
        <span data-uid="918">Copy</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="f49">
          ⌘+C
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Cut">
        <span data-uid="a3e">Cut</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="b34">
          ⌘+X
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Paste">
        <span data-uid="09d">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="706">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-">
        <span data-uid="821">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="46d">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <hr
        style={{
          color: "yello",
          background: "purple",
          stroke: "grey",
          backgroundColor: "/*rgb(128, 0, 128, 1)*/",
          border: "0.5px solid rgb(255, 20, 20, 1)",
          height: 1,
        }}
        data-uid="ContextMenu-hr"
      />
      <UIContextMenuRow data-uid="ContextMenu-Backward">
        <span data-uid="ab9">Bring Backward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="c2b">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Forward">
        <span data-uid="cb2">Bring Forward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="07c">
          ⌘+V
        </span>
      </UIContextMenuRow>
    </div>
  );
};

export var App = (props) => {
  return (
    <div
      style={{
        padding: 20,
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        WebkitTextRendering: "subpixel-antialiased",
      }}
      data-uid="App-root"
    >
      <div
        style={{
          width: 270,
          height: 215,
          background: "hsl(0,0%,97%)",
          backgroundColor: "rgb(247, 247, 247, 1)",
          boxShadow:
            "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
          borderRadius: 3,
        }}
        data-uid="App-div"
      >
        <div
          style={{
            paddingLeft: 8,
            paddingRight: 8,
            fontFamily: "Inter",
            fontSize: 11,
            color: "hsl(0,0%,10%)",
            display: "flex",
            alignItems: "center",
            height: 34,
          }}
          data-uid="App-div-div"
        >
          <span style={{ fontWeight: 600 }} data-uid="App-div-div-span">
            Popup Menu
          </span>
        </div>
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            paddingLeft: 8,
            paddingRight: 8,
          }}
          data-uid="57e"
        >
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Select"
          >
            <Row data-uid="681" />
            <Row data-uid="04c">Select Elements</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Copy"
          >
            <div data-uid="2a2" />
            <div data-uid="329">Copy</div>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Pase"
          >
            <div data-uid="2ce" />
            <div data-uid="1a3">Paste</div>
          </UIListGridRow>
          <UIListGridRow data-uid="App-Check">
            <Row style={{ justifyContent: "center" }} data-uid="a1d">
              ✓
            </Row>
            <Row data-uid="2d9">A little label</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Magic"
          >
            <div data-uid="765" />
            <div data-uid="a20">Magic</div>
          </UIListGridRow>
        </div>
      </div>
    </div>
  );
};
export var storyboard = (
  <Storyboard data-uid="sb" >
    <Scene
      style={{ position: "absolute", left: 0, top: 0, width: 313, height: 261 }}
      data-uid="scene-App"
    >
      <App data-uid="App-instance" />
    </Scene>
    <Scene
      data-label="Scene 1"
      style={{
        position: "absolute",
        padding: 20,
        left: 0,
        top: 299,
        width: 280,
        height: 196,
      }}
      data-uid="scene-1"
    >
      <ContextMenu data-uid="ContextMenu-instance" />
    </Scene>
    <Scene
      data-label="Scene 2"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: -19,
        width: 400,
        height: 300,
      }}
      data-uid="scene-2"
    >
      <Card data-uid="Card-instance" testid="card-scene" />
    </Scene>
    <Scene
      data-label="List of Cards"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: 350,
        width: 500,
        height: 400,
      }}
      data-uid="scene-CardList"
    >
      <CardList data-uid="CardList-instance" />
    </Scene>
    <Scene
      data-label="Card component out of place for focus mode"
      style={{
        position: "absolute",
        padding: 20,
        left: 1060,
        top: -31,
        width: 500,
        height: 400,
      }}
      data-uid="scene-ManualCardList"
    >
      <ManualCardList data-uid="ManualCardList-uid" />
    </Scene>
  </Storyboard>
);
`

const TestProjectAlpineClimb = generateTestProjectAlpineClimb(false, false)
const TestProjectAlpineClimbWithConditional = generateTestProjectAlpineClimb(true, false)
const TestProjectAlpineClimbWithConditionalWithSiblings = generateTestProjectAlpineClimb(true, true)

const TestProjectAlpineClimbWithFragments = `
import * as React from "react";
import { Scene, Storyboard } from "utopia-api";
import styled from "@emotion/styled";

export const Col = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      gap: 34,
      padding: 12,
      alignItems: "stretch",
      backgroundColor: "yellow",
    }}
    data-uid="Col-Root"
  >
    {props.children}
  </div>
);

export const LabelRow = (props) => (
  <div
    style={{
      color: "white",
      display: "flex",
      alignItems: "center",
      fontFamily: "sans-serif",
      backdropFilter: "blur(6px) brightness(120%)",
      paddingLeft: 12,
      paddingRight: 12,
      position: "absolute",
      bottom: 0,
      left: 0,
      height: 34,
      right: 0,
    }}
    data-uid="LabelRow-Root"
  >
    <div style={{ flex: "1 0 150px" }} data-uid="LabelRow-div">
      Beautiful Hackney Street Arrrrt
    </div>
    <Button data-uid="LabelRow-Button">Hello</Button>
  </div>
);

export const Button = styled.button({
  minWidth: 40,
  minHeight: 22,
  boxShadow: "1px 1px 0px 1px black",
  backgroundColor: "#00FFAA",
  color: "black",
});

export const CardList = (props) => {
  cards = [1, 2, 3, 4, 5];

  return (
    <>
      <h2 data-uid="CardList-h2">List of available street art</h2>
      <Col data-uid="CardList-Col">
        {
          // @utopia/uid=cardlist-expr
          cards.map((card) => (
            <Card data-uid="CardList-Card" testid={'generated-card-'+ card} />
          ))
        }
      </Col>
    </>
  );
};

export const ManualCardList = (props) => {
  return (
    <>
      <h2 data-uid="ManualCardList-h2">List of available street art</h2>
      <Col data-uid="ManualCardList-Col">
        <Card data-uid="ManualCardList-Card-1" />
        <Card data-uid="ManualCardList-Card-2" />
      </Col>
    </>
  );
};

export const Card = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      width: 364,
      height: 250,
      backgroundColor: "hsl(0,0%,95%)",
      boxShadow: "0px 0px 0px 1px black",
    }}
    data-testid={props.testid}
    data-uid="Card-Root"
  >
    <Row
      style={{ minHeight: 200, position: "relative", overflow: "hidden" }}
      data-uid="Card-Row"
    >
      <img
        src="https://www.hackneycitizen.co.uk/wp-content/uploads/nerone-1-620.jpg"
        data-uid="Card-img"
      />
      <LabelRow data-uid="Card-LabelRow" />
    </Row>
    <Row style={{ minHeight: 40, gap: 12 }} data-uid="Card-Row-Buttons">
      <Button data-uid="Card-Button-1">Hello</Button>
      <Button data-uid="Card-Button-2">Button</Button>
      <Button data-uid="Card-Button-3">Button</Button>
    </Row>
  </div>
);

export const FlexRow = styled.div({
  display: "flex",
  alignItems: "center",
});

export const Row = styled(FlexRow)({});
export const UIRow = styled.div({
  minHeight: 34,
  paddingLeft: 8,
  paddingRight: 8,
  display: "flex",
  alignItems: "center",
});

export const UIListRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});
export const UIListGridRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "28px 1fr",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const UIContextMenuRow = styled(UIRow)({
  height: 22,
  borderRadius: 2,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "1fr auto",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const ContextMenu = (props) => {
  return (
    <div
      style={{
        borderRadius: 4,
        padding: 4,
        backgroundColor: "hsl(0,0%,95%)",
        paddingTop: 4,
        paddingBottom: 4,
        width: 202,
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        boxShadow:
          "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
      }}
      data-uid="ContextMenu-Root"
    >
      <UIContextMenuRow data-uid="ContextMenu-Copy">
        <span data-uid="918">Copy</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="f49">
          ⌘+C
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Cut">
        <span data-uid="a3e">Cut</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="b34">
          ⌘+X
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Paste">
        <span data-uid="09d">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="706">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-">
        <span data-uid="821">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="46d">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <hr
        style={{
          color: "yello",
          background: "purple",
          stroke: "grey",
          backgroundColor: "/*rgb(128, 0, 128, 1)*/",
          border: "0.5px solid rgb(255, 20, 20, 1)",
          height: 1,
        }}
        data-uid="ContextMenu-hr"
      />
      <UIContextMenuRow data-uid="ContextMenu-Backward">
        <span data-uid="ab9">Bring Backward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="c2b">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Forward">
        <span data-uid="cb2">Bring Forward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="07c">
          ⌘+V
        </span>
      </UIContextMenuRow>
    </div>
  );
};

export var App = (props) => {
  return (
    <div
      style={{
        padding: 20,
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        WebkitTextRendering: "subpixel-antialiased",
      }}
      data-uid="App-root"
    >
      <div
        style={{
          width: 270,
          height: 215,
          background: "hsl(0,0%,97%)",
          backgroundColor: "rgb(247, 247, 247, 1)",
          boxShadow:
            "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
          borderRadius: 3,
        }}
        data-uid="App-div"
      >
        <div
          style={{
            paddingLeft: 8,
            paddingRight: 8,
            fontFamily: "Inter",
            fontSize: 11,
            color: "hsl(0,0%,10%)",
            display: "flex",
            alignItems: "center",
            height: 34,
          }}
          data-uid="App-div-div"
        >
          <span style={{ fontWeight: 600 }} data-uid="App-div-div-span">
            Popup Menu
          </span>
        </div>
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            paddingLeft: 8,
            paddingRight: 8,
          }}
          data-uid="57e"
        >
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Select"
          >
            <Row data-uid="681" />
            <Row data-uid="04c">Select Elements</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Copy"
          >
            <div data-uid="2a2" />
            <div data-uid="329">Copy</div>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Pase"
          >
            <div data-uid="2ce" />
            <div data-uid="1a3">Paste</div>
          </UIListGridRow>
          <UIListGridRow data-uid="App-Check">
            <Row style={{ justifyContent: "center" }} data-uid="a1d">
              ✓
            </Row>
            <Row data-uid="2d9">A little label</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Magic"
          >
            <div data-uid="765" />
            <div data-uid="a20">Magic</div>
          </UIListGridRow>
        </div>
      </div>
    </div>
  );
};
export var storyboard = (
  <Storyboard data-uid="sb" >
    <Scene
      style={{ position: "absolute", left: 0, top: 0, width: 313, height: 261 }}
      data-uid="scene-App"
    >
      <App data-uid="App-instance" />
    </Scene>
    <Scene
      data-label="Scene 1"
      style={{
        position: "absolute",
        padding: 20,
        left: 0,
        top: 299,
        width: 280,
        height: 196,
      }}
      data-uid="scene-1"
    >
      <ContextMenu data-uid="ContextMenu-instance" />
    </Scene>
    <Scene
      data-label="Scene 2"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: -19,
        width: 400,
        height: 300,
      }}
      data-uid="scene-2"
    >
      <Card data-uid="Card-instance" testid="card-scene" />
    </Scene>
    <Scene
      data-label="List of Cards"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: 350,
        width: 500,
        height: 400,
      }}
      data-uid="scene-CardList"
    >
      <CardList data-uid="CardList-instance" />
    </Scene>
    <Scene
      data-label="Card component out of place for focus mode"
      style={{
        position: "absolute",
        padding: 20,
        left: 1060,
        top: -31,
        width: 500,
        height: 400,
      }}
      data-uid="scene-ManualCardList"
    >
      <ManualCardList data-uid="ManualCardList-uid" />
    </Scene>
  </Storyboard>
);
`

const TestProjectAlpineClimbWithCardWithRootFragment = `
import * as React from "react";
import { Scene, Storyboard } from "utopia-api";
import styled from "@emotion/styled";

export const Col = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      gap: 34,
      padding: 12,
      alignItems: "stretch",
      backgroundColor: "yellow",
    }}
    data-uid="Col-Root"
  >
    {props.children}
  </div>
);

export const LabelRow = (props) => (
  <div
    style={{
      color: "white",
      display: "flex",
      alignItems: "center",
      fontFamily: "sans-serif",
      backdropFilter: "blur(6px) brightness(120%)",
      paddingLeft: 12,
      paddingRight: 12,
      position: "absolute",
      bottom: 0,
      left: 0,
      height: 34,
      right: 0,
    }}
    data-uid="LabelRow-Root"
  >
    <div style={{ flex: "1 0 150px" }} data-uid="LabelRow-div">
      Beautiful Hackney Street Arrrrt
    </div>
    <Button data-uid="LabelRow-Button">Hello</Button>
  </div>
);

export const Button = styled.button({
  minWidth: 40,
  minHeight: 22,
  boxShadow: "1px 1px 0px 1px black",
  backgroundColor: "#00FFAA",
  color: "black",
});

export const CardList = (props) => {
  cards = [1, 2, 3, 4, 5];

  return (
    <>
      <h2 data-uid="CardList-h2">List of available street art</h2>
      <Col data-uid="CardList-Col">
        {
          // @utopia/uid=cardlist-expr
          cards.map((card) => (
            <Card data-uid="CardList-Card" testid={'generated-card-'+ card} />
          ))
        }
      </Col>
    </>
  );
};

export const ManualCardList = (props) => {
  return (
    <>
      <h2 data-uid="ManualCardList-h2">List of available street art</h2>
      <Col data-uid="ManualCardList-Col">
        <Card data-uid="ManualCardList-Card-1" />
        <Card data-uid="ManualCardList-Card-2" />
      </Col>
    </>
  );
};

export const Card = (props) => (
  <React.Fragment data-uid="Card-Fragment-Root">
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        width: 364,
        height: 250,
        backgroundColor: "hsl(0,0%,95%)",
        boxShadow: "0px 0px 0px 1px black",
      }}
      data-testid={props.testid}
      data-uid="Card-Root"
    >
      <Row
        style={{ minHeight: 200, position: "relative", overflow: "hidden" }}
        data-uid="Card-Row"
      >
        <img
          src="https://www.hackneycitizen.co.uk/wp-content/uploads/nerone-1-620.jpg"
          data-uid="Card-img"
        />
        <LabelRow data-uid="Card-LabelRow" />
      </Row>
      <Row style={{ minHeight: 40, gap: 12 }} data-uid="Card-Row-Buttons">
        <Button data-uid="Card-Button-1">Hello</Button>
        <Button data-uid="Card-Button-2">Button</Button>
        <Button data-uid="Card-Button-3">Button</Button>
      </Row>
    </div>
  </React.Fragment>
);

export const FlexRow = styled.div({
  display: "flex",
  alignItems: "center",
});

export const Row = styled(FlexRow)({});
export const UIRow = styled.div({
  minHeight: 34,
  paddingLeft: 8,
  paddingRight: 8,
  display: "flex",
  alignItems: "center",
});

export const UIListRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});
export const UIListGridRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "28px 1fr",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const UIContextMenuRow = styled(UIRow)({
  height: 22,
  borderRadius: 2,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "1fr auto",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const ContextMenu = (props) => {
  return (
    <div
      style={{
        borderRadius: 4,
        padding: 4,
        backgroundColor: "hsl(0,0%,95%)",
        paddingTop: 4,
        paddingBottom: 4,
        width: 202,
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        boxShadow:
          "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
      }}
      data-uid="ContextMenu-Root"
    >
      <UIContextMenuRow data-uid="ContextMenu-Copy">
        <span data-uid="918">Copy</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="f49">
          ⌘+C
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Cut">
        <span data-uid="a3e">Cut</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="b34">
          ⌘+X
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Paste">
        <span data-uid="09d">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="706">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-">
        <span data-uid="821">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="46d">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <hr
        style={{
          color: "yello",
          background: "purple",
          stroke: "grey",
          backgroundColor: "/*rgb(128, 0, 128, 1)*/",
          border: "0.5px solid rgb(255, 20, 20, 1)",
          height: 1,
        }}
        data-uid="ContextMenu-hr"
      />
      <UIContextMenuRow data-uid="ContextMenu-Backward">
        <span data-uid="ab9">Bring Backward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="c2b">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Forward">
        <span data-uid="cb2">Bring Forward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="07c">
          ⌘+V
        </span>
      </UIContextMenuRow>
    </div>
  );
};

export var App = (props) => {
  return (
    <div
      style={{
        padding: 20,
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        WebkitTextRendering: "subpixel-antialiased",
      }}
      data-uid="App-root"
    >
      <div
        style={{
          width: 270,
          height: 215,
          background: "hsl(0,0%,97%)",
          backgroundColor: "rgb(247, 247, 247, 1)",
          boxShadow:
            "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
          borderRadius: 3,
        }}
        data-uid="App-div"
      >
        <div
          style={{
            paddingLeft: 8,
            paddingRight: 8,
            fontFamily: "Inter",
            fontSize: 11,
            color: "hsl(0,0%,10%)",
            display: "flex",
            alignItems: "center",
            height: 34,
          }}
          data-uid="App-div-div"
        >
          <span style={{ fontWeight: 600 }} data-uid="App-div-div-span">
            Popup Menu
          </span>
        </div>
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            paddingLeft: 8,
            paddingRight: 8,
          }}
          data-uid="57e"
        >
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Select"
          >
            <Row data-uid="681" />
            <Row data-uid="04c">Select Elements</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Copy"
          >
            <div data-uid="2a2" />
            <div data-uid="329">Copy</div>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Pase"
          >
            <div data-uid="2ce" />
            <div data-uid="1a3">Paste</div>
          </UIListGridRow>
          <UIListGridRow data-uid="App-Check">
            <Row style={{ justifyContent: "center" }} data-uid="a1d">
              ✓
            </Row>
            <Row data-uid="2d9">A little label</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Magic"
          >
            <div data-uid="765" />
            <div data-uid="a20">Magic</div>
          </UIListGridRow>
        </div>
      </div>
    </div>
  );
};
export var storyboard = (
  <Storyboard data-uid="sb" >
    <Scene
      style={{ position: "absolute", left: 0, top: 0, width: 313, height: 261 }}
      data-uid="scene-App"
    >
      <App data-uid="App-instance" />
    </Scene>
    <Scene
      data-label="Scene 1"
      style={{
        position: "absolute",
        padding: 20,
        left: 0,
        top: 299,
        width: 280,
        height: 196,
      }}
      data-uid="scene-1"
    >
      <ContextMenu data-uid="ContextMenu-instance" />
    </Scene>
    <Scene
      data-label="Scene 2"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: -19,
        width: 400,
        height: 300,
      }}
      data-uid="scene-2"
    >
      <Card data-uid="Card-instance" testid="card-scene" />
    </Scene>
    <Scene
      data-label="List of Cards"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: 350,
        width: 500,
        height: 400,
      }}
      data-uid="scene-CardList"
    >
      <CardList data-uid="CardList-instance" />
    </Scene>
    <Scene
      data-label="Card component out of place for focus mode"
      style={{
        position: "absolute",
        padding: 20,
        left: 1060,
        top: -31,
        width: 500,
        height: 400,
      }}
      data-uid="scene-ManualCardList"
    >
      <ManualCardList data-uid="ManualCardList-uid" />
    </Scene>
  </Storyboard>
);
`

const TestProjectPlayground = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Playground = () => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='pg-root'
      data-testid='pg-root'
    >
      <div
        style={{
          height: 100,
          position: 'absolute',
          left: 160.5,
          top: 150,
        }}
        data-uid='pg-div'
      >
        <img
          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
          alt='Utopia logo'
          style={{ height: '100%' }}
          data-uid='pg-img'
        />
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 400,
        height: 400,
        position: 'absolute',
        left: 10,
        top: 10,
      }}
      data-uid='sc'
    >
      <Playground data-uid='pg' />
    </Scene>
  </Storyboard>
)
`

const TestProjectScene2Children = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = (props) => {
  return (
    <div style={props.style} data-uid='app-root'>
      <div data-uid='app-div'>
        <span data-uid='app-span' data-testid='app-span'>
          App
        </span>
      </div>
    </div>
  )
}

const Card1 = (props) => {
  return (
    <div style={props.style} data-uid='card1-root'>
      <div data-uid='card1-div'>
        <span
          data-uid='card-span-1'
          data-testid='card-span-1'
        >
          Card1
        </span>
      </div>
    </div>
  )
}

const Card2 = (props) => {
  return (
    <div style={props.style} data-uid='card2-root'>
      <div data-uid='card2-div'>
        <span
          data-uid='card-span-2'
          data-testid='card-span-2'
        >
          Card2
        </span>
      </div>
    </div>
  )
}

const SBChild = (props) => {
  return (
    <div style={props.style} data-uid='sbchild-root'>
      <div data-uid='sbchild-div'>
        <span
          data-uid='sbchild-span'
          data-testid='sbchild-span'
        >
          Storyboard Child
        </span>
      </div>
    </div>
  )
}

const GeneratedComponent = (props) => {
  return (
    <div style={props.style} data-uid='generated-root'>
      <div data-uid='generated-div'>
        <span
          data-uid='generated-span'
          data-testid={props.testid}
        >
          Generated
        </span>
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 300,
        height: 300,
        position: 'absolute',
        left: 10,
        top: 10,
      }}
      data-uid='sc-cards'
    >
      <Card1
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 20,
          top: 20,
          width: 100,
          height: 100,
        }}
        data-uid='card1'
      />
      <Card2
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 150,
          top: 150,
          width: 100,
          height: 100,
        }}
        data-uid='card2'
      />
    </Scene>
    <Scene
      style={{
        width: 300,
        height: 300,
        position: 'absolute',
        left: 350,
        top: 10,
      }}
      data-uid='sc-app'
    >
      <App
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 20,
          top: 20,
          width: 100,
          height: 100,
        }}
        data-uid='app'
      />
    </Scene>
    <Scene
      style={{
        width: 300,
        height: 300,
        position: 'absolute',
        left: 10,
        top: 350,
      }}
      data-uid='sc-generated'
    >
      {
        // @utopia/uid=5f0
        [1, 2, 3].map((i) => (
          <GeneratedComponent
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 10 + (i - 1) * 85,
              top: 10 + (i - 1) * 85,
              width: 80,
              height: 80,
            }}
            data-uid='generated'
            testid={'generated-span-' + i}
          />
        ))
      }
    </Scene>
    <SBChild
      style={{
        position: 'absolute',
        left: 350,
        top: 350,
        width: 100,
        height: 100,
      }}
      data-uid='sbchild'
    />
  </Storyboard>
)
`

const TestProjectOverflownText = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Playground = () => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='pg-root'
      data-testid='pg-root'
    >
      <div
        style={{
          position: 'absolute',
          height: 10,
          width: 10,
          left: 160.5,
          top: 150,
        }}
        data-uid='pg-div'
        data-testid='pg-div'
      >
        Hello
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 400,
        height: 400,
        position: 'absolute',
        left: 10,
        top: 10,
      }}
      data-uid='sc'
    >
      <Playground data-uid='pg' />
    </Scene>
  </Storyboard>
)
`
