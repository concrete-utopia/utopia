import * as EP from '../../core/shared/element-path'
import { StoryboardFilePath } from '../editor/store/editor-state'
import type { EditorRenderResult } from './ui-jsx.test-utils'
import { renderTestEditorWithModel } from './ui-jsx.test-utils'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import type { ElementPath } from '../../core/shared/project-file-types'
import { setFocusedElement } from '../../components/editor/actions/action-creators'
import { mouseClickAtPoint, mouseMoveToPoint } from './event-helpers.test-utils'
import { cmdModifier, emptyModifiers } from '../../utils/modifiers'

function exampleProject(
  positionSetting: string | null,
  elementTopShifts: [number, number, number],
): string {
  const positionLine = positionSetting == null ? '' : `position: '${positionSetting}',`
  return `import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";

export var App = (props) => {
  return (
    <div
      data-uid="other-app-root"
      style={{
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
      }}
    >
      <div
        data-uid='div-red'
        data-testid='div-red'
        style={{
          ${positionLine}
          left: 50,
          top: ${50 + elementTopShifts[0]},
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-label='Red'
      />
      <div
        data-uid='div-green'
        data-testid='div-green'
        style={{
          ${positionLine}
          left: 60,
          top: ${60 + elementTopShifts[1]},
          width: 100,
          height: 100,
          backgroundColor: 'green',
        }}
        data-label='Green'
      />
      <div
        data-uid='div-blue'
        data-testid='div-blue'
        style={{
          ${positionLine}
          left: 70,
          top: ${70 + elementTopShifts[2]},
          width: 100,
          height: 100,
          backgroundColor: 'blue',
        }}
        data-label='Blue'
      />
    </div>
  );
};

export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene-1"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid="app" />
    </Scene>
  </Storyboard>
);
`
}

function createAndRenderModifiedProject(modifiedFiles: {
  [filename: string]: string
}): Promise<EditorRenderResult> {
  const project = createModifiedProject(modifiedFiles)
  return renderTestEditorWithModel(project, 'await-first-dom-report')
}

function createExampleProject(
  positionSetting: string | null,
  elementTopShifts: [number, number, number],
): Promise<EditorRenderResult> {
  return createAndRenderModifiedProject({
    [StoryboardFilePath]: exampleProject(positionSetting, elementTopShifts),
  })
}

async function checkOverlappingElements(
  positionSetting: string | null,
  elementTopShifts: [number, number, number],
  focusedOn: ElementPath | null,
  targetDivId: string,
  cmdPressed: boolean,
  x: number,
  y: number,
  expectedSelectedElements: Array<ElementPath>,
): Promise<void> {
  const renderResult = await createExampleProject(positionSetting, elementTopShifts)

  const targetDiv = renderResult.renderedDOM.getByTestId(targetDivId)
  const targetDivBounds = targetDiv.getBoundingClientRect()

  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  if (focusedOn != null) {
    await renderResult.dispatch([setFocusedElement(focusedOn)], true)
  }

  const targetPoint = {
    x: targetDivBounds.left + x,
    y: targetDivBounds.top + y,
  }

  const modifiers = cmdPressed ? cmdModifier : emptyModifiers
  await mouseMoveToPoint(canvasControlsLayer, targetPoint, { modifiers: modifiers })
  await mouseClickAtPoint(canvasControlsLayer, targetPoint, { modifiers: modifiers })

  const newlySelectedElements = renderResult.getEditorState().editor.selectedViews
  expect(newlySelectedElements).toEqual(expectedSelectedElements)
}

const redDivElementPath = EP.elementPath([
  ['storyboard', 'scene-1', 'app'],
  ['other-app-root', 'div-red'],
])

const greenDivElementPath = EP.elementPath([
  ['storyboard', 'scene-1', 'app'],
  ['other-app-root', 'div-green'],
])

const blueDivElementPath = EP.elementPath([
  ['storyboard', 'scene-1', 'app'],
  ['other-app-root', 'div-blue'],
])

const otherAppRootElementPath = EP.elementPath([
  ['storyboard', 'scene-1', 'app'],
  ['other-app-root'],
])

const zeroElementTopShifts: [number, number, number] = [0, 0, 0]

const relativeElementTopShifts: [number, number, number] = [0, -100, -200]

const overlappingElementTestCases: Array<
  [
    string | null,
    [number, number, number],
    ElementPath | null,
    string,
    boolean,
    number,
    number,
    Array<ElementPath>,
  ]
> = [
  ['absolute', zeroElementTopShifts, null, 'div-red', true, 5, 5, [redDivElementPath]],
  ['absolute', zeroElementTopShifts, null, 'div-green', true, 5, 5, [greenDivElementPath]],
  ['absolute', zeroElementTopShifts, null, 'div-blue', true, 5, 5, [blueDivElementPath]],
  ['absolute', zeroElementTopShifts, null, 'div-red', false, 5, 5, [redDivElementPath]],
  ['absolute', zeroElementTopShifts, null, 'div-green', false, 5, 5, [greenDivElementPath]],
  ['absolute', zeroElementTopShifts, null, 'div-blue', false, 5, 5, [blueDivElementPath]],
  [
    'absolute',
    zeroElementTopShifts,
    otherAppRootElementPath,
    'div-red',
    true,
    5,
    5,
    [redDivElementPath],
  ],
  [
    'absolute',
    zeroElementTopShifts,
    otherAppRootElementPath,
    'div-green',
    true,
    5,
    5,
    [greenDivElementPath],
  ],
  [
    'absolute',
    zeroElementTopShifts,
    otherAppRootElementPath,
    'div-blue',
    true,
    5,
    5,
    [blueDivElementPath],
  ],
  ['relative', relativeElementTopShifts, null, 'div-red', true, 5, 5, [redDivElementPath]],
  ['relative', relativeElementTopShifts, null, 'div-green', true, 5, 5, [greenDivElementPath]],
  ['relative', relativeElementTopShifts, null, 'div-blue', true, 5, 5, [blueDivElementPath]],
  ['relative', relativeElementTopShifts, null, 'div-red', false, 5, 5, [redDivElementPath]],
  ['relative', relativeElementTopShifts, null, 'div-green', false, 5, 5, [greenDivElementPath]],
  ['relative', relativeElementTopShifts, null, 'div-blue', false, 5, 5, [blueDivElementPath]],
  [
    'relative',
    relativeElementTopShifts,
    otherAppRootElementPath,
    'div-red',
    true,
    5,
    5,
    [redDivElementPath],
  ],
  [
    'relative',
    relativeElementTopShifts,
    otherAppRootElementPath,
    'div-green',
    true,
    5,
    5,
    [greenDivElementPath],
  ],
  [
    'relative',
    relativeElementTopShifts,
    otherAppRootElementPath,
    'div-blue',
    true,
    5,
    5,
    [blueDivElementPath],
  ],
  [null, zeroElementTopShifts, null, 'div-red', true, 5, 5, [redDivElementPath]],
  [null, zeroElementTopShifts, null, 'div-green', true, 5, 5, [greenDivElementPath]],
  [null, zeroElementTopShifts, null, 'div-blue', true, 5, 5, [blueDivElementPath]],
  [null, zeroElementTopShifts, null, 'div-red', false, 5, 5, [redDivElementPath]],
  [null, zeroElementTopShifts, null, 'div-green', false, 5, 5, [greenDivElementPath]],
  [null, zeroElementTopShifts, null, 'div-blue', false, 5, 5, [blueDivElementPath]],
  [null, zeroElementTopShifts, otherAppRootElementPath, 'div-red', true, 5, 5, [redDivElementPath]],
  [
    null,
    zeroElementTopShifts,
    otherAppRootElementPath,
    'div-green',
    true,
    5,
    5,
    [greenDivElementPath],
  ],
  [
    null,
    zeroElementTopShifts,
    otherAppRootElementPath,
    'div-blue',
    true,
    5,
    5,
    [blueDivElementPath],
  ],
]

describe('Selecting overlapping elements', () => {
  for (const [
    positionSetting,
    elementTopShifts,
    focusedOn,
    targetDiv,
    cmdPressed,
    x,
    y,
    expectedSelectedElements,
  ] of overlappingElementTestCases) {
    const focusedOnText =
      focusedOn == null ? 'with nothing focused' : `with ${EP.toString(focusedOn)} focused`
    const positionText =
      positionSetting == null
        ? 'with no position value'
        : `with a position value of ${positionSetting}`
    it(`clicking inside ${targetDiv} ${positionText} (with cmd: ${cmdPressed}) shifted by (${x},${y}) ${focusedOnText} selects the expected element`, async () => {
      await checkOverlappingElements(
        positionSetting,
        elementTopShifts,
        focusedOn,
        targetDiv,
        cmdPressed,
        x,
        y,
        expectedSelectedElements,
      )
    })
  }
})
