import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../event-helpers.test-utils'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElements = await renderResult.renderedDOM.findAllByTestId(targetTestId)
  const targetElement = targetElements[0]
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  })

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
  })
}

const defaultTestCode = `
  <div
    style={{
      position: 'absolute',
      width: 700,
      height: 600,
    }}
    data-uid='container'
    data-testid='container'
  >
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      {[1, 2].map((n) => (
        <div
          style={{
            position: 'absolute',
            left: 20 + (n * 100),
            top: 150,
            width: 100,
            height: 100,
            borderWidth: 10,
            borderColor: 'black',
            borderStyle: 'solid',
            backgroundColor: 'yellow',
          }}
          data-uid='generatedabsolutechild'
          data-testid='generatedabsolutechild'
        />
      ))}
      <div
        style={{
          position: 'absolute',
          left: 93.5,
          top: 58,
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      />
    </div>
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent'
      data-testid='flexparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
    </div>
  </div>
`

function makeTestProjectCodeWithComponentInnards(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
${componentInnards}
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 2000, height: 2000 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

function makeTestProjectCodeWithSnippet(snippet: string): string {
  return makeTestProjectCodeWithComponentInnards(`
  return (
${snippet}
  )
`)
}

describe('Absolute Reparent To Flex Strategy', () => {
  beforeEach(() => {
    viewport.set(2200, 1000)
  })
  it('reparents to the end', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexParent = await renderResult.renderedDOM.findByTestId('flexparent')
    const flexParentRect = flexParent.getBoundingClientRect()
    const flexParentEnd = {
      x: flexParentRect.x + flexParentRect.width - 15,
      y: flexParentRect.y + flexParentRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flexParentEnd.x - absoluteChildCenter.x,
      y: flexParentEnd.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'absolutechild', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
  <div
    style={{
      position: 'absolute',
      width: 700,
      height: 600,
    }}
    data-uid='container'
    data-testid='container'
  >
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      {[1, 2].map((n) => (
        <div
          style={{
            position: 'absolute',
            left: 20 + (n * 100),
            top: 150,
            width: 100,
            height: 100,
            borderWidth: 10,
            borderColor: 'black',
            borderStyle: 'solid',
            backgroundColor: 'yellow',
          }}
          data-uid='generatedabsolutechild'
          data-testid='generatedabsolutechild'
        />
      ))}
    </div>
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent'
      data-testid='flexparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
      <div
        style={{
          position: 'relative',
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      />
    </div>
  </div>`),
    )
  })
  it('reparents to between the two existing flex elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildEnd = {
      x: firstFlexChildRect.x + firstFlexChildRect.width - 5,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: firstFlexChildEnd.x - absoluteChildCenter.x + 50,
      y: firstFlexChildEnd.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'absolutechild', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
  <div
    style={{
      position: 'absolute',
      width: 700,
      height: 600,
    }}
    data-uid='container'
    data-testid='container'
  >
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      {[1, 2].map((n) => (
        <div
          style={{
            position: 'absolute',
            left: 20 + (n * 100),
            top: 150,
            width: 100,
            height: 100,
            borderWidth: 10,
            borderColor: 'black',
            borderStyle: 'solid',
            backgroundColor: 'yellow',
          }}
          data-uid='generatedabsolutechild'
          data-testid='generatedabsolutechild'
        />
      ))}
    </div>
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent'
      data-testid='flexparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          position: 'relative',
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
    </div>
  </div>`),
    )
  })
  it('cannot reparent a generated element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const generatedAbsolutechildren = await renderResult.renderedDOM.findAllByTestId(
      'generatedabsolutechild',
    )
    const absoluteChild = generatedAbsolutechildren[0]
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildCenter = {
      x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: firstFlexChildCenter.x - absoluteChildCenter.x,
      y: firstFlexChildCenter.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'generatedabsolutechild', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(defaultTestCode),
    )
  })
})
