import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  pressKey,
} from '../../event-helpers.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import * as EP from '../../../../core/shared/element-path'

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
    midDragCallback: () => pressKey('2', { modifiers: modifiers }), // Switch to flow reparenting strategy
  })
}

function makeTestProjectCodeWithComponentInnards(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

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

describe('Absolute Reparent To Flow Strategy', () => {
  it('reparents to the end', async () => {
    const renderResult = await renderTestEditorWithCode(
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
            position: 'absolute',
            width: 250,
            height: 400,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent'
          data-testid='flowparent'
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
            data-uid='flowchild1'
            data-testid='flowchild1'
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
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flowParent = await renderResult.renderedDOM.findByTestId('flowparent')
    const flowParentRect = flowParent.getBoundingClientRect()
    const flowParentEnd = {
      x: flowParentRect.x + flowParentRect.width / 2,
      y: flowParentRect.y + flowParentRect.height - 25,
    }

    const dragDelta = windowPoint({
      x: flowParentEnd.x - absoluteChildCenter.x,
      y: flowParentEnd.y - absoluteChildCenter.y,
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
        position: 'absolute',
        width: 250,
        height: 400,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flowparent'
      data-testid='flowparent'
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
        data-uid='flowchild1'
        data-testid='flowchild1'
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
        data-uid='flowchild2'
        data-testid='flowchild2'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'yellow',
          contain: 'layout',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      />
    </div>
  </div>`),
    )
  })
  it('reparents to the end in row layout', async () => {
    const renderResult = await renderTestEditorWithCode(
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
            position: 'absolute',
            width: 250,
            height: 400,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent'
          data-testid='flowparent'
        >
          <div
            style={{
              width: 100,
              height: 100,
              borderWidth: 10,
              borderColor: 'black',
              borderStyle: 'solid',
              backgroundColor: 'teal',
              display: 'inline-block',
            }}
            data-uid='flowchild1'
            data-testid='flowchild1'
          />
          <div
            style={{
              width: 100,
              height: 100,
              borderWidth: 10,
              borderColor: 'black',
              borderStyle: 'solid',
              backgroundColor: 'red',
              display: 'inline-block',
            }}
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flowParent = await renderResult.renderedDOM.findByTestId('flowparent')
    const flowParentRect = flowParent.getBoundingClientRect()
    const flowParentEnd = {
      x: flowParentRect.x + flowParentRect.width - 25,
      y: flowParentRect.y + flowParentRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flowParentEnd.x - absoluteChildCenter.x,
      y: flowParentEnd.y - absoluteChildCenter.y,
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
        position: 'absolute',
        width: 250,
        height: 400,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flowparent'
      data-testid='flowparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'teal',
          display: 'inline-block',
        }}
        data-uid='flowchild1'
        data-testid='flowchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'red',
          display: 'inline-block',
        }}
        data-uid='flowchild2'
        data-testid='flowchild2'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'yellow',
          display: 'inline-block',
          contain: 'layout',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      />
    </div>
  </div>`),
    )
  })
  it('reparents to between the two existing flow elements', async () => {
    const renderResult = await renderTestEditorWithCode(
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
            position: 'absolute',
            width: 250,
            height: 400,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent'
          data-testid='flowparent'
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
            data-uid='flowchild1'
            data-testid='flowchild1'
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
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )

    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
    const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
    const firstFlowChildEnd = {
      x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
      y: firstFlowChildRect.y + firstFlowChildRect.height - 5,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: firstFlowChildEnd.x - absoluteChildCenter.x + 10,
      y: firstFlowChildEnd.y - absoluteChildCenter.y,
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
        position: 'absolute',
        width: 250,
        height: 400,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flowparent'
      data-testid='flowparent'
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
        data-uid='flowchild1'
        data-testid='flowchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          borderWidth: 10,
          borderColor: 'black',
          borderStyle: 'solid',
          backgroundColor: 'yellow',
          contain: 'layout',
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
        data-uid='flowchild2'
        data-testid='flowchild2'
      />
    </div>
  </div>`),
    )
  })
  it('cannot reparent a generated element', async () => {
    const testCode = `
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
            position: 'absolute',
            width: 250,
            height: 400,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent'
          data-testid='flowparent'
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
            data-uid='flowchild1'
            data-testid='flowchild1'
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
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
      </div>
    `

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(testCode),
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
    const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
    const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
    const firstFlowChildCenter = {
      x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
      y: firstFlowChildRect.y + firstFlowChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: firstFlowChildCenter.x - absoluteChildCenter.x,
      y: firstFlowChildCenter.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'generatedabsolutechild', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(testCode),
    )
  })

  it('cannot reparent a code element', async () => {
    const testCode = `
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
          {
            // @utopia/uid=2e3
            [1, 2].map((n) => (
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
            ))
          }
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
            position: 'absolute',
            width: 250,
            height: 400,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent'
          data-testid='flowparent'
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
            data-uid='flowchild1'
            data-testid='flowchild1'
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
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
      </div>
    `

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(testCode),
      'await-first-dom-report',
    )

    // To select a code element we first select the parent and then double click on one of
    // generated children of the code element
    const parentPath = EP.fromString(
      'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent',
    )
    await renderResult.dispatch([selectComponents([parentPath], false)], false)
    await renderResult.getDispatchFollowUpActionsFinished()

    const generatedAbsolutechildren = await renderResult.renderedDOM.findAllByTestId(
      'generatedabsolutechild',
    )
    const absoluteChild = generatedAbsolutechildren[0]
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    await mouseDoubleClickAtPoint(canvasControlsLayer, absoluteChildCenter)

    const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
    const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
    const firstFlowChildCenter = {
      x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
      y: firstFlowChildRect.y + firstFlowChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    await mouseDragFromPointToPoint(canvasControlsLayer, absoluteChildCenter, firstFlowChildCenter)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(testCode),
    )

    // Ensure that the code element was selected, and not one of the generated children
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/2e3'),
    ])
  })
})
