import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = {
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  }

  mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
  })
}

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

describe('Flow Reparent To Flow Strategy', () => {
  it('reparents flow element to flow parent', async () => {
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
            backgroundColor: 'blue',
          }}
          data-uid='flowparent1'
          data-testid='flowparent1'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'purple',
            }}
            data-uid='flowchild1'
            data-testid='flowchild1'
          />
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'pink',
            }}
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            width: 250,
            height: 500,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent2'
          data-testid='flowparent2'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'teal',
            }}
            data-uid='flowchild3'
            data-testid='flowchild3'
          />
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'red',
            }}
            data-uid='flowchild4'
            data-testid='flowchild4'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )

    const targetFlowParent = await renderResult.renderedDOM.findByTestId('flowparent1')
    const targetFlowParentRect = targetFlowParent.getBoundingClientRect()
    const targetFlowParentEnd = {
      x: targetFlowParentRect.x + targetFlowParentRect.width / 2,
      y: targetFlowParentRect.y + targetFlowParentRect.height - 15,
    }
    const flowChildToReparent = await renderResult.renderedDOM.findByTestId('flowchild3')
    const flowChildToReparentRect = flowChildToReparent.getBoundingClientRect()
    const flowChildToReparentCenter = {
      x: flowChildToReparentRect.x + flowChildToReparentRect.width / 2,
      y: flowChildToReparentRect.y + flowChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlowParentEnd.x - flowChildToReparentCenter.x + 5,
      y: targetFlowParentEnd.y - flowChildToReparentCenter.y,
    })

    dragElement(renderResult, 'flowchild3', dragDelta, cmdModifier)

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
          backgroundColor: 'blue',
        }}
        data-uid='flowparent1'
        data-testid='flowparent1'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'purple',
          }}
          data-uid='flowchild1'
          data-testid='flowchild1'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'pink',
          }}
          data-uid='flowchild2'
          data-testid='flowchild2'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
          }}
          data-uid='flowchild3'
          data-testid='flowchild3'
        />
      </div>
      <div
        style={{
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flowparent2'
        data-testid='flowparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flowchild4'
          data-testid='flowchild4'
        />
      </div>
    </div>
  `),
    )
  })
  it('reparents flow element to flow parent in row layout', async () => {
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
            backgroundColor: 'blue',
          }}
          data-uid='flowparent1'
          data-testid='flowparent1'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'purple',
              display: 'inline-block',
            }}
            data-uid='flowchild1'
            data-testid='flowchild1'
          />
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'pink',
              display: 'inline-block',
            }}
            data-uid='flowchild2'
            data-testid='flowchild2'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            width: 250,
            height: 500,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flowparent2'
          data-testid='flowparent2'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'teal',
            }}
            data-uid='flowchild3'
            data-testid='flowchild3'
          />
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'red',
            }}
            data-uid='flowchild4'
            data-testid='flowchild4'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )

    const targetFlowParent = await renderResult.renderedDOM.findByTestId('flowparent1')
    const targetFlowParentRect = targetFlowParent.getBoundingClientRect()
    const targetFlowParentEnd = {
      x: targetFlowParentRect.x + targetFlowParentRect.width - 15,
      y: targetFlowParentRect.y + targetFlowParentRect.height - 2,
    }
    const flowChildToReparent = await renderResult.renderedDOM.findByTestId('flowchild3')
    const flowChildToReparentRect = flowChildToReparent.getBoundingClientRect()
    const flowChildToReparentCenter = {
      x: flowChildToReparentRect.x + flowChildToReparentRect.width / 2,
      y: flowChildToReparentRect.y + flowChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlowParentEnd.x - flowChildToReparentCenter.x + 5,
      y: targetFlowParentEnd.y - flowChildToReparentCenter.y,
    })

    dragElement(renderResult, 'flowchild3', dragDelta, cmdModifier)

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
          backgroundColor: 'blue',
        }}
        data-uid='flowparent1'
        data-testid='flowparent1'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'purple',
            display: 'inline-block',
          }}
          data-uid='flowchild1'
          data-testid='flowchild1'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'pink',
            display: 'inline-block',
          }}
          data-uid='flowchild2'
          data-testid='flowchild2'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            display: 'inline-block',
          }}
          data-uid='flowchild3'
          data-testid='flowchild3'
        />
      </div>
      <div
        style={{
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flowparent2'
        data-testid='flowparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flowchild4'
          data-testid='flowchild4'
        />
      </div>
    </div>
  `),
    )
  })
})
