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
  mouseDragFromPointWithDelta,
  pressKey,
} from '../../event-helpers.test-utils'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = {
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  }

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

describe('Flex Reparent To Flow Strategy', () => {
  it('reparents flex element to flow parent', async () => {
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
              display: 'flex',
              flexDirection: 'row',
              position: 'absolute',
              width: 250,
              height: 500,
              left: 350,
              top: 0,
              backgroundColor: 'lightgreen',
            }}
            data-uid='flexparent2'
            data-testid='flexparent2'
          >
            <div
              style={{
                width: 100,
                height: 100,
                backgroundColor: 'teal',
              }}
              data-uid='flexchild3'
              data-testid='flexchild3'
            />
            <div
              style={{
                width: 100,
                height: 100,
                backgroundColor: 'red',
              }}
              data-uid='flexchild4'
              data-testid='flexchild4'
            />
          </div>
        </div>`),
      'await-first-dom-report',
    )

    const targetFlexParent = await renderResult.renderedDOM.findByTestId('flowparent1')
    const targetFlexParentRect = targetFlexParent.getBoundingClientRect()
    const targetFlexParentEnd = {
      x: targetFlexParentRect.x + targetFlexParentRect.width / 2,
      y: targetFlexParentRect.y + targetFlexParentRect.height - 15,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexParentEnd.x - flexChildToReparentCenter.x + 5,
      y: targetFlexParentEnd.y - flexChildToReparentCenter.y,
    })

    await dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier)

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
          data-uid='flexchild3'
          data-testid='flexchild3'
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
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })

  it('reparents flex element to flow parent in row layout', async () => {
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
            display: 'flex',
            flexDirection: 'row',
            position: 'absolute',
            width: 250,
            height: 500,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flexparent2'
          data-testid='flexparent2'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'teal',
            }}
            data-uid='flexchild3'
            data-testid='flexchild3'
          />
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'red',
            }}
            data-uid='flexchild4'
            data-testid='flexchild4'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )

    const targetFlexParent = await renderResult.renderedDOM.findByTestId('flowparent1')
    const targetFlexParentRect = targetFlexParent.getBoundingClientRect()
    const targetFlexParentEnd = {
      x: targetFlexParentRect.x + targetFlexParentRect.width - 15,
      y: targetFlexParentRect.y + targetFlexParentRect.height / 2,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexParentEnd.x - flexChildToReparentCenter.x + 5,
      y: targetFlexParentEnd.y - flexChildToReparentCenter.y,
    })

    await dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier)

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
          data-uid='flexchild3'
          data-testid='flexchild3'
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
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })

  it('reparents flex as first child when moving the mouse to the top edge of the first child', async () => {
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
            display: 'flex',
            flexDirection: 'row',
            position: 'absolute',
            width: 250,
            height: 500,
            left: 350,
            top: 0,
            backgroundColor: 'lightgreen',
          }}
          data-uid='flexparent2'
          data-testid='flexparent2'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'teal',
            }}
            data-uid='flexchild3'
            data-testid='flexchild3'
          />
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'red',
            }}
            data-uid='flexchild4'
            data-testid='flexchild4'
          />
        </div>
      </div>
    `),
      'await-first-dom-report',
    )

    const targetFlexChild = await renderResult.renderedDOM.findByTestId('flowchild1')
    const targetFlexChildRect = targetFlexChild.getBoundingClientRect()
    const targetFlexChildCenter = {
      x: targetFlexChildRect.x + 10,
      y: targetFlexChildRect.y + targetFlexChildRect.height / 2,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexChildCenter.x - flexChildToReparentCenter.x - 5,
      y: targetFlexChildCenter.y - flexChildToReparentCenter.y,
    })

    await dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier)

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
            backgroundColor: 'teal',
            display: 'inline-block',
          }}
          data-uid='flexchild3'
          data-testid='flexchild3'
        />
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
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })
})
