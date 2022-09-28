import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { offsetPoint, windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import {
  absoluteReparentStrategy,
  forcedAbsoluteReparentStrategy,
} from './absolute-reparent-strategy'
import {
  flexReparentToAbsoluteStrategy,
  forcedFlexReparentToAbsoluteStrategy,
} from './flex-reparent-to-absolute-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
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
        width: 250,
        height: 500,
        backgroundColor: 'orange',
      }}
      data-uid='staticparent'
      data-testid='staticparent'
    />
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 250,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      <div
        style={{
          position: 'absolute',
          left: 50,
          top: 58,
          width: 100,
          height: 100,
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
        left: 500,
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
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
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

const allReparentStrategies = () => [
  absoluteReparentStrategy,
  absoluteReparentToFlexStrategy,
  forcedAbsoluteReparentStrategy,
  flexReparentToAbsoluteStrategy,
  forcedFlexReparentToAbsoluteStrategy,
  flexReparentToFlexStrategy,
]

describe('Forced Absolute Reparent Strategies', () => {
  beforeEach(() => {
    viewport.set(2200, 1000)
  })
  it('Absolute to forced absolute can be applied', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
      [() => [forcedAbsoluteReparentStrategy]],
    )

    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const secondFlexChild = await renderResult.renderedDOM.findByTestId('flexchild2')
    const secondFlexChildRect = secondFlexChild.getBoundingClientRect()
    const secondFlexChildCenter = {
      x: secondFlexChildRect.x + secondFlexChildRect.width / 2,
      y: secondFlexChildRect.y + secondFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: secondFlexChildCenter.x - absoluteChildCenter.x,
      y: secondFlexChildCenter.y - absoluteChildCenter.y,
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
        width: 250,
        height: 500,
        backgroundColor: 'orange',
      }}
      data-uid='staticparent'
      data-testid='staticparent'
    />
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 250,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    />
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 500,
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
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      >
        <div
          style={{
            position: 'absolute',
            left: 100,
            top: 0,
            width: 100,
            height: 100,
            backgroundColor: 'yellow',
          }}
          data-uid='absolutechild'
          data-testid='absolutechild'
        />
      </div>
    </div>
  </div>`),
    )
  })
  it('Absolute to forced absolute is not the default choice', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
      [allReparentStrategies],
    )

    const absoluteChild = await renderResult.renderedDOM.findByTestId('absolutechild')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const secondFlexChild = await renderResult.renderedDOM.findByTestId('flexchild2')
    const secondFlexChildRect = secondFlexChild.getBoundingClientRect()
    const secondFlexChildCenter = {
      x: secondFlexChildRect.x + secondFlexChildRect.width / 2,
      y: secondFlexChildRect.y + secondFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: secondFlexChildCenter.x - absoluteChildCenter.x,
      y: secondFlexChildCenter.y - absoluteChildCenter.y,
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
        width: 250,
        height: 500,
        backgroundColor: 'orange',
      }}
      data-uid='staticparent'
      data-testid='staticparent'
    />
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 250,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    />
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 500,
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
          backgroundColor: 'teal',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
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
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      />
    </div>
  </div>`),
    )
  })
  it('Flex to forced absolute can be applied', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
      [() => [forcedFlexReparentToAbsoluteStrategy]],
    )
    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildCenter = {
      x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    const staticParent = await renderResult.renderedDOM.findByTestId('staticparent')
    const staticParentRect = staticParent.getBoundingClientRect()
    const staticParentCenter = {
      x: staticParentRect.x + staticParentRect.width / 2,
      y: staticParentRect.y + staticParentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: staticParentCenter.x - firstFlexChildCenter.x,
      y: staticParentCenter.y - firstFlexChildCenter.y,
    })
    await dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier)

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
            width: 250,
            height: 500,
            backgroundColor: 'orange',
          }}
          data-uid='staticparent'
          data-testid='staticparent'
        >
          <div
            style={{
              width: 100,
              height: 100,
              backgroundColor: 'teal',
              position: 'absolute',
              left: 75,
              top: 200,
            }}
            data-uid='flexchild1'
            data-testid='flexchild1'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            width: 250,
            height: 500,
            left: 250,
            top: 0,
            backgroundColor: 'lightblue',
          }}
          data-uid='absoluteparent'
          data-testid='absoluteparent'
        >
          <div
            style={{
              position: 'absolute',
              left: 50,
              top: 58,
              width: 100,
              height: 100,
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
            left: 500,
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
              backgroundColor: 'red',
            }}
            data-uid='flexchild2'
            data-testid='flexchild2'
          />
        </div>
      </div>`),
    )
  })
  it('Flex to forced absolute is not the default choice', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
      [allReparentStrategies],
    )
    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildCenter = {
      x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    const staticParent = await renderResult.renderedDOM.findByTestId('staticparent')
    const staticParentRect = staticParent.getBoundingClientRect()
    const staticParentCenter = {
      x: staticParentRect.x + staticParentRect.width / 2,
      y: staticParentRect.y + staticParentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    const dragDelta = windowPoint({
      x: staticParentCenter.x - firstFlexChildCenter.x,
      y: staticParentCenter.y - firstFlexChildCenter.y,
    })
    await dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier)

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
            width: 250,
            height: 500,
            backgroundColor: 'orange',
          }}
          data-uid='staticparent'
          data-testid='staticparent'
        />
        <div
          style={{
            position: 'absolute',
            width: 250,
            height: 500,
            left: 250,
            top: 0,
            backgroundColor: 'lightblue',
          }}
          data-uid='absoluteparent'
          data-testid='absoluteparent'
        >
          <div
            style={{
              position: 'absolute',
              left: 50,
              top: 58,
              width: 100,
              height: 100,
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
            left: 500,
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
              backgroundColor: 'red',
            }}
            data-uid='flexchild2'
            data-testid='flexchild2'
          />
        </div>
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            position: 'absolute',
            left: 75,
            top: 200,
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
      </div>`),
    )
  })
})
