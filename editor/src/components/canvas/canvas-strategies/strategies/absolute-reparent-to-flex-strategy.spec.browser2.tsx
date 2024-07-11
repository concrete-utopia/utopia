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
import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { ExtraPadding } from './reparent-helpers/reparent-strategy-sibling-position-helpers'
import { navigatorEntryToKey } from '../../../../components/editor/store/editor-state'
import { selectComponents } from '../../../editor/actions/action-creators'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../../utils/utils.test-utils'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  skipMouseUp: boolean = false,
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

  if (skipMouseUp) {
    await mouseDownAtPoint(canvasControlsLayer, startPoint, {
      modifiers: modifiers,
    })
    await mouseMoveToPoint(
      canvasControlsLayer,
      {
        x: startPoint.x + dragDelta.x,
        y: startPoint.y + dragDelta.y,
      },
      {
        eventOptions: {
          movementX: dragDelta.x,
          movementY: dragDelta.y,
          buttons: 1,
        },
        modifiers: cmdModifier,
      },
    )
  } else {
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: modifiers,
    })
  }
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
      {
        // @utopia/uid=6f1
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

describe('Absolute Reparent To Flex Strategy', () => {
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
      {
        // @utopia/uid=6f1
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
      x: firstFlexChildEnd.x - absoluteChildCenter.x + 10,
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
      {
        // @utopia/uid=6f1
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

  it('cannot reparent a code element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
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

    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildCenter = {
      x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()

    await mouseDragFromPointToPoint(canvasControlsLayer, absoluteChildCenter, firstFlexChildCenter)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(defaultTestCode),
    )

    // Ensure that the code element was selected, and not one of the generated children
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/6f1'),
    ])
  })
})

const complexProject = (flexDirection: 'row' | 'column' = 'row') => `
<div
  style={{
    width: '100%',
    height: '100%',
    position: 'relative',
  }}
  data-uid='container'
>
  <div
    style={{
      position: 'absolute',
      display: 'flex',
      flexDirection: '${flexDirection}',
    }}
    data-uid='flexcontainer'
    data-testid='flexcontainer'
  >
    <div
      style={{
        backgroundColor: '#04FF00AB',
        display: 'flex',
        flexDirection: '${flexDirection}',
      }}
      data-uid='child1flex'
      data-testid='child1flex'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 50,
          height: 50,
        }}
        data-uid='innerchild1'
        data-testid='innerchild1'
      />
      <div
        style={{
          backgroundColor: '#AA00FFAB',
          width: 50,
          height: 50,
        }}
        data-uid='innerchild2'
        data-testid='innerchild2'
      />
    </div>
    <div
      style={{
        backgroundColor: '#0D00FFAB',
        width: 100,
        height: 100,
      }}
      data-uid='child2'
      data-testid='child2'
    />
  </div>
  <div
    style={{
      backgroundColor: '#FF0033AB',
      position: 'absolute',
      width: 40,
      height: 40,
    }}
    data-uid='targetdiv'
    data-testid='targetdiv'
  />
</div>
`

const PaddingThreshold = ExtraPadding(1)
describe('Absolute Reparent To Flex Strategy with more complex flex layouts', () => {
  it('moving the element into a flex layout after the last child reparents to the end as a sibling', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('targetdiv')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexParent = await renderResult.renderedDOM.findByTestId('flexcontainer')
    const flexParentRect = flexParent.getBoundingClientRect()
    const flexParentEnd = {
      x: flexParentRect.x + flexParentRect.width - PaddingThreshold / 2,
      y: flexParentRect.y + flexParentRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flexParentEnd.x - absoluteChildCenter.x,
      y: flexParentEnd.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'targetdiv', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/targetdiv',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/targetdiv',
      ],
    )
  })
  it('moving the element into a flex layout to the middle of the last child reparents to the child', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('targetdiv')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexSibling = await renderResult.renderedDOM.findByTestId('child2')
    const flexSiblingRect = flexSibling.getBoundingClientRect()
    const flexSiblingCenter = {
      x: flexSiblingRect.x + flexSiblingRect.width / 2,
      y: flexSiblingRect.y + flexSiblingRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flexSiblingCenter.x - absoluteChildCenter.x,
      y: flexSiblingCenter.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'targetdiv', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2/targetdiv',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2/targetdiv',
      ],
    )
  })
  it('moving the element into a flex layout to the edge of the last child reparents to the end as a sibling', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('targetdiv')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexSibling = await renderResult.renderedDOM.findByTestId('child2')
    const flexSiblingRect = flexSibling.getBoundingClientRect()
    const flexSiblingEnd = {
      x: flexSiblingRect.x + flexSiblingRect.width - PaddingThreshold / 2,
      y: flexSiblingRect.y + flexSiblingRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flexSiblingEnd.x - absoluteChildCenter.x,
      y: flexSiblingEnd.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'targetdiv', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/targetdiv',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/targetdiv',
      ],
    )
  })
  it('moving the element to the edge of a flex in flex layout will reparent to the outer flex container', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('targetdiv')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexInnerChild = await renderResult.renderedDOM.findByTestId('innerchild1')
    const flexInnerChildRect = flexInnerChild.getBoundingClientRect()
    const flexInnerChildLeft = {
      x: flexInnerChildRect.x + PaddingThreshold / 2,
      y: flexInnerChildRect.y + flexInnerChildRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flexInnerChildLeft.x - absoluteChildCenter.x,
      y: flexInnerChildLeft.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'targetdiv', dragDelta, cmdModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/targetdiv',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/targetdiv',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
      ],
    )
  })
  it('moving the element to the center of a flex in flex layout will reparent to flex grandchild', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const absoluteChild = await renderResult.renderedDOM.findByTestId('targetdiv')
    const absoluteChildRect = absoluteChild.getBoundingClientRect()
    const absoluteChildCenter = {
      x: absoluteChildRect.x + absoluteChildRect.width / 2,
      y: absoluteChildRect.y + absoluteChildRect.height / 2,
    }
    const flexInnerChild = await renderResult.renderedDOM.findByTestId('innerchild2')
    const flexInnerChildRect = flexInnerChild.getBoundingClientRect()
    const flexInnerChildCenter = {
      x: flexInnerChildRect.x + flexInnerChildRect.width / 2,
      y: flexInnerChildRect.y + flexInnerChildRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: flexInnerChildCenter.x - absoluteChildCenter.x,
      y: flexInnerChildCenter.y - absoluteChildCenter.y,
    })
    await dragElement(renderResult, 'targetdiv', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2/targetdiv',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2/targetdiv',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
      ],
    )
  })
  it('moving one of the inner element over its parents edge while inside the parent area doesnt trigger reparent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const child = await renderResult.renderedDOM.findByTestId('innerchild2')
    const childRect = child.getBoundingClientRect()
    const childCenter = {
      x: childRect.x + childRect.width / 2,
      y: childRect.y + childRect.height / 2,
    }
    const parent = await renderResult.renderedDOM.findByTestId('child1flex')
    const parentRect = parent.getBoundingClientRect()
    const parentRightEdge = {
      x: parentRect.x + parentRect.width - PaddingThreshold / 3,
      y: parentRect.y + parentRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: parentRightEdge.x - childCenter.x,
      y: parentRightEdge.y - childCenter.y,
    })
    await dragElement(renderResult, 'innerchild2', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/targetdiv',
      ],
    )
  })
  it('moving one of the inner element over its parents edge from the outside triggers reparent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const child = await renderResult.renderedDOM.findByTestId('innerchild2')
    const childRect = child.getBoundingClientRect()
    const childCenter = {
      x: childRect.x + childRect.width / 2,
      y: childRect.y + childRect.height / 2,
    }
    const parent = await renderResult.renderedDOM.findByTestId('child1flex')
    const parentRect = parent.getBoundingClientRect()
    const parentRightEdge = {
      x: parentRect.x + parentRect.width + PaddingThreshold / 3,
      y: parentRect.y + parentRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: parentRightEdge.x - childCenter.x,
      y: parentRightEdge.y - childCenter.y,
    })
    await dragElement(renderResult, 'innerchild2', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/innerchild2',
    ])
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child1flex/innerchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/innerchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/targetdiv',
      ],
    )
  })
  it('moving the inner child element to reparent to the parent sibling visually keeps the original layout until mouseup', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(complexProject('row')),
      'await-first-dom-report',
    )
    const child = await renderResult.renderedDOM.findByTestId('innerchild2')
    const childRect = child.getBoundingClientRect()
    const childCenter = {
      x: childRect.x + childRect.width / 2,
      y: childRect.y + childRect.height / 2,
    }
    const newParentTarget = await renderResult.renderedDOM.findByTestId('child2')
    const newParentTargetRect = newParentTarget.getBoundingClientRect()
    const newParentTargetCenter = {
      x: newParentTargetRect.x + newParentTargetRect.width / 2,
      y: newParentTargetRect.y + newParentTargetRect.height / 2,
    }

    const dragDelta = windowPoint({
      x: newParentTargetCenter.x - childCenter.x,
      y: newParentTargetCenter.y - childCenter.y,
    })
    await dragElement(renderResult, 'innerchild2', dragDelta, emptyModifiers, true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const newParentTargetRectDuringDrag = newParentTarget.getBoundingClientRect()
    expect(newParentTargetRectDuringDrag?.x).toEqual(newParentTargetRect?.x)
    expect(newParentTargetRectDuringDrag?.y).toEqual(newParentTargetRect?.y)

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'utopia-storyboard-uid/scene-aaa/app-entity:container/flexcontainer/child2/innerchild2',
    ])
  })
})
