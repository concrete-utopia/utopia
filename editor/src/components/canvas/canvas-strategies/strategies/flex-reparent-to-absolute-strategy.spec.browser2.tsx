import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { act } from '@testing-library/react'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { offsetPoint, windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { setFeatureForBrowserTests, wait } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/meta-actions'
import * as EP from '../../../../core/shared/element-path'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  click: boolean,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = {
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  }

  if (click) {
    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  }

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
      <div
        style={{
          position: 'absolute',
          left: 93.5,
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

describe('Flex Reparent To Absolute Strategy', () => {
  it('reparents flex element to absolute parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
    const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
    const targetAbsoluteParentCenter = {
      x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
      y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
    }
    const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
    const firstFlexChildCenter = {
      x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
      y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetAbsoluteParentCenter.x - firstFlexChildCenter.x,
      y: targetAbsoluteParentCenter.y - firstFlexChildCenter.y,
    })
    await dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier, true)

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
      <div
        style={{
          position: 'absolute',
          left: 93.5,
          top: 58,
          width: 100,
          height: 100,
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            position: 'absolute',
            left: -0.5,
            top: 0,
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
      </div>
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
          backgroundColor: 'red',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
    </div>
  </div>`),
    )
  })
})

describe('Flex Reparent to Absolute – children affecting elements', () => {
  setFeatureForBrowserTests('Fragment support', true)
  ;(['fragment', 'div'] as const).forEach((divOrFragment) => {
    describe(`– ${divOrFragment} parents`, () => {
      it('reparents regular child from a children-affecting flex parent to absolute', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(fragmentTestCode(divOrFragment)),
          'await-first-dom-report',
        )

        const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
        const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
        const targetAbsoluteParentCenter = {
          x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
          y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
        }
        const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
        const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
        const firstFlexChildCenter = {
          x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
          y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
        }

        await renderResult.getDispatchFollowUpActionsFinished()
        const dragDelta = windowPoint({
          x: targetAbsoluteParentCenter.x - firstFlexChildCenter.x,
          y: targetAbsoluteParentCenter.y - firstFlexChildCenter.y,
        })
        await dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier, true)

        await renderResult.getDispatchFollowUpActionsFinished()

        expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
          'utopia-storyboard-uid',
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:container',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/flexchild1', // <- flexChild1 is successfully reparented
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent/children-affecting',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent/children-affecting/flexchild2',
        ])
      })

      it('reparents children-affecting element from flex to absolute', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(fragmentTestCode(divOrFragment)),
          'await-first-dom-report',
        )

        const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
        const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
        const targetAbsoluteParentCenter = {
          x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
          y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
        }
        const firstFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
        const firstFlexChildRect = firstFlexChild.getBoundingClientRect()
        const firstFlexChildCenter = {
          x: firstFlexChildRect.x + firstFlexChildRect.width / 2,
          y: firstFlexChildRect.y + firstFlexChildRect.height / 2,
        }

        await renderResult.getDispatchFollowUpActionsFinished()
        const dragDelta = windowPoint({
          x: targetAbsoluteParentCenter.x - firstFlexChildCenter.x,
          y: targetAbsoluteParentCenter.y - firstFlexChildCenter.y,
        })

        // selecting the fragment-like parent manually, so that dragElement drags _it_ instead of child-2!
        await renderResult.dispatch(
          selectComponents(
            [
              EP.fromString(
                'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent/children-affecting',
              ),
            ],
            false,
          ),
          true,
        )
        await dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier, false)

        await renderResult.getDispatchFollowUpActionsFinished()

        expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
          'utopia-storyboard-uid',
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:container',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting/flexchild1',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting/flexchild2',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent',
        ])
      })
    })
  })
})

function fragmentTestCode(divOrFragment: 'div' | 'fragment') {
  return `
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
      <div
        style={{
          position: 'absolute',
          left: 93.5,
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
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent'
      data-testid='flexparent'
    >
      ${
        divOrFragment === 'div'
          ? `<div data-uid='children-affecting' data-testid='children-affecting'>`
          : `<React.Fragment data-uid='children-affecting' data-testid='children-affecting'>`
      }
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
      ${divOrFragment === 'div' ? `</div>` : `</React.Fragment>`}
    </div>
  </div>
`
}
