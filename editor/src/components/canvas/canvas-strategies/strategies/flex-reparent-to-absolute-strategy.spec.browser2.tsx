import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import { windowPoint, WindowPoint } from '../../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../../utils/modifiers'
import { NavigatorEntry } from '../../../editor/store/editor-state'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { setFeatureForBrowserTests } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { AllContentAffectingTypes, ContentAffectingType } from './group-like-helpers'
import {
  getClosingGroupLikeTag,
  getOpeningGroupLikeTag,
  getRegularNavigatorTargets,
} from './group-like-helpers.test-utils'
import { mockGenerateUid } from '../../../../core/model/element-template-utils.test-utils'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  click: boolean,
  midDragCallback?: () => Promise<void>,
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
    midDragCallback: midDragCallback,
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
  const setNextGeneratedUids = mockGenerateUid()

  AllContentAffectingTypes.forEach((type) => {
    describe(`– ${type} parents`, () => {
      it('reparents regular child from a children-affecting flex parent to absolute', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(fragmentTestCode(type, setNextGeneratedUids)),
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

        expect(getRegularNavigatorTargets(renderResult)).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:container',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/flexchild1', // <- flexChild1 is successfully reparented
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent/children-affecting',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent/children-affecting/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent/children-affecting/inner-fragment/flexchild2',
        ])
      })

      it('reparents children-affecting element from flex to absolute', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(fragmentTestCode(type, setNextGeneratedUids)),
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
        await dragElement(renderResult, 'flexchild1', dragDelta, cmdModifier, false, async () => {
          // mid drag: make sure that flexchild1 is _not_ visible at the original location, even if it's a fragment's child
          const flexChildOnes = await renderResult.renderedDOM.getAllByTestId('flexchild1')
          expect(flexChildOnes.length).toBe(2)
          expect(flexChildOnes[0].style.visibility).not.toEqual('hidden')
          expect(flexChildOnes[1].style.visibility).toEqual('hidden')
        })

        await renderResult.getDispatchFollowUpActionsFinished()

        expect(getRegularNavigatorTargets(renderResult)).toEqual([
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:container',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting', // <- the children-affecting element have been properly reparented
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting/inner-fragment/flexchild1',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/absoluteparent/absolutechild/children-affecting/inner-fragment/flexchild2',
          'utopia-storyboard-uid/scene-aaa/app-entity:container/flexparent',
        ])

        const propsOfFragment =
          renderResult.getEditorState().editor.allElementProps[
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting'
          ]
        // the fragment-like element continues to have no style prop
        expect(propsOfFragment?.style == null).toBeTruthy()
        const propsOfInnerFragment =
          renderResult.getEditorState().editor.allElementProps[
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/otherparent/children-affecting/inner-fragment'
          ]
        // the inner fragment-like element continues to have no style prop
        expect(propsOfInnerFragment?.style == null).toBeTruthy()
      })
    })
  })
})

function fragmentTestCode(type: ContentAffectingType, setFakeUids: (_: string[]) => void) {
  if (type === 'conditional') {
    setFakeUids([
      'skip1',
      'skip2',
      'skip3',
      'skip4',
      'skip5',
      'skip6',
      'inner-fragment',
      'inner-fragment-2',
    ])
  } else {
    setFakeUids(['skip1', 'skip2', 'inner-fragment', 'skip3', 'children-affecting'])
  }

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
      ${getOpeningGroupLikeTag(type)}
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
      ${getClosingGroupLikeTag(type)}
    </div>
  </div>
`
}
