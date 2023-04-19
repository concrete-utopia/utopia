import {
  EditorRenderResult,
  renderTestEditorWithCode,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import { offsetPoint, windowPoint, WindowPoint } from '../../core/shared/math-utils'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { selectComponents } from '../editor/actions/action-creators'
import * as EP from '../../core/shared/element-path'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { NavigatorItemTestId } from './navigator-item/navigator-item'
import {
  selectComponentsForTest,
  setFeatureForBrowserTests,
  wait,
} from '../../utils/utils.test-utils'
import {
  navigatorEntryToKey,
  regularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'

const SceneRootId = 'sceneroot'
const DragMeId = 'dragme'

function dragElement(
  renderResult: EditorRenderResult,
  dragTargetID: string,
  dropTargetID: string,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  hoverEvents: 'apply-hover-events' | 'do-not-apply-hover-events',
): void {
  const dragTarget = renderResult.renderedDOM.getByTestId(dragTargetID)
  const dropTarget = renderResult.renderedDOM.getByTestId(dropTargetID)

  const endPoint = offsetPoint(startPoint, dragDelta)

  fireEvent(
    dragTarget,
    new MouseEvent('dragstart', {
      bubbles: true,
      cancelable: true,
      clientX: startPoint.x,
      clientY: startPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    dragTarget,
    new MouseEvent('drag', {
      bubbles: true,
      cancelable: true,
      clientX: endPoint.x,
      clientY: endPoint.y,
      movementX: dragDelta.x,
      movementY: dragDelta.y,
      buttons: 1,
    }),
  )

  if (hoverEvents === 'apply-hover-events') {
    fireEvent(
      dropTarget,
      new MouseEvent('dragenter', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        movementX: dragDelta.x,
        movementY: dragDelta.y,
        buttons: 1,
      }),
    )

    fireEvent(
      dropTarget,
      new MouseEvent('dragover', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        movementX: dragDelta.x,
        movementY: dragDelta.y,
        buttons: 1,
      }),
    )

    fireEvent(
      dropTarget,
      new MouseEvent('drop', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        buttons: 1,
      }),
    )
  }
}

function getProjectCode(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='${SceneRootId}'
        data-testid='${SceneRootId}'
        data-label='${SceneRootId}'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='${DragMeId}'
          data-testid='${DragMeId}'
          data-label='${DragMeId}'
        >
          drag me
        </div>
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

const projectWithHierarchy = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 207,
        height: 311,
      }}
      data-testid='parent1'
      data-uid='parent1'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: -18,
          top: 164,
          position: 'absolute',
        }}
        data-uid='child1'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: -18,
          top: -38,
          position: 'absolute',
        }}
        data-uid='755'
      />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 51,
        top: 444,
        width: 207,
        height: 311,
      }}
      data-uid='parent2'
      data-testid='parent2'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: -18,
          top: 164,
          position: 'absolute',
        }}
        data-uid='aaa'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: -18,
          top: -38,
          position: 'absolute',
        }}
        data-uid='aab'
      />
    </div>
    <span
      style={{
        position: 'absolute',
        wordBreak: 'break-word',
        left: 326,
        top: 215,
        width: 143,
        height: 19,
      }}
      data-testid='text'
      data-uid='text'
    >
      Cannot reparent here
    </span>
  </Storyboard>
)
`

const projectWithGroupsAndNotGroups = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 1136,
          top: 325,
          width: 141,
          height: 190,
        }}
        data-uid='groupchild'
      />
    </div>
    <React.Fragment data-uid='fragment'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 257.5,
          top: 229,
          width: 250,
          height: 238,
        }}
        data-uid='fragmentchild'
      />
    </React.Fragment>
    <div
      data-uid='offsetparent'
      style={{
        position: 'absolute',
        width: 310,
        height: 575,
        left: 566,
        top: -2,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 165,
          top: 360,
          width: 119,
          height: 193,
        }}
        data-uid='offsetchild'
      />
    </div>
    <div
      data-uid='nonoffsetparent'
      style={{
        width: 310,
        height: 575,
        left: 248,
        top: 515,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 83,
          top: 274,
          width: 119,
          height: 193,
        }}
        data-uid='nonoffsetchild'
      />
    </div>
  </Storyboard>
)
`

const projectWithExpression = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      {
        // @utopia/uid=conditional
        true ? 'Hello' : null
      }
    </div>
  </Storyboard>
)
`

describe('Navigator', () => {
  describe('selecting elements', () => {
    it('by clicking the center of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMePath))),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height / 2,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })

    it('by clicking the center of the item which is an expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithExpression,
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString('sb/group/conditional/0e3')

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        'NavigatorItemTestId-synthetic_sb/group/conditional/0e3_attribute',
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height / 2,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })

    it('by clicking the top of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMePath))),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + 1,
      })

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })

    it('by clicking the bottom of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMePath))),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height - 1,
      })

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })
  })

  describe('reordering', () => {
    it('reorders to before the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to before `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders to after the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + firstDivElementRect.height,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to after `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders to after the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y + notDraggableDivElementRect.height - 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to after the last sibling `notdrag` under its parent
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents under the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 20,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/dragme', // <- moved to under the first sibling
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents under grandparent', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() => {
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
          windowPoint(dragMeElementCenter),
          windowPoint({ x: -65, y: 0 }),
          'apply-hover-events',
        )
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
        'regular-utopia-storyboard-uid/dragme', // <- moved to under the grandparent
      ])
    })

    it('reparents under cousin element', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const cousinDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
      )
      const cousinDivElementRect = cousinDivElement.getBoundingClientRect()
      const cousinDivElementCenter = getDomRectCenter(cousinDivElementRect)
      const dragTo = {
        x: cousinDivElementCenter.x,
        y: cousinDivElementRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling/dragme', // <- moved to under the cousin element
      ])
    })

    it('attempt to reparent non-reparentable item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const notDragElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDragElementRect = notDragElement.getBoundingClientRect()
      const notDragElementCenter = getDomRectCenter(notDragElementRect)
      const cousinDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
      )
      const cousinDivElementRect = cousinDivElement.getBoundingClientRect()
      const cousinDivElementCenter = getDomRectCenter(cousinDivElementRect)
      const dragTo = {
        x: cousinDivElementCenter.x,
        y: cousinDivElementRect.y + 10,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - notDragElementCenter.x,
        y: dragTo.y - notDragElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/notdrag')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(notDragElementCenter),
          dragDelta,
          'do-not-apply-hover-events',
        ),
      )

      expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(null)
      expect(renderResult.getEditorState().editor.navigator.dropTargetHint.displayAtEntry).toEqual(
        null,
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('does not reparent to invalid target', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('can reparent top-level element', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const parent1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1`,
      )
      const parent1DndContainerRect = parent1DndContainer.getBoundingClientRect()
      const parent1DndContainerCenter = getDomRectCenter(parent1DndContainerRect)

      const child1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent2`,
      )
      const child1DndContainerRect = child1DndContainer.getBoundingClientRect()
      const child1DndContainerCenter = getDomRectCenter(child1DndContainerRect)
      const dragTo = {
        x: child1DndContainerCenter.x,
        y: child1DndContainerRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - parent1DndContainerCenter.x,
        y: dragTo.y - parent1DndContainerCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1')])

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_sb/parent1`,
          `navigator-item-regular_sb/parent2`,
          windowPoint(parent1DndContainerCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent2',
        'regular-sb/parent2/parent1', // <- parent1 and its children moved under parent2
        'regular-sb/parent2/parent1/child1', // <- parent1 and its children moved under parent2
        'regular-sb/parent2/parent1/755', // <- parent1 and its children moved under parent2
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
    })

    it('cannot reparent parent element inside itself', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const parent1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1`,
      )
      const parent1DndContainerRect = parent1DndContainer.getBoundingClientRect()
      const parent1DndContainerCenter = getDomRectCenter(parent1DndContainerRect)

      const child1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1/child1`,
      )
      const child1DndContainerRect = child1DndContainer.getBoundingClientRect()
      const child1DndContainerCenter = getDomRectCenter(child1DndContainerRect)
      const dragTo = {
        x: child1DndContainerCenter.x,
        y: child1DndContainerRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - parent1DndContainerCenter.x,
        y: dragTo.y - parent1DndContainerCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1')])

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_sb/parent1`,
          `navigator-item-regular_sb/parent1/child1`,
          windowPoint(parent1DndContainerCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent1', // <- cannot be reparented under its own child
        'regular-sb/parent1/child1',
        'regular-sb/parent1/755',
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
    })

    it('cannot reparent inside an element that does not support children', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const parent1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1`,
      )
      const parent1DndContainerRect = parent1DndContainer.getBoundingClientRect()
      const parent1DndContainerCenter = getDomRectCenter(parent1DndContainerRect)

      const textDndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/text`,
      )
      const textDndContainerRect = textDndContainer.getBoundingClientRect()
      const textDndContainerCenter = getDomRectCenter(textDndContainerRect)
      const dragTo = {
        x: textDndContainerCenter.x,
        y: textDndContainerRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - parent1DndContainerCenter.x,
        y: dragTo.y - parent1DndContainerCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1')])

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_sb/parent1`,
          `navigator-item-regular_sb/text`,
          windowPoint(parent1DndContainerCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent1', // <- cannot be reparented under `text`
        'regular-sb/parent1/child1',
        'regular-sb/parent1/755',
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
    })
  })

  describe('derived data', () => {
    it('element warnings', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithGroupsAndNotGroups,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/group/groupchild')])

      const { elementWarnings } = editor.getEditorState().derived

      expect(elementWarnings['sb/group/groupchild'].value.absoluteWithUnpositionedParent).toEqual(
        false,
      )
      expect(
        elementWarnings['sb/fragment/fragmentchild'].value.absoluteWithUnpositionedParent,
      ).toEqual(false)
      expect(
        elementWarnings['sb/offsetparent/offsetchild'].value.absoluteWithUnpositionedParent,
      ).toEqual(false)
      expect(
        elementWarnings['sb/nonoffsetparent/nonoffsetchild'].value.absoluteWithUnpositionedParent,
      ).toEqual(true)
    })
  })

  describe('reparenting to children-affecting elements', () => {
    it('reparenting into fragment reparents to the correct index', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithGroupsAndNotGroups,
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/offsetparent/offsetchild`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const dragToElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/fragment/fragmentchild`,
      )
      const dragToElementRect = dragToElement.getBoundingClientRect()
      const dragToElementRectCenter = getDomRectCenter(dragToElementRect)
      const dragTo = {
        x: dragToElementRectCenter.x,
        y: dragToElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/offsetparent/offsetchild')])

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_sb/offsetparent/offsetchild`,
          `navigator-item-drop-before-regular_sb/fragment/fragmentchild`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/group',
        'regular-sb/group/groupchild',
        'regular-sb/fragment',
        'regular-sb/fragment/offsetchild', // <- offsetchild is moved to under fragment from offsetparent
        'regular-sb/fragment/fragmentchild',
        'regular-sb/offsetparent',
        'regular-sb/nonoffsetparent',
        'regular-sb/nonoffsetparent/nonoffsetchild',
      ])
    })

    it('reparenting into sizeless div reparents to the right index', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithGroupsAndNotGroups,
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/offsetparent/offsetchild`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const dragToElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/nonoffsetparent/nonoffsetchild`,
      )
      const dragToElementRect = dragToElement.getBoundingClientRect()
      const dragToElementRectCenter = getDomRectCenter(dragToElementRect)
      const dragTo = {
        x: dragToElementRectCenter.x,
        y: dragToElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/offsetparent/offsetchild')])

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_sb/offsetparent/offsetchild`,
          `navigator-item-drop-before-regular_sb/nonoffsetparent/nonoffsetchild`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/group',
        'regular-sb/group/groupchild',
        'regular-sb/fragment',
        'regular-sb/fragment/fragmentchild',
        'regular-sb/offsetparent',
        'regular-sb/nonoffsetparent',
        'regular-sb/nonoffsetparent/offsetchild', // <- offsetchild is moved to under nonoffsetparent from offsetparent
        'regular-sb/nonoffsetparent/nonoffsetchild',
      ])
    })
  })
})
