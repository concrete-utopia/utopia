import {
  EditorRenderResult,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import { offsetPoint, windowPoint, WindowPoint } from '../../core/shared/math-utils'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { selectComponents } from '../editor/actions/action-creators'
import * as EP from '../../core/shared/element-path'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { wait } from '../../utils/utils.test-utils'
import { NavigatorItemTestId } from './navigator-item/navigator-item'

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

describe('Navigator', () => {
  describe('selecting elements', () => {
    it('by clicking the center of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePathString = EP.toString(
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`),
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(dragMePathString),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height / 2,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([dragMePathString])
    })

    it('by clicking the top of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePathString = EP.toString(
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`),
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(dragMePathString),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + 1,
      })

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([dragMePathString])
    })

    it('by clicking the bottom of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePathString = EP.toString(
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`),
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(dragMePathString),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height - 1,
      })

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([dragMePathString])
    })
  })

  describe('reordering', () => {
    it('reorders to before the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-before-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to before `firstdiv`
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders to after the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to after `firstdiv`
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders to after the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to after the last sibling `notdrag` under its parent
        'utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents under the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 15,
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
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/dragme', // <- moved to under the first sibling
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents under grandparent', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
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
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
          windowPoint(dragMeElementCenter),
          windowPoint({ x: -65, y: 0 }),
          'apply-hover-events',
        )
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/parentsibling',
        'utopia-storyboard-uid/dragme', // <- moved to under the grandparent
      ])
    })

    it('reparents under cousin element', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const cousinDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/parentsibling`,
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
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/parentsibling',
        'utopia-storyboard-uid/scene-aaa/parentsibling/dragme', // <- moved to under the cousin element
      ])
    })

    it('attempt to reparent non-reparentable item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const notDragElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDragElementRect = notDragElement.getBoundingClientRect()
      const notDragElementCenter = getDomRectCenter(notDragElementRect)
      const cousinDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/parentsibling`,
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
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          `navigator-item-drop-after-utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(notDragElementCenter),
          dragDelta,
          'do-not-apply-hover-events',
        ),
      )

      expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(null)
      expect(
        renderResult.getEditorState().editor.navigator.dropTargetHint.displayAtElementPath,
      ).toEqual(null)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag', // <- cannot be moved
        'utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('does not reparent to invalid target', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      act(() =>
        dragElement(
          renderResult,
          `navigator-item-drag-utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().derived.navigatorTargets.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa',
        'utopia-storyboard-uid/scene-aaa/sceneroot',
        'utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- cannot be moved
        'utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('can reparent top-level element', () => expect('not implemented').toEqual('implemented'))

    it('cannot reparent parent element inside itself', () =>
      expect('not implemented').toEqual('implemented'))

    it('cannot reparent inside an element that does not support children', () =>
      expect('not implemented').toEqual('implemented'))
  })
})
