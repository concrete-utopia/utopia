import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { act, fireEvent, screen } from '@testing-library/react'
import { offsetPoint, windowPoint, WindowPoint } from '../../core/shared/math-utils'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { selectComponents, setNavigatorRenamingTarget } from '../editor/actions/action-creators'
import * as EP from '../../core/shared/element-path'
import {
  dispatchMouseClickEventAtPoint,
  mouseClickAtPoint,
} from '../canvas/event-helpers.test-utils'
import { NavigatorItemTestId } from './navigator-item/navigator-item'
import { selectComponentsForTest, wait } from '../../utils/utils.test-utils'
import {
  navigatorEntryToKey,
  regularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'
import { NO_OP } from '../../core/shared/utils'
import {
  BottomDropTargetLineTestId,
  DragItemTestId,
  ReparentDropTargetTestId,
  TopDropTargetLineTestId,
} from './navigator-item/navigator-item-dnd-container'
import { ElementPath } from '../../core/shared/project-file-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

const SceneRootId = 'sceneroot'
const DragMeId = 'dragme'

const ASYNC_NOOP = async () => NO_OP()

async function dragElement(
  renderResult: EditorRenderResult,
  dragTargetID: string,
  dropTargetID: string,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  hoverEvents: 'apply-hover-events' | 'do-not-apply-hover-events',
  midDragCallback: () => Promise<void> = ASYNC_NOOP,
): Promise<void> {
  const dragTarget = renderResult.renderedDOM.getByTestId(dragTargetID)
  const dropTarget = renderResult.renderedDOM.getByTestId(dropTargetID)

  const endPoint = offsetPoint(startPoint, dragDelta)

  await act(async () => {
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
  })

  await act(async () => {
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
  })

  if (hoverEvents === 'apply-hover-events') {
    await act(async () => {
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
    })

    await act(async () => {
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
    })

    await midDragCallback()

    await act(async () => {
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
    })
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
        true ? <div>'Hello'</div> : null
      }
    </div>
  </Storyboard>
)
`

const projectWithFlexContainerAndCanvas = `import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        contain: 'layout',
      }}
      data-uid='root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50.5,
          top: 26,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 37,
          padding: '21px 28.5px',
          alignItems: 'flex-end',
        }}
        data-uid='container'
      >
        <div
          style={{
            backgroundColor: '#0075ff',
            width: 58,
            height: 75,
            contain: 'layout',
          }}
          data-uid='flex-child'
        />
        <div
          style={{
            backgroundColor: '#f24e1d',
            width: 65,
            height: 63,
            contain: 'layout',
          }}
          data-uid='663'
        />
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 55,
          top: 226,
          width: 298,
          height: 117,
        }}
        data-uid='abs-container'
      >
        <div
          style={{
            backgroundColor: '#35a853',
            position: 'absolute',
            left: 32,
            top: 23,
            width: 55,
            height: 74,
          }}
          data-uid='abs-child'
        />
        <div
          style={{
            backgroundColor: '#fbbc07',
            position: 'absolute',
            left: 207,
            top: 25,
            width: 55,
            height: 72,
          }}
          data-uid='8ae'
        />
      </div>
    </div>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='sb'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='scene'
      >
        <App
          data-uid='app'
          style={{
            position: '',
            bottom: 0,
            left: 0,
            right: 0,
            top: 0,
          }}
        />
      </Scene>
      <div
        style={{
          backgroundColor: '#09cf83',
          position: 'absolute',
          left: 456,
          top: 26,
          width: 96,
          height: 95,
        }}
        data-uid='dragme'
      />
    </Storyboard>
  )
}
`

describe('Navigator', () => {
  async function doBasicDrag(
    editor: EditorRenderResult,
    dragMeElementPath: ElementPath,
    targetElementPath: ElementPath,
    dropTarget = BottomDropTargetLineTestId,
  ): Promise<{ dragMeElement: HTMLElement; startingDragMeElementStyle: CSSStyleDeclaration }> {
    const dragMeElement = await editor.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMeElementPath))}`,
    )

    const startingDragMeElementStyle = { ...dragMeElement.style }

    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

    const targetElement = await editor.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(targetElementPath))}`,
    )
    const targetElementRect = targetElement.getBoundingClientRect()
    const targetElementCenter = getDomRectCenter(targetElementRect)
    const dragTo = {
      x: targetElementCenter.x,
      y: targetElementRect.y + 3,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    await selectComponentsForTest(editor, [dragMeElementPath])

    await act(async () =>
      dragElement(
        editor,
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMeElementPath))),
        dropTarget(varSafeNavigatorEntryToKey(regularNavigatorEntry(targetElementPath))),
        windowPoint(dragMeElementCenter),
        dragDelta,
        'apply-hover-events',
      ),
    )

    await editor.getDispatchFollowUpActionsFinished()

    return { dragMeElement, startingDragMeElementStyle }
  }

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

      const dragMePath = EP.fromString('sb/group/conditional/33d')

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        'NavigatorItemTestId-regular_sb/group/conditional/33d',
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

      const clickMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const clickMeElement = renderResult.renderedDOM.getByTestId(
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(clickMePath))),
      )

      const clickMeElementRect = clickMeElement.getBoundingClientRect()

      dispatchMouseClickEventAtPoint(
        windowPoint({
          x: clickMeElementRect.x + clickMeElementRect.width / 2,
          y: clickMeElementRect.y + 1,
        }),
      )

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(clickMePath)])
    })

    it('by clicking the bottom of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const clickMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const clickMeElement = await renderResult.renderedDOM.findByTestId(
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(clickMePath))),
      )

      const clickMeElementRect = clickMeElement.getBoundingClientRect()
      dispatchMouseClickEventAtPoint(
        windowPoint({
          x: clickMeElementRect.x + clickMeElementRect.width / 2,
          y: clickMeElementRect.y + clickMeElementRect.height - 1,
        }),
      )

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(clickMePath)])
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(
              'before',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
            )
            expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target line is shown
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(
              'after',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
            )
            expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target line is shown
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
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

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(
              'after',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
            )
            expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target line is shown
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
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

      await act(async () =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(
              'reparent',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target lines are not shown
            const dropTargetBefore = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetBefore.style.opacity).toEqual('0')

            const dropTargetAfter = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetAfter.style.opacity).toEqual('0')
          },
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
      await act(async () =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
          windowPoint(dragMeElementCenter),
          windowPoint({ x: -25, y: -25 }),
          'apply-hover-events',
          async () => {
            await wait(2)
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(
              'reparent',
            )

            // highlight is shown on grandparent
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa`,
            )
            expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target line is shown in original location
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/dragme', // <- moved to under the grandparent
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
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

      await act(async () =>
        dragElement(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(dragMeElementCenter),
          dragDelta,
          'apply-hover-events',
          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint.type).toEqual(
              'reparent',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
            )
            expect((parentEntry.firstChild as HTMLElement).style.border).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            const dropTargetAfter = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
            )
            expect(dropTargetAfter.style.opacity).toEqual('0')
          },
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

      await act(async () =>
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

      await act(async () =>
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

      await act(async () =>
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
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString('sb/parent2/parent1'),
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

      await act(async () =>
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

      await act(async () =>
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

    it('cannot reparent into component that does not support children', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='container'>
            <div data-uid='dragme' />
            </div>
          </div>
            `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/dragme`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
    })

    it('cannot reparent before the root element of component', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='container'>
            <div data-uid='dragme' />
            </div>
          </div>
            `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/dragme`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath, TopDropTargetLineTestId)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
    })

    it('cannot reparent after the root element of component', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='container'>
            <div data-uid='dragme' />
            </div>
          </div>
            `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/dragme`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath, BottomDropTargetLineTestId)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
    })
    it('reparenting an element to the storyboard between 2 scenes', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const childElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1/child1`,
      )
      const childElementRect = childElement.getBoundingClientRect()
      const childElementRectCenter = getDomRectCenter(childElementRect)

      const lastChildElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1/755`,
      )
      const lastChildElementRect = lastChildElement.getBoundingClientRect()
      const dragTo = {
        x: lastChildElementRect.x,
        y: lastChildElementRect.y + lastChildElementRect.height,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - childElementRectCenter.x,
        y: dragTo.y - childElementRectCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1/child1')])

      await act(async () =>
        dragElement(
          renderResult,
          DragItemTestId('regular_sb/parent1/child1'),
          BottomDropTargetLineTestId('regular_sb/parent1/755'),
          windowPoint(childElementRectCenter),
          dragDelta,
          'apply-hover-events',
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent1',
        'regular-sb/parent1/755',
        'regular-sb/child1',
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString('sb/child1'),
      ])
    })
  })

  describe('reparenting among layout systems', () => {
    it('reparenting from the canvas to a flex container', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexContainerAndCanvas,
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString('sb/dragme')
      const targetElementPath = EP.fromString('sb/scene/app:root/container/flex-child')

      const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
        editor,
        dragMeElementPath,
        targetElementPath,
      )

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/app',
        'regular-sb/scene/app:root',
        'regular-sb/scene/app:root/container',
        'regular-sb/scene/app:root/container/flex-child',
        'regular-sb/scene/app:root/container/dragme', // <- dragme moved here
        'regular-sb/scene/app:root/container/663',
        'regular-sb/scene/app:root/abs-container',
        'regular-sb/scene/app:root/abs-container/abs-child',
        'regular-sb/scene/app:root/abs-container/8ae',
      ])

      expect(dragMeElement.style.position).toEqual('')
      expect(dragMeElement.style.top).toEqual('')
      expect(dragMeElement.style.left).toEqual('')
      expect(dragMeElement.style.bottom).toEqual('')
      expect(dragMeElement.style.right).toEqual('')
      expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
      expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
    })

    const flexLayoutSnippet = (thing: string) => `
    <div
      style={{
        width: '100%',
        height: '100%',
        contain: 'layout',
      }}
      data-uid='root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50.5,
          top: 26,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 37,
          padding: '21px 28.5px',
          alignItems: 'flex-end',
        }}
        data-uid='flex'
      >
        <div
          style={{
            backgroundColor: '#35a853',
            width: 106,
            height: 193,
            contain: 'layout',
          }}
          data-uid='aaa'
        />
        <div
          style={{
            backgroundColor: '#f24e1d',
            width: 65,
            height: 63,
            contain: 'layout',
          }}
          data-uid='aab'
        />
        <div
          style={{
            backgroundColor: '#0075ff',
            width: 58,
            height: 75,
            contain: 'layout',
          }}
          data-uid='fle'
        />
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 40.5,
          top: 311.5,
          width: 362,
          height: 235,
          padding: '21px 28.5px',
        }}
        data-uid='container'
      >
        ${thing}
      </div>
    </div>
    `

    it('reparenting an element pinned to the top/left to flex', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          flexLayoutSnippet(
            `<div
          style={{
            backgroundColor: '#35a853',
            contain: 'layout',
            top: 21,
            position: 'absolute',
            width: 300,
            height: 200,
            left: 26,
          }}
          data-uid='thing'
          data-testid='thing'
          data-label='the Thing'
        />`,
          ),
        ),
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/thing`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath)

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/thing', // <- thing is moved into the flex container
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
      ])

      const element = editor.renderedDOM.getByTestId('thing')

      expect(element.style.position).toEqual('')
      expect(element.style.top).toEqual('')
      expect(element.style.left).toEqual('')
      expect(element.style.bottom).toEqual('')
      expect(element.style.right).toEqual('')
      expect(element.style.width).toEqual('300px')
      expect(element.style.height).toEqual('200px')
    })

    it('reparenting an element pinned on multiple sides to flex', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          flexLayoutSnippet(`
        <div
          style={{
            backgroundColor: '#35a853',
            contain: 'layout',
            top: 21,
            position: 'absolute',
            bottom: 21,
            width: 300,
            left: 26,
          }}
          data-uid='thing'
          data-testid='thing'
          data-label='the Thing'
        />`),
        ),
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/thing`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath)

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/thing', // <- thing is moved into the flex container
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
      ])

      const element = editor.renderedDOM.getByTestId('thing')

      expect(element.style.position).toEqual('')
      expect(element.style.top).toEqual('')
      expect(element.style.left).toEqual('')
      expect(element.style.bottom).toEqual('')
      expect(element.style.right).toEqual('')
      expect(element.style.width).toEqual('300px')
      expect(element.style.height).toEqual('193px')
    })

    it('reparenting an element with relative sizing to flex', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          flexLayoutSnippet(`
        <div
          style={{
            backgroundColor: '#35a853',
            contain: 'layout',
            width: '60%',
            height: '50%',
            position: 'absolute',
            left: 31,
            top: 21,
          }}
          data-uid='thing'
          data-testid='thing'
          data-label='the Thing'
        />`),
        ),
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/thing`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath)

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/thing', // <- thing is moved into the flex container
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
      ])

      const element = editor.renderedDOM.getByTestId('thing')

      expect(element.style.position).toEqual('')
      expect(element.style.top).toEqual('')
      expect(element.style.left).toEqual('')
      expect(element.style.bottom).toEqual('')
      expect(element.style.right).toEqual('')
      expect(element.style.width).toEqual('217px')
      expect(element.style.height).toEqual('117.5px')
    })

    it('reparenting from a flex container to the canvas', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexContainerAndCanvas,
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString('sb/scene/app:root/container/flex-child')
      const targetElementPath = EP.fromString('sb/dragme')

      const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
        editor,
        dragMeElementPath,
        targetElementPath,
      )

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/app',
        'regular-sb/scene/app:root',
        'regular-sb/scene/app:root/container',
        'regular-sb/scene/app:root/container/663',
        'regular-sb/scene/app:root/abs-container',
        'regular-sb/scene/app:root/abs-container/abs-child',
        'regular-sb/scene/app:root/abs-container/8ae',
        'regular-sb/dragme',
        'regular-sb/flex-child', // <- flex-child moved here
      ])

      expect(dragMeElement.style.position).toEqual('')
      expect(dragMeElement.style.top).toEqual('')
      expect(dragMeElement.style.left).toEqual('')
      expect(dragMeElement.style.bottom).toEqual('')
      expect(dragMeElement.style.right).toEqual('')
      expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
      expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
    })

    it('reparenting from an absolute container to a flex container', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexContainerAndCanvas,
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString('sb/scene/app:root/abs-container/abs-child')
      const targetElementPath = EP.fromString('sb/scene/app:root/container/flex-child')

      const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
        editor,
        dragMeElementPath,
        targetElementPath,
      )

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/app',
        'regular-sb/scene/app:root',
        'regular-sb/scene/app:root/container',
        'regular-sb/scene/app:root/container/flex-child',
        'regular-sb/scene/app:root/container/abs-child', // <- abs-child moved here
        'regular-sb/scene/app:root/container/663',
        'regular-sb/scene/app:root/abs-container',
        'regular-sb/scene/app:root/abs-container/8ae',
        'regular-sb/dragme',
      ])

      expect(dragMeElement.style.position).toEqual('')
      expect(dragMeElement.style.top).toEqual('')
      expect(dragMeElement.style.left).toEqual('')
      expect(dragMeElement.style.bottom).toEqual('')
      expect(dragMeElement.style.right).toEqual('')
      expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
      expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
    })

    it('reparenting from a flex container to an absolute container', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexContainerAndCanvas,
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString('sb/scene/app:root/container/flex-child')
      const targetElementPath = EP.fromString('sb/scene/app:root/abs-container/abs-child')
      const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
        editor,
        dragMeElementPath,
        targetElementPath,
      )

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/app',
        'regular-sb/scene/app:root',
        'regular-sb/scene/app:root/container',
        'regular-sb/scene/app:root/container/663',
        'regular-sb/scene/app:root/abs-container',
        'regular-sb/scene/app:root/abs-container/abs-child',
        'regular-sb/scene/app:root/abs-container/flex-child', // <- flex-child moved here
        'regular-sb/scene/app:root/abs-container/8ae',
        'regular-sb/dragme',
      ])

      expect(dragMeElement.style.position).toEqual('')
      expect(dragMeElement.style.top).toEqual('')
      expect(dragMeElement.style.left).toEqual('')
      expect(dragMeElement.style.bottom).toEqual('')
      expect(dragMeElement.style.right).toEqual('')
      expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
      expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
    })

    it('reparenting between flex containers hardcodes width/height', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 69,
          top: 65,
          width: 383,
          height: 579,
          display: 'flex',
          flexDirection: 'row',
          gap: 29,
          padding: '18px 27px',
          alignItems: 'flex-end',
        }}
        data-uid='flex1'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: '100%',
            contain: 'layout',
            flexGrow: 1,
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: '100%',
            contain: 'layout',
            flexGrow: 1,
          }}
          data-uid='c8e'
        />
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 592,
          top: 40,
          display: 'flex',
          flexDirection: 'column',
          gap: 29,
          padding: '18px 27px',
          alignItems: 'flex-end',
          width: 195,
          height: 771,
        }}
        data-uid='flex2'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            contain: 'layout',
            width: 141,
            height: 353,
          }}
          data-uid='flexchild2'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            contain: 'layout',
            width: '100%',
            flexGrow: 1,
          }}
          data-uid='aaf'
        />
      </div>
    </div>
      `),
        'await-first-dom-report',
      )

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex1/flexchild1`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex2/flexchild2`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath)

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex1/c8e',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2/flexchild2',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2/flexchild1',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2/aaf',
      ])

      const element = editor.renderedDOM.getByTestId('flexchild1')

      expect(element.style.position).toEqual('')
      expect(element.style.top).toEqual('')
      expect(element.style.left).toEqual('')
      expect(element.style.bottom).toEqual('')
      expect(element.style.right).toEqual('')
      expect(element.style.width).toEqual('150px')
      expect(element.style.height).toEqual('543px')
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

      await act(async () =>
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

      await act(async () =>
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

  describe('rename', () => {
    it('can rename entries', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='aaa'>hello</div>
          <div data-uid='bbb'>there</div>
        </div>
        `),
        'await-first-dom-report',
      )

      const target = EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/aaa`)
      await act(async () => {
        await renderResult.dispatch([setNavigatorRenamingTarget(target)], true)
      })

      const input: HTMLInputElement = await screen.findByTestId('navigator-item-label-hello')
      await act(async () => {
        document.execCommand('insertText', false, 'aloha')
        fireEvent.blur(input)
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/bbb',
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div data-uid='aaa' data-label='aloha'>hello</div>
            <div data-uid='bbb'>there</div>
          </div>
        `),
      )
    })
  })
})
