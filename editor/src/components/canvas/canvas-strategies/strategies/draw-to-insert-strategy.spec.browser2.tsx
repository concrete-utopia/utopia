import { setRightMenuTab } from '../../../editor/actions/action-creators'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import * as EP from '../../../../core/shared/element-path'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import { RightMenuTab } from '../../../editor/store/editor-state'
import { FOR_TESTS_setNextGeneratedUid } from '../../../../core/model/element-template-utils.test-utils'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'

function slightlyOffsetWindowPointBecauseVeryWeirdIssue(point: { x: number; y: number }) {
  // FIXME when running in headless chrome, the result of getBoundingClientRect will be slightly
  // offset for some unknown reason, meaning the inserted element will be 1 pixel of in each dimension
  return { x: point.x - 0.001, y: point.y - 0.001 }
}

async function setupInsertTest(inputCode: string): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

  const newUID = 'ddd'
  FOR_TESTS_setNextGeneratedUid(newUID)

  return renderResult
}

function ensureInInsertMode(renderResult: EditorRenderResult): void {
  expect(renderResult.getEditorState().editor.mode.type).toEqual('insert')
}

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "runClickToInsertTest" ] }] */

describe('draw-to-insert', () => {
  describe('Inserting into absolute', () => {
    const inputCode = makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
      }}
    >
      <div
        data-uid='bbb'
        data-testid='bbb'
        style={{
          position: 'absolute',
          left: 10,
          top: 10,
          width: 380,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          position: 'absolute',
          left: 10,
          top: 200,
          width: 380,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `)

    describe('Click to insert with default size', () => {
      async function runClickToInsertTest(renderResult: EditorRenderResult) {
        const targetElement = renderResult.renderedDOM.getByTestId('bbb')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: targetElementBounds.x + 65,
          y: targetElementBounds.y + 55,
        })

        // Move before clicking
        await mouseMoveToPoint(canvasControlsLayer, point)

        // Highlight should show the candidate parent
        expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

        // Click in bbb
        await mouseClickAtPoint(canvasControlsLayer, point)

        await renderResult.getDispatchFollowUpActionsFinished()

        // Check that the inserted element is a child of bbb
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div
            data-uid='aaa'
            style={{
              width: '100%',
              height: '100%',
              backgroundColor: '#FFFFFF',
              position: 'relative',
            }}
          >
            <div
              data-uid='bbb'
              data-testid='bbb'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 380,
                height: 180,
                backgroundColor: '#d3d3d3',
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 15,
                  top: 5,
                  width: 100,
                  height: 100,
                }}
                data-uid='ddd'
              />
            </div>
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                left: 10,
                top: 200,
                width: 380,
                height: 190,
                backgroundColor: '#FF0000',
              }}
            />
          </div>
        `),
        )
      }

      it('works with the keyboard shortcut', async () => {
        const renderResult = await setupInsertTest(inputCode)
        await pressKey('d')
        ensureInInsertMode(renderResult)
        await runClickToInsertTest(renderResult)
      })
    })
  })

  describe('Inserting into grid', () => {
    const project = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 0,
        top: 0,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          position: 'absolute',
          left: 10,
          top: 10,
          width: 500,
          height: 300,
          display: 'grid',
          gap: 2,
          gridTemplateColumns: '1fr 1fr 1fr',
          gridTemplateRows: '1fr',
          backgroundColor: '#9dc1ea',
        }}
        data-uid='grid'
        data-testid='grid'
      />
    </Scene>
  </Storyboard>
)
`

    it('can click to insert into a grid', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid')])

      await pressKey('d')
      ensureInInsertMode(editor)

      const grid = editor.renderedDOM.getByTestId('grid')
      const gridBB = grid.getBoundingClientRect()

      const target: WindowPoint = windowPoint({
        x: gridBB.x + 300,
        y: gridBB.y + 150,
      })

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await mouseMoveToPoint(canvasControlsLayer, target)
      await mouseClickAtPoint(canvasControlsLayer, target)
      await editor.getDispatchFollowUpActionsFinished()

      const child = grid.firstChild
      if (child == null) {
        throw new Error('Draw to insert should be able to insert an element')
      }

      const { gridRow, gridColumn, width, height, position, top, left } = (child as HTMLElement)
        .style

      expect({ gridRow, gridColumn, width, height, position, top, left }).toEqual({
        gridColumn: '2',
        gridRow: '1',
        height: '100px',
        left: '',
        position: '',
        top: '',
        width: '100px',
      })
    })

    it('can click to insert into a grid when zoomed in', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid')])
      await editor.dispatch([CanvasActions.zoom(2)], true)
      await editor.getDispatchFollowUpActionsFinished()

      await pressKey('d')
      ensureInInsertMode(editor)

      const grid = editor.renderedDOM.getByTestId('grid')
      const gridBB = grid.getBoundingClientRect()

      const target: WindowPoint = windowPoint({
        x: gridBB.x + 300,
        y: gridBB.y + 150,
      })

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await mouseMoveToPoint(canvasControlsLayer, target)
      await mouseClickAtPoint(canvasControlsLayer, target)
      await editor.getDispatchFollowUpActionsFinished()

      const child = grid.firstChild
      if (child == null) {
        throw new Error('Draw to insert should be able to insert an element')
      }

      const { gridRow, gridColumn, width, height, position, top, left } = (child as HTMLElement)
        .style

      expect({ gridRow, gridColumn, width, height, position, top, left }).toEqual({
        gridColumn: '1',
        gridRow: '1',
        height: '100px',
        left: '',
        position: '',
        top: '',
        width: '100px',
      })
    })

    it('can draw to insert into a grid', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid')])

      await pressKey('d')
      ensureInInsertMode(editor)

      const grid = editor.renderedDOM.getByTestId('grid')
      const gridBB = grid.getBoundingClientRect()

      const target: WindowPoint = windowPoint({
        x: gridBB.x + 250,
        y: gridBB.y + 150,
      })

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await mouseMoveToPoint(canvasControlsLayer, target)
      await mouseDragFromPointToPoint(canvasControlsLayer, target, {
        x: target.x + 40,
        y: target.y + 60,
      })
      await editor.getDispatchFollowUpActionsFinished()

      const child = grid.firstChild
      if (child == null) {
        throw new Error('Draw to insert should be able to insert an element')
      }

      const { gridRow, gridColumn, width, height, position, top, left } = (child as HTMLElement)
        .style

      expect({ gridRow, gridColumn, width, height, position, top, left }).toEqual({
        gridColumn: '2',
        gridRow: '1',
        height: '60px',
        left: '84px',
        position: 'absolute',
        top: '151px',
        width: '40px',
      })
    })

    it('can draw to insert into a grid when zoomed in', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid')])

      await editor.dispatch([CanvasActions.zoom(2)], true)
      await editor.getDispatchFollowUpActionsFinished()

      await pressKey('d')
      ensureInInsertMode(editor)

      const grid = editor.renderedDOM.getByTestId('grid')
      const gridBB = grid.getBoundingClientRect()

      const target: WindowPoint = windowPoint({
        x: gridBB.x + 250,
        y: gridBB.y + 150,
      })

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await mouseMoveToPoint(canvasControlsLayer, target)
      await mouseDragFromPointToPoint(canvasControlsLayer, target, {
        x: target.x + 40,
        y: target.y + 60,
      })
      await editor.getDispatchFollowUpActionsFinished()

      const child = grid.firstChild
      if (child == null) {
        throw new Error('Draw to insert should be able to insert an element')
      }

      const { gridRow, gridColumn, width, height, position, top, left } = (child as HTMLElement)
        .style

      expect({ gridRow, gridColumn, width, height, position, top, left }).toEqual({
        gridColumn: '1',
        gridRow: '1',
        height: '30px',
        left: '126px',
        position: 'absolute',
        top: '76px',
        width: '20px',
      })
    })
  })
})
