import type { ElementPath } from 'utopia-shared/src/types'
import * as EP from '../../../../core/shared/element-path'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { offsetPoint, windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { GridCellTestId } from '../../controls/grid-controls-for-strategies'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { Point } from '../../event-helpers.test-utils'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
  mouseUpAtPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'

describe('grid reparent strategies', () => {
  describe('reparent into a grid', () => {
    it('from the storyboard', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          extraCode: `
      <div
        style={{
          backgroundColor: '#f0f',
          position: 'absolute',
          left: 500,
          top: 500,
          width: 79,
          height: 86,
        }}
        data-uid='dragme'
        data-testid='dragme'
      />
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/dragme')])

      await dragElement(editor, 'dragme', windowPoint({ x: -200, y: -200 }), cmdModifier)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridColumn: 3,
            gridRow: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          }),
        ),
      )
    })
    it('from a flex container', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          width: 386,
          height: 202,
          display: 'flex',
          gap: 10,
        }}
        data-uid='flex'
        data-testid='flex'
      >
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='bar'
        />
      </div>
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/flex/dragme')])

      await dragElement(editor, 'dragme', windowPoint({ x: -200, y: -200 }), cmdModifier)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          width: 386,
          height: 202,
          display: 'flex',
          gap: 10,
        }}
        data-uid='flex'
        data-testid='flex'
      >
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='bar'
        />
      </div>
      `,
            insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridColumn: 3,
            gridRow: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          }),
        ),
      )
    })
    it('from a flow element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          padding: 10,
        }}
        data-uid='flow'
        data-testid='flow'
      >
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='bar'
        />
      </div>
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/flow/dragme')])

      await dragElement(editor, 'dragme', windowPoint({ x: -200, y: -200 }), cmdModifier)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          padding: 10,
        }}
        data-uid='flow'
        data-testid='flow'
      >
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='bar'
        />
      </div>
      `,
            insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridColumn: 3,
            gridRow: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          }),
        ),
      )
    })
  })
  describe('reparent out of a grid', () => {
    it('into the storyboard', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridRow: 2,
            gridColumn: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/grid/dragme')])

      await dragOut(editor, EP.fromString('sb/grid/dragme'), { x: 2000, y: 1000 })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            position: 'absolute',
            left: 1627,
            top: 934,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          }),
        ),
      )
    })
    it('into a flex container', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridRow: 2,
            gridColumn: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          display: 'flex',
          gap: 30,
          padding: 30,
        }}
        data-uid='flex'
        data-testid='flex'
      >
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='bar'
          data-testid='bar'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='baz'
        />
      </div>
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/grid/dragme')])

      const barElement = editor.renderedDOM.getByTestId('bar')
      const barRect = barElement.getBoundingClientRect()
      const endPoint = {
        x: barRect.x - 10,
        y: barRect.y + barRect.height / 2,
      }

      await dragOut(editor, EP.fromString('sb/grid/dragme'), endPoint)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 500,
            top: 500,
            display: 'flex',
            gap: 30,
            padding: 30,
          }}
          data-uid='flex'
          data-testid='flex'
        >
          <div
            style={{
              backgroundColor: '#0ff',
              width: 79,
              height: 86,
            }}
            data-uid='foo'
          />
          <div
            style={{
              backgroundColor: '#f0f',
              width: 79,
              height: 86,
            }}
            data-uid='dragme'
            data-testid='dragme'
          />
          <div
            style={{
              backgroundColor: '#0ff',
              width: 79,
              height: 86,
            }}
            data-uid='bar'
            data-testid='bar'
          />
          <div
            style={{
              backgroundColor: '#0ff',
              width: 79,
              height: 86,
            }}
            data-uid='baz'
          />
        </div>
      `,
          }),
        ),
      )
    })
    it('into a flow element with flow strategy selected', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridRow: 2,
            gridColumn: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          padding: 10,
        }}
        data-uid='flow'
        data-testid='flow'
      >
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='foo'
          data-testid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='bar'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
          }}
          data-uid='baz'
        />
      </div>
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/grid/dragme')])

      const fooElement = editor.renderedDOM.getByTestId('foo')
      const fooRect = fooElement.getBoundingClientRect()
      const endPoint = {
        x: fooRect.x + fooRect.width / 2,
        y: fooRect.y + fooRect.height / 2,
      }

      await dragOut(editor, EP.fromString('sb/grid/dragme'), endPoint, async () => {
        await pressKey('3', { modifiers: cmdModifier }) // this should select the Reparent (Flow) strategy
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 500,
            top: 500,
            padding: 10,
          }}
          data-uid='flow'
          data-testid='flow'
        >
          <div
            style={{
              backgroundColor: '#0ff',
              width: 79,
              height: 86,
            }}
            data-uid='foo'
            data-testid='foo'
          >
            <div
              style={{
                backgroundColor: '#f0f',
                width: 79,
                height: 86,
              }}
              data-uid='dragme'
              data-testid='dragme'
            />
          </div>
          <div
            style={{
              backgroundColor: '#0ff',
              width: 79,
              height: 86,
            }}
            data-uid='bar'
          />
          <div
            style={{
              backgroundColor: '#0ff',
              width: 79,
              height: 86,
            }}
            data-uid='baz'
          />
        </div>
      `,
          }),
        ),
      )
    })
    it('into a grid container', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridRow: 2,
            gridColumn: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          extraCode: `
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 500,
          top: 500,
          padding: 10,
          display: 'grid',
          gap: 10,
          gridTemplateRows: '1fr 1fr',
          gridTemplateColumns: '1fr 1fr 1fr 1fr',
        }}
        data-uid='another-grid'
        data-testid='another-grid'
      >
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
            gridRow: 1,
            gridColumn: 1,
          }}
          data-uid='foo'
          data-testid='foo'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
            gridRow: 2,
            gridColumn: 3,
          }}
          data-uid='bar'
          data-testid='bar'
        />
        <div
          style={{
            backgroundColor: '#0ff',
            width: 79,
            height: 86,
            gridColumn: 2,
            gridColumn: 2,
          }}
          data-uid='baz'
        />
      </div>
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/grid/dragme')])

      await dragOutToAnotherGrid(
        editor,
        'another-grid',
        {
          x: 10,
          y: 180,
        },
        EP.fromString('sb/grid/dragme'),
      )
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 500,
        top: 500,
        padding: 10,
        display: 'grid',
        gap: 10,
        gridTemplateRows: '1fr 1fr',
        gridTemplateColumns: '1fr 1fr 1fr 1fr',
      }}
      data-uid='another-grid'
      data-testid='another-grid'
    >
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
          gridRow: 1,
          gridColumn: 1,
        }}
        data-uid='foo'
        data-testid='foo'
      />
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
          gridRow: 2,
          gridColumn: 3,
        }}
        data-uid='bar'
        data-testid='bar'
      />
      <div
        style={{
          backgroundColor: '#f0f',
          width: 79,
          height: 86,
          gridColumn: 1,
          gridRow: 2,
        }}
        data-uid='dragme'
        data-testid='dragme'
      />
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
          gridColumn: 2,
          gridColumn: 2,
        }}
        data-uid='baz'
      />
    </div>
      `,
          }),
        ),
      )
    })
    it('into a grid container with reorder (no explicit gridRow/gridColumn props', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 79,
            height: 86,
            gridRow: 2,
            gridColumn: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          extraCode: `
     <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 500,
        top: 500,
        padding: 10,
        display: 'grid',
        gap: 10,
        gridTemplateRows: '1fr 1fr',
        gridTemplateColumns: '1fr 1fr 1fr',
        width: 317,
      }}
      data-uid='91c5676d-d826-423e-86d5-10b80717'
      data-testid='another-grid'
    >
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
        }}
        data-uid='3995ebba-85b5-4293-964c-0c78fbe6'
        data-testid='foo'
      />
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
        }}
        data-uid='eacc03a2-05bd-4f77-a8b2-3ab490ec'
        data-testid='bar'
      />
    </div>
    `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/grid/dragme')])

      await dragOutToAnotherGrid(
        editor,
        'another-grid',
        {
          x: 300,
          y: 20,
        },
        EP.fromString('sb/grid/dragme'),
      )

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
        <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 500,
        top: 500,
        padding: 10,
        display: 'grid',
        gap: 10,
        gridTemplateRows: '1fr 1fr',
        gridTemplateColumns: '1fr 1fr 1fr',
        width: 317,
      }}
      data-uid='91c5676d-d826-423e-86d5-10b80717'
      data-testid='another-grid'
    >
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
        }}
        data-uid='3995ebba-85b5-4293-964c-0c78fbe6'
        data-testid='foo'
      />
      <div
        style={{
          backgroundColor: '#0ff',
          width: 79,
          height: 86,
        }}
        data-uid='eacc03a2-05bd-4f77-a8b2-3ab490ec'
        data-testid='bar'
      />
      <div
        style={{
          backgroundColor: '#f0f',
          width: 79,
          height: 86,
		  gridColumn: 3,
		  gridRow: 1,
        }}
        data-uid='dragme'
        data-testid='dragme'
      />
    </div>
      `,
          }),
        ),
      )
    })
    it('when the cell has no explicit size', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode({
          insideGrid: `
        <div
          style={{
            backgroundColor: '#f0f',
            gridRow: 2,
            gridColumn: 2,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/grid/dragme')])

      await dragOut(editor, EP.fromString('sb/grid/dragme'), { x: 2000, y: 1000 })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
        <div
          style={{
            backgroundColor: '#f0f',
            width: 87,
            height: 135,
            position: 'absolute',
            left: 1627,
            top: 934,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      `,
          }),
        ),
      )
    })
  })
})

function makeTestProjectCode(params: { extraCode?: string; insideGrid?: string }) {
  const insideGrid = params.insideGrid?.trim()
  const extraCode = params.extraCode?.trim()
  return `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb' data-testid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 100,
        top: 100,
        width: 300,
        height: 300,
        display: 'grid',
        gridTemplateColumns: '1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr',
        gridGap: 10,
        padding: 10,
      }}
      data-uid='grid'
      data-testid='grid'
    ${insideGrid == null ? '/>' : `>${insideGrid}</div>`}
    ${extraCode ?? ''}
  </Storyboard>
)
`
}

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const endPoint = offsetPoint(startPoint, dragDelta)

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
    modifiers: modifiers,
  })
}

async function dragOut(
  renderResult: EditorRenderResult,
  cell: ElementPath,
  endPoint: Point,
  midDragCallback?: () => Promise<void>,
): Promise<void> {
  const sourceGridCell = renderResult.renderedDOM.getByTestId(GridCellTestId(cell))
  const sourceRect = sourceGridCell.getBoundingClientRect()

  await mouseClickAtPoint(
    sourceGridCell,
    { x: sourceRect.x + 5, y: sourceRect.y + 5 },
    { modifiers: cmdModifier },
  )

  await mouseDownAtPoint(
    sourceGridCell,
    { x: sourceRect.x + 5, y: sourceRect.y + 5 },
    {
      modifiers: cmdModifier,
    },
  )

  const delta: Point = {
    x: endPoint.x - sourceRect.x + 5,
    y: endPoint.y - sourceRect.y + 5,
  }
  await mouseMoveToPoint(sourceGridCell, endPoint, {
    eventOptions: {
      movementX: delta.x,
      movementY: delta.y,
      buttons: 1,
    },
    modifiers: cmdModifier,
  })
  if (midDragCallback != null) {
    await midDragCallback()
  }
  await mouseUpAtPoint(renderResult.renderedDOM.getByTestId(CanvasControlsContainerID), endPoint, {
    modifiers: cmdModifier,
  })
}

async function dragOutToAnotherGrid(
  renderResult: EditorRenderResult,
  anotherGridTestId: string,
  offsetInAnotherGrid: Point,
  cell: ElementPath,
) {
  const sourceGridCell = renderResult.renderedDOM.getByTestId(GridCellTestId(cell))
  const sourceRect = sourceGridCell.getBoundingClientRect()

  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
  const anotherGrid = renderResult.renderedDOM.getByTestId(anotherGridTestId)
  const anotherGridRect = anotherGrid.getBoundingClientRect()

  // selecting the cell
  await mouseClickAtPoint(
    sourceGridCell,
    { x: sourceRect.x + 5, y: sourceRect.y + 5 },
    { modifiers: cmdModifier },
  )

  // starting the drag
  await mouseDownAtPoint(
    sourceGridCell,
    { x: sourceRect.x + 5, y: sourceRect.y + 5 },
    { modifiers: cmdModifier },
  )

  // first move over target grid hovers the grid controls, so the dom sampler can run
  await mouseMoveToPoint(
    canvasControlsLayer,
    { x: anotherGridRect.x + offsetInAnotherGrid.x, y: anotherGridRect.y + offsetInAnotherGrid.y },
    { modifiers: cmdModifier },
  )

  // second move runs the strategy for the first time with cell metadata
  await mouseMoveToPoint(
    canvasControlsLayer,
    { x: anotherGridRect.x + offsetInAnotherGrid.x, y: anotherGridRect.y + offsetInAnotherGrid.y },
    { modifiers: cmdModifier },
  )

  await mouseUpAtPoint(
    canvasControlsLayer,
    { x: anotherGridRect.x + offsetInAnotherGrid.x, y: anotherGridRect.y + offsetInAnotherGrid.y },
    { modifiers: cmdModifier },
  )
}
