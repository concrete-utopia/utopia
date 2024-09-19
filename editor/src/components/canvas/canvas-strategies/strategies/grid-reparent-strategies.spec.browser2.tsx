import type { ElementPath } from 'utopia-shared/src/types'
import * as EP from '../../../../core/shared/element-path'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { offsetPoint, windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { GridCellTestId } from '../../controls/grid-controls'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { Point } from '../../event-helpers.test-utils'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseUpAtPoint,
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

      await dragOut(editor, 'grid', EP.fromString('sb/grid/dragme'), { x: 2000, y: 1000 })

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
            top: 391,
            left: 492,
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

      await dragOut(editor, 'grid', EP.fromString('sb/grid/dragme'), { x: 2600, y: 1600 })

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
    it('into a flow element', async () => {
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

      await dragOut(editor, 'grid', EP.fromString('sb/grid/dragme'), { x: 2200, y: 1800 })

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

      await dragOut(editor, 'grid', EP.fromString('sb/grid/dragme'), { x: 2200, y: 2200 })

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

      await dragOut(editor, 'grid', EP.fromString('sb/grid/dragme'), { x: 2000, y: 1000 })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCode({
            extraCode: `
        <div
          style={{
            backgroundColor: '#f0f',
            position: 'absolute',
            top: 391,
            left: 492,
            width: 86.5,
            height: 135,
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
  gridTestId: string,
  cell: ElementPath,
  endPoint: Point,
) {
  const grid = renderResult.renderedDOM.getByTestId(gridTestId)
  const gridRect = grid.getBoundingClientRect()

  const sourceGridCell = renderResult.renderedDOM.getByTestId(GridCellTestId(cell))
  const sourceRect = sourceGridCell.getBoundingClientRect()

  await mouseClickAtPoint(
    sourceGridCell,
    { x: sourceRect.x + 5, y: sourceRect.y + 5 },
    { modifiers: cmdModifier },
  )
  await mouseDragFromPointToPoint(sourceGridCell, sourceRect, endPoint, {
    modifiers: cmdModifier,
  })
  await mouseUpAtPoint(grid, gridRect, { modifiers: cmdModifier })
}
