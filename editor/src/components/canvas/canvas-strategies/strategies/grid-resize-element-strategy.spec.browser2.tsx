import type {
  ElementPath,
  JSExpressionNestedObject,
  JSExpressionValue,
  JSXAttributesEntry,
  JSXElement,
} from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  getRectCenter,
  localPoint,
  type LocalPoint,
  localRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { mouseDownAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import type { GridResizeEdge } from '../interaction-state'
import { gridCellTargetId } from './grid-cell-bounds'
import { ResizePointTestId } from '../../controls/select-mode/absolute-resize-control'
import { gridEdgeToEdgePosition } from '../../controls/grid-controls-for-strategies'

async function runCellResizeTest(
  editor: EditorRenderResult,
  edge: GridResizeEdge,
  dragToCellTestId: string,
  elementPathToDrag: ElementPath = EP.fromString('sb/scene/grid/ddd'),
) {
  await selectComponentsForTest(editor, [elementPathToDrag])

  const resizeControl = editor.renderedDOM.getByTestId(
    ResizePointTestId(gridEdgeToEdgePosition(edge)),
  )

  const resizeControlBox = resizeControl.getBoundingClientRect()
  await mouseDownAtPoint(resizeControl, { x: resizeControlBox.x + 5, y: resizeControlBox.y + 5 })
  const targetGridCell = editor.renderedDOM.getByTestId(dragToCellTestId)

  await mouseDragFromPointToPoint(
    resizeControl,
    {
      x: resizeControl.getBoundingClientRect().x + 2,
      y: resizeControl.getBoundingClientRect().y + 2,
    },
    getRectCenter(
      localRectangle({
        x: targetGridCell.getBoundingClientRect().x,
        y: targetGridCell.getBoundingClientRect().y,
        width: targetGridCell.getBoundingClientRect().width,
        height: targetGridCell.getBoundingClientRect().height,
      }),
    ),
    {
      moveBeforeMouseDown: true,
    },
  )
}

async function runCellResizeTestWithDragVector(
  editor: EditorRenderResult,
  edge: GridResizeEdge,
  dragVector: LocalPoint,
  elementPathToDrag: ElementPath = EP.fromString('sb/scene/grid/ddd'),
) {
  await selectComponentsForTest(editor, [elementPathToDrag])

  const resizeControl = editor.renderedDOM.getByTestId(
    ResizePointTestId(gridEdgeToEdgePosition(edge)),
  )

  const resizeControlCenter = getRectCenter(
    localRectangle({
      x: resizeControl.getBoundingClientRect().x,
      y: resizeControl.getBoundingClientRect().y,
      width: resizeControl.getBoundingClientRect().width,
      height: resizeControl.getBoundingClientRect().height,
    }),
  )
  await mouseDragFromPointToPoint(
    resizeControl,
    resizeControlCenter,
    offsetPoint(resizeControlCenter, dragVector),
    {
      moveBeforeMouseDown: true,
    },
  )
}

describe('grid resize element strategy', () => {
  describe('column-end', () => {
    it('can enlarge element', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await runCellResizeTest(
        editor,
        'column-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 10),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '11',
        gridColumnStart: '7',
        gridRowStart: '2',
        gridRowEnd: 'auto',
      })
    })

    it('can shrink element', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await runCellResizeTest(
        editor,
        'column-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '9',
        gridColumnStart: '7',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })

    it('can enlarge element in grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'column-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 10),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '11',
        gridColumnStart: '7',
        gridRowStart: '2',
        gridRowEnd: 'auto',
      })
    })

    it('can shrink element in grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'column-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '9',
        gridColumnStart: '7',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })
  })

  describe('column-start', () => {
    it('can enlarge element', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await runCellResizeTest(
        editor,
        'column-start',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 6),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '10',
        gridColumnStart: '6',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })

    it('can shrink element', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await runCellResizeTest(
        editor,
        'column-start',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '10',
        gridColumnStart: '8',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })

    it('can enlarge element in grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'column-start',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 6),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '10',
        gridColumnStart: '6',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })

    it('can shrink element in grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'column-start',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '10',
        gridColumnStart: '8',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })
  })

  describe('row-end', () => {
    it('can resize element', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await runCellResizeTest(
        editor,
        'row-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 3, 6),
      )
      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: '4',
          gridRowStart: '2',
        })
      }

      await runCellResizeTest(
        editor,
        'row-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
      )
      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })

    it('can resize element in grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'row-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 3, 6),
      )
      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: '4',
          gridRowStart: '2',
        })
      }

      await runCellResizeTest(
        editor,
        'row-end',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
      )
      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })
  })

  describe('row-start', () => {
    it('can resize element', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await runCellResizeTest(
        editor,
        'row-start',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 1, 6),
      )

      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: '3',
          gridRowStart: '1',
        })
      }

      {
        await runCellResizeTest(
          editor,
          'row-start',
          gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })

    it('can resize element in grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'row-start',
        gridCellTargetId(EP.fromString('sb/scene/grid'), 1, 6),
      )

      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: '3',
          gridRowStart: '1',
        })
      }

      {
        await runCellResizeTest(
          editor,
          'row-start',
          gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 8),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-child').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '10',
          gridColumnStart: '7',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })
  })

  it('can resize element with mouse move outside of grid cells', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')
    await runCellResizeTest(
      editor,
      'column-end',
      gridCellTargetId(EP.fromString('sb/scene/grid'), 1, 8),
    )

    {
      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '9',
        gridColumnStart: '7',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    }

    {
      // moving a 2 cell wide element in the middle, over the gap between 2 cells
      await runCellResizeTestWithDragVector(
        editor,
        'row-start',
        localPoint({
          x: 0,
          y: -50,
        }),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '9',
        gridColumnStart: '7',
        gridRowEnd: '3',
        gridRowStart: '1',
      })
    }
  })

  it('can resize element with mouse move outside of grid cells in grid component', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeGridComponent,
      'await-first-dom-report',
    )
    await runCellResizeTest(
      editor,
      'column-end',
      gridCellTargetId(EP.fromString('sb/scene/grid'), 1, 8),
    )

    {
      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '9',
        gridColumnStart: '7',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    }

    {
      // moving a 2 cell wide element in the middle, over the gap between 2 cells
      await runCellResizeTestWithDragVector(
        editor,
        'row-start',
        localPoint({
          x: 0,
          y: -50,
        }),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId('grid-child').style
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '9',
        gridColumnStart: '7',
        gridRowEnd: '3',
        gridRowStart: '1',
      })
    }
  })

  it('removes the grid-area prop on resize', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeWithGridArea, 'await-first-dom-report')

    await runCellResizeTest(
      editor,
      'column-end',
      gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 10),
    )

    const styleProp = (
      unsafeCast<JSXElement>(
        MetadataUtils.getJsxElementChildFromMetadata(
          editor.getEditorState().editor.jsxMetadata,
          EP.fromString('sb/scene/grid/ddd'),
        ),
      ).props.find(
        (p): p is JSXAttributesEntry => p.type === 'JSX_ATTRIBUTES_ENTRY' && p.key === 'style',
      )?.value as JSExpressionNestedObject
    ).content.flatMap((c) =>
      c.type === 'PROPERTY_ASSIGNMENT'
        ? [[c.key, unsafeCast<JSExpressionValue<any>>(c.value).value]]
        : [],
    )

    expect(styleProp).toEqual([
      ['minHeight', 0],
      ['backgroundColor', '#db48f6'],
      ['width', '100%'],
      ['height', '100%'],
      ['gridColumn', '7 / 11'],
      ['gridRow', 2],
    ])
  })

  describe('grids inside grids', () => {
    it("can move a grid element that's also a grid", async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: -94,
        top: 698,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 'max-content',
        gridTemplateColumns: '2.4fr 1fr 1fr',
        gridTemplateRows: '99px 109px 90px',
      }}
    >
      <div
        data-uid='grid-inside-grid'
        data-testid='grid-inside-grid'
        style={{
          backgroundColor: 'green',
          contain: 'layout',
          display: 'grid',
          gap: 5,
          gridTemplateColumns: '1fr 1fr 1fr',
          gridTemplateRows: '1fr',
          gridColumn: 1,
          gridRow: 2,
		  width: '100%',
		  height: '100%',
        }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 66,
            height: 67,
            contain: 'layout',
          }}
          data-uid='ad3e425d-a224-44a9-a303-465f80e0'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 66,
            height: 67,
            contain: 'layout',
          }}
          data-uid='d805fd1f-fdcd-422f-a785-f023409c'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 66,
            height: 67,
            contain: 'layout',
          }}
          data-uid='b2124463-bd84-414f-960b-29689a92'
        />
      </div>
    </div>
  </Storyboard>
)
`,
        'await-first-dom-report',
      )

      await runCellResizeTest(
        editor,
        'column-end',
        gridCellTargetId(EP.fromString('sb/grid'), 2, 3),
        EP.fromString('sb/grid/grid-inside-grid'),
      )

      {
        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('grid-inside-grid').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '4',
          gridColumnStart: '1',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })
  })

  it('also works for stretching cells', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await runCellResizeTest(
      editor,
      'column-end',
      gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 10),
      EP.fromString('sb/scene/grid/eee'),
    )

    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
      editor.renderedDOM.getByTestId('grid-child-stretch').style
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '11',
      gridColumnStart: '4',
      gridRowStart: '4',
      gridRowEnd: 'auto',
    })
  })

  it('also works for stretching cells in grid component', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeGridComponent,
      'await-first-dom-report',
    )

    await runCellResizeTest(
      editor,
      'column-end',
      gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 10),
      EP.fromString('sb/scene/grid/eee'),
    )

    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
      editor.renderedDOM.getByTestId('grid-child-stretch').style
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '11',
      gridColumnStart: '4',
      gridRowStart: '4',
      gridRowEnd: 'auto',
    })
  })

  describe('spans', () => {
    it('respects column start spans', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithCustomPlacement({ gridColumn: 'span 2', gridRow: '2' }),
        'await-first-dom-report',
      )

      // enlarge to the right
      {
        await runCellResizeTest(
          editor,
          'column-end',
          gridCellTargetId(EP.fromString('sb/grid'), 2, 3),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 3',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }

      // shrink from the left
      {
        await runCellResizeTest(
          editor,
          'column-start',
          gridCellTargetId(EP.fromString('sb/grid'), 2, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: '4',
          gridColumnStart: 'span 2',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }

      // enlarge back from the left
      {
        await runCellResizeTest(
          editor,
          'column-start',
          gridCellTargetId(EP.fromString('sb/grid'), 2, 1),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 3',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })
    it('respects column end spans', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithCustomPlacement({ gridColumn: '2 / span 2', gridRow: '2' }),
        'await-first-dom-report',
      )

      // enlarge to the right
      {
        await runCellResizeTest(
          editor,
          'column-end',
          gridCellTargetId(EP.fromString('sb/grid'), 2, 4),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'span 3',
          gridColumnStart: '2',
          gridRowEnd: 'auto',
          gridRowStart: '2',
        })
      }
    })
    it('respects row start spans', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithCustomPlacement({ gridColumn: '2', gridRow: 'span 2' }),
        'await-first-dom-report',
      )

      // enlarge to the bottom
      {
        await runCellResizeTest(
          editor,
          'row-end',
          gridCellTargetId(EP.fromString('sb/grid'), 3, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: '2',
          gridRowEnd: 'auto',
          gridRowStart: 'span 3',
        })
      }

      // shrink from the top
      {
        await runCellResizeTest(
          editor,
          'row-start',
          gridCellTargetId(EP.fromString('sb/grid'), 2, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: '2',
          gridRowEnd: '4',
          gridRowStart: 'span 2',
        })
      }

      // enlarge back from the top
      {
        await runCellResizeTest(
          editor,
          'row-start',
          gridCellTargetId(EP.fromString('sb/grid'), 1, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: '2',
          gridRowEnd: 'auto',
          gridRowStart: 'span 3',
        })
      }
    })
    it('respects row end spans', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithCustomPlacement({ gridColumn: '2', gridRow: '2 / span 2' }),
        'await-first-dom-report',
      )

      // enlarge to the bottom
      {
        await runCellResizeTest(
          editor,
          'row-end',
          gridCellTargetId(EP.fromString('sb/grid'), 4, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: '2',
          gridRowEnd: 'span 3',
          gridRowStart: '2',
        })
      }
    })
    it('uses spans for flow elements', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithCustomPlacement({ gridColumn: 'auto', gridRow: 'auto' }),
        'await-first-dom-report',
      )

      // enlarge to the right, spans in flow
      {
        await runCellResizeTest(
          editor,
          'column-end',
          gridCellTargetId(EP.fromString('sb/grid'), 1, 3),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 3',
          gridRowEnd: 'auto',
          gridRowStart: 'auto',
        })
      }

      // enlarge to the bottom, still spans in flow
      {
        await runCellResizeTest(
          editor,
          'row-end',
          gridCellTargetId(EP.fromString('sb/grid'), 4, 3),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 3',
          gridRowEnd: 'auto',
          gridRowStart: 'span 4',
        })
      }

      // shrink from the right, still spans in flow
      {
        await runCellResizeTest(
          editor,
          'column-end',
          gridCellTargetId(EP.fromString('sb/grid'), 4, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 2',
          gridRowEnd: 'auto',
          gridRowStart: 'span 4',
        })
      }

      // shrink from the bottom, still spans in flow
      {
        await runCellResizeTest(
          editor,
          'row-end',
          gridCellTargetId(EP.fromString('sb/grid'), 3, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 2',
          gridRowEnd: 'auto',
          gridRowStart: 'span 3',
        })
      }

      // shrink from the top, it spans but now is pinned
      {
        await runCellResizeTest(
          editor,
          'row-start',
          gridCellTargetId(EP.fromString('sb/grid'), 2, 2),
          EP.fromString('sb/grid/cell'),
        )

        const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
          editor.renderedDOM.getByTestId('cell').style
        expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
          gridColumnEnd: 'auto',
          gridColumnStart: 'span 2',
          gridRowEnd: '4',
          gridRowStart: 'span 2',
        })
      }
    })
  })
})

const ProjectCode = `import * as React from 'react'
import { Scene, Storyboard, Placeholder } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 847,
        height: 895,
        position: 'absolute',
        left: 46,
        top: 131,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          display: 'grid',
          gridTemplateRows: '75px 75px 75px 75px',
          gridTemplateColumns:
            '50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px',
          gridGap: 16,
          height: 482,
          width: 786,
          position: 'absolute',
          left: 31,
          top: 0,
        }}
        data-uid='grid'
      >
        <div
          style={{
            minHeight: 0,
            backgroundColor: '#f3785f',
            gridColumnEnd: 5,
            gridColumnStart: 1,
            gridRowEnd: 3,
            gridRowStart: 1,
          }}
          data-uid='aaa'
        />
        <Placeholder
          style={{
            minHeight: 0,
            backgroundColor: '#23565b',
            gridColumnStart: 11,
            gridColumnEnd: 13,
            gridRowStart: 1,
            gridRowEnd: 2,
          }}
          data-uid='bbb'
        />
        <Placeholder
          style={{
            minHeight: 0,
            gridColumnEnd: 5,
            gridRowEnd: 4,
            gridColumnStart: 1,
            gridRowStart: 3,
            backgroundColor: '#0074ff',
			width: 25,
			height: 30,
          }}
          data-uid='grid-child-not-filling'
		  data-testid='grid-child-not-filling'
        />
        <div
          style={{
            minHeight: 0,
            gridColumnEnd: 10,
            gridRowEnd: 3,
            gridColumnStart: 7,
            gridRowStart: 2,
            backgroundColor: '#db48f6',
			width: '100%',
			height: '100%',
          }}
          data-uid='ddd'
          data-testid='grid-child'
        />
        <div
          style={{
            backgroundColor: '#9f0',
            alignSelf: 'stretch',
            justifySelf: 'stretch',
            gridColumn: 4,
            gridRow: 4,
          }}
          data-uid='eee'
          data-testid='grid-child-stretch'
        />
      </div>
    </Scene>
  </Storyboard>
)
`

const ProjectCodeGridComponent = `import * as React from 'react'
import { Scene, Storyboard, Placeholder } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 847,
        height: 895,
        position: 'absolute',
        left: 46,
        top: 131,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Grid
        style={{
          height: 482,
          width: 786,
          position: 'absolute',
          left: 31,
          top: 0,
        }}
        data-uid='grid'
      >
        <div
          style={{
            minHeight: 0,
            backgroundColor: '#f3785f',
            gridColumnEnd: 5,
            gridColumnStart: 1,
            gridRowEnd: 3,
            gridRowStart: 1,
          }}
          data-uid='aaa'
        />
        <Placeholder
          style={{
            minHeight: 0,
            backgroundColor: '#23565b',
            gridColumnStart: 11,
            gridColumnEnd: 13,
            gridRowStart: 1,
            gridRowEnd: 2,
          }}
          data-uid='bbb'
        />
        <Placeholder
          style={{
            minHeight: 0,
            gridColumnEnd: 5,
            gridRowEnd: 4,
            gridColumnStart: 1,
            gridRowStart: 3,
            backgroundColor: '#0074ff',
            width: 25,
            height: 30,
          }}
          data-uid='grid-child-not-filling'
          data-testid='grid-child-not-filling'
        />
        <div
          style={{
            minHeight: 0,
            gridColumnEnd: 10,
            gridRowEnd: 3,
            gridColumnStart: 7,
            gridRowStart: 2,
            backgroundColor: '#db48f6',
            width: '100%',
            height: '100%',
          }}
          data-uid='ddd'
          data-testid='grid-child'
        />
        <div
          style={{
            backgroundColor: '#9f0',
            alignSelf: 'stretch',
            justifySelf: 'stretch',
            gridColumn: 4,
            gridRow: 4,
          }}
          data-uid='eee'
          data-testid='grid-child-stretch'
        />
      </Grid>
    </Scene>
  </Storyboard>
)

export function Grid(props) {
  return (
    <div
      style={{
        ...props.style,
        display: 'flex',
        flexDirection: 'column',
      }}
    >
      <div
        style={{
          display: 'grid',
          gridTemplateRows: '75px 75px 75px 75px',
          gridTemplateColumns:
            '50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px',
          gridGap: 16,
        }}
      >
        {props.children}
      </div>
      <div
        style={{ height: 100 }}
      />
    </div>
  )
}
`

const ProjectCodeWithGridArea = `import * as React from 'react'
import { Scene, Storyboard, Placeholder } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 847,
        height: 895,
        position: 'absolute',
        left: 46,
        top: 131,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          display: 'grid',
          gridTemplateRows: '75px 75px 75px 75px',
          gridTemplateColumns:
            '50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px',
          gridGap: 16,
          height: 482,
          width: 786,
          position: 'absolute',
          left: 31,
          top: 0,
        }}
        data-uid='grid'
      >
         <div
          style={{
            minHeight: 0,
            gridArea: '2 / 7 / 3 / 10',
            backgroundColor: '#db48f6',
			width: '100%',
			height: '100%',
          }}
          data-uid='ddd'
          data-testid='grid-child'
        />
      </div>
    </Scene>
  </Storyboard>
)
`

function unsafeCast<T>(a: unknown): T {
  return a as T
}

function makeProjectCodeWithCustomPlacement(params: {
  gridColumn: string
  gridRow: string
}): string {
  return `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        width: 600,
        height: 400,
        display: 'grid',
        gap: 10,
        gridTemplateColumns: '1fr 1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr 1fr 1fr',
      }}
      data-uid='grid'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          alignSelf: 'stretch',
          justifySelf: 'stretch',
          gridColumn: '${params.gridColumn}',
          gridRow: '${params.gridRow}',
        }}
        data-uid='cell'
		data-testid='cell'
      />
    </div>
  </Storyboard>
)
`
}
