import * as EP from '../../../../core/shared/element-path'
import {
  getRectCenter,
  localRectangle,
  offsetPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'
import { GridCellTestId } from '../../controls/grid-controls'
import { mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { gridCellTargetId } from './grid-cell-bounds'

describe('grid rearrange move strategy', () => {
  it('can rearrange elements on a grid', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest(editor, {
      scale: 1,
      pathString: `sb/scene/grid/${testId}`,
      testId: testId,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  it('can rearrange element with no explicit grid props set', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'bbb'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest(editor, {
      scale: 1,
      pathString: `sb/scene/grid/${testId}`,
      testId: testId,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: '3',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })
  })

  it('can rearrange elements on a grid (zoom out)', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest(editor, {
      scale: 0.5,
      pathString: `sb/scene/grid/${testId}`,
      testId: testId,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  it('can rearrange elements on a grid (zoom in)', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest(editor, {
      scale: 2,
      pathString: `sb/scene/grid/${testId}`,
      testId: testId,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  describe('grids within grids', () => {
    it('can move a grid child that is a grid itself', async () => {
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

      const testId = 'grid-inside-grid'
      const elementPathToDrag = EP.fromString(`sb/grid/${testId}`)

      await selectComponentsForTest(editor, [elementPathToDrag])

      const sourceGridCell = editor.renderedDOM.getByTestId(GridCellTestId(elementPathToDrag))
      const targetGridCell = editor.renderedDOM.getByTestId(
        gridCellTargetId(EP.fromString('sb/grid'), 2, 3),
      )

      const sourceRect = sourceGridCell.getBoundingClientRect()
      const targetRect = targetGridCell.getBoundingClientRect()

      await mouseDragFromPointToPoint(
        sourceGridCell,
        {
          x: sourceRect.x + 10,
          y: sourceRect.y + 10,
        },
        getRectCenter(
          localRectangle({
            x: targetRect.x,
            y: targetRect.y,
            width: targetRect.width,
            height: targetRect.height,
          }),
        ),
      )

      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } =
        editor.renderedDOM.getByTestId(testId).style

      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: 'auto',
        gridColumnStart: '3',
        gridRowEnd: 'auto',
        gridRowStart: '2',
      })
    })
  })

  describe('absolute move within grid', () => {
    const ProjectCode = `import { Scene, Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        data-uid='grid'
        style={{
          backgroundColor: '#fefefe',
          position: 'absolute',
          left: 123,
          top: 133,
          width: 461,
          height: 448,
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gridTemplateRows: '1fr 1fr',
          border: '5px solid #000',
          gridGap: 10,
        }}
      >
        <div
          data-uid='child'
          data-testid='child'
          style={{
            backgroundColor: '#59a6ed',
            position: 'absolute',
            left: 12,
            top: 16,
            width: 144,
            height: 134,
            gridColumn: 1,
            gridRow: 1,
          }}
        />
      </div>
    </Scene>
  </Storyboard>
)
`
    it('can move absolute element inside a grid cell', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      const child = editor.renderedDOM.getByTestId('child')

      {
        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '1',
          gridRow: '1',
          left: '12px',
          top: '16px',
        })
      }

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid/child')])

      const childBounds = child.getBoundingClientRect()
      const childCenter = windowPoint({
        x: Math.floor(childBounds.left + childBounds.width / 2),
        y: Math.floor(childBounds.top + childBounds.height / 2),
      })

      await mouseDragFromPointToPoint(
        editor.renderedDOM.getByTestId(GridCellTestId(EP.fromString('sb/scene/grid/child'))),
        childCenter,
        offsetPoint(childCenter, windowPoint({ x: 20, y: 20 })),
      )

      {
        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '1',
          gridRow: '1',
          left: '36px',
          top: '40px',
        })
      }
    })

    it('can move absolute element inside a grid cell, zoomed in', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      await editor.dispatch([CanvasActions.zoom(2)], true)

      const child = editor.renderedDOM.getByTestId('child')

      {
        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '1',
          gridRow: '1',
          left: '12px',
          top: '16px',
        })
      }

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid/child')])

      const childBounds = child.getBoundingClientRect()
      const childCenter = windowPoint({
        x: Math.floor(childBounds.left + childBounds.width / 2),
        y: Math.floor(childBounds.top + childBounds.height / 2),
      })

      await mouseDragFromPointToPoint(
        editor.renderedDOM.getByTestId(GridCellTestId(EP.fromString('sb/scene/grid/child'))),
        childCenter,
        offsetPoint(childCenter, windowPoint({ x: 240, y: 240 })),
      )

      {
        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '1',
          gridRow: '1',
          left: '136.5px',
          top: '140.5px',
        })
      }
    })

    it('can move absolute element among grid cells', async () => {
      const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

      const child = editor.renderedDOM.getByTestId('child')

      {
        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '1',
          gridRow: '1',
          left: '12px',
          top: '16px',
        })
      }

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid/child')])

      const childBounds = child.getBoundingClientRect()
      const childCenter = windowPoint({
        x: Math.floor(childBounds.left + childBounds.width / 2),
        y: Math.floor(childBounds.top + childBounds.height / 2),
      })

      await mouseDragFromPointToPoint(
        editor.renderedDOM.getByTestId(GridCellTestId(EP.fromString('sb/scene/grid/child'))),
        childCenter,
        offsetPoint(childCenter, windowPoint({ x: 240, y: 240 })),
      )

      {
        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '2',
          gridRow: '2',
          left: '25.5px',
          top: '36px',
        })
      }
    })
  })
})

async function runMoveTest(
  editor: EditorRenderResult,
  props: { scale: number; pathString: string; testId: string },
) {
  const elementPathToDrag = EP.fromString(props.pathString)

  await selectComponentsForTest(editor, [elementPathToDrag])

  await editor.dispatch([CanvasActions.zoom(props.scale)], true)

  const sourceGridCell = editor.renderedDOM.getByTestId(GridCellTestId(elementPathToDrag))
  const targetGridCell = editor.renderedDOM.getByTestId(
    gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 3),
  )

  const sourceRect = sourceGridCell.getBoundingClientRect()
  const targetRect = targetGridCell.getBoundingClientRect()

  await mouseDragFromPointToPoint(
    sourceGridCell,
    {
      x: sourceRect.x + 10,
      y: sourceRect.y + 10,
    },
    getRectCenter(
      localRectangle({
        x: targetRect.x,
        y: targetRect.y,
        width: targetRect.width,
        height: targetRect.height,
      }),
    ),
  )

  return editor.renderedDOM.getByTestId(props.testId).style
}

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
          data-testid='aaa'
        />
        <div
          style={{
            minHeight: 0,
            backgroundColor: '#23565b',
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
        <Placeholder
          style={{
            minHeight: 0,
            gridColumnEnd: 5,
            gridRowEnd: 4,
            gridColumnStart: 1,
            gridRowStart: 3,
            backgroundColor: '#0074ff',
          }}
          data-uid='ccc'
        />
        <Placeholder
          style={{
            minHeight: 0,
            gridColumnEnd: 9,
            gridRowEnd: 4,
            gridColumnStart: 5,
            gridRowStart: 3,
          }}
          data-uid='ddd'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
