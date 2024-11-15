import * as EP from '../../../../core/shared/element-path'
import {
  getRectCenter,
  localRectangle,
  offsetPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'
import { GridCellTestId } from '../../controls/grid-controls-for-strategies'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../../event-helpers.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { gridCellTargetId } from './grid-cell-bounds'
import { runGridMoveTest } from './grid.test-utils'

describe('grid element change location strategy', () => {
  it('can change the location of elements on a grid', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  describe('component items', () => {
    it('can change the location of components on a grid when component takes style prop', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithItemComponent(`export function Item(props) {
  return (
    <div
      style={{...props.style}}
      data-testid={props['data-testid']}
    />
  )
}`),
        'await-first-dom-report',
      )
      const testId = 'aaa'
      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
        editor,
        {
          scale: 1,
          pathString: `sb/scene/grid/${testId}`,
          testId: testId,
        },
      )
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '7',
        gridColumnStart: '3',
        gridRowEnd: '4',
        gridRowStart: '2',
      })
    })

    it('can not change the location of components on a grid when component doesnt take style prop', async () => {
      const editor = await renderTestEditorWithCode(
        makeProjectCodeWithItemComponent(`export function Item(props) {
  return (
    <div data-testid={props['data-testid']} />
  )
}`),
        'await-first-dom-report',
      )

      const testId = 'aaa'
      await runGridMoveTest(
        editor,
        {
          scale: 1,
          pathString: `sb/scene/grid/${testId}`,
          testId: testId,
        },
        (ed) => {
          const strategies = ed.getEditorState().strategyState.sortedApplicableStrategies
          const changeLocationStrategy = strategies?.find(
            (s) => s.strategy.id === 'grid-change-element-location-strategy',
          )
          expect(changeLocationStrategy).toBeUndefined()
          const reorderStrategy = strategies?.find(
            (s) => s.strategy.id === 'reorder-grid-move-strategy',
          )
          expect(reorderStrategy).not.toBeUndefined()
        },
      )
    })
  })

  describe('grid component', () => {
    it('can change the location of elements in a grid component', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeGridComponent,
        'await-first-dom-report',
      )

      const testId = 'aaa'
      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
        editor,
        {
          scale: 1,
          pathString: `sb/scene/grid/${testId}`,
          testId: testId,
        },
      )
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '7',
        gridColumnStart: '3',
        gridRowEnd: '4',
        gridRowStart: '2',
      })
    })

    it('can change the location of elements in a grid component with non-root grid inside', async () => {
      const editor = await renderTestEditorWithCode(
        ProjectCodeComponentNonRootGrid,
        'await-first-dom-report',
      )

      const testId = 'aaa'
      const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
        editor,
        {
          scale: 1,
          pathString: `sb/scene/grid/${testId}`,
          testId: testId,
        },
      )
      expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
        gridColumnEnd: '7',
        gridColumnStart: '3',
        gridRowEnd: '4',
        gridRowStart: '2',
      })
    })
  })

  it('can not change location of a multicell element out of the grid', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    // moving a multi-cell element to the left, but it is already on the left of the grid
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
        targetCell: { row: 2, column: 1 },
        draggedCell: { row: 2, column: 2 },
      },
    )
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '5',
      gridColumnStart: '1',
      gridRowEnd: '3',
      gridRowStart: '1',
    })
  })

  it('can change the location of element with no explicit grid props set', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'bbb'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
        tab: true,
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: '3',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })
  })

  it('can change the location of a multicell element (shorthand)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeReorderwithMultiCellChildShorthand,
      'await-first-dom-report',
    )

    const testId = 'orange'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
        targetCell: { row: 3, column: 1 },
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnStart: '1',
      gridColumnEnd: '3',
      gridRowStart: '3',
      gridRowEnd: 'auto',
    })
  })

  it('can change the location of elements on a grid (zoom out)', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 0.5,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  it('can change the location of elements on a grid (zoom in)', async () => {
    const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    const testId = 'aaa'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 2,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  it('can change the location of an element with span (from start, implicit end)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeWithSpanningItems,
      'await-first-dom-report',
    )

    const testId = 'pink'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '5',
      gridColumnStart: 'span 2',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })
  })

  it('can change the location of an element with span (from start, explicit end)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeWithSpanningItems,
      'await-first-dom-report',
    )

    const testId = 'cyan'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '5',
      gridColumnStart: 'span 2',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })
  })

  it('can change the location of an element with span (from end, explicit start)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeWithSpanningItems,
      'await-first-dom-report',
    )

    const testId = 'orange'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
        targetCell: { row: 3, column: 2 },
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: 'span 3',
      gridColumnStart: '2',
      gridRowEnd: 'auto',
      gridRowStart: '3',
    })
  })

  it('can change the location of an element with span (longhand start)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeWithSpanningItems,
      'await-first-dom-report',
    )

    const testId = 'blue'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '5',
      gridColumnStart: 'span 2',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })
  })

  it('can change the location of an element with span (longhand end)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeWithSpanningItems,
      'await-first-dom-report',
    )

    const testId = 'purple'
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
      editor,
      {
        scale: 1,
        pathString: `sb/scene/grid/${testId}`,
        testId: testId,
      },
    )

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: 'span 2',
      gridColumnStart: '3',
      gridRowEnd: 'auto',
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

      const testId = 'grid-inside-grid'
      const elementPathToDrag = EP.fromString(`sb/grid/${testId}`)

      await selectComponentsForTest(editor, [elementPathToDrag])

      const sourceGridCell = editor.renderedDOM.getByTestId(GridCellTestId(elementPathToDrag))
      const targetGridCell = editor.renderedDOM.getByTestId(
        gridCellTargetId(EP.fromString('sb/grid'), 2, 3),
      )

      const sourceRect = sourceGridCell.getBoundingClientRect()
      const targetRect = targetGridCell.getBoundingClientRect()

      const dragFrom = {
        x: sourceRect.x + 10,
        y: sourceRect.y + 10,
      }
      const dragTo = getRectCenter(
        localRectangle({
          x: targetRect.x,
          y: targetRect.y,
          width: targetRect.width,
          height: targetRect.height,
        }),
      )
      await mouseDownAtPoint(sourceGridCell, dragFrom)
      await mouseMoveToPoint(sourceGridCell, dragTo)
      await mouseUpAtPoint(sourceGridCell, dragTo)

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
    describe('when the cell has positioning', () => {
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
          gridGap: 0,
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

        const endPoint = offsetPoint(childCenter, windowPoint({ x: 20, y: 20 }))

        const dragTarget = editor.renderedDOM.getByTestId(
          GridCellTestId(EP.fromString('sb/scene/grid/child')),
        )
        await mouseDownAtPoint(dragTarget, childCenter)
        await mouseMoveToPoint(dragTarget, endPoint)
        await mouseUpAtPoint(dragTarget, endPoint)

        const { top, left, gridColumn, gridRow } = child.style
        expect({ top, left, gridColumn, gridRow }).toEqual({
          gridColumn: '1',
          gridRow: '1',
          left: '32px',
          top: '36px',
        })
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

        const dragTarget = editor.renderedDOM.getByTestId(
          GridCellTestId(EP.fromString('sb/scene/grid/child')),
        )
        const endPoint = offsetPoint(childCenter, windowPoint({ x: 240, y: 240 }))
        await mouseDownAtPoint(dragTarget, childCenter)
        await mouseMoveToPoint(dragTarget, endPoint)
        await mouseUpAtPoint(dragTarget, endPoint)

        {
          const { top, left, gridColumn, gridRow } = child.style
          expect({ top, left, gridColumn, gridRow }).toEqual({
            gridColumn: '1',
            gridRow: '1',
            left: '132px',
            top: '136px',
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

        const endPoint = offsetPoint(childCenter, windowPoint({ x: 240, y: 240 }))
        const dragTarget = editor.renderedDOM.getByTestId(
          GridCellTestId(EP.fromString('sb/scene/grid/child')),
        )

        await mouseDownAtPoint(dragTarget, childCenter)
        await mouseMoveToPoint(dragTarget, endPoint)
        await mouseUpAtPoint(dragTarget, endPoint)

        {
          const { top, left, gridColumn, gridRow } = child.style
          expect({ top, left, gridColumn, gridRow }).toEqual({
            gridColumn: '2',
            gridRow: '2',
            left: '21.5px',
            top: '32px',
          })
        }
      })

      it("can move element spanning multiple cell by a cell that isn't the root cell", async () => {
        const editor = await renderTestEditorWithCode(
          `import { Scene, Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
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
          gridTemplateColumns: '1fr 1fr 1fr 1fr',
          gridTemplateRows: '1fr 1fr 1fr 1fr',
          gridGap: 10,
        }}
      >
        <div
          data-uid='child'
          data-testid='child'
          style={{
            position: 'absolute',
            wordBreak: 'break-word',
            width: 127,
            height: 122,
            backgroundColor: '#ff2800',
            gridColumn: 2,
            gridRow: 2,
            top: 45,
            left: 41.25,
          }}
        />
      </div>
    </Scene>
  </Storyboard>
)
`,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/grid/child')])

        const child = editor.renderedDOM.getByTestId('child')
        const childBounds = child.getBoundingClientRect()
        const startPoint = windowPoint({
          x: Math.floor(childBounds.left + childBounds.width - 3),
          y: Math.floor(childBounds.top + childBounds.height - 3),
        })

        const endPoint = offsetPoint(startPoint, windowPoint({ x: -100, y: -100 }))
        const dragTarget = editor.renderedDOM.getByTestId(
          GridCellTestId(EP.fromString('sb/scene/grid/child')),
        )

        await mouseDownAtPoint(dragTarget, startPoint)
        await mouseMoveToPoint(dragTarget, endPoint)
        await mouseUpAtPoint(dragTarget, endPoint)

        {
          const { top, left, gridColumnStart, gridColumnEnd, gridRowStart, gridRowEnd } =
            child.style
          expect({ top, left, gridColumnStart, gridColumnEnd, gridRowStart, gridRowEnd }).toEqual({
            gridColumnStart: '1',
            gridColumnEnd: 'auto',
            gridRowStart: '1',
            gridRowEnd: 'auto',
            left: '59px',
            top: '59.5px',
          })
        }
      })
    })
    describe('when the cell has no positioning', () => {
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
          }}
        />
      </div>
    </Scene>
  </Storyboard>
)
`
      it('performs an absolute move relative to the grid', async () => {
        const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

        const child = editor.renderedDOM.getByTestId('child')

        {
          const { top, left, gridColumn, gridRow } = child.style
          expect({ top, left, gridColumn, gridRow }).toEqual({
            gridColumn: '',
            gridRow: '',
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

        const dragTarget = editor.renderedDOM.getByTestId(
          GridCellTestId(EP.fromString('sb/scene/grid/child')),
        )
        const endPoint = offsetPoint(childCenter, windowPoint({ x: 280, y: 120 }))

        await mouseDownAtPoint(dragTarget, childCenter)
        await mouseMoveToPoint(dragTarget, endPoint)
        await mouseUpAtPoint(dragTarget, endPoint)

        {
          const { top, left, gridColumn, gridRow } = child.style
          expect({ top, left, gridColumn, gridRow }).toEqual({
            gridColumn: '',
            gridRow: '',
            left: '292px',
            top: '136px',
          })
        }
      })
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

const makeProjectCodeWithItemComponent = (
  itemComponentCode: string,
) => `import * as React from 'react'
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
        <Item
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
        <Item
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

${itemComponentCode}
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
      </Grid>
    </Scene>
  </Storyboard>
)

export function Grid(props) {
  return (
    <div
      style={{
        ...props.style,
        display: 'grid',
        gridTemplateRows: '75px 75px 75px 75px',
        gridTemplateColumns:
          '50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px 50px',
        gridGap: 16,
      }}
      data-uid='f84914f31dbc6c5d9b44c11ae54139ef'
    >
      {props.children}
    </div>
  )
}
`

const ProjectCodeComponentNonRootGrid = `import * as React from 'react'
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
          position: 'absolute',
          left: 31,
          top: 0,
        }}
        data-uid='grid'
      >
        <div
          style={{
            minHeight: 0,
            backgroundColor: '#23565b',
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
        <div
          style={{
            minHeight: 0,
            backgroundColor: '#f3785f',
            gridColumn: '1 / 5',
            gridRow: '1 / 3',
          }}
          data-uid='aaa'
          data-testid='aaa'
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

const ProjectCodeReorderwithMultiCellChildShorthand = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 600,
        height: 600,
        position: 'absolute',
        left: 0,
        top: 0,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 500,
          height: 500,
          display: 'grid',
          gridTemplateColumns: '1fr 1fr 1fr',
          gridTemplateRows: '1fr 1fr 1fr',
          gridGap: 10,
          padding: 10,
        }}
        data-testid='grid'
        data-uid='grid'
      >
        <div
          style={{
            backgroundColor: '#f09',
            width: '100%',
            height: '100%',
          }}
          data-uid='pink'
		  data-testid='pink'
          data-label='pink'
        />
        <div
          style={{
            backgroundColor: '#f90',
            width: '100%',
            height: '100%',
            gridColumn: '2 / 4',
          }}
          data-uid='orange'
		  data-testid='orange'
          data-label='orange'
        />
        <div
          style={{
            backgroundColor: '#0f9',
            width: '100%',
            height: '100%',
          }}
          data-uid='cyan'
		  data-testid='cyan'
          data-label='cyan'
        />
        <div
          style={{
            backgroundColor: '#09f',
            width: '100%',
            height: '100%',
          }}
          data-uid='blue'
		  data-testid='blue'
          data-label='blue'
        />
      </div>
    </Scene>
  </Storyboard>
)
`

const ProjectCodeWithSpanningItems = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 600,
        height: 600,
        position: 'absolute',
        left: 0,
        top: 0,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 500,
          height: 500,
          display: 'grid',
          gridTemplateColumns: '1fr 1fr 1fr 1fr',
          gridTemplateRows: '1fr 1fr 1fr 1fr',
          gridGap: 10,
          padding: 10,
        }}
        data-testid='grid'
        data-uid='grid'
      >
        <div
          style={{
            backgroundColor: '#f09',
            width: '100%',
            height: '100%',
            gridColumn: 'span 2',
          }}
          data-uid='pink'
          data-testid='pink'
          data-label='pink'
        />
        <div
          style={{
            backgroundColor: '#f90',
            width: '100%',
            height: '100%',
            gridColumn: '2 / span 3',
          }}
          data-uid='orange'
          data-testid='orange'
          data-label='orange'
        />
        <div
          style={{
            backgroundColor: '#0f9',
            width: '100%',
            height: '100%',
            gridColumn: 'span 2 / 4',
            gridRow: 3,
          }}
          data-uid='cyan'
          data-testid='cyan'
          data-label='cyan'
        />
        <div
          style={{
            backgroundColor: '#09f',
            width: '100%',
            height: '100%',
            gridColumnStart: 'span 2',
            gridRow: 4,
          }}
          data-uid='blue'
          data-testid='blue'
          data-label='blue'
        />
        <div
          style={{
            backgroundColor: '#90f',
            width: '100%',
            height: '100%',
            gridColumnEnd: 'span 2',
            gridRow: 4,
          }}
          data-uid='purple'
          data-testid='purple'
          data-label='purple'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
