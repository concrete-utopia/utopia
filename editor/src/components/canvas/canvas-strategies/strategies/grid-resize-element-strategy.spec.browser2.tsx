import * as EP from '../../../../core/shared/element-path'
import { getRectCenter, localRectangle } from '../../../../core/shared/math-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { GridResizeEdgeTestId } from '../../controls/grid-controls'
import { mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import type { GridResizeEdge } from '../interaction-state'
import { gridCellTargetId } from './grid-helpers'

async function runCellResizeTest(
  editor: EditorRenderResult,
  edge: GridResizeEdge,
  dragToCellTestId: string,
) {
  const elementPathToDrag = EP.fromString('sb/scene/grid/ddd')

  await selectComponentsForTest(editor, [elementPathToDrag])

  const resizeControl = editor.renderedDOM.getByTestId(GridResizeEdgeTestId(edge))
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
  )
}

describe('grid rearrange move strategy', () => {
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
  })

  it('removes the grid-area prop on resize', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeWithGridArea, 'await-first-dom-report')

    await runCellResizeTest(
      editor,
      'column-end',
      gridCellTargetId(EP.fromString('sb/scene/grid'), 2, 10),
    )

    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, gridArea } =
      editor.renderedDOM.getByTestId('grid-child').style
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, gridArea }).toEqual({
      gridArea: '',
      gridColumnEnd: '11',
      gridColumnStart: '7',
      gridRowStart: '2',
      gridRowEnd: 'auto',
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
          }}
          data-uid='ccc'
        />
        <div
          style={{
            minHeight: 0,
            gridColumnEnd: 10,
            gridRowEnd: 3,
            gridColumnStart: 7,
            gridRowStart: 2,
            backgroundColor: '#db48f6',
          }}
          data-uid='ddd'
          data-testid='grid-child'
        />
      </div>
    </Scene>
  </Storyboard>
)
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
            backgroundColor: '#f3785f',
            gridArea: '2 / 3 / 7 / 10',
          }}
          data-uid='ddd'
          data-testid='grid-child'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
