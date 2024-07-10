import * as EP from '../../../../core/shared/element-path'
import { getRectCenter, localRectangle } from '../../../../core/shared/math-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import CanvasActions from '../../canvas-actions'
import { GridCellTestId } from '../../controls/grid-controls'
import { mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { gridCellTargetId } from './grid-helpers'

describe('grid rearrange move strategy', () => {
  it('can rearrange elements on a grid', async () => {
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest({
      scale: 1,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })
  it('can rearrange elements on a grid (zoom out)', async () => {
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest({
      scale: 0.5,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })

  it('can rearrange elements on a grid (zoom in)', async () => {
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runMoveTest({
      scale: 2,
    })
    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '7',
      gridColumnStart: '3',
      gridRowEnd: '4',
      gridRowStart: '2',
    })
  })
})

async function runMoveTest(props: { scale: number }) {
  const editor = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

  const elementPathToDrag = EP.fromString('sb/scene/grid/aaa')

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
      x: sourceRect.x * (props.scale > 1 ? props.scale : 1) + 10,
      y: sourceRect.y * (props.scale > 1 ? props.scale : 1) + 10,
    },
    getRectCenter(
      localRectangle({
        x: targetRect.x * (props.scale > 1 ? props.scale : 1),
        y: targetRect.y * (props.scale > 1 ? props.scale : 1),
        width: targetRect.width,
        height: targetRect.height,
      }),
    ),
  )

  return editor.renderedDOM.getByTestId('aaa').style
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
        <Placeholder
          style={{
            minHeight: 0,
            backgroundColor: '#23565b',
            gridColumnStart: 5,
            gridColumnEnd: 7,
            gridRowStart: 1,
            gridRowEnd: 3,
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
