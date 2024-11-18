import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isLeft } from '../../../../core/shared/either'
import { isJSXElement } from '../../../../core/shared/element-template'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import type { GridCellCoordinates } from './grid-cell-bounds'
import { runGridMoveTest } from './grid.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'

describe('grid reorder', () => {
  it('reorders an element without setting positioning (inside contiguous area)', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeReorder, 'await-first-dom-report')
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, cells } =
      await runReorderTest(editor, 'sb/scene/grid', 'orange', { row: 1, column: 3 })

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '',
      gridColumnStart: '',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(cells).toEqual(['pink', 'cyan', 'orange', 'blue'])
  })
  it('reorders an element in a grid component without setting positioning (inside contiguous area)', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeReorderGridComponent,
      'await-first-dom-report',
    )
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, cells } =
      await runReorderTest(editor, 'sb/scene/grid', 'orange', { row: 1, column: 3 })

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '',
      gridColumnStart: '',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(cells).toEqual(['pink', 'cyan', 'orange', 'blue'])
  })
  it('reorders a component (which does not take style props) inside contiguous area', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeReorderWithComponentItem,
      'await-first-dom-report',
    )
    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, cells } =
      await runReorderTest(editor, 'sb/scene/grid', 'orange', { row: 1, column: 3 })

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '',
      gridColumnStart: '',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(cells).toEqual(['pink', 'cyan', 'orange', 'blue'])
  })
  it('reorders an element without setting positioning (edge of contiguous area)', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeReorder, 'await-first-dom-report')

    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, cells } =
      await runReorderTest(editor, 'sb/scene/grid', 'orange', { row: 2, column: 1 })

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: '',
      gridColumnStart: '',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(cells).toEqual(['pink', 'cyan', 'blue', 'orange'])
  })
  it('reorders an element setting positioning when outside of contiguous area', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeReorder, 'await-first-dom-report')

    const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd, cells } =
      await runReorderTest(editor, 'sb/scene/grid', 'orange', { row: 2, column: 2 }, { tab: true })

    expect({ gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: '2',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })

    expect(cells).toEqual(['pink', 'cyan', 'blue', 'orange'])
  })
  it('reorders an element setting positioning also relative to other fixed elements', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeReorder, 'await-first-dom-report')

    const first = await runReorderTest(
      editor,
      'sb/scene/grid',
      'orange',
      { row: 2, column: 2 },
      { tab: true },
    )

    expect({
      gridRowStart: first.gridRowStart,
      gridRowEnd: first.gridRowEnd,
      gridColumnStart: first.gridColumnStart,
      gridColumnEnd: first.gridColumnEnd,
    }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: '2',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })

    expect(first.cells).toEqual(['pink', 'cyan', 'blue', 'orange'])

    await selectComponentsForTest(editor, [])

    const second = await runReorderTest(
      editor,
      'sb/scene/grid',
      'pink',
      { row: 2, column: 3 },
      { tab: true },
    )

    expect({
      gridRowStart: second.gridRowStart,
      gridRowEnd: second.gridRowEnd,
      gridColumnStart: second.gridColumnStart,
      gridColumnEnd: second.gridColumnEnd,
    }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: '3',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })

    expect(second.cells).toEqual(['cyan', 'blue', 'orange', 'pink'])
  })

  it('reorders and removes positioning when moving back to contiguous', async () => {
    const editor = await renderTestEditorWithCode(ProjectCodeReorder, 'await-first-dom-report')

    const first = await runReorderTest(
      editor,
      'sb/scene/grid',
      'orange',
      { row: 2, column: 2 },
      { tab: true },
    )
    expect({
      gridRowStart: first.gridRowStart,
      gridRowEnd: first.gridRowEnd,
      gridColumnStart: first.gridColumnStart,
      gridColumnEnd: first.gridColumnEnd,
    }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: '2',
      gridRowEnd: 'auto',
      gridRowStart: '2',
    })

    expect(first.cells).toEqual(['pink', 'cyan', 'blue', 'orange'])

    const second = await runReorderTest(
      editor,
      'sb/scene/grid',
      'orange',
      { row: 1, column: 1 },
      { tab: true },
    )

    expect({
      gridRowStart: second.gridRowStart,
      gridRowEnd: second.gridRowEnd,
      gridColumnStart: second.gridColumnStart,
      gridColumnEnd: second.gridColumnEnd,
    }).toEqual({
      gridColumnEnd: '',
      gridColumnStart: '',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(second.cells).toEqual(['orange', 'pink', 'cyan', 'blue'])
  })

  it('reorders when element occupies multiple cells', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeReorderWithMultiCellChild,
      'await-first-dom-report',
    )

    const result = await runReorderTest(
      editor,
      'sb/scene/grid',
      'orange',
      { row: 1, column: 1 },
      { tab: true },
    )

    expect({
      gridRowStart: result.gridRowStart,
      gridRowEnd: result.gridRowEnd,
      gridColumnStart: result.gridColumnStart,
      gridColumnEnd: result.gridColumnEnd,
    }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: 'span 2',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(result.cells).toEqual(['orange', 'pink', 'cyan', 'blue'])
  })

  it('reordering a spanning element keeps its size', async () => {
    const editor = await renderTestEditorWithCode(
      ProjectCodeReorderWithSpanningChild,
      'await-first-dom-report',
    )

    const result = await runReorderTest(
      editor,
      'sb/scene/grid',
      'orange',
      { row: 3, column: 2 },
      { tab: true },
    )

    expect({
      gridRowStart: result.gridRowStart,
      gridRowEnd: result.gridRowEnd,
      gridColumnStart: result.gridColumnStart,
      gridColumnEnd: result.gridColumnEnd,
    }).toEqual({
      gridColumnEnd: 'auto',
      gridColumnStart: 'span 3',
      gridRowEnd: '',
      gridRowStart: '',
    })

    expect(result.cells).toEqual(['pink', 'cyan', 'blue', 'orange'])
  })
})

async function runReorderTest(
  editor: EditorRenderResult,
  gridPath: string,
  testId: string,
  targetCell: GridCellCoordinates,
  options?: { tab?: boolean },
) {
  const { gridRowStart, gridRowEnd, gridColumnStart, gridColumnEnd } = await runGridMoveTest(
    editor,
    {
      scale: 1,
      gridPath: gridPath,
      testId: testId,
      targetCell: targetCell,
      tab: options?.tab,
    },
  )

  const grid = editor.getEditorState().editor.jsxMetadata[gridPath]
  if (isLeft(grid.element) || !isJSXElement(grid.element.value)) {
    throw new Error('expected jsx element')
  }

  const cells = MetadataUtils.getChildrenOrdered(
    editor.getEditorState().editor.jsxMetadata,
    editor.getEditorState().editor.elementPathTree,
    EP.fromString(gridPath),
  )

  return {
    gridRowStart: gridRowStart,
    gridRowEnd: gridRowEnd,
    gridColumnStart: gridColumnStart,
    gridColumnEnd: gridColumnEnd,
    cells: cells.map((c) => EP.toUid(c.elementPath)),
  }
}

const ProjectCodeReorder = `import * as React from 'react'
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

const ProjectCodeReorderGridComponent = `import * as React from 'react'
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
      <Grid
        style={{
          backgroundColor: '#aaaaaa33',
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
      data-uid='43adc9c0cf5a0bbf66c67e2e37466c18'
    >
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr 1fr',
          gridTemplateRows: '1fr 1fr 1fr',
          gridGap: 10,
          width: 500,
          height: 500,
          padding: 10,
        }}
        data-uid='f84914f31dbc6c5d9b44c11ae54139ef'
      >
        {props.children}
      </div>
      <div
        style={{ height: 100 }}
        data-uid='f5827ce0f04916c792a1da2bd69b6cad'
      />
    </div>
  )
}
`

const ProjectCodeReorderWithComponentItem = `import * as React from 'react'
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
        <Item
          backgroundColor='#f09'
          data-uid='pink'
          data-testid='pink'
          data-label='pink'
        />
        <Item
          backgroundColor='#f90'
          data-uid='orange'
          data-testid='orange'
          data-label='orange'
        />
        <Item
          backgroundColor='#0f9'
          data-uid='cyan'
          data-testid='cyan'
          data-label='cyan'
        />
        <Item
          backgroundColor='#09f'
          data-uid='blue'
          data-testid='blue'
          data-label='blue'
        />
      </div>
    </Scene>
  </Storyboard>
)

export function Item(props) {
  return (
    <div
      style={{
        backgroundColor: props.backgroundColor,
        width: '100%',
        height: '100%',
      }}
      data-uid={props['data-uid']}
      data-testid={props['data-testid']}
      data-label={props['data-label']}
    />
  )
}
`

const ProjectCodeReorderWithMultiCellChild = `import * as React from 'react'
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
            gridColumnStart: 2,
            gridColumnEnd: 4,
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

const ProjectCodeReorderWithSpanningChild = `import * as React from 'react'
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
            gridColumn: 'span 3',
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
