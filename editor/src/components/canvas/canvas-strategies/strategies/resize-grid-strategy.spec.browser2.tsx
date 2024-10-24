import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { selectComponents } from '../../../../components/editor/actions/meta-actions'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../../event-helpers.test-utils'
import { canvasPoint } from '../../../../core/shared/math-utils'

const makeTestProject = (columns: string, rows: string) => `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: 25,
        top: 305,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 600,
        gridTemplateColumns: '${columns}',
        gridTemplateRows: '${rows}',
        height: 'max-content',
      }}
    >
      <div
        data-uid='row-1-column-1'
        data-testid='row-1-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 1,
          gridColumnEnd: 1,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-1-column-2'
        data-testid='row-1-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-1-column-3'
        data-testid='row-1-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-2-column-1'
        data-testid='row-2-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 3,
          gridColumnEnd: 4,
          gridRowStart: 2,
          gridRowEnd: 4,
        }}
      />
      <div
        data-uid='row-2-column-2'
        data-testid='row-2-column-2'
        style={{
          backgroundColor: 'blue',
          gridColumnStart: 2,
          gridColumnEnd: 2,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div 
        data-uid='row-2-column-3'
        data-testid='row-2-column-3'
        style={{ backgroundColor: 'pink' }} 
      />
      <div
        data-uid='row-3-column-1'
        data-testid='row-3-column-1'
        style={{ backgroundColor: 'green' }} 
      />
      <div
        data-uid='row-3-column-2'
        data-testid='row-3-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-3-column-3'
        data-testid='row-3-column-3'
        style={{ backgroundColor: 'pink' }}
      />
    </div>
  </Storyboard>
)
`

const makeTestProjectForAGeneratedGrid = (columns: string, rows: string) => `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: 25,
        top: 305,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 600,
        gridTemplateColumns: '${columns}',
        gridTemplateRows: '${rows}',
        height: 'max-content',
      }}
    >
      {[1,2,3].map(n => {
        return <div
        data-uid={\`row-1-column-\${n}\`}
        data-testid={\`row-1-column-\${n}\`}
        style={{ backgroundColor: 'pink' }}
      />
      })}
    </div>
  </Storyboard>
)
`

describe('resize a grid', () => {
  it('update a fractionally sized column', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProject('2.4fr 1fr 1fr', '99px 109px 90px'),
      'await-first-dom-report',
    )
    const target = EP.fromString(`sb/grid/row-1-column-2`)
    await renderResult.dispatch(selectComponents([target], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const resizeControl = renderResult.renderedDOM.getByTestId(`grid-column-handle-1`)
    const resizeControlRect = resizeControl.getBoundingClientRect()
    const startPoint = canvasPoint({
      x: resizeControlRect.x + resizeControlRect.width / 2,
      y: resizeControlRect.y + resizeControlRect.height / 2,
    })
    const endPoint = canvasPoint({
      x: startPoint.x + 100,
      y: startPoint.y,
    })
    await mouseMoveToPoint(resizeControl, startPoint)
    await mouseDownAtPoint(resizeControl, startPoint)
    await mouseMoveToPoint(canvasControlsLayer, endPoint)
    await mouseUpAtPoint(canvasControlsLayer, endPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: 25,
        top: 305,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 600,
        gridTemplateColumns: '2.4fr 1.8fr 1fr',
        gridTemplateRows: '99px 109px 90px',
        height: 'max-content',
      }}
    >
      <div
        data-uid='row-1-column-1'
        data-testid='row-1-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 1,
          gridColumnEnd: 1,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-1-column-2'
        data-testid='row-1-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-1-column-3'
        data-testid='row-1-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-2-column-1'
        data-testid='row-2-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 3,
          gridColumnEnd: 4,
          gridRowStart: 2,
          gridRowEnd: 4,
        }}
      />
      <div
        data-uid='row-2-column-2'
        data-testid='row-2-column-2'
        style={{
          backgroundColor: 'blue',
          gridColumnStart: 2,
          gridColumnEnd: 2,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-2-column-3'
        data-testid='row-2-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-3-column-1'
        data-testid='row-3-column-1'
        style={{ backgroundColor: 'green' }}
      />
      <div
        data-uid='row-3-column-2'
        data-testid='row-3-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-3-column-3'
        data-testid='row-3-column-3'
        style={{ backgroundColor: 'pink' }}
      />
    </div>
  </Storyboard>
)
`)
  })

  it('update a fractionally sized column with generated content', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectForAGeneratedGrid('2.4fr 1fr 1fr', '99px 109px 90px'),
      'await-first-dom-report',
    )
    const target = EP.fromString(`sb/grid`)
    await renderResult.dispatch(selectComponents([target], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const resizeControl = renderResult.renderedDOM.getByTestId(`grid-column-handle-1`)
    const resizeControlRect = resizeControl.getBoundingClientRect()
    const startPoint = canvasPoint({
      x: resizeControlRect.x + resizeControlRect.width / 2,
      y: resizeControlRect.y + resizeControlRect.height / 2,
    })
    const endPoint = canvasPoint({
      x: startPoint.x + 100,
      y: startPoint.y,
    })
    await mouseMoveToPoint(resizeControl, startPoint)
    await mouseDownAtPoint(resizeControl, startPoint)
    await mouseMoveToPoint(canvasControlsLayer, endPoint)
    await mouseUpAtPoint(canvasControlsLayer, endPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: 25,
        top: 305,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 600,
        gridTemplateColumns: '2.4fr 1.8fr 1fr',
        gridTemplateRows: '99px 109px 90px',
        height: 'max-content',
      }}
    >
      {[1, 2, 3].map((n) => {
        return (
          <div
            data-uid={\`row-1-column-\${n}\`}
            data-testid={\`row-1-column-\${n}\`}
            style={{ backgroundColor: 'pink' }}
          />
        )
      })}
    </div>
  </Storyboard>
)
`)
  })

  it('update a pixel sized row', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProject('2.4fr 1fr 1fr', '99px 109px 90px'),
      'await-first-dom-report',
    )
    const target = EP.fromString(`sb/grid/row-1-column-2`)
    await renderResult.dispatch(selectComponents([target], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const resizeControl = renderResult.renderedDOM.getByTestId(`grid-row-handle-1`)
    const resizeControlRect = resizeControl.getBoundingClientRect()
    const startPoint = canvasPoint({
      x: resizeControlRect.x + resizeControlRect.width / 2,
      y: resizeControlRect.y + resizeControlRect.height / 2,
    })
    const endPoint = canvasPoint({
      x: startPoint.x,
      y: startPoint.y + 20,
    })
    await mouseMoveToPoint(resizeControl, startPoint)
    await mouseDownAtPoint(resizeControl, startPoint)
    await mouseMoveToPoint(canvasControlsLayer, endPoint)
    await mouseUpAtPoint(canvasControlsLayer, endPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: 25,
        top: 305,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 600,
        gridTemplateColumns: '2.4fr 1fr 1fr',
        gridTemplateRows: '99px 129px 90px',
        height: 'max-content',
      }}
    >
      <div
        data-uid='row-1-column-1'
        data-testid='row-1-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 1,
          gridColumnEnd: 1,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-1-column-2'
        data-testid='row-1-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-1-column-3'
        data-testid='row-1-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-2-column-1'
        data-testid='row-2-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 3,
          gridColumnEnd: 4,
          gridRowStart: 2,
          gridRowEnd: 4,
        }}
      />
      <div
        data-uid='row-2-column-2'
        data-testid='row-2-column-2'
        style={{
          backgroundColor: 'blue',
          gridColumnStart: 2,
          gridColumnEnd: 2,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-2-column-3'
        data-testid='row-2-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-3-column-1'
        data-testid='row-3-column-1'
        style={{ backgroundColor: 'green' }}
      />
      <div
        data-uid='row-3-column-2'
        data-testid='row-3-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-3-column-3'
        data-testid='row-3-column-3'
        style={{ backgroundColor: 'pink' }}
      />
    </div>
  </Storyboard>
)
`)
  })

  it('update a repeat (fr) sized column', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProject('repeat(3, 1fr)', '99px 109px 90px'),
      'await-first-dom-report',
    )
    const target = EP.fromString(`sb/grid/row-1-column-2`)
    await renderResult.dispatch(selectComponents([target], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const resizeControl = renderResult.renderedDOM.getByTestId(`grid-column-handle-1`)
    const resizeControlRect = resizeControl.getBoundingClientRect()
    const startPoint = canvasPoint({
      x: resizeControlRect.x + resizeControlRect.width / 2,
      y: resizeControlRect.y + resizeControlRect.height / 2,
    })
    const endPoint = canvasPoint({
      x: startPoint.x + 100,
      y: startPoint.y,
    })
    await mouseMoveToPoint(resizeControl, startPoint)
    await mouseDownAtPoint(resizeControl, startPoint)
    await mouseMoveToPoint(canvasControlsLayer, endPoint)
    await mouseUpAtPoint(canvasControlsLayer, endPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        position: 'absolute',
        left: 25,
        top: 305,
        display: 'grid',
        gap: 10,
        width: 600,
        height: 600,
        gridTemplateColumns: 'repeat(3, 1.5fr)',
        gridTemplateRows: '99px 109px 90px',
        height: 'max-content',
      }}
    >
      <div
        data-uid='row-1-column-1'
        data-testid='row-1-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 1,
          gridColumnEnd: 1,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-1-column-2'
        data-testid='row-1-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-1-column-3'
        data-testid='row-1-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-2-column-1'
        data-testid='row-2-column-1'
        style={{
          backgroundColor: 'green',
          gridColumnStart: 3,
          gridColumnEnd: 4,
          gridRowStart: 2,
          gridRowEnd: 4,
        }}
      />
      <div
        data-uid='row-2-column-2'
        data-testid='row-2-column-2'
        style={{
          backgroundColor: 'blue',
          gridColumnStart: 2,
          gridColumnEnd: 2,
          gridRowStart: 2,
          gridRowEnd: 2,
        }}
      />
      <div
        data-uid='row-2-column-3'
        data-testid='row-2-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-3-column-1'
        data-testid='row-3-column-1'
        style={{ backgroundColor: 'green' }}
      />
      <div
        data-uid='row-3-column-2'
        data-testid='row-3-column-2'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-3-column-3'
        data-testid='row-3-column-3'
        style={{ backgroundColor: 'pink' }}
      />
    </div>
  </Storyboard>
)
`)
  })
})
