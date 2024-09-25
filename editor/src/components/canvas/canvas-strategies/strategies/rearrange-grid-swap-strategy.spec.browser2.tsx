import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { selectComponents } from '../../../../components/editor/actions/meta-actions'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseDownAtPoint,
  mouseMoveToPoint,
  mouseUpAtPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import { canvasPoint } from '../../../../core/shared/math-utils'
import { GridCellTestId } from '../../controls/grid-controls'

const testProject = `
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
        gridTemplateColumns: '2.4fr 1fr 1fr',
        gridTemplateRows: '99px 109px 90px',
        height: 'max-content',
      }}
    >
      <div
        data-uid='row-1-column-1'
        data-testid='row-1-column-1'
        style={{
          backgroundColor: 'green',
        }}
      />
      <div
        data-uid='row-2-column-1'
        data-testid='row-2-column-1'
        style={{ backgroundColor: 'blue' }}
      />
      <div
        data-uid='row-1-column-3'
        data-testid='row-1-column-3'
        style={{ backgroundColor: 'pink' }}
      />
      <div
        data-uid='row-1-column-2'
        data-testid='row-1-column-2'
        style={{
          backgroundColor: 'green',
		  gridRow: 'auto',
		  gridColumn: 'auto',
        }}
      />
      <div
        data-uid='row-2-column-2'
        data-testid='row-2-column-2'
        style={{
          backgroundColor: 'blue',
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

describe('swap an element', () => {
  it('swap out an element for another from a different column and row', async () => {
    const renderResult = await renderTestEditorWithCode(testProject, 'await-first-dom-report')
    const draggedItem = EP.fromString(`sb/grid/row-1-column-2`)
    await renderResult.dispatch(selectComponents([draggedItem], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const draggedItemElement = renderResult.renderedDOM.getByTestId(
      GridCellTestId(EP.fromString(`sb/grid/row-1-column-2`)),
    )
    const draggedItemRect = draggedItemElement.getBoundingClientRect()
    const startPoint = canvasPoint({
      x: draggedItemRect.x + draggedItemRect.width / 2,
      y: draggedItemRect.y + draggedItemRect.height / 2,
    })
    const swapTargetElement = renderResult.renderedDOM.getByTestId(
      GridCellTestId(EP.fromString(`sb/grid/row-2-column-1`)),
    )
    const swapTargetRect = swapTargetElement.getBoundingClientRect()
    const endPoint = canvasPoint({
      x: swapTargetRect.x + swapTargetRect.width / 2,
      y: swapTargetRect.y + swapTargetRect.height / 2,
    })
    await mouseMoveToPoint(draggedItemElement, startPoint)
    await mouseDownAtPoint(draggedItemElement, startPoint)
    await mouseMoveToPoint(canvasControlsLayer, endPoint)
    await pressKey('Tab')
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(
      renderResult.getEditorState().editor.canvas.interactionSession?.userPreferredStrategy,
    ).toEqual('rearrange-grid-swap-strategy')
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
        gridTemplateRows: '99px 109px 90px',
        height: 'max-content',
      }}
    >
      <div
        data-uid='row-1-column-1'
        data-testid='row-1-column-1'
        style={{ backgroundColor: 'green' }}
      />
      <div
        data-uid='row-1-column-2'
        data-testid='row-1-column-2'
        style={{
          backgroundColor: 'green',
          gridColumn: 'auto',
          gridRow: 'auto',
        }}
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
          backgroundColor: 'blue',
          gridColumn: 'auto',
          gridRow: 'auto',
        }}
      />
      <div
        data-uid='row-2-column-2'
        data-testid='row-2-column-2'
        style={{ backgroundColor: 'blue' }}
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
