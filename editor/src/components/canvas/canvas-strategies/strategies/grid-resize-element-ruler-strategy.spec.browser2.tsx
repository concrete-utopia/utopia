import * as EP from '../../../../core/shared/element-path'
import {
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../../../utils/utils.test-utils'
import {
  RulerMarkerColumnEndTestId,
  RulerMarkerColumnStartTestId,
  RulerMarkerRowEndTestId,
  RulerMarkerRowStartTestId,
} from '../../controls/grid-controls'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../../event-helpers.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('grid resize element ruler strategy', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Grid Ruler Markers', true)

  it('can resize a pinned element horizontally', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/pinned')])

    const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
    const controlRect = control.getBoundingClientRect()
    const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
    const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
    await mouseDownAtPoint(control, startPoint)
    await mouseMoveToPoint(control, endPoint)
    await mouseUpAtPoint(control, endPoint)

    const element = await renderResult.renderedDOM.findByTestId('pinned')
    expect(element.style.gridColumn).toBe('2 / 4')
    expect(element.style.gridRow).toBe('3')
  })

  it('can resize a pinned element vertically', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/pinned')])

    const control = await renderResult.renderedDOM.findByTestId(RulerMarkerRowEndTestId)
    const controlRect = control.getBoundingClientRect()
    const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
    const endPoint = { x: controlRect.x + 5, y: controlRect.y + 100 }
    await mouseDownAtPoint(control, startPoint)
    await mouseMoveToPoint(control, endPoint)
    await mouseUpAtPoint(control, endPoint)

    const element = await renderResult.renderedDOM.findByTestId('pinned')
    expect(element.style.gridColumn).toBe('2')
    expect(element.style.gridRow).toBe('3 / 5')
  })

  it('can resize a flow element horizontally', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow1')])

    const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
    const controlRect = control.getBoundingClientRect()
    const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
    const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
    await mouseDownAtPoint(control, startPoint)
    await mouseMoveToPoint(control, endPoint)
    await mouseUpAtPoint(control, endPoint)

    const element = await renderResult.renderedDOM.findByTestId('flow1')
    expect(element.style.gridColumn).toBe('span 2')
    expect(element.style.gridRow).toBe('auto')
  })

  it('can resize a flow element vertically', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow1')])

    const control = await renderResult.renderedDOM.findByTestId(RulerMarkerRowEndTestId)
    const controlRect = control.getBoundingClientRect()
    const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
    const endPoint = { x: controlRect.x + 5, y: controlRect.y + 100 }
    await mouseDownAtPoint(control, startPoint)
    await mouseMoveToPoint(control, endPoint)
    await mouseUpAtPoint(control, endPoint)

    const element = await renderResult.renderedDOM.findByTestId('flow1')
    expect(element.style.gridColumn).toBe('auto')
    expect(element.style.gridRow).toBe('span 2')
  })

  it('can resize a flow element into a pinned element', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow1')])

    {
      const control = await renderResult.renderedDOM.findByTestId(RulerMarkerRowEndTestId)
      const controlRect = control.getBoundingClientRect()
      const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
      const endPoint = { x: controlRect.x + 5, y: controlRect.y + 100 }
      await mouseDownAtPoint(control, startPoint)
      await mouseMoveToPoint(control, endPoint)
      await mouseUpAtPoint(control, endPoint)

      const element = await renderResult.renderedDOM.findByTestId('flow1')
      expect(element.style.gridColumn).toBe('auto')
      expect(element.style.gridRow).toBe('span 2')
    }

    {
      const control = await renderResult.renderedDOM.findByTestId(RulerMarkerRowStartTestId)
      const controlRect = control.getBoundingClientRect()
      const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
      const endPoint = { x: controlRect.x + 5, y: controlRect.y + 100 }
      await mouseDownAtPoint(control, startPoint)
      await mouseMoveToPoint(control, endPoint)
      await mouseUpAtPoint(control, endPoint)

      const element = await renderResult.renderedDOM.findByTestId('flow1')
      expect(element.style.gridColumn).toBe('auto')
      expect(element.style.gridRowEnd).toBe('3')
    }
  })

  it('can resize a non-stretching pinned element', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/pinned-fixed')])

    const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
    const controlRect = control.getBoundingClientRect()
    const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
    const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
    await mouseDownAtPoint(control, startPoint)
    await mouseMoveToPoint(control, endPoint)
    await mouseUpAtPoint(control, endPoint)

    const element = await renderResult.renderedDOM.findByTestId('pinned-fixed')
    expect(element.style.gridColumn).toBe('1 / 3')
    expect(element.style.gridRow).toBe('2')
  })

  it('can resize a non-stretching flow element', async () => {
    const renderResult = await renderTestEditorWithCode(ProjectCode, 'await-first-dom-report')

    await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow-fixed')])

    const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
    const controlRect = control.getBoundingClientRect()
    const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
    const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
    await mouseDownAtPoint(control, startPoint)
    await mouseMoveToPoint(control, endPoint)
    await mouseUpAtPoint(control, endPoint)

    const element = await renderResult.renderedDOM.findByTestId('flow-fixed')
    expect(element.style.gridColumn).toBe('span 2')
    expect(element.style.gridRow).toBe('auto')
  })

  describe('spans', () => {
    it('can resize a flow spanning element (start, from the right)', async () => {
      const renderResult = await renderTestEditorWithCode(
        ProjectCodeWithSpans,
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow1')])

      const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
      const controlRect = control.getBoundingClientRect()
      const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
      const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
      await mouseDownAtPoint(control, startPoint)
      await mouseMoveToPoint(control, endPoint)
      await mouseUpAtPoint(control, endPoint)

      const element = await renderResult.renderedDOM.findByTestId('flow1')
      expect(element.style.gridColumn).toBe('span 3')
      expect(element.style.gridRow).toBe('auto')
    })

    it('can resize a flow spanning element (start, from the left)', async () => {
      const renderResult = await renderTestEditorWithCode(
        ProjectCodeWithSpans,
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow1')])

      const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnStartTestId)
      const controlRect = control.getBoundingClientRect()
      const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
      const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
      await mouseDownAtPoint(control, startPoint)
      await mouseMoveToPoint(control, endPoint)
      await mouseUpAtPoint(control, endPoint)

      const element = await renderResult.renderedDOM.findByTestId('flow1')
      expect(element.style.gridColumn).toBe('')
      expect(element.style.gridColumnStart).toBe('')
      expect(element.style.gridColumnEnd).toBe('3')
      expect(element.style.gridRow).toBe('auto')
    })

    it('can resize a flow spanning element (end)', async () => {
      const renderResult = await renderTestEditorWithCode(
        ProjectCodeWithSpans,
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/flow2')])

      // expand to the right
      {
        const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
        const controlRect = control.getBoundingClientRect()
        const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
        const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
        await mouseDownAtPoint(control, startPoint)
        await mouseMoveToPoint(control, endPoint)
        await mouseUpAtPoint(control, endPoint)

        const element = await renderResult.renderedDOM.findByTestId('flow2')
        expect(element.style.gridColumn).toBe('2 / span 3')
        expect(element.style.gridRow).toBe('auto')
      }

      // shrink from the left
      {
        const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnStartTestId)
        const controlRect = control.getBoundingClientRect()
        const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
        const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
        await mouseDownAtPoint(control, startPoint)
        await mouseMoveToPoint(control, endPoint)
        await mouseUpAtPoint(control, endPoint)

        const element = await renderResult.renderedDOM.findByTestId('flow2')
        expect(element.style.gridColumn).toBe('3 / span 2')
        expect(element.style.gridRow).toBe('auto')
      }

      // expand back from the left
      {
        const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnStartTestId)
        const controlRect = control.getBoundingClientRect()
        const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
        const endPoint = { x: controlRect.x - 400, y: controlRect.y + 5 }
        await mouseDownAtPoint(control, startPoint)
        await mouseMoveToPoint(control, endPoint)
        await mouseUpAtPoint(control, endPoint)

        const element = await renderResult.renderedDOM.findByTestId('flow2')
        expect(element.style.gridColumn).toBe('1 / span 4')
        expect(element.style.gridRow).toBe('auto')
      }
    })

    it('can resize a pinned spanning element', async () => {
      const renderResult = await renderTestEditorWithCode(
        ProjectCodeWithSpans,
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid/pinned')])

      const control = await renderResult.renderedDOM.findByTestId(RulerMarkerColumnEndTestId)
      const controlRect = control.getBoundingClientRect()
      const startPoint = { x: controlRect.x + 5, y: controlRect.y + 5 }
      const endPoint = { x: controlRect.x + 200, y: controlRect.y + 5 }
      await mouseDownAtPoint(control, startPoint)
      await mouseMoveToPoint(control, endPoint)
      await mouseUpAtPoint(control, endPoint)

      const element = await renderResult.renderedDOM.findByTestId('pinned')
      expect(element.style.gridColumn).toBe('2 / span 3')
      expect(element.style.gridRow).toBe('3')
    })
  })
})

const ProjectCode = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 210,
        top: 80,
        width: 690,
        height: 459,
        display: 'grid',
        gap: 10,
        gridTemplateColumns: '1fr 1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr 1fr 1fr',
      }}
      data-uid='grid'
      data-testid='grid'
    >
      <div
        style={{
          backgroundColor: '#09f',
          alignSelf: 'stretch',
          justifySelf: 'stretch',
        }}
        data-uid='flow1'
        data-testid='flow1'
      />
      <div
        style={{
          backgroundColor: '#0f9',
          justifySelf: 'stretch',
          alignSelf: 'stretch',
        }}
        data-uid='flow2'
        data-testid='flow2'
      />
      <div
        style={{
          backgroundColor: '#f0f',
          width: 50,
          height: 50,
        }}
        data-uid='flow-fixed'
        data-testid='flow-fixed'
      />
      <div
        style={{
          backgroundColor: '#f09',
          gridColumn: 2,
          gridRow: 3,
          alignSelf: 'stretch',
          justifySelf: 'stretch',
        }}
        data-uid='pinned'
        data-testid='pinned'
      />
      <div
        style={{
          backgroundColor: '#ff0',
          gridColumn: 1,
          gridRow: 2,
          width: 50,
          height: 50,
        }}
        data-uid='pinned-fixed'
        data-testid='pinned-fixed'
      />
    </div>
  </Storyboard>
)
`

const ProjectCodeWithSpans = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 210,
        top: 80,
        width: 690,
        height: 459,
        display: 'grid',
        gap: 10,
        gridTemplateColumns: '1fr 1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr 1fr 1fr',
      }}
      data-uid='grid'
      data-testid='grid'
    >
      <div
        style={{
          backgroundColor: '#09f',
          alignSelf: 'stretch',
          justifySelf: 'stretch',
          gridColumn: 'span 2',
        }}
        data-uid='flow1'
        data-testid='flow1'
      />
      <div
        style={{
          backgroundColor: '#f90',
          alignSelf: 'stretch',
          justifySelf: 'stretch',
          gridColumn: '2 / span 2',
        }}
        data-uid='flow2'
        data-testid='flow2'
      />
      <div
        style={{
          backgroundColor: '#f09',
          gridColumn: '2 / span 2',
          gridRow: 3,
          alignSelf: 'stretch',
          justifySelf: 'stretch',
        }}
        data-uid='pinned'
        data-testid='pinned'
      />
    </div>
  </Storyboard>
)
`
