import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import * as EP from '../../core/shared/element-path'
import { act, fireEvent, screen } from '@testing-library/react'

describe('flex section', () => {
  describe('grid dimensions', () => {
    it('can type a number with unit for dimension', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control = await screen.findByTestId('grid-dimension-column-0')
      await typeIntoField(control, '200px')
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] 200px 1fr 1fr 1fr 1fr')
    })
    it('can type a number without unit for dimension', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control = await screen.findByTestId('grid-dimension-column-0')
      await typeIntoField(control, '2')
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] 2fr 1fr 1fr 1fr 1fr')
    })
    it('can type a fractional number for dimension', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control = await screen.findByTestId('grid-dimension-column-0')
      await typeIntoField(control, '2fr')
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] 2fr 1fr 1fr 1fr 1fr')
    })
    it('can type a keyword for dimension', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control = await screen.findByTestId('grid-dimension-column-0')
      await typeIntoField(control, 'min-content')
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] min-content 1fr 1fr 1fr 1fr')
    })
    it('ignores a typed invalid keyword for dimension', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control = await screen.findByTestId('grid-dimension-column-0')
      await typeIntoField(control, 'not-a-keyword')
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] 1fr 1fr 1fr 1fr 1fr')
    })
    it('defaults to auto if empty', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control = await screen.findByTestId('grid-dimension-column-0')
      await typeIntoField(control, null)
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] auto 1fr 1fr 1fr 1fr')
    })
    it('updates a repeat expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        gridProjectWithRepeat,
        'await-first-dom-report',
      )
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])

      const grid = await renderResult.renderedDOM.findByTestId('grid')
      const input: HTMLInputElement = await screen.findByTestId('grid-dimension-column-1')
      await typeIntoField(input, 'repeat(2, 0.5fr 42px)')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] 1fr repeat(2, 0.5fr 42px) 2fr')
      expect(input.value).toBe('repeat(2, 0.5fr 42px)')
    })
    it('does not show area names in the input', async () => {
      const renderResult = await renderTestEditorWithCode(gridProject, 'await-first-dom-report')
      await selectComponentsForTest(renderResult, [EP.fromString('sb/grid')])
      const control: HTMLInputElement = await screen.findByTestId('grid-dimension-column-0')
      const grid = await renderResult.renderedDOM.findByTestId('grid')
      expect(grid.style.gridTemplateColumns).toEqual('[area1] 1fr 1fr 1fr 1fr 1fr')
      expect(control.value).toBe('1fr')
    })
  })
})

async function typeIntoField(target: HTMLElement, value: string | null) {
  await act(async () => {
    fireEvent.change(target, { target: { value: value } })
    fireEvent.blur(target)
    fireEvent.keyDown(target, { key: 'Enter', keyCode: 13 })
  })
}

const gridProject = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        display: 'grid',
        gridTemplateRows: '80px 1fr 1fr',
        gridTemplateColumns: '[area1] 1fr 1fr 1fr 1fr 1fr',
        gridGap: 10,
        height: 322,
        width: 364,
        backgroundColor: '#FFFFFF',
        padding: 20,
      }}
    >
      <div
        data-uid='foo'
        data-testid='foo'
        style={{
          backgroundColor: '#aaaaaa33',
          gridColumn: 3,
          gridRow: 2,
        }}
      />
      <div
        data-uid='bar'
        data-testid='bar'
        style={{
          width: 41,
          height: 23,
          border: '1px solid #000',
          gridColumn: 'area1',
          gridRow: 1,
          backgroundColor: '#09f',
        }}
      />
      <div
        data-uid='baz'
        data-testid='baz'
        style={{
          height: 53,
          width: 'max-content',
          gridColumn: 2,
          gridRow: 2,
          backgroundColor: '#0f9',
        }}
      >
        a test
      </div>
    </div>
  </Storyboard>
)
`

const gridProjectWithoutTemplate = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        display: 'grid',
        gridGap: 10,
        height: 322,
        width: 364,
        backgroundColor: '#FFFFFF',
        padding: 20,
      }}
    >
      <div
        data-uid='foo'
        data-testid='foo'
        style={{
          backgroundColor: '#aaaaaa33',
          gridColumn: 3,
          gridRow: 2,
        }}
      />
      <div
        data-uid='bar'
        data-testid='bar'
        style={{
          width: 41,
          height: 23,
          border: '1px solid #000',
          gridColumn: 'area1',
          gridRow: 1,
          backgroundColor: '#09f',
        }}
      />
      <div
        data-uid='baz'
        data-testid='baz'
        style={{
          height: 53,
          width: 'max-content',
          gridColumn: 2,
          gridRow: 2,
          backgroundColor: '#0f9',
        }}
      >
        a test
      </div>
    </div>
  </Storyboard>
)
`

const gridProjectWithRepeat = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      data-testid='grid'
      style={{
        display: 'grid',
        gridTemplateRows: '80px 1fr 1fr',
        gridTemplateColumns: '[area1] 1fr repeat(2, 10px 30px) 2fr',
        gridGap: 10,
        height: 322,
        width: 364,
        backgroundColor: '#FFFFFF',
        padding: 20,
      }}
    >
      <div
        data-uid='foo'
        data-testid='foo'
        style={{
          backgroundColor: '#aaaaaa33',
          gridColumn: 3,
          gridRow: 2,
        }}
      />
      <div
        data-uid='bar'
        data-testid='bar'
        style={{
          width: 41,
          height: 23,
          border: '1px solid #000',
          gridColumn: 'area1',
          gridRow: 1,
          backgroundColor: '#09f',
        }}
      />
      <div
        data-uid='baz'
        data-testid='baz'
        style={{
          height: 53,
          width: 'max-content',
          gridColumn: 2,
          gridRow: 2,
          backgroundColor: '#0f9',
        }}
      >
        a test
      </div>
    </div>
  </Storyboard>
)
`
