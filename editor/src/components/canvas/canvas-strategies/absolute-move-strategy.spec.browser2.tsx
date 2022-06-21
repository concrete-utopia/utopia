import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../ui-jsx.test-utils'
import { act, fireEvent } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import { selectComponents } from '../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'

describe('Absolute Move Strategy', () => {
  it('moves absolute positioned element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const areaControl = renderResult.renderedDOM.getByTestId('bbb')
    const areaControlBounds = areaControl.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    fireEvent(
      canvasControlsLayer,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 20,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 20,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 20,
        }),
      )
      await dispatchDone
    })
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 80, top: 25, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('moves absolute element with snapping', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const areaControl = renderResult.renderedDOM.getByTestId('bbb')
    const areaControlBounds = areaControl.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    fireEvent(
      canvasControlsLayer,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 14,
          clientY: areaControlBounds.top - 18,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 14,
          clientY: areaControlBounds.top - 18,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 14,
          clientY: areaControlBounds.top - 18,
        }),
      )
      await dispatchDone
    })
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 30, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('moves absolute element without snapping, pressing cmd', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const areaControl = renderResult.renderedDOM.getByTestId('bbb')
    const areaControlBounds = areaControl.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    fireEvent(
      canvasControlsLayer,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: areaControlBounds.left + 14,
          clientY: areaControlBounds.top - 18,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: areaControlBounds.left + 14,
          clientY: areaControlBounds.top - 18,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: areaControlBounds.left + 14,
          clientY: areaControlBounds.top - 18,
        }),
      )
      await dispatchDone
    })
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 49, top: 27, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
})
