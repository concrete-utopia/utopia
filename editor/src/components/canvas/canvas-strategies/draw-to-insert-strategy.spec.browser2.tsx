import { act, fireEvent } from '@testing-library/react'
import { jsxElement, simpleAttribute } from '../../../core/shared/element-template'
import { windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { EditorAction } from '../../editor/action-types'
import { enableInsertModeForJSXElement } from '../../editor/actions/action-creators'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
} from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { wait } from '../../../utils/utils.test-utils'

function slightlyOffsetWindowPointBecauseVeryWeirdIssue(point: {
  x: number
  y: number
}): WindowPoint {
  // FIXME when running in headless chrome, the result of getBoundingClientRect will be slightly
  // offset for some unknown reason, meaning the inserted element will be 1 pixel of in each dimension
  return windowPoint({ x: point.x - 0.001, y: point.y - 0.001 })
}

async function fireDragEvent(
  canvasControlsLayer: HTMLElement,
  startPoint: WindowPoint,
  endPoint: WindowPoint,
) {
  await act(async () => {
    fireEvent(
      canvasControlsLayer,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        clientX: startPoint.x,
        clientY: startPoint.y,
        buttons: 1,
      }),
    )

    fireEvent(
      canvasControlsLayer,
      new MouseEvent('mousemove', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
        buttons: 1,
      }),
    )

    fireEvent(
      canvasControlsLayer,
      new MouseEvent('mouseup', {
        bubbles: true,
        cancelable: true,
        clientX: endPoint.x,
        clientY: endPoint.y,
      }),
    )
  })
}

describe('Inserting into absolute', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  const newElementUID = 'ddd'
  const newElement = jsxElement(
    'div',
    newElementUID,
    [
      simpleAttribute('data-uid', newElementUID),
      simpleAttribute('style', { position: 'absolute' }),
    ],
    [],
  )

  async function startInsertMode(
    dispatch: (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => Promise<void>,
  ) {
    await act(() =>
      dispatch([enableInsertModeForJSXElement(newElement, newElementUID, {}, null)], false),
    )
  }

  const inputCode = makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
      }}
    >
      <div
        data-uid='bbb'
        data-testid='bbb'
        style={{
          position: 'absolute',
          left: 10,
          top: 10,
          width: 380,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          position: 'absolute',
          left: 10,
          top: 200,
          width: 380,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `)

  it('Should honour the initial target when dragging to insert', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 25,
      y: targetElementBounds.y + 305,
    })

    // Drag from inside bbb to inside ccc
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a child of bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'absolute',
              left: 10,
              top: 10,
              width: 380,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              data-uid='ddd'
              style={{
                position: 'absolute',
                left: 5,
                top: 5,
                width: 20,
                height: 300,
              }}
            />
          </div>
          <div
            data-uid='ccc'
            style={{
              position: 'absolute',
              left: 10,
              top: 200,
              width: 380,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Should not clear the intended target when dragging to insert past the scene boundary', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 1005,
      y: targetElementBounds.y + 15,
    })

    // Drag from inside bbb to outside of the scene
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a child of bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'absolute',
              left: 10,
              top: 10,
              width: 380,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              data-uid='ddd'
              style={{
                position: 'absolute',
                left: 5,
                top: 5,
                width: 1000,
                height: 10,
              }}
            />
          </div>
          <div
            data-uid='ccc'
            style={{
              position: 'absolute',
              left: 10,
              top: 200,
              width: 380,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })
})

describe('Inserting into flex row', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  const newElementUID = 'ddd'
  const newElement = jsxElement(
    'div',
    newElementUID,
    [
      simpleAttribute('data-uid', newElementUID),
      simpleAttribute('style', { position: 'absolute' }),
    ],
    [],
  )

  async function startInsertMode(
    dispatch: (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => Promise<void>,
  ) {
    await act(() =>
      dispatch([enableInsertModeForJSXElement(newElement, newElementUID, {}, null)], false),
    )
  }

  const inputCode = makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        gap: 10,
      }}
    >
      <div
        data-uid='bbb'
        data-testid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          width: 100,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `)

  it('Insert into zero position in flex', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 25,
      y: targetElementBounds.y + 305,
    })

    // Drag horizontally close to the zero position
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is before bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            gap: 10,
          }}
        >
          <div
            data-uid='ddd'
            style={{
              position: 'relative',
              width: 20,
              height: 300,
            }}
          />
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          /> 
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Insert into first position in flex', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + targetElementBounds.height + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 25,
      y: targetElementBounds.y + targetElementBounds.height + 305,
    })

    // Drag horizontally close to the first position
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is after bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            gap: 10,
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            data-uid='ddd'
            style={{
              position: 'relative',
              width: 20,
              height: 300,
            }}
          />
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Insert into first position in flex, backwards drag', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 25,
      y: targetElementBounds.y + 305,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + 5,
    })

    // Drag starts horizontally close to the first position, dragging towards the top left
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is before bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            gap: 10,
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            data-uid='ddd'
            style={{
              position: 'relative',
              width: 20,
              height: 300,
            }}
          />
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Insert inside a flex child with absolute layout', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 10,
      y: targetElementBounds.y + 10,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 30,
      y: targetElementBounds.y + 40,
    })

    // Drag starts inside bbb
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a child of bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            gap: 10,
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              data-uid='ddd'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 20,
                height: 30,
              }}
            />
          </div>
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Drag inside a flex child close to the edge, which inserts as a sibling', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 3,
      y: targetElementBounds.y + 3,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 23,
      y: targetElementBounds.y + 33,
    })

    // Drag starts inside bbb, but very close to its edge (3px)
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is before bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
          position: 'relative',
          display: 'flex',
          gap: 10,
        }}
      >
        <div
          data-uid='ddd'
          style={{
            position: 'relative',
            width: 20,
            height: 30,
          }}
        />
        <div
          data-uid='bbb'
          data-testid='bbb'
          style={{
            width: 180,
            height: 180,
            backgroundColor: '#d3d3d3',
          }}
        />
        <div
          data-uid='ccc'
          style={{
            width: 100,
            height: 190,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
    `),
    )
  })
})

describe('Inserting into flex column', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  const newElementUID = 'ddd'
  const newElement = jsxElement(
    'div',
    newElementUID,
    [
      simpleAttribute('data-uid', newElementUID),
      simpleAttribute('style', { position: 'absolute' }),
    ],
    [],
  )

  async function startInsertMode(
    dispatch: (actions: ReadonlyArray<EditorAction>, waitForDOMReport: boolean) => Promise<void>,
  ) {
    await act(() =>
      dispatch([enableInsertModeForJSXElement(newElement, newElementUID, {}, null)], false),
    )
  }

  const inputCode = makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
      }}
    >
      <div
        data-uid='bbb'
        data-testid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          width: 100,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `)

  it('Insert into zero position in flex, column layout', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 305,
      y: targetElementBounds.y + 25,
    })

    // Drag vertically close to the first position
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is before bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            gap: 10,
          }}
        >
          <div
            data-uid='ddd'
            style={{
              position: 'relative',
              width: 300,
              height: 20,
            }}
          />
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Insert into first position in flex, column layout', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + targetElementBounds.height + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 305,
      y: targetElementBounds.y + targetElementBounds.height + 25,
    })

    // Drag vertically close to the first position
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is after bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            gap: 10,
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            data-uid='ddd'
            style={{
              position: 'relative',
              width: 300,
              height: 20,
            }}
          />
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })

  it('Insert into first position in flex, column layout, backwards drag', async () => {
    const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
    await startInsertMode(renderResult.dispatch)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 105,
      y: targetElementBounds.y + targetElementBounds.height + 25,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width - 195,
      y: targetElementBounds.y + targetElementBounds.height + 5,
    })

    // Drag starts vertically close to the first position, dragging towards the top left
    await fireDragEvent(canvasControlsLayer, startPoint, endPoint)

    // Check that the inserted element is a sibling of bbb, position is after bbb
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            width: '100%',
            height: '100%',
            backgroundColor: '#FFFFFF',
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            gap: 10,
          }}
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            data-uid='ddd'
            style={{
              position: 'relative',
              width: 300,
              height: 20,
            }}
          />
          <div
            data-uid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
    )
  })
})
