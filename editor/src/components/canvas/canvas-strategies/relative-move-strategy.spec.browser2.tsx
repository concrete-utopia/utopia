import { fireEvent } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import { offsetPoint, windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../ui-jsx.test-utils'

function dragElement(
  canvasControl: HTMLElement,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  shiftPressed: boolean,
) {
  const endPoint = offsetPoint(startPoint, dragDelta)
  fireEvent(
    canvasControl,
    new MouseEvent('mousedown', {
      bubbles: true,
      cancelable: true,
      metaKey: true,
      shiftKey: shiftPressed,
      clientX: startPoint.x,
      clientY: startPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    canvasControl,
    new MouseEvent('mousemove', {
      bubbles: true,
      cancelable: true,
      shiftKey: shiftPressed,
      clientX: endPoint.x,
      clientY: endPoint.y,
      buttons: 1,
    }),
  )

  fireEvent(
    window,
    new MouseEvent('mouseup', {
      bubbles: true,
      cancelable: true,
      shiftKey: shiftPressed,
      clientX: endPoint.x,
      clientY: endPoint.y,
    }),
  )
}

const setupAndDrag = async (
  project: string,
  elementId: string,
  x: number,
  y: number,
  modifiers?: { shiftPressed?: boolean },
): Promise<EditorRenderResult> => {
  const renderResult = await renderTestEditorWithCode(project, 'await-first-dom-report')
  return drag(renderResult, elementId, x, y, modifiers)
}

const drag = async (
  renderResult: EditorRenderResult,
  elementId: string,
  x: number,
  y: number,
  modifiers?: { shiftPressed?: boolean },
): Promise<EditorRenderResult> => {
  const targetElement = renderResult.renderedDOM.getByTestId(elementId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const dragDelta = windowPoint({ x, y })

  act(() => dragElement(canvasControlsLayer, startPoint, dragDelta, !!modifiers?.shiftPressed))

  await renderResult.getDispatchFollowUpActionsFinished()
  return renderResult
}

describe('Relative move', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  describe('when the element position is relative', () => {
    it('does not trigger when the threshold is not met', async () => {
      const project = makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#f0f',
              position: 'relative',
              width: 200,
              height: 200,
            }}
            data-uid='bar'
            data-testid='bar'
          />
        </div>
      `)

      const result = await setupAndDrag(project, 'bar', 1, 1)

      expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
            <div
              style={{
                backgroundColor: '#f0f',
                position: 'relative',
                width: 200,
                height: 200,
              }}
              data-uid='bar'
              data-testid='bar'
            />
          </div>
        `),
      )
    })

    describe('when the element has no offsets', () => {
      it('sets the TL offsets', async () => {
        const project = makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#f0f',
              position: 'relative',
              width: 200,
              height: 200,
            }}
            data-uid='bar'
            data-testid='bar'
          />
        </div>
      `)

        const result = await setupAndDrag(project, 'bar', 15, 15)

        expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
            <div
              style={{
                backgroundColor: '#f0f',
                position: 'relative',
                width: 200,
                height: 200,
                left: 15,
                top: 15
              }}
              data-uid='bar'
              data-testid='bar'
            />
          </div>
        `),
        )
      })
    })

    describe('when the element has offsets', () => {
      it('updates the offsets', async () => {
        const project = makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#f0f',
              position: 'relative',
              width: 200,
              height: 200,
              left: 10,
              top: 10
            }}
            data-uid='bar'
            data-testid='bar'
          />
        </div>
      `)

        const result = await setupAndDrag(project, 'bar', 15, 15)

        expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
            <div
              style={{
                backgroundColor: '#f0f',
                position: 'relative',
                width: 200,
                height: 200,
                left: 25,
                top: 25
              }}
              data-uid='bar'
              data-testid='bar'
            />
          </div>
        `),
        )
      })

      describe('when vertical or horizontal offsets are missing', () => {
        it('sets the missing offsets', async () => {
          const project = makeTestProjectCodeWithSnippet(`
            <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
              <div
                style={{
                  backgroundColor: '#f0f',
                  position: 'relative',
                  width: 200,
                  height: 200,
                  top: 10
                }}
                data-uid='bar'
                data-testid='bar'
              />
            </div>
          `)

          const result = await setupAndDrag(project, 'bar', 15, 15)

          expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                <div
                  style={{
                    backgroundColor: '#f0f',
                    position: 'relative',
                    width: 200,
                    height: 200,
                    top: 25,
                    left: 15
                  }}
                  data-uid='bar'
                  data-testid='bar'
                />
              </div>
            `),
          )
        })
      })

      describe('TLBR behavior', () => {
        describe('honoring explicitly defined properties', () => {
          it('right', async () => {
            const project = makeTestProjectCodeWithSnippet(`
              <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                <div
                  style={{
                    backgroundColor: '#f0f',
                    position: 'relative',
                    width: 200,
                    height: 200,
                    top: 100,
                    right: -20
                  }}
                  data-uid='bar'
                  data-testid='bar'
                />
              </div>
            `)

            const result = await setupAndDrag(project, 'bar', 15, 15)

            expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
              makeTestProjectCodeWithSnippet(`
                <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                  <div
                    style={{
                      backgroundColor: '#f0f',
                      position: 'relative',
                      width: 200,
                      height: 200,
                      top: 115,
                      right: -35
                    }}
                    data-uid='bar'
                    data-testid='bar'
                  />
                </div>
              `),
            )
          })
          it('bottom', async () => {
            const project = makeTestProjectCodeWithSnippet(`
              <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                <div
                  style={{
                    backgroundColor: '#f0f',
                    position: 'relative',
                    width: 200,
                    height: 200,
                    bottom: -10,
                    left: 25
                  }}
                  data-uid='bar'
                  data-testid='bar'
                />
              </div>
            `)

            const result = await setupAndDrag(project, 'bar', 15, 15)

            expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
              makeTestProjectCodeWithSnippet(`
                <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                  <div
                    style={{
                      backgroundColor: '#f0f',
                      position: 'relative',
                      width: 200,
                      height: 200,
                      bottom: -25,
                      left: 40
                    }}
                    data-uid='bar'
                    data-testid='bar'
                  />
                </div>
              `),
            )
          })
          it('mixed', async () => {
            const project = makeTestProjectCodeWithSnippet(`
              <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                <div
                  style={{
                    backgroundColor: '#f0f',
                    position: 'relative',
                    width: 200,
                    height: 200,
                    bottom: 10,
                    left: 25,
                    top: 10
                  }}
                  data-uid='bar'
                  data-testid='bar'
                />
              </div>
            `)

            const result = await setupAndDrag(project, 'bar', 15, 15)

            expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
              makeTestProjectCodeWithSnippet(`
                <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
                  <div
                    style={{
                      backgroundColor: '#f0f',
                      position: 'relative',
                      width: 200,
                      height: 200,
                      bottom: -5,
                      left: 40,
                      top: 25
                    }}
                    data-uid='bar'
                    data-testid='bar'
                  />
                </div>
              `),
            )
          })
        })
      })
    })
  })

  describe('when the element position is absolute', () => {
    it('does not change it', async () => {
      const project = makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#f0f',
              position: 'absolute',
              width: 200,
              height: 200
            }}
            data-uid='bar'
            data-testid='bar'
          />
        </div>
      `)

      const result = await setupAndDrag(project, 'bar', 15, 15)

      expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
            <div
              style={{
                backgroundColor: '#f0f',
                position: 'absolute',
                width: 200,
                height: 200,
                left: 15,
                top: 15
              }}
              data-uid='bar'
              data-testid='bar'
            />
          </div>
        `),
      )
    })
  })

  describe('honors drag modifiers', () => {
    it('shift', async () => {
      const project = makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#f0f',
              position: 'relative',
              width: 200,
              height: 200,
              top: 10,
              left: 15
            }}
            data-uid='bar'
            data-testid='bar'
          />
        </div>
      `)

      const result = await setupAndDrag(project, 'bar', 100, 15, { shiftPressed: true })
      expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
            <div
              style={{
                backgroundColor: '#f0f',
                position: 'relative',
                width: 200,
                height: 200,
                top: 10,
                left: 115
              }}
              data-uid='bar'
              data-testid='bar'
            />
          </div>
        `),
      )
    })
  })
})
