import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier, shiftModifier } from '../../../../utils/modifiers'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'

const setupAndDrag = async (
  project: string,
  elementId: string,
  x: number,
  y: number,
  modifiers?: Modifiers,
): Promise<EditorRenderResult> => {
  const renderResult = await renderTestEditorWithCode(project, 'await-first-dom-report')

  const targetElement = renderResult.renderedDOM.getByTestId(elementId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = { x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 }
  const dragDelta = { x, y }

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, { modifiers })

  await renderResult.getDispatchFollowUpActionsFinished()
  return renderResult
}

describe('Relative move', () => {
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

      it('in flex parent flex reorder wins', async () => {
        const project = makeTestProjectCodeWithSnippet(`
        <div style={{ display: 'flex', width: '100%', height: '100%' }} data-uid='foo'>
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
          <div
            style={{
              backgroundColor: '#f0f',
              position: 'relative',
              width: 200,
              height: 200,
            }}
            data-uid='bar2'
            data-testid='bar2'
          />
        </div>
      `)

        const renderResult = await renderTestEditorWithCode(project, 'await-first-dom-report')

        const targetElement = renderResult.renderedDOM.getByTestId('bar')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const startPoint = { x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 }

        await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })

        await mouseMoveToPoint(canvasControlsLayer, {
          x: startPoint.x + 10,
          y: startPoint.y + 10,
        })

        const activeStrategy = renderResult.getEditorState().strategyState.currentStrategy
        expect(activeStrategy).toEqual('FLEX_REORDER')
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

      const result = await setupAndDrag(project, 'bar', 100, 15, shiftModifier)
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

  describe('move with a local frame', () => {
    it('ignores the local frame measurements', async () => {
      const project = makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#09f',
              width: 50,
              height: 50
            }}
            data-uid='baz'
          />
          <div
            style={{
              backgroundColor: '#f0f',
              display: 'inline-block',
              position: 'relative',
              width: 50,
              height: 50
            }}
            data-uid='bar'
            data-testid='bar'
          />
          <div
            style={{
              backgroundColor: '#f90',
              display: 'inline-block',
              width: 50,
              height: 50
            }}
            data-uid='qux'
          />
          <div
            style={{
              backgroundColor: '#9f0',
              display: 'inline-block',
              width: 50,
              height: 50
            }}
            data-uid='waldo'
          />
          <div
            style={{
              backgroundColor: '#0f9',
              width: 50,
              height: 50
            }}
            data-uid='corge'
          />
        </div>
      `)

      const result = await setupAndDrag(project, 'bar', 15, 15)

      expect(getPrintedUiJsCode(result.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='foo'>
          <div
            style={{
              backgroundColor: '#09f',
              width: 50,
              height: 50
            }}
            data-uid='baz'
          />
          <div
            style={{
              backgroundColor: '#f0f',
              display: 'inline-block',
              position: 'relative',
              width: 50,
              height: 50,
              left: 15,
              top: 15
            }}
            data-uid='bar'
            data-testid='bar'
          />
          <div
            style={{
              backgroundColor: '#f90',
              display: 'inline-block',
              width: 50,
              height: 50
            }}
            data-uid='qux'
          />
          <div
            style={{
              backgroundColor: '#9f0',
              display: 'inline-block',
              width: 50,
              height: 50
            }}
            data-uid='waldo'
          />
          <div
            style={{
              backgroundColor: '#0f9',
              width: 50,
              height: 50
            }}
            data-uid='corge'
          />
        </div>
        `),
      )
    })
  })
})
