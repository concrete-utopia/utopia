import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = {
    x: targetElementBounds.x + targetElementBounds.width / 2,
    y: targetElementBounds.y + targetElementBounds.height / 2,
  }

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
  })
}

function makeTestProjectCodeWithComponentInnards(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export var App = (props) => {
${componentInnards}
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 2000, height: 2000 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

function makeTestProjectCodeWithSnippet(snippet: string): string {
  return makeTestProjectCodeWithComponentInnards(`
  return (
${snippet}
  )
`)
}

describe('Flow Reparent To Absolute Strategy', () => {
  it('reparents flow element to absolute parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            position: 'absolute',
            width: 700,
            height: 600,
          }}
          data-uid='container'
          data-testid='container'
        >
          <div
            style={{
              position: 'absolute',
              width: 250,
              height: 500,
              left: 0,
              top: 0,
              backgroundColor: 'lightblue',
            }}
            data-uid='absoluteparent'
            data-testid='absoluteparent'
          >
            <div
              style={{
                position: 'absolute',
                left: 93.5,
                top: 58,
                width: 100,
                height: 100,
                backgroundColor: 'yellow',
              }}
              data-uid='absolutechild'
              data-testid='absolutechild'
            />
          </div>
          <div
            style={{
              position: 'absolute',
              width: 250,
              height: 500,
              left: 350,
              top: 0,
              backgroundColor: 'lightgreen',
            }}
            data-uid='flowparent'
            data-testid='flowparent'
          >
            <div
              style={{
                width: 100,
                height: 100,
                backgroundColor: 'teal',
              }}
              data-uid='flowchild1'
              data-testid='flowchild1'
            />
            <div
              style={{
                width: 100,
                height: 100,
                backgroundColor: 'red',
              }}
              data-uid='flowchild2'
              data-testid='flowchild2'
            />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
    const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
    const targetAbsoluteParentCenter = {
      x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
      y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
    }
    const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
    const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
    const firstFlowChildCenter = {
      x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
      y: firstFlowChildRect.y + firstFlowChildRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetAbsoluteParentCenter.x - firstFlowChildCenter.x,
      y: targetAbsoluteParentCenter.y - firstFlowChildCenter.y,
    })
    await dragElement(renderResult, 'flowchild1', dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
  <div
    style={{
      position: 'absolute',
      width: 700,
      height: 600,
    }}
    data-uid='container'
    data-testid='container'
  >
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'lightblue',
      }}
      data-uid='absoluteparent'
      data-testid='absoluteparent'
    >
      <div
        style={{
          position: 'absolute',
          left: 93.5,
          top: 58,
          width: 100,
          height: 100,
          backgroundColor: 'yellow',
        }}
        data-uid='absolutechild'
        data-testid='absolutechild'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
            position: 'absolute',
            left: 0,
            top: 0,
          }}
          data-uid='flowchild1'
          data-testid='flowchild1'
        />
      </div>
    </div>
    <div
      style={{
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flowparent'
      data-testid='flowparent'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-uid='flowchild2'
        data-testid='flowchild2'
      />
    </div>
  </div>`),
    )
  })
  describe('reparent into conditionals', () => {
    it('reparents into conditional when active branch is empty', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              position: 'absolute',
              width: 700,
              height: 600,
            }}
            data-uid='container'
            data-testid='container'
          >
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'lightblue',
              }}
              data-uid='absoluteparent'
              data-testid='absoluteparent'
            >
              <div
                style={{
                  position: 'absolute',
                  left: 93.5,
                  top: 58,
                  width: 100,
                  height: 100,
                  backgroundColor: 'yellow',
                }}
                data-uid='absolutechild'
                data-testid='absolutechild'
              >
              {
                // @utopia/uid=cond
                true ? null : <div data-uid='false-branch' />
              }
              </div>
            </div>
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flowparent'
              data-testid='flowparent'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'teal',
                }}
                data-uid='flowchild1'
                data-testid='flowchild1'
              />
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flowchild2'
                data-testid='flowchild2'
              />
            </div>
          </div>
        `),
        'await-first-dom-report',
      )

      const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
      const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
      const targetAbsoluteParentCenter = {
        x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
        y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
      }
      const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
      const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
      const firstFlowChildCenter = {
        x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
        y: firstFlowChildRect.y + firstFlowChildRect.height / 2,
      }

      await renderResult.getDispatchFollowUpActionsFinished()

      const dragDelta = windowPoint({
        x: targetAbsoluteParentCenter.x - firstFlowChildCenter.x,
        y: targetAbsoluteParentCenter.y - firstFlowChildCenter.y,
      })
      await dragElement(renderResult, 'flowchild1', dragDelta, cmdModifier)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              position: 'absolute',
              width: 700,
              height: 600,
            }}
            data-uid='container'
            data-testid='container'
          >
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'lightblue',
              }}
              data-uid='absoluteparent'
              data-testid='absoluteparent'
            >
              <div
                style={{
                  position: 'absolute',
                  left: 93.5,
                  top: 58,
                  width: 100,
                  height: 100,
                  backgroundColor: 'yellow',
                }}
                data-uid='absolutechild'
                data-testid='absolutechild'
              >
              {
                // @utopia/uid=cond
                true ? (
                  <div
                    style={{
                      width: 100,
                      height: 100,
                      backgroundColor: 'teal',
                      position: 'absolute',
                      left: 0,
                      top: 0,
                    }}
                    data-uid='flowchild1'
                    data-testid='flowchild1'
                  />
                ) : <div data-uid='false-branch' />
              }
              </div>
            </div>
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flowparent'
              data-testid='flowparent'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flowchild2'
                data-testid='flowchild2'
              />
            </div>
          </div>`),
      )
    })
    it('does not reparent into conditional when active branch is not empty', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              position: 'absolute',
              width: 700,
              height: 600,
            }}
            data-uid='container'
            data-testid='container'
          >
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'lightblue',
              }}
              data-uid='absoluteparent'
              data-testid='absoluteparent'
            >
              <div
                style={{
                  position: 'absolute',
                  left: 93.5,
                  top: 58,
                  width: 100,
                  height: 100,
                  backgroundColor: 'yellow',
                }}
                data-uid='absolutechild'
                data-testid='absolutechild'
              >
              {
                // @utopia/uid=cond
                true ? <div data-uid='true-branch' /> : null
              }
              </div>
            </div>
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flowparent'
              data-testid='flowparent'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'teal',
                }}
                data-uid='flowchild1'
                data-testid='flowchild1'
              />
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flowchild2'
                data-testid='flowchild2'
              />
            </div>
          </div>
        `),
        'await-first-dom-report',
      )

      const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
      const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
      const targetAbsoluteParentCenter = {
        x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
        y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
      }
      const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
      const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
      const firstFlowChildCenter = {
        x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
        y: firstFlowChildRect.y + firstFlowChildRect.height / 2,
      }

      await renderResult.getDispatchFollowUpActionsFinished()

      const dragDelta = windowPoint({
        x: targetAbsoluteParentCenter.x - firstFlowChildCenter.x,
        y: targetAbsoluteParentCenter.y - firstFlowChildCenter.y,
      })
      await dragElement(renderResult, 'flowchild1', dragDelta, cmdModifier)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              position: 'absolute',
              width: 700,
              height: 600,
            }}
            data-uid='container'
            data-testid='container'
          >
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'lightblue',
              }}
              data-uid='absoluteparent'
              data-testid='absoluteparent'
            >
              <div
                style={{
                  position: 'absolute',
                  left: 93.5,
                  top: 58,
                  width: 100,
                  height: 100,
                  backgroundColor: 'yellow',
                }}
                data-uid='absolutechild'
                data-testid='absolutechild'
              >
              {
                // @utopia/uid=cond
                true ? <div data-uid='true-branch' /> : null
              }
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'teal',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                }}
                data-uid='flowchild1'
                data-testid='flowchild1'
              />
              </div>
            </div>
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flowparent'
              data-testid='flowparent'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flowchild2'
                data-testid='flowchild2'
              />
            </div>
          </div>`),
      )
    })
    it('respects conditional overrides', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              position: 'absolute',
              width: 700,
              height: 600,
            }}
            data-uid='container'
            data-testid='container'
          >
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'lightblue',
              }}
              data-uid='absoluteparent'
              data-testid='absoluteparent'
            >
              <div
                style={{
                  position: 'absolute',
                  left: 93.5,
                  top: 58,
                  width: 100,
                  height: 100,
                  backgroundColor: 'yellow',
                }}
                data-uid='absolutechild'
                data-testid='absolutechild'
              >
              {
                // @utopia/uid=cond
                // @utopia/conditional=false
                true ? <div data-uid='true-branch' /> : null
              }
              </div>
            </div>
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flowparent'
              data-testid='flowparent'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'teal',
                }}
                data-uid='flowchild1'
                data-testid='flowchild1'
              />
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flowchild2'
                data-testid='flowchild2'
              />
            </div>
          </div>
        `),
        'await-first-dom-report',
      )

      const targetAbsoluteParent = await renderResult.renderedDOM.findByTestId('absolutechild')
      const targetAbsoluteParentRect = targetAbsoluteParent.getBoundingClientRect()
      const targetAbsoluteParentCenter = {
        x: targetAbsoluteParentRect.x + targetAbsoluteParentRect.width / 2,
        y: targetAbsoluteParentRect.y + targetAbsoluteParentRect.height / 2,
      }
      const firstFlowChild = await renderResult.renderedDOM.findByTestId('flowchild1')
      const firstFlowChildRect = firstFlowChild.getBoundingClientRect()
      const firstFlowChildCenter = {
        x: firstFlowChildRect.x + firstFlowChildRect.width / 2,
        y: firstFlowChildRect.y + firstFlowChildRect.height / 2,
      }

      await renderResult.getDispatchFollowUpActionsFinished()

      const dragDelta = windowPoint({
        x: targetAbsoluteParentCenter.x - firstFlowChildCenter.x,
        y: targetAbsoluteParentCenter.y - firstFlowChildCenter.y,
      })
      await dragElement(renderResult, 'flowchild1', dragDelta, cmdModifier)

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              position: 'absolute',
              width: 700,
              height: 600,
            }}
            data-uid='container'
            data-testid='container'
          >
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'lightblue',
              }}
              data-uid='absoluteparent'
              data-testid='absoluteparent'
            >
              <div
                style={{
                  position: 'absolute',
                  left: 93.5,
                  top: 58,
                  width: 100,
                  height: 100,
                  backgroundColor: 'yellow',
                }}
                data-uid='absolutechild'
                data-testid='absolutechild'
              >
              {
                // @utopia/uid=cond
                // @utopia/conditional=false
                true ? (
                  <div data-uid='true-branch' />
                ) : (
                  <div
                    style={{
                      width: 100,
                      height: 100,
                      backgroundColor: 'teal',
                      position: 'absolute',
                      left: 0,
                      top: 0,
                    }}
                    data-uid='flowchild1'
                    data-testid='flowchild1'
                  />
                )
              }
              </div>
            </div>
            <div
              style={{
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flowparent'
              data-testid='flowparent'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flowchild2'
                data-testid='flowchild2'
              />
            </div>
          </div>`),
      )
    })
  })
})
