import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier } from '../../../../utils/modifiers'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { navigatorEntryToKey } from '../../../../components/editor/store/editor-state'
import { getNavigatorTargetsFromEditorState } from '../../../navigator/navigator-utils'

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

const defaultTestCode = `
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
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 0,
        top: 0,
        backgroundColor: 'blue',
      }}
      data-uid='flexparent1'
      data-testid='flexparent1'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'purple',
        }}
        data-uid='flexchild1'
        data-testid='flexchild1'
      />
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'pink',
        }}
        data-uid='flexchild2'
        data-testid='flexchild2'
      />
    </div>
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        position: 'absolute',
        width: 250,
        height: 500,
        left: 350,
        top: 0,
        backgroundColor: 'lightgreen',
      }}
      data-uid='flexparent2'
      data-testid='flexparent2'
    >
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'teal',
        }}
        data-uid='flexchild3'
        data-testid='flexchild3'
      />
      <div
        style={{
          width: 100,
          height: 100,
          backgroundColor: 'red',
        }}
        data-uid='flexchild4'
        data-testid='flexchild4'
      />
    </div>
  </div>
`

const TestProjectWithFragment = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 52,
        top: 480,
        width: 669,
        height: 338,
        display: 'flex',
        gap: 33,
      }}
      data-uid='parent2'
      data-testid='parent2'
    >
      <div
        style={{
          backgroundColor: '#ff0000',
          width: 156,
          height: 184,
          contain: 'layout',
        }}
        data-uid='0b5'
      />

      <div
        style={{
          backgroundColor: '#00abff',
          width: 123,
          height: 215,
          contain: 'layout',
        }}
        data-uid='aaa'
      />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 47,
        top: 89,
        width: 669,
        height: 338,
        display: 'flex',
        gap: 33,
      }}
      data-uid='parent1'
      data-testid='parent1'
    >
      <div
        style={{
          backgroundColor: '#ff0000',
          width: 156,
          height: 184,
          contain: 'layout',
        }}
        data-uid='aac'
      />
      <React.Fragment data-uid='fragment'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 94,
            height: 171,
            contain: 'layout',
          }}
          data-uid='b71'
          data-testid='fragment-child1'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 156,
            height: 184,
            contain: 'layout',
          }}
          data-uid='050'
        />
      </React.Fragment>
      <div
        style={{
          backgroundColor: '#00abff',
          width: 123,
          height: 215,
          contain: 'layout',
        }}
        data-uid='aad'
      />
    </div>
  </Storyboard>
)
`

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

describe('Flex Reparent To Flex Strategy', () => {
  it('reparents flex element to other flex parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const targetFlexParent = await renderResult.renderedDOM.findByTestId('flexparent1')
    const targetFlexParentRect = targetFlexParent.getBoundingClientRect()
    const targetFlexParentEnd = {
      x: targetFlexParentRect.x + targetFlexParentRect.width - 15,
      y: targetFlexParentRect.y + targetFlexParentRect.height / 2,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexParentEnd.x - flexChildToReparentCenter.x + 5,
      y: targetFlexParentEnd.y - flexChildToReparentCenter.y,
    })
    await dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier)

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
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 0,
          top: 0,
          backgroundColor: 'blue',
        }}
        data-uid='flexparent1'
        data-testid='flexparent1'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'purple',
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'pink',
          }}
          data-uid='flexchild2'
          data-testid='flexchild2'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
          }}
          data-uid='flexchild3'
          data-testid='flexchild3'
        />
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })

  describe('with fragments', () => {
    it('reparents fragment to other flex parent', async () => {
      const renderResult = await renderTestEditorWithCode(
        TestProjectWithFragment,
        'await-first-dom-report',
      )

      const targetFlexParent = await renderResult.renderedDOM.findByTestId('parent2')
      const targetFlexParentRect = targetFlexParent.getBoundingClientRect()
      const targetFlexParentEnd = {
        x: targetFlexParentRect.x + targetFlexParentRect.width - 15,
        y: targetFlexParentRect.y + targetFlexParentRect.height / 2,
      }

      const flexChildToReparent = await renderResult.renderedDOM.findByTestId('fragment-child1')
      const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
      const flexChildToReparentCenter = {
        x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
        y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
      }

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/parent1/fragment`),
      ])

      const dragDelta = windowPoint({
        x: targetFlexParentEnd.x - flexChildToReparentCenter.x + 5,
        y: targetFlexParentEnd.y - flexChildToReparentCenter.y,
      })

      await mouseDragFromPointWithDelta(canvasControlsLayer, flexChildToReparentCenter, dragDelta, {
        modifiers: cmdModifier,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const navigatorTargets = getNavigatorTargetsFromEditorState(
        renderResult.getEditorState().editor,
      ).visibleNavigatorTargets.map(navigatorEntryToKey)
      expect(navigatorTargets).toEqual([
        'regular-utopia-storyboard-uid/parent2',
        'regular-utopia-storyboard-uid/parent2/0b5',
        'regular-utopia-storyboard-uid/parent2/aaa',
        'regular-utopia-storyboard-uid/parent2/fragment', // <- fragment and its contents are reparented to parent2 from parent1
        'regular-utopia-storyboard-uid/parent2/fragment/b71',
        'regular-utopia-storyboard-uid/parent2/fragment/050',
        'regular-utopia-storyboard-uid/parent1',
        'regular-utopia-storyboard-uid/parent1/aac',
        'regular-utopia-storyboard-uid/parent1/aad',
      ])
    })
  })

  it('reparents flex as first child when moving the mouse to the left edge of the first child', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(defaultTestCode),
      'await-first-dom-report',
    )

    const targetFlexChild = await renderResult.renderedDOM.findByTestId('flexchild1')
    const targetFlexChildRect = targetFlexChild.getBoundingClientRect()
    const targetFlexChildCenter = {
      x: targetFlexChildRect.x + 10,
      y: targetFlexChildRect.y + targetFlexChildRect.height / 2,
    }
    const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
    const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
    const flexChildToReparentCenter = {
      x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
      y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
    }

    await renderResult.getDispatchFollowUpActionsFinished()
    const dragDelta = windowPoint({
      x: targetFlexChildCenter.x - flexChildToReparentCenter.x - 5,
      y: targetFlexChildCenter.y - flexChildToReparentCenter.y,
    })
    await dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier)

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
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 0,
          top: 0,
          backgroundColor: 'blue',
        }}
        data-uid='flexparent1'
        data-testid='flexparent1'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'teal',
          }}
          data-uid='flexchild3'
          data-testid='flexchild3'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'purple',
          }}
          data-uid='flexchild1'
          data-testid='flexchild1'
        />
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'pink',
          }}
          data-uid='flexchild2'
          data-testid='flexchild2'
        />
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          position: 'absolute',
          width: 250,
          height: 500,
          left: 350,
          top: 0,
          backgroundColor: 'lightgreen',
        }}
        data-uid='flexparent2'
        data-testid='flexparent2'
      >
        <div
          style={{
            width: 100,
            height: 100,
            backgroundColor: 'red',
          }}
          data-uid='flexchild4'
          data-testid='flexchild4'
        />
      </div>
    </div>
  `),
    )
  })

  describe('conditionals', () => {
    it('reparents into conditional, but with absolute positioning', async () => {
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
                display: 'flex',
                flexDirection: 'row',
                position: 'absolute',
                width: 250,
                height: 500,
                left: 0,
                top: 0,
                backgroundColor: 'blue',
              }}
              data-uid='flexparent1'
              data-testid='flexparent1'
            >
              {
                // @utopia/uid=cond
                true ? null : <div data-uid='false-branch' />
              }
            </div>
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                position: 'absolute',
                width: 250,
                height: 500,
                left: 350,
                top: 0,
                backgroundColor: 'lightgreen',
              }}
              data-uid='flexparent2'
              data-testid='flexparent2'
            >
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'teal',
                }}
                data-uid='flexchild3'
                data-testid='flexchild3'
              />
              <div
                style={{
                  width: 100,
                  height: 100,
                  backgroundColor: 'red',
                }}
                data-uid='flexchild4'
                data-testid='flexchild4'
              />
            </div>
          </div>
        `),
        'await-first-dom-report',
      )

      const targetFlexParent = await renderResult.renderedDOM.findByTestId('flexparent1')
      const targetFlexParentRect = targetFlexParent.getBoundingClientRect()
      const targetFlexParentEnd = {
        x: targetFlexParentRect.x + targetFlexParentRect.width - 15,
        y: targetFlexParentRect.y + targetFlexParentRect.height / 2,
      }
      const flexChildToReparent = await renderResult.renderedDOM.findByTestId('flexchild3')
      const flexChildToReparentRect = flexChildToReparent.getBoundingClientRect()
      const flexChildToReparentCenter = {
        x: flexChildToReparentRect.x + flexChildToReparentRect.width / 2,
        y: flexChildToReparentRect.y + flexChildToReparentRect.height / 2,
      }

      await renderResult.getDispatchFollowUpActionsFinished()
      const dragDelta = windowPoint({
        x: targetFlexParentEnd.x - flexChildToReparentCenter.x + 5,
        y: targetFlexParentEnd.y - flexChildToReparentCenter.y,
      })
      await dragElement(renderResult, 'flexchild3', dragDelta, cmdModifier)

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
              display: 'flex',
              flexDirection: 'row',
              position: 'absolute',
              width: 250,
              height: 500,
              left: 0,
              top: 0,
              backgroundColor: 'blue',
            }}
            data-uid='flexparent1'
            data-testid='flexparent1'
          >
            {
              // @utopia/uid=cond
              true ? (
                <div
                  style={{
                    width: 100,
                    height: 100,
                    backgroundColor: 'teal',
                  }}
                  data-uid='flexchild3'
                  data-testid='flexchild3'
                />
              ) : <div data-uid='false-branch' />
            }
          </div>
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              position: 'absolute',
              width: 250,
              height: 500,
              left: 350,
              top: 0,
              backgroundColor: 'lightgreen',
            }}
            data-uid='flexparent2'
            data-testid='flexparent2'
          >
            <div
              style={{
                width: 100,
                height: 100,
                backgroundColor: 'red',
              }}
              data-uid='flexchild4'
              data-testid='flexchild4'
            />
          </div>
        </div>
      `),
      )
    })
  })
})
