/* eslint-disable jest/expect-expect */
import { act } from 'react-dom/test-utils'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { WindowPoint, windowPoint, offsetPoint } from '../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../utils/modifiers'
import { selectComponents } from '../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import {
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
} from '../event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
  TestSceneUID,
} from '../ui-jsx.test-utils'

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  mouseDownOffset: WindowPoint,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  includeMouseUp: boolean,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({
    x: targetElementBounds.x + mouseDownOffset.x,
    y: targetElementBounds.y + mouseDownOffset.y,
  })
  const endPoint = offsetPoint(startPoint, dragDelta)

  if (includeMouseUp) {
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
      modifiers: modifiers,
    })
  } else {
    mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: modifiers })
    mouseMoveToPoint(canvasControlsLayer, endPoint, {
      modifiers: modifiers,
      eventOptions: { buttons: 1 },
    })
  }
}

const defaultMouseDownOffset = windowPoint({ x: 20, y: 20 })

describe('Unified Reparent Fitness Function Tests', () => {
  it('if an element is larger than its parent, we still allow reparent to its grandparent, if the reparenting starts from the area of the original parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 200,
                height: 200,
              }}
              data-uid='ccc'
              data-testid='ccc'
            />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 120, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)
    dragElement(renderResult, 'ccc', defaultMouseDownOffset, dragDelta, emptyModifiers, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 160,
              top: 40,
              width: 200,
              height: 200,
            }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })

  it('if an element is larger than its parent, we still allow reparent to its grandparent, if the reparenting starts from the area of the original parent, even if the original parent is not a viable parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 200,
                height: 200,
              }}
              data-uid='ccc'
              data-testid='ccc'
            />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 120, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(renderResult, 'ccc', defaultMouseDownOffset, dragDelta, emptyModifiers, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              left: 40,
              top: 40,
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 120,
              top: 0,
              width: 200,
              height: 200,
            }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })

  it('if a target parent is smaller than the dragged element, we do not offer reparenting into it without cmd', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 50,
              top: 50,
              width: 50,
              height: 50,
            }}
            data-uid='targetparent'
            data-testid='targetparent'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 100,
              height: 100,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 60, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'draggedElement'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(
      renderResult,
      'draggedElement',
      defaultMouseDownOffset,
      dragDelta,
      emptyModifiers,
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          width: '100%',
          height: '100%',
        }}
        data-uid='aaa'
      >
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 50,
            width: 50,
            height: 50,
          }}
          data-uid='targetparent'
          data-testid='targetparent'
        />
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 60,
            top: 0,
            width: 100,
            height: 100,
          }}
          data-uid='draggedElement'
          data-testid='draggedElement'
        />
      </div>
    `),
    )
  })

  it('if a target parent is smaller than the dragged element, we allow reparenting into it with cmd', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 50,
              top: 0,
              width: 50,
              height: 50,
            }}
            data-uid='targetparent'
            data-testid='targetparent'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 100,
              height: 100,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 60, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'draggedElement'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(
      renderResult,
      'draggedElement',
      defaultMouseDownOffset,
      dragDelta,
      cmdModifier,
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          width: '100%',
          height: '100%',
        }}
        data-uid='aaa'
      >
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 0,
            width: 50,
            height: 50,
          }}
          data-uid='targetparent'
          data-testid='targetparent'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 10,
              top: 0,
              width: 100,
              height: 100,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      </div>
    `),
    )
  })

  it('ignores elements under the mouse at drag start', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 100,
              height: 100,
            }}
            data-uid='targetparent'
            data-testid='targetparent'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 10, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'draggedElement'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(
      renderResult,
      'draggedElement',
      defaultMouseDownOffset,
      dragDelta,
      emptyModifiers,
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 100,
              height: 100,
            }}
            data-uid='targetparent'
            data-testid='targetparent'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 10,
              top: 0,
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
    `),
    )
  })

  it('in a flex context, dragging over a sibling without cmd means reorder', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
            display: 'flex',
            flexDirection: 'row',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    // we only slightly drag over the sibling, definitely not reach the mid-point
    const dragDelta = windowPoint({ x: -25, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'draggedElement'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(
      renderResult,
      'draggedElement',
      defaultMouseDownOffset,
      dragDelta,
      emptyModifiers,
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
            display: 'flex',
            flexDirection: 'row',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })

  it('in a flex context, dragging over a sibling holding cmd means reparent to sibling', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
            display: 'flex',
            flexDirection: 'row',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    // we only slightly drag over the sibling, definitely not reach the mid-point, but beyond the "flex reorder zone"
    const dragDelta = windowPoint({ x: -35, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'draggedElement'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(
      renderResult,
      'draggedElement',
      defaultMouseDownOffset,
      dragDelta,
      cmdModifier,
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
            display: 'flex',
            flexDirection: 'row',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              position: 'relative',
              backgroundColor: '#0091FFAA',
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
            data-testid='bbb'
          >
            <div
              style={{
                position: 'absolute',
                backgroundColor: '#0091FFAA',
                width: 50,
                height: 50,
                left: 65,
                top: 0,
              }}
              data-uid='draggedElement'
              data-testid='draggedElement'
            />
          </div>
        </div>
      `),
    )
  })

  it('in a flex context, dragging over a sibling holding cmd, but only dragging to the edge of the sibling means reorder', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
            display: 'flex',
            flexDirection: 'row',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    // we only slightly drag over the sibling, definitely not reach the mid-point
    const dragDelta = windowPoint({ x: -25, y: 0 })

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'draggedElement'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    dragElement(
      renderResult,
      'draggedElement',
      defaultMouseDownOffset,
      dragDelta,
      cmdModifier,
      true,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: 'white',
            position: 'absolute',
            width: '100%',
            height: '100%',
            display: 'flex',
            flexDirection: 'row',
          }}
          data-uid='aaa'
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
              width: 50,
              height: 50,
            }}
            data-uid='draggedElement'
            data-testid='draggedElement'
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              width: 100,
              height: 100,
            }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
})

describe('Target parent filtering', () => {
  function makeFilteringProjectWithCode(code: string): string {
    const projectCode = `
      import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'

      export var ${BakedInStoryboardVariableName} = (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene
            style={{
              width: 350,
              height: 350,
              position: 'absolute',
              left: 0,
              top: 0,
            }}
            data-uid='${TestSceneUID}'
          >
            ${code}
          </Scene>
        </Storyboard>
      )
    `

    return formatTestProjectCode(projectCode)
  }

  // Target element overlaps both smaller and larger elements, starting 25px to the right and below the top left of the larger,
  // and 25px to the left and above the smaller
  const startingCode = makeFilteringProjectWithCode(`
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 50,
        top: 50,
        width: 250,
        height: 250,
      }}
      data-label='larger sibling'
      data-uid='large'
      data-testid='large'
    >
      <div
        style={{
          backgroundColor: '#11FF00',
          position: 'absolute',
          left: 50,
          top: 50,
          width: 150,
          height: 150,
        }}
        data-label='smaller element'
        data-uid='small'
        data-testid='small'
      />
    </div>
    <div
      style={{
        backgroundColor: '#FF0000AB',
        position: 'absolute',
        left: 75,
        top: 75,
        width: 200,
        height: 200,
      }}
      data-label='drag me'
      data-uid='dragme'
      data-testid='dragme'
    />
  `)

  it('Dragging with cmd with cursor over the larger element prevents reparenting into the larger element', async () => {
    const renderResult = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, 'dragme']])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    // mouse down inside the target, but not in the area over the smaller element
    const mouseDownOffset = windowPoint({ x: 10, y: 10 })
    // drag to the left so that the cursor is over the larger element, and no longer over where the target started
    const dragDelta = windowPoint({ x: -20, y: 0 })

    dragElement(renderResult, 'dragme', mouseDownOffset, dragDelta, cmdModifier, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    // The result should be that we've just moved the element without reparenting it
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeFilteringProjectWithCode(`
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 50,
            width: 250,
            height: 250,
          }}
          data-label='larger sibling'
          data-uid='large'
          data-testid='large'
        >
          <div
            style={{
              backgroundColor: '#11FF00',
              position: 'absolute',
              left: 50,
              top: 50,
              width: 150, 
              height: 150,
            }}
            data-label='smaller element'
            data-uid='small'
            data-testid='small'
          />
        </div>
        <div
          style={{
            backgroundColor: '#FF0000AB',
            position: 'absolute',
            left: 55,
            top: 75,
            width: 200,
            height: 200,
          }}
          data-label='drag me'
          data-uid='dragme'
          data-testid='dragme'
        />
      `),
    )
  })

  it('Dragging with cmd with cursor over the larger element allows reparenting into the smaller element', async () => {
    const renderResult = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, 'dragme']])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    // mouse down inside the target, but not in the area over the smaller element
    const mouseDownOffset = windowPoint({ x: 10, y: 10 })
    // drag to the right so that the cursor is over the smaller element
    const dragDelta = windowPoint({ x: 20, y: 20 })

    dragElement(renderResult, 'dragme', mouseDownOffset, dragDelta, cmdModifier, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    // The dragged element should have been reparented into the smaller element
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeFilteringProjectWithCode(`
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 50,
            width: 250,
            height: 250,
          }}
          data-label='larger sibling'
          data-uid='large'
          data-testid='large'
        >
          <div
            style={{
              backgroundColor: '#11FF00',
              position: 'absolute',
              left: 50,
              top: 50,
              width: 150, 
              height: 150,
            }}
            data-label='smaller element'
            data-uid='small'
            data-testid='small'
          >
            <div
              style={{
                backgroundColor: '#FF0000AB',
                position: 'absolute',
                left: -5,
                top: -5,
                width: 200,
                height: 200,
              }}
              data-label='drag me'
              data-uid='dragme'
              data-testid='dragme'
            />
          </div>
        </div>
      `),
    )
  })

  it('Dragging with cmd with cursor over the smaller element prevents reparenting into the smaller element', async () => {
    const renderResult = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, 'dragme']])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    // mouse down inside the target in the area over the smaller element
    const mouseDownOffset = windowPoint({ x: 30, y: 30 })
    // drag to the right so that the cursor remains over the smaller element
    const dragDelta = windowPoint({ x: 10, y: 10 })

    dragElement(renderResult, 'dragme', mouseDownOffset, dragDelta, cmdModifier, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    // The result should be that we've just moved the element without reparenting it
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeFilteringProjectWithCode(`
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 50,
            width: 250,
            height: 250,
          }}
          data-label='larger sibling'
          data-uid='large'
          data-testid='large'
        >
          <div
            style={{
              backgroundColor: '#11FF00',
              position: 'absolute',
              left: 50,
              top: 50,
              width: 150, 
              height: 150,
            }}
            data-label='smaller element'
            data-uid='small'
            data-testid='small'
          />
        </div>
        <div
          style={{
            backgroundColor: '#FF0000AB',
            position: 'absolute',
            left: 85,
            top: 85,
            width: 200,
            height: 200,
          }}
          data-label='drag me'
          data-uid='dragme'
          data-testid='dragme'
        />
      `),
    )
  })

  it('Dragging with cmd with cursor over the smaller element allows reparenting into the larger element', async () => {
    const renderResult = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, 'dragme']])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    // mouse down inside the target in the area over the smaller element
    const mouseDownOffset = windowPoint({ x: 30, y: 30 })
    // drag to the left so that the cursor is over the larger element
    const dragDelta = windowPoint({ x: -10, y: -10 })

    dragElement(renderResult, 'dragme', mouseDownOffset, dragDelta, cmdModifier, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    // The dragged element should have been reparented into the larger element
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeFilteringProjectWithCode(`
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 50,
            width: 250,
            height: 250,
          }}
          data-label='larger sibling'
          data-uid='large'
          data-testid='large'
        >
          <div
            style={{
              backgroundColor: '#11FF00',
              position: 'absolute',
              left: 50,
              top: 50,
              width: 150, 
              height: 150,
            }}
            data-label='smaller element'
            data-uid='small'
            data-testid='small'
          />
          <div
            style={{
              backgroundColor: '#FF0000AB',
              position: 'absolute',
              left: 15,
              top: 15,
              width: 200,
              height: 200,
            }}
            data-label='drag me'
            data-uid='dragme'
            data-testid='dragme'
          />
        </div>
      `),
    )
  })

  it('Dragging without cmd with cursor over the smaller element prevents reparenting into the larger element', async () => {
    const renderResult = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, 'dragme']])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    // mouse down inside the target in the area over the smaller element
    const mouseDownOffset = windowPoint({ x: 30, y: 30 })
    // drag to the left so that the cursor is over the larger element
    const dragDelta = windowPoint({ x: -10, y: -10 })

    dragElement(renderResult, 'dragme', mouseDownOffset, dragDelta, emptyModifiers, true)

    await renderResult.getDispatchFollowUpActionsFinished()

    // The result should be that we've just moved the element without reparenting it
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeFilteringProjectWithCode(`
        <div
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 50,
            top: 50,
            width: 250,
            height: 250,
          }}
          data-label='larger sibling'
          data-uid='large'
          data-testid='large'
        >
          <div
            style={{
              backgroundColor: '#11FF00',
              position: 'absolute',
              left: 50,
              top: 50,
              width: 150, 
              height: 150,
            }}
            data-label='smaller element'
            data-uid='small'
            data-testid='small'
          />
        </div>
        <div
          style={{
            backgroundColor: '#FF0000AB',
            position: 'absolute',
            left: 65,
            top: 65,
            width: 200,
            height: 200,
          }}
          data-label='drag me'
          data-uid='dragme'
          data-testid='dragme'
        />
      `),
    )
  })
})

function getVariedProjectCodeWithAFlexContainer(flexDirection: string): string {
  return `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Button = () => {
  return (
    <div
      data-uid='buttondiv'
      data-testid='buttondiv'
      data-label='buttondiv'
      style={{
        width: 100,
        height: 30,
        backgroundColor: 'pink',
      }}
    >
      BUTTON
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 241,
            top: 142,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
        <div
          style={{
            backgroundColor: '#0091FFAA',
            height: 111,
            width: 140,
            position: 'absolute',
            left: 210,
            top: 346,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          <Button
            data-uid='button'
            data-testid='button'
            data-label='button'
          />
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          display: 'flex',
          flexDirection: '${flexDirection}',
          gap: '50px',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'relative',
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'relative',
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
}

async function checkReparentIndicator(
  renderResult: EditorRenderResult,
  expectedLeft: number,
  expectedTop: number,
  expectedWidth: number,
  expectedHeight: number,
): Promise<void> {
  const element = await renderResult.renderedDOM.findByTestId('flex-reparent-indicator-0')
  const bounds = element.getBoundingClientRect()
  expect(bounds.left).toEqual(expectedLeft)
  expect(bounds.top).toEqual(expectedTop)
  expect(bounds.width).toEqual(expectedWidth)
  expect(bounds.height).toEqual(expectedHeight)
}

describe('Reparent indicators', () => {
  it(`shows the reparent indicator before all the elements in a 'row' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    dragElement(renderResult, 'seconddiv', defaultMouseDownOffset, dragDelta, emptyModifiers, false)

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 431, 636, 2, 161.5)
  })

  it(`shows the reparent indicator before all the elements in a 'row-reverse' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row-reverse'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x + flexContainerBounds.width - 2,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    dragElement(renderResult, 'seconddiv', defaultMouseDownOffset, dragDelta, emptyModifiers, false)

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 831, 636, 2, 161.5)
  })

  it(`shows the reparent indicator between two elements in a 'row' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x + 130,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    dragElement(renderResult, 'seconddiv', defaultMouseDownOffset, dragDelta, emptyModifiers, false)

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 565, 636, 2, 123)
  })

  it(`shows the reparent indicator between two elements in a 'row-reverse' container`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getVariedProjectCodeWithAFlexContainer('row-reverse'),
      'await-first-dom-report',
    )

    // Select the target first.
    const targetPath = EP.fromString('storyboard/scene/sceneroot/seconddiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    // Start dragging the target.
    const targetElement = renderResult.renderedDOM.getByTestId('seconddiv')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const flexContainer = renderResult.renderedDOM.getByTestId('parentsibling')
    const flexContainerBounds = flexContainer.getBoundingClientRect()

    const startPoint = { x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 }
    const endPoint = {
      x: flexContainerBounds.x + flexContainerBounds.width - 130,
      y: flexContainerBounds.y + flexContainerBounds.height / 2,
    }
    const dragDelta = windowPoint({
      x: endPoint.x - startPoint.x,
      y: endPoint.y - startPoint.y,
    })

    dragElement(renderResult, 'seconddiv', defaultMouseDownOffset, dragDelta, emptyModifiers, false)

    await renderResult.getDispatchFollowUpActionsFinished()

    // Check the indicator presence and position.
    await checkReparentIndicator(renderResult, 697, 636, 2, 123)
  })
})
