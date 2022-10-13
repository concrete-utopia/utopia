import { setRightMenuTab } from '../../../editor/actions/action-creators'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
  EditorRenderResult,
  TestSceneUID,
  TestAppUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import * as EP from '../../../../core/shared/element-path'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointToPointNoMouseDown,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import { RightMenuTab } from '../../../editor/store/editor-state'
import { FOR_TESTS_setNextGeneratedUid } from '../../../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { CanvasRectangle } from '../../../../core/shared/math-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

// FIXME These tests will probably start to fail if the insert menu becomes too long, at which point we may
// have to insert some mocking to restrict the available items there

function slightlyOffsetWindowPointBecauseVeryWeirdIssue(point: { x: number; y: number }) {
  // FIXME when running in headless chrome, the result of getBoundingClientRect will be slightly
  // offset for some unknown reason, meaning the inserted element will be 1 pixel of in each dimension
  return { x: point.x - 0.001, y: point.y - 0.001 }
}

async function setupInsertTest(inputCode: string): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

  const newUID = 'ddd'
  FOR_TESTS_setNextGeneratedUid(newUID)

  return renderResult
}

async function enterInsertModeFromInsertMenu(
  renderResult: EditorRenderResult,
  elementType: 'div' | 'img' = 'div',
) {
  const insertButton = renderResult.renderedDOM.getByTestId(`insert-item-${elementType}`)
  const insertButtonBounds = insertButton.getBoundingClientRect()

  const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
    x: insertButtonBounds.x + insertButtonBounds.width / 2,
    y: insertButtonBounds.y + insertButtonBounds.height / 2,
  })

  mouseMoveToPoint(insertButton, point)
  mouseClickAtPoint(insertButton, point)

  await renderResult.getDispatchFollowUpActionsFinished()
}

async function enterInsertModeFromInsertMenuStartDrag(renderResult: EditorRenderResult) {
  const insertButton = renderResult.renderedDOM.getByTestId('insert-item-div')
  const insertButtonBounds = insertButton.getBoundingClientRect()

  const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
    x: insertButtonBounds.x + insertButtonBounds.width / 2,
    y: insertButtonBounds.y + insertButtonBounds.height / 2,
  })

  mouseMoveToPoint(insertButton, point)
  mouseDownAtPoint(insertButton, point)

  await renderResult.getDispatchFollowUpActionsFinished()
}

function isIndicatorBeforeSiblingBBB(
  metadata: ElementInstanceMetadataMap,
  reparentLine: CanvasRectangle,
): boolean {
  const targetSibling = EP.fromString(
    `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
  )
  const targetParent = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa`)

  const parentFrame = MetadataUtils.getFrameInCanvasCoords(targetParent, metadata)
  const nextSiblingFrame = MetadataUtils.getFrameInCanvasCoords(targetSibling, metadata)

  if (parentFrame == null || nextSiblingFrame == null) {
    return false
  } else {
    return (
      reparentLine.x >= parentFrame.x &&
      reparentLine.y >= parentFrame.y &&
      reparentLine.x <= nextSiblingFrame.x &&
      reparentLine.y <= nextSiblingFrame.y
    )
  }
}

function isIndicatorBetweenSiblingsBBBCCC(
  metadata: ElementInstanceMetadataMap,
  reparentLine: CanvasRectangle,
  flexDirection: 'row' | 'column',
): boolean {
  const targetSiblingBefore = EP.fromString(
    `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
  )
  const targetSiblingAfter = EP.fromString(
    `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`,
  )
  const prevSiblingFrame = MetadataUtils.getFrameInCanvasCoords(targetSiblingBefore, metadata)
  const nextSiblingFrame = MetadataUtils.getFrameInCanvasCoords(targetSiblingAfter, metadata)
  if (prevSiblingFrame == null || nextSiblingFrame == null) {
    return false
  } else {
    const prevSiblingEdge =
      flexDirection === 'row'
        ? {
            x: prevSiblingFrame.x + prevSiblingFrame.width,
            y: prevSiblingFrame.y,
          }
        : {
            x: prevSiblingFrame.x,
            y: prevSiblingFrame.y + prevSiblingFrame.height,
          }
    return (
      reparentLine.x >= prevSiblingEdge.x &&
      reparentLine.y >= prevSiblingEdge.y &&
      reparentLine.x <= nextSiblingFrame.x &&
      reparentLine.y <= nextSiblingFrame.y
    )
  }
}

describe('Inserting into absolute', () => {
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Drag from inside bbb to inside ccc
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 5,
                top: 5,
                width: 20,
                height: 300,
              }}
              data-uid='ddd'
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

  it('Should drag to insert into targets smaller than the element', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 1005,
      y: targetElementBounds.y + 1005,
    })

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Drag from inside bbb to inside ccc
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 5,
                top: 5,
                width: 1000,
                height: 1000,
              }}
              data-uid='ddd'
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

  it('Click to insert with default size', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 65,
      y: targetElementBounds.y + 55,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Click in bbb
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 15,
                top: 5,
                width: 100,
                height: 100,
              }}
              data-uid='ddd'
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

  it('Click to insert into an element smaller than the default size', async () => {
    const renderResult = await setupInsertTest(
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
              width: 10,
              height: 10,
              backgroundColor: '#d3d3d3',
            }}
          />
        </div>
    `),
    )
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Click in bbb
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              width: 10,
              height: 10,
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: -45,
                top: -45,
                width: 100,
                height: 100,
              }}
              data-uid='ddd'
            />
          </div>
        </div>
      `),
    )
  })

  it('Should not clear the intended target when dragging to insert past the scene boundary', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Drag from inside bbb to outside of the scene
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 5,
                top: 5,
                width: 1000,
                height: 10,
              }}
              data-uid='ddd'
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

  it('when drag ends outside the canvas in insert mode, it is cancelled', async () => {
    const renderResult = await setupInsertTest(inputCode)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })

    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 2005,
      y: targetElementBounds.y + 2005,
    })

    await enterInsertModeFromInsertMenuStartDrag(renderResult)
    await renderResult.getDispatchFollowUpActionsFinished()

    mouseMoveToPoint(canvasControlsLayer, startPoint)

    mouseDragFromPointToPointNoMouseDown(canvasControlsLayer, startPoint, endPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(inputCode)
  })
  it('Drag to insert into a zero sized element', async () => {
    const testCode = makeTestProjectCodeWithSnippet(`
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
            backgroundColor: '#d3d3d3',
          }}
        />
      </div>
    `)
    const renderResult = await setupInsertTest(testCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x,
      y: targetElementBounds.y,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 40,
      y: targetElementBounds.y + 50,
    })

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Drag from inside bbb to inside ccc
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 40,
                height: 50,
              }}
              data-uid='ddd'
            />
          </div>
        </div>
      `),
    )
  })
  it('Click to insert into a zero size element', async () => {
    const testCode = makeTestProjectCodeWithSnippet(`
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
              backgroundColor: '#d3d3d3',
            }}
          />
        </div>
    `)
    const renderResult = await setupInsertTest(testCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x,
      y: targetElementBounds.y,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Click in bbb
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: -50,
                top: -50,
                width: 100,
                height: 100,
              }}
              data-uid='ddd'
            />
          </div>
        </div>
      `),
    )
  })
})

describe('Inserting into flex row', () => {
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
          position: 'relative',
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)
    const indicatorBeforeSibling = isIndicatorBeforeSiblingBBB(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
    )
    expect(indicatorBeforeSibling).toEqual(true)

    // Drag horizontally close to the zero position
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 20,
              height: 300,
            }}
            data-uid='ddd'
          />
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'relative',
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

  it('Click to insert into zero position in flex', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)
    const indicatorBeforeSibling = isIndicatorBeforeSiblingBBB(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
    )
    expect(indicatorBeforeSibling).toEqual(true)

    // Click horizontally close to the zero position
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 100,
              height: 100,
            }}
            data-uid='ddd'
          />
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'relative',
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)
    const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
      'row',
    )
    expect(indicatorBetweenSiblings).toEqual(true)

    // Drag horizontally close to the first position
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 20,
              height: 300,
            }}
            data-uid='ddd'
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

  it('Click to insert into first position in flex', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + targetElementBounds.height + 5,
    }) // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)
    const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
      'row',
    )
    expect(indicatorBetweenSiblings).toEqual(true)

    // Click horizontally close to the first position
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 100,
              height: 100,
            }}
            data-uid='ddd'
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)

    const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
      'row',
    )
    expect(indicatorBetweenSiblings).toEqual(true)

    // Drag starts horizontally close to the first position, dragging towards the top left
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 20,
              height: 300,
            }}
            data-uid='ddd'
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Drag starts inside bbb
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 10,
                top: 10,
                width: 20,
                height: 30,
              }}
              data-uid='ddd'
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

  it('Click to insert inside a flex child with absolute layout', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 10,
      y: targetElementBounds.y + 10,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Click inside bbb
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          >
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: -40,
                top: -40,
                width: 100,
                height: 100,
              }}
              data-uid='ddd'
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)

    const indicatorBeforeSibling = isIndicatorBeforeSiblingBBB(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
    )
    expect(indicatorBeforeSibling).toEqual(true)

    // Drag starts inside bbb, but very close to its edge (3px)
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
          style={{
            backgroundColor: '#0091FFAA',
            position: 'relative',
            width: 20,
            height: 30,
          }}
          data-uid='ddd'
        />
        <div
          data-uid='bbb'
          data-testid='bbb'
          style={{
            position: 'relative',
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

  it('Click inside a flex child close to the edge, which inserts as a sibling', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 3,
      y: targetElementBounds.y + 3,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)

    const indicatorBeforeSibling = isIndicatorBeforeSiblingBBB(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
    )
    expect(indicatorBeforeSibling).toEqual(true)

    // Click inside bbb, but very close to its edge (3px)
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
          style={{
            backgroundColor: '#0091FFAA',
            position: 'relative',
            width: 100,
            height: 100,
          }}
          data-uid='ddd'
        />
        <div
          data-uid='bbb'
          data-testid='bbb'
          style={{
            position: 'relative',
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
          position: 'relative',
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)

    const indicatorBeforeSibling = isIndicatorBeforeSiblingBBB(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
    )
    expect(indicatorBeforeSibling).toEqual(true)

    // Drag vertically close to the first position
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 300,
              height: 20,
            }}
            data-uid='ddd'
          />
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'relative',
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

  it('Click to insert into zero position in flex, column layout', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + 5,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)

    const indicatorBeforeSibling = isIndicatorBeforeSiblingBBB(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
    )
    expect(indicatorBeforeSibling).toEqual(true)

    // Click vertically close to the first position
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 100,
              height: 100,
            }}
            data-uid='ddd'
          />
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'relative',
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)

    const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
      'column',
    )
    expect(indicatorBetweenSiblings).toEqual(true)

    // Drag vertically close to the first position
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 300,
              height: 20,
            }}
            data-uid='ddd'
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

  it('Click to insert into first position in flex, column layout', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + targetElementBounds.width + 5,
      y: targetElementBounds.y + targetElementBounds.height + 5,
    })

    // Move before clicking
    mouseMoveToPoint(canvasControlsLayer, point)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)
    const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
      'column',
    )
    expect(indicatorBetweenSiblings).toEqual(true)

    // Click vertically close to the first position
    mouseClickAtPoint(canvasControlsLayer, point)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 100,
              height: 100,
            }}
            data-uid='ddd'
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
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult)

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

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
    // Shows flex indicator line at index position target
    expect(
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
    ).toEqual(1)
    const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
      renderResult.getEditorState().editor.jsxMetadata,
      renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
      'column',
    )
    expect(indicatorBetweenSiblings).toEqual(true)

    // Drag starts vertically close to the first position, dragging towards the top left
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
              position: 'relative',
              width: 180,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          />
          <div
            style={{
              backgroundColor: '#0091FFAA',
              position: 'relative',
              width: 300,
              height: 20,
            }}
            data-uid='ddd'
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

describe('Inserting an image', () => {
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

  it('Draw to insert to an absolute layout keeps aspect ratio', async () => {
    const renderResult = await setupInsertTest(inputCode)
    await enterInsertModeFromInsertMenu(renderResult, 'img')

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 15, // with aspect ratio lock this 10px with should be ignored
      y: targetElementBounds.y + 305,
    })

    // Move before starting dragging
    mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    // Drag from inside bbb to inside ccc
    mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()

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
            <img              
              style={{
                width: 300,
                height: 300,
                position: 'absolute',
                left: 5,
                top: 5,
              }}
              src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
              data-uid='ddd'
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
