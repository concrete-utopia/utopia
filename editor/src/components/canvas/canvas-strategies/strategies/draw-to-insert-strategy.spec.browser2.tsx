import { setRightMenuTab } from '../../../editor/actions/action-creators'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  getPrintedUiJsCode,
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
  pressKey,
} from '../../event-helpers.test-utils'
import { RightMenuTab } from '../../../editor/store/editor-state'
import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../../../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  isInfinityRectangle,
  offsetPoint,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { Direction } from '../../../inspector/common/css-utils'
import { expectSingleUndo2Saves } from '../../../../utils/utils.test-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers, shiftModifier } from '../../../../utils/modifiers'

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

function ensureInInsertMode(renderResult: EditorRenderResult): void {
  expect(renderResult.getEditorState().editor.mode.type).toEqual('insert')
}

async function enterInsertModeFromInsertMenu(
  renderResult: EditorRenderResult,
  elementType: string = 'div',
) {
  const insertButton = renderResult.renderedDOM.getByTestId(`insert-item-${elementType}`)
  const insertButtonBounds = insertButton.getBoundingClientRect()

  const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
    x: insertButtonBounds.x + insertButtonBounds.width / 2,
    y: insertButtonBounds.y + insertButtonBounds.height / 2,
  })

  await mouseMoveToPoint(insertButton, point)
  await mouseClickAtPoint(insertButton, point)

  await renderResult.getDispatchFollowUpActionsFinished()

  ensureInInsertMode(renderResult)
}

async function enterInsertModeFromInsertMenuStartDrag(renderResult: EditorRenderResult) {
  const insertButton = renderResult.renderedDOM.getByTestId('insert-item-div')
  const insertButtonBounds = insertButton.getBoundingClientRect()

  const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
    x: insertButtonBounds.x + insertButtonBounds.width / 2,
    y: insertButtonBounds.y + insertButtonBounds.height / 2,
  })

  await mouseMoveToPoint(insertButton, point)
  await mouseDownAtPoint(insertButton, point)

  await renderResult.getDispatchFollowUpActionsFinished()

  ensureInInsertMode(renderResult)
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

  if (parentFrame == null || nextSiblingFrame == null || isInfinityRectangle(nextSiblingFrame)) {
    return false
  } else {
    const isBelowRightOfParentTopLeft =
      isInfinityRectangle(parentFrame) ||
      (reparentLine.x >= parentFrame.x && reparentLine.y >= parentFrame.y)

    return (
      isBelowRightOfParentTopLeft &&
      reparentLine.x <= nextSiblingFrame.x &&
      reparentLine.y <= nextSiblingFrame.y
    )
  }
}

function isIndicatorBetweenSiblingsBBBCCC(
  metadata: ElementInstanceMetadataMap,
  reparentLine: CanvasRectangle,
  flexDirection: Direction,
): boolean {
  const targetSiblingBefore = EP.fromString(
    `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
  )
  const targetSiblingAfter = EP.fromString(
    `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`,
  )
  const prevSiblingFrame = MetadataUtils.getFrameInCanvasCoords(targetSiblingBefore, metadata)
  const nextSiblingFrame = MetadataUtils.getFrameInCanvasCoords(targetSiblingAfter, metadata)
  if (
    prevSiblingFrame == null ||
    nextSiblingFrame == null ||
    isInfinityRectangle(prevSiblingFrame) ||
    isInfinityRectangle(nextSiblingFrame)
  ) {
    return false
  } else {
    const prevSiblingEdge =
      flexDirection === 'horizontal'
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

const testDrawToInsertImageAspectRatio = async (inputCode: string, expectedCode: string) => {
  const renderResult = await setupInsertTest(makeTestProjectCodeWithSnippet(inputCode))
  await enterInsertModeFromInsertMenu(renderResult, 'img')

  const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
  await mouseMoveToPoint(canvasControlsLayer, startPoint)

  // Highlight should show the candidate parent
  expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

  // Drag from inside bbb to inside ccc
  await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

  await renderResult.getDispatchFollowUpActionsFinished()

  // Check that the inserted element is a child of bbb
  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
    makeTestProjectCodeWithSnippet(expectedCode),
  )
}

async function drawToInsertTestMaybeAddsFlexGrow(
  dragDelta: CanvasPoint,
  expectedStyle: React.CSSProperties,
  flexDirection: 'row' | 'column',
  parentSize: number | null = null,
) {
  const flexElementWithChildren = (insertedSibling: string = '') =>
    makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: ${parentSize == null ? "'100%'" : parentSize},
        height: ${parentSize == null ? "'100%'" : parentSize},
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        gap: 10,
        flexDirection: '${flexDirection}',
        alignItems: 'flex-start',
        justifyContent: 'flex-start',
        padding: 10,
      }}
    >
      <div
        data-uid='bb1'
        style={{
          width: 160,
          height: 74,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='bb2'
        style={{
          backgroundColor: '#FF0000',
          width: 80,
          height: 80,
        }}
      />
      <div
        data-uid='bb3'
        data-testid='bb3'
        style={{
          backgroundColor: '#FF0000',
          height: 60,
          width: 50,
        }}
      />
      ${insertedSibling}
    </div>
  `)

  const renderResult = await setupInsertTest(flexElementWithChildren())
  await enterInsertModeFromInsertMenu(renderResult)

  const targetElement = renderResult.renderedCanvas.getByTestId('bb3')
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
    x: targetElementBounds.x + targetElementBounds.width + 5,
    y: targetElementBounds.y + targetElementBounds.height + 5,
  }) as CanvasPoint
  const endPoint = offsetPoint(startPoint, dragDelta)

  // Move before starting dragging
  await mouseMoveToPoint(canvasControlsLayer, startPoint)

  // Highlight should show the candidate parent
  expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])

  // Drag horizontally close to the first position
  await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

  await renderResult.getDispatchFollowUpActionsFinished()

  const insertedSiblingCode = `<div
    style={${JSON.stringify(expectedStyle)}}
    data-uid='ddd'
  />
  `

  // Check that the inserted element contains the correct style property
  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
    flexElementWithChildren(insertedSiblingCode),
  )
}

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "runInsertTest", "runClickToInsertTest", "drawToInsertTestMaybeAddsFlexGrow", "testDrawToInsertImageAspectRatio" ] }] */

describe('draw-to-insert', () => {
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

    describe('Inserting into simple target', () => {
      async function runInsertTest(
        dragDelta: { x: number; y: number },
        modifiers: Modifiers,
        expectedSize: { width: number; height: number },
      ) {
        const renderResult = await setupInsertTest(inputCode)
        await enterInsertModeFromInsertMenu(renderResult)

        const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: targetElementBounds.x + 5,
          y: targetElementBounds.y + 5,
        })
        const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: startPoint.x + dragDelta.x,
          y: startPoint.y + dragDelta.y,
        })

        // Move before starting dragging
        await mouseMoveToPoint(canvasControlsLayer, startPoint)

        // Highlight should show the candidate parent
        expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

        // Drag from inside bbb to inside ccc
        await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
          modifiers: modifiers,
        })

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
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 5,
                  top: 5,
                  width: ${expectedSize.width},
                  height: ${expectedSize.height},
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
      }

      it('Should honour the initial target when dragging to insert', async () => {
        const xDelta = 20
        const yDelta = 300
        await runInsertTest({ x: xDelta, y: yDelta }, emptyModifiers, {
          width: xDelta,
          height: yDelta,
        })
      })

      it('Should lock aspect ratio to 1:1 when holding shift and dragging vertically', async () => {
        const xDelta = 20
        const yDelta = 300

        // The result should be the same regardless of which dimension of the drag is the larger
        await runInsertTest({ x: xDelta, y: yDelta }, shiftModifier, {
          width: yDelta,
          height: yDelta,
        })
      })

      it('Should lock aspect ratio to 1:1 when holding shift and dragging horizontally', async () => {
        const xDelta = 300
        const yDelta = 20

        // The result should be the same regardless of which dimension of the drag is the larger
        await runInsertTest({ x: xDelta, y: yDelta }, shiftModifier, {
          width: xDelta,
          height: xDelta,
        })
      })

      it('Should insert a conditional', async () => {
        const renderResult = await setupInsertTest(inputCode)
        await enterInsertModeFromInsertMenu(renderResult, 'Conditional')

        FOR_TESTS_setNextGeneratedUids([
          'skip1',
          'skip2',
          'skip3',
          'skip4',
          'skip5',
          'skip6',
          'skip7',
          'false-branch',
        ])

        const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const dragDelta = windowPoint({ x: 50, y: 40 })

        const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: targetElementBounds.x + 5,
          y: targetElementBounds.y + 5,
        })
        const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: startPoint.x + dragDelta.x,
          y: startPoint.y + dragDelta.y,
        })

        // Move before starting dragging
        await mouseMoveToPoint(canvasControlsLayer, startPoint)

        // Highlight should show the candidate parent
        expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

        // Drag from inside bbb to inside ccc
        await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
            {true ? (
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 5,
                  top: 5,
                  width: 50,
                  height: 40,
                }}
                data-uid='ddd'
              />
            ) : (
              <div
                style={{
                  position: 'absolute',
                  left: 5,
                  top: 5,
                  width: 100,
                  height: 100,
                }}
                data-uid='fal'
              >
                False branch
              </div>
            )}
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

      it('Should insert a fragment', async () => {
        const renderResult = await setupInsertTest(inputCode)
        await enterInsertModeFromInsertMenu(renderResult, 'Fragment')

        const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const dragDelta = windowPoint({ x: 50, y: 40 })

        const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: targetElementBounds.x + 5,
          y: targetElementBounds.y + 5,
        })
        const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: startPoint.x + dragDelta.x,
          y: startPoint.y + dragDelta.y,
        })

        // Move before starting dragging
        await mouseMoveToPoint(canvasControlsLayer, startPoint)

        // Highlight should show the candidate parent
        expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

        // Drag from inside bbb to inside ccc
        await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
            <React.Fragment>
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 5,
                  top: 5,
                  width: 50,
                  height: 40,
                }}
                data-uid='ddd'
              />
            </React.Fragment>
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

    it('Should drag to insert into targets smaller than the element', async () => {
      const renderResult = await setupInsertTest(inputCode)
      await enterInsertModeFromInsertMenu(renderResult)

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Drag from inside bbb to inside ccc
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
                backgroundColor: '#aaaaaa33',
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

    describe('Click to insert with default size', () => {
      async function runClickToInsertTest(renderResult: EditorRenderResult) {
        const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
          x: targetElementBounds.x + 65,
          y: targetElementBounds.y + 55,
        })

        // Move before clicking
        await mouseMoveToPoint(canvasControlsLayer, point)

        // Highlight should show the candidate parent
        expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

        // Click in bbb
        await mouseClickAtPoint(canvasControlsLayer, point)

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
                  backgroundColor: '#aaaaaa33',
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
      }

      it('works from the insert menu', async () => {
        const renderResult = await setupInsertTest(inputCode)
        await enterInsertModeFromInsertMenu(renderResult)
        await runClickToInsertTest(renderResult)
      })

      it('works with the keyboard shortcut', async () => {
        const renderResult = await setupInsertTest(inputCode)
        await pressKey('d')
        ensureInInsertMode(renderResult)
        await runClickToInsertTest(renderResult)
      })
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 5,
        y: targetElementBounds.y + 5,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Click in bbb
      await mouseClickAtPoint(canvasControlsLayer, point)

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
                backgroundColor: '#aaaaaa33',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Drag from inside bbb to outside of the scene
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
                backgroundColor: '#aaaaaa33',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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

      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      await mouseDragFromPointToPointNoMouseDown(canvasControlsLayer, startPoint, endPoint)
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Drag from inside bbb to inside ccc
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
                backgroundColor: '#aaaaaa33',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x,
        y: targetElementBounds.y,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Click in bbb
      await mouseClickAtPoint(canvasControlsLayer, point)

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
                backgroundColor: '#aaaaaa33',
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

    it('Ignores fragments and flattens their children', async () => {
      const inputWithFragments = makeTestProjectCodeWithSnippet(`
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='aaa'
        data-testid='aaa'
      >
        <React.Fragment>
          <div
            style={{
              backgroundColor: '#09F',
              position: 'absolute',
              left: 20,
              top: 30,
              width: 50,
              height: 50,
            }}
            data-uid='b01'
          />
          <div
            style={{
              backgroundColor: '#09F',
              position: 'absolute',
              left: 100,
              top: 100,
              width: 100,
              height: 50,
            }}
            data-uid='b02'
          />
          <>
            <div
              style={{
                backgroundColor: '#09F',
                position: 'absolute',
                left: 300,
                top: 300,
                width: 25,
                height: 25,
              }}
              data-uid='b03'
            />
          </>
        </React.Fragment>
        <div
          style={{
            backgroundColor: '#09F',
            position: 'absolute',
            left: 200,
            top: 400,
            width: 50,
            height: 50,
          }}
          data-uid='b04'
        />
      </div>
    `)

      const renderResult = await setupInsertTest(inputWithFragments)
      await enterInsertModeFromInsertMenu(renderResult)

      const targetElement = renderResult.renderedCanvas.getByTestId('aaa')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 200,
        y: targetElementBounds.y + 200,
      })
      const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 250,
        y: targetElementBounds.y + 250,
      })

      // Move before starting dragging
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])

      // Drag from inside bbb to inside ccc
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

      await renderResult.getDispatchFollowUpActionsFinished()

      // Check that the inserted element is a child of bbb
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='aaa'
        data-testid='aaa'
      >
        <React.Fragment>
          <div
            style={{
              backgroundColor: '#09F',
              position: 'absolute',
              left: 20,
              top: 30,
              width: 50,
              height: 50,
            }}
            data-uid='b01'
          />
          <div
            style={{
              backgroundColor: '#09F',
              position: 'absolute',
              left: 100,
              top: 100,
              width: 100,
              height: 50,
            }}
            data-uid='b02'
          />
          <>
            <div
              style={{
                backgroundColor: '#09F',
                position: 'absolute',
                left: 300,
                top: 300,
                width: 25,
                height: 25,
              }}
              data-uid='b03'
            />
          </>
        </React.Fragment>
        <div
          style={{
            backgroundColor: '#09F',
            position: 'absolute',
            left: 200,
            top: 400,
            width: 50,
            height: 50,
          }}
          data-uid='b04'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 200,
            top: 200,
            width: 50,
            height: 50,
          }}
          data-uid='ddd'
        />
      </div>
      `),
      )
    })
  })

  describe('Inserting into Static', () => {
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
          position: 'static',
          width: 380,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          position: 'static',
          width: 380,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `)

    it('By default, it inserts as absolute into a flow parent', async () => {
      const renderResult = await setupInsertTest(inputCode)
      await enterInsertModeFromInsertMenu(renderResult)

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 5,
        y: targetElementBounds.y + 15,
      })
      const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 25,
        y: targetElementBounds.y + 35,
      })

      // Move before starting dragging
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Drag from inside bbb to inside ccc
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              position: 'static',
              width: 380,
              height: 180,
              backgroundColor: '#d3d3d3',
              contain: 'layout',
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 5,
                top: 15,
                width: 20,
                height: 20,
              }}
              data-uid='ddd'
            />
          </div>
          <div
            data-uid='ccc'
            style={{
              position: 'static',
              width: 380,
              height: 190,
              backgroundColor: '#FF0000',
            }}
          />
          
        </div>
      `),
      )
    })

    it('Using the strategy picker, it happily inserts into a static element', async () => {
      const renderResult = await setupInsertTest(inputCode)
      await enterInsertModeFromInsertMenu(renderResult)

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 5,
        y: targetElementBounds.y + 15,
      })
      const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 25,
        y: targetElementBounds.y + 35,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      // Move before starting dragging
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
        midDragCallback: async () => {
          await pressKey('2') // this should select the 'Draw to Insert (Flow)' strategy
        },
      })

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
              position: 'static',
              width: 380,
              height: 180,
              backgroundColor: '#d3d3d3',
            }}
          >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 20,
              height: 20,
              contain: 'layout',
            }}
            data-uid='ddd'
          />
          </div>
          <div
            data-uid='ccc'
            style={{
              position: 'static',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

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
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              backgroundColor: '#aaaaaa33',
              width: 20,
              height: 300,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 5,
        y: targetElementBounds.y + 5,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

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
      await mouseClickAtPoint(canvasControlsLayer, point)

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
              backgroundColor: '#aaaaaa33',
              width: 100,
              height: 100,
              contain: 'layout',
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

    it('Inserting a wrapped element into the 0th position in flex', async () => {
      const renderResult = await setupInsertTest(inputCode)
      await enterInsertModeFromInsertMenu(renderResult, 'Conditional')

      FOR_TESTS_setNextGeneratedUids([
        'skip1',
        'skip2',
        'skip3',
        'skip4',
        'skip5',
        'skip6',
        'skip7',
        'false-branch',
      ])
      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

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

      await expectSingleUndo2Saves(renderResult, () =>
        // Drag horizontally close to the zero position
        mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint),
      )

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
          {true ? (
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 20,
                height: 300,
                contain: 'layout',
              }}
              data-uid='ddd'
            />
          ) : (
            <div
              style={{ width: 100, height: 300 }}
              data-uid='fal'
            >
              False branch
            </div>
          )}
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
      // Shows flex indicator line at index position target
      expect(
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
      ).toEqual(1)
      const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
        'horizontal',
      )
      expect(indicatorBetweenSiblings).toEqual(true)

      // Drag horizontally close to the first position
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              backgroundColor: '#aaaaaa33',
              width: 20,
              height: 300,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + targetElementBounds.width + 5,
        y: targetElementBounds.y + targetElementBounds.height + 5,
      }) // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
      // Shows flex indicator line at index position target
      expect(
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
      ).toEqual(1)
      const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
        'horizontal',
      )
      expect(indicatorBetweenSiblings).toEqual(true)

      // Click horizontally close to the first position
      await mouseClickAtPoint(canvasControlsLayer, point)

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
              backgroundColor: '#aaaaaa33',
              width: 100,
              height: 100,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
      // Shows flex indicator line at index position target
      expect(
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
      ).toEqual(1)

      const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
        'horizontal',
      )
      expect(indicatorBetweenSiblings).toEqual(true)

      // Drag starts horizontally close to the first position, dragging towards the top left
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              backgroundColor: '#aaaaaa33',
              width: 20,
              height: 300,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Drag starts inside bbb
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
                backgroundColor: '#aaaaaa33',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 10,
        y: targetElementBounds.y + 10,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

      // Click inside bbb
      await mouseClickAtPoint(canvasControlsLayer, point)

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
                backgroundColor: '#aaaaaa33',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

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
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
            backgroundColor: '#aaaaaa33',
            width: 20,
            height: 30,
            contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + 3,
        y: targetElementBounds.y + 3,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

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
      await mouseClickAtPoint(canvasControlsLayer, point)

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
            backgroundColor: '#aaaaaa33',
            width: 100,
            height: 100,
            contain: 'layout',
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
    describe('insertion adds flex grow on reaching edge if possible', () => {
      it('inserting into the end of flex row adds flexGrow when reaching parent`s edge', async () => {
        const expectedStyle = {
          backgroundColor: '#aaaaaa33',
          height: 25,
          contain: 'layout',
          flexGrow: 1,
        }
        await drawToInsertTestMaybeAddsFlexGrow(canvasPoint({ x: 60, y: 25 }), expectedStyle, 'row')
      })
      it('inserting into the end of flex row adds width when not reaching parent`s edge', async () => {
        const expectedStyle = {
          backgroundColor: '#aaaaaa33',
          width: 50,
          height: 25,
          contain: 'layout',
        }
        await drawToInsertTestMaybeAddsFlexGrow(canvasPoint({ x: 50, y: 25 }), expectedStyle, 'row')
      })
      it('inserting into the end of flex row only adds width when siblings are shrinked already (no open space for insertion)', async () => {
        const expectedStyle = {
          backgroundColor: '#aaaaaa33',
          width: 60,
          height: 25,
          contain: 'layout',
        }
        await drawToInsertTestMaybeAddsFlexGrow(
          canvasPoint({ x: 60, y: 25 }),
          expectedStyle,
          'row',
          100,
        )
      })
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

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
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              backgroundColor: '#aaaaaa33',
              width: 300,
              height: 20,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + targetElementBounds.width + 5,
        y: targetElementBounds.y + 5,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

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
      await mouseClickAtPoint(canvasControlsLayer, point)

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
              backgroundColor: '#aaaaaa33',
              width: 100,
              height: 100,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
      // Shows flex indicator line at index position target
      expect(
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
      ).toEqual(1)

      const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
        'vertical',
      )
      expect(indicatorBetweenSiblings).toEqual(true)

      // Drag vertically close to the first position
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              backgroundColor: '#aaaaaa33',
              width: 300,
              height: 20,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const point = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
        x: targetElementBounds.x + targetElementBounds.width + 5,
        y: targetElementBounds.y + targetElementBounds.height + 5,
      })

      // Move before clicking
      await mouseMoveToPoint(canvasControlsLayer, point)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
      // Shows flex indicator line at index position target
      expect(
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
      ).toEqual(1)
      const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
        'vertical',
      )
      expect(indicatorBetweenSiblings).toEqual(true)

      // Click vertically close to the first position
      await mouseClickAtPoint(canvasControlsLayer, point)

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
              backgroundColor: '#aaaaaa33',
              width: 100,
              height: 100,
              contain: 'layout',
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

      const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
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
      await mouseMoveToPoint(canvasControlsLayer, startPoint)

      // Highlight should show the candidate parent
      expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['aaa'])
      // Shows flex indicator line at index position target
      expect(
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines.length,
      ).toEqual(1)
      const indicatorBetweenSiblings = isIndicatorBetweenSiblingsBBBCCC(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.canvas.controls.flexReparentTargetLines[0],
        'vertical',
      )
      expect(indicatorBetweenSiblings).toEqual(true)

      // Drag starts vertically close to the first position, dragging towards the top left
      await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

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
              backgroundColor: '#aaaaaa33',
              width: 300,
              height: 20,
              contain: 'layout',
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
    describe('insertion adds flex grow on reaching edge if possible', () => {
      it('inserting into the end of flex column adds flexGrow when reaching parent`s edge', async () => {
        const expectedStyle = {
          backgroundColor: '#aaaaaa33',
          width: 60,
          contain: 'layout',
          flexGrow: 1,
        }
        await drawToInsertTestMaybeAddsFlexGrow(
          canvasPoint({ x: 60, y: 136 }),
          expectedStyle,
          'column',
        )
      })
      it('inserting into the end of flex column adds width when not reaching parent`s edge', async () => {
        const expectedStyle = {
          backgroundColor: '#aaaaaa33',
          width: 50,
          height: 130,
          contain: 'layout',
        }
        await drawToInsertTestMaybeAddsFlexGrow(
          canvasPoint({ x: 50, y: 130 }),
          expectedStyle,
          'column',
        )
      })
      it('inserting into the end of flex column only adds width when siblings are shrinked already (no open space for insertion)', async () => {
        const expectedStyle = {
          backgroundColor: '#aaaaaa33',
          width: 60,
          height: 136,
          contain: 'layout',
        }
        await drawToInsertTestMaybeAddsFlexGrow(
          canvasPoint({ x: 60, y: 136 }),
          expectedStyle,
          'column',
          100,
        )
      })
    })
  })

  describe('Inserting an image', () => {
    it('Draw to insert to an absolute layout keeps aspect ratio', async () => {
      const inputCode = `
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
    `
      const expectedCode = `
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
            src='/editor/utopia-logo-white-fill.png?hash=nocommit'
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
    `
      await testDrawToInsertImageAspectRatio(inputCode, expectedCode)
    })

    it('Draw to insert to a flex layout keeps aspect ratio', async () => {
      const inputCode = `
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
        }}
      >
        <div
          data-uid='bbb'
          data-testid='bbb'
          style={{
            backgroundColor: '#f09',
            display: 'flex',
            width: 400,
            height: 400
          }}
        />
      </div>
    `
      const expectedCode = `
      <div
        data-uid='aaa'
        style={{ width: '100%', height: '100%' }}
      >
        <div
          data-uid='bbb'
          data-testid='bbb'
          style={{
            backgroundColor: '#f09',
            display: 'flex',
            width: 400,
            height: 400
          }}
        >
          <img
            style={{
              width: 300,
              height: 300,
              contain: 'layout',
            }}
            src='/editor/utopia-logo-white-fill.png?hash=nocommit'
            data-uid='ddd'
          />
        </div>
      </div>
    `
      await testDrawToInsertImageAspectRatio(inputCode, expectedCode)
    })
  })

  describe('Conditionals support', () => {
    ;[true, false].forEach((enabled) => {
      describe(`with the Conditional feature switch ${enabled ? 'enabled' : 'disabled'}`, () => {
        const inputCode = makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        data-testid='aaa'
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
            width: 150,
            height: 150,
            backgroundColor: '#d3d3d3',
          }}
        />
        {true ? (
          <div
            data-uid='ccc'
            style={{
              position: 'absolute',
              left: 100,
              top: 200,
              width: 150,
              height: 150,
              backgroundColor: '#FF0000',
            }}
          />
        ) : null}
      </div>
    `)

        it('Draw to insert into a sibling of the conditional', async () => {
          const renderResult = await setupInsertTest(inputCode)
          await enterInsertModeFromInsertMenu(renderResult)

          const targetElement = renderResult.renderedCanvas.getByTestId('bbb')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
            x: targetElementBounds.x + 5,
            y: targetElementBounds.y + 5,
          })
          const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
            x: targetElementBounds.x + 25,
            y: targetElementBounds.y + 305,
          })

          // Move before starting dragging
          await mouseMoveToPoint(canvasControlsLayer, startPoint)

          // Highlight should show the candidate parent
          expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual([
            'bbb',
          ])

          // Drag from inside bbb to inside ccc
          await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
            midDragCallback: async () => {
              expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                'Draw to Insert (Abs)',
              )
            },
          })

          await renderResult.getDispatchFollowUpActionsFinished()

          // Check that the inserted element is a child of bbb
          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
          <div
            data-uid='aaa'
            data-testid='aaa'
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
                width: 150,
                height: 150,
                backgroundColor: '#d3d3d3',
              }}
            >
              <div              
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 5,
                  top: 5,
                  width: 20,
                  height: 300,
                }}
                data-uid='ddd'
              />
            </div>
            {true ? (
              <div
                data-uid='ccc'
                style={{
                  position: 'absolute',
                  left: 100,
                  top: 200,
                  width: 150,
                  height: 150,
                  backgroundColor: '#FF0000',
                }}
              />
            ) : null}
          </div>
        `),
          )
        })

        it('Draw to insert into the parent of a conditional works', async () => {
          const renderResult = await setupInsertTest(inputCode)
          await enterInsertModeFromInsertMenu(renderResult)

          const targetElement = renderResult.renderedCanvas.getByTestId('aaa')
          const targetElementBounds = targetElement.getBoundingClientRect()
          const canvasControlsLayer =
            renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

          const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
            x: targetElementBounds.x + 170,
            y: targetElementBounds.y + 10,
          })
          const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
            x: targetElementBounds.x + 220,
            y: targetElementBounds.y + 310,
          })

          // Move before starting dragging
          await mouseMoveToPoint(canvasControlsLayer, startPoint)

          // Highlight should show the candidate parent
          expect(renderResult.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual([
            'aaa',
          ])

          await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint, {
            midDragCallback: async () => {
              expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
                'Draw to Insert (Abs)',
              )
            },
          })

          await renderResult.getDispatchFollowUpActionsFinished()

          // Check that the inserted element is a child of bbb
          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
          <div
            data-uid='aaa'
            data-testid='aaa'
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
                width: 150,
                height: 150,
                backgroundColor: '#d3d3d3',
              }}
            />
            {true ? (
              <div
                data-uid='ccc'
                style={{
                  position: 'absolute',
                  left: 100,
                  top: 200,
                  width: 150,
                  height: 150,
                  backgroundColor: '#FF0000',
                }}
              />
            ) : null}
            <div              
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 170,
                top: 10,
                width: 50,
                height: 300,
              }}
              data-uid='ddd'
            />
          </div>
        `),
          )
        })
      })
    })
  })
})
