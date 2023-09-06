import { FOR_TESTS_setNextGeneratedUids } from '../../core/model/element-template-utils.test-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import * as EP from '../../core/shared/element-path'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
} from '../canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { InsertConditionalButtonTestId, InsertMenuButtonTestId } from './canvas-toolbar'

function slightlyOffsetWindowPointBecauseVeryWeirdIssue(point: { x: number; y: number }) {
  // FIXME when running in headless chrome, the result of getBoundingClientRect will be slightly
  // offset for some unknown reason, meaning the inserted element will be 1 pixel of in each dimension
  return { x: point.x - 0.001, y: point.y - 0.001 }
}

describe('canvas toolbar', () => {
  it('can insert conditionals via the canvas toolbar', async () => {
    const editor = await renderTestEditorWithCode(
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
      'await-first-dom-report',
    )
    const targetElement = editor.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 1005,
      y: targetElementBounds.y + 1005,
    })

    FOR_TESTS_setNextGeneratedUids(['new-div'])

    const insertMenuButton = editor.renderedDOM.getByTestId(InsertMenuButtonTestId)
    await mouseClickAtPoint(
      insertMenuButton,
      getDomRectCenter(insertMenuButton.getBoundingClientRect()),
    )

    const insertConditionalButton = editor.renderedDOM.getByTestId(InsertConditionalButtonTestId)
    const insertConditionalButtonRect = insertConditionalButton.getBoundingClientRect()
    await mouseClickAtPoint(insertConditionalButton, getDomRectCenter(insertConditionalButtonRect))

    // Move before starting dragging
    await mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(editor.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

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

    // Drag from inside bbb to inside ccc
    await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await editor.getDispatchFollowUpActionsFinished()

    // Check that the inserted element is a child of bbb
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
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
              {true ? (
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 5,
                    top: 5,
                    width: 1000,
                    height: 1000,
                  }}
                  data-uid='new-div'
                />
                ) : (
                  <div
                    style={{
                      position: 'absolute',
                      left: 5,
                      top: 5,
                      width: 1000,
                      height: 1000,
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
})
