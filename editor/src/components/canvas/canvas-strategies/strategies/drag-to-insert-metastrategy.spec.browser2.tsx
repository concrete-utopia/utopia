import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../../../../core/model/element-template-utils.test-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers } from '../../../../utils/modifiers'
import {
  expectSingleUndo2Saves,
  slightlyOffsetPointBecauseVeryWeirdIssue,
} from '../../../../utils/utils.test-utils'
import { setRightMenuTab } from '../../../editor/actions/action-creators'
import { RightMenuTab, navigatorEntryToKey } from '../../../editor/store/editor-state'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { DragOutlineControlTestId } from '../../controls/select-mode/drag-outline-control'
import {
  mouseDownAtPoint,
  mouseMoveToPoint,
  mouseUpAtPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'

// FIXME These tests will probably start to fail if the insert menu becomes too long, at which point we may
// have to insert some mocking to restrict the available items there

async function setupInsertTest(inputCode: string): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

  const newUID = 'ddd'
  FOR_TESTS_setNextGeneratedUid(newUID)

  return renderResult
}

async function startDraggingFromInsertMenuDivButtonToPoint(
  targetPoint: { x: number; y: number },
  modifiers: Modifiers,
  renderResult: EditorRenderResult,
  elementType: string = 'div',
): Promise<void> {
  const insertButton = renderResult.renderedDOM.getByTestId(`insert-item-${elementType}`)
  const insertButtonBounds = insertButton.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = slightlyOffsetPointBecauseVeryWeirdIssue({
    x: insertButtonBounds.x + insertButtonBounds.width / 2,
    y: insertButtonBounds.y + insertButtonBounds.height / 2,
  })

  const endPoint = slightlyOffsetPointBecauseVeryWeirdIssue(targetPoint)

  await mouseMoveToPoint(insertButton, startPoint)
  await mouseDownAtPoint(insertButton, startPoint)
  await mouseMoveToPoint(canvasControlsLayer, endPoint, {
    modifiers: modifiers,
    eventOptions: { buttons: 1 },
  })
}

async function finishDraggingToPoint(
  targetPoint: { x: number; y: number },
  modifiers: Modifiers,
  renderResult: EditorRenderResult,
): Promise<void> {
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
  const endPoint = slightlyOffsetPointBecauseVeryWeirdIssue(targetPoint)
  await mouseUpAtPoint(canvasControlsLayer, endPoint, { modifiers: modifiers })
}

async function dragFromInsertMenuDivButtonToPoint(
  targetPoint: { x: number; y: number },
  modifiers: Modifiers,
  renderResult: EditorRenderResult,
  showDragOutline: 'show-drag-outline' | 'no-drag-outline',
  elementType: string = 'div',
) {
  await expectSingleUndo2Saves(renderResult, async () => {
    await startDraggingFromInsertMenuDivButtonToPoint(
      targetPoint,
      modifiers,
      renderResult,
      elementType,
    )
    const dragOutlineControl = await renderResult.renderedDOM.queryByTestId(
      DragOutlineControlTestId,
    )
    if (showDragOutline === 'show-drag-outline') {
      expect(dragOutlineControl).not.toBeNull()
    } else {
      expect(dragOutlineControl).toBeNull()
    }
    await finishDraggingToPoint(targetPoint, modifiers, renderResult)
  })

  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('drag-to-insert', () => {
  describe('Dragging from the insert menu into an absolute layout', () => {
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
        data-uid='larger'
        data-testid='larger'
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
        data-uid='smaller'
        data-testid='smaller'
        style={{
          position: 'absolute',
          left: 10,
          top: 200,
          width: 20,
          height: 20,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `)

    it('Should insert a div into an absolute layout', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetParentElement = renderResult.renderedDOM.getByTestId('larger')
      const targetParentElementBounds = targetParentElement.getBoundingClientRect()
      const targetPoint = {
        x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
        y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'no-drag-outline',
      )

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
            data-uid='larger'
            data-testid='larger'
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
                left: 140,
                top: 40,
                width: 100,
                height: 100,
              }}
              data-uid='ddd'
            />
          </div>
          <div
            data-uid='smaller'
            data-testid='smaller'
            style={{
              position: 'absolute',
              left: 10,
              top: 200,
              width: 20,
              height: 20,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
      )
    })

    it('Should insert into a smaller element', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetParentElement = renderResult.renderedDOM.getByTestId('smaller')
      const targetParentElementBounds = targetParentElement.getBoundingClientRect()
      const targetPoint = {
        x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
        y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'no-drag-outline',
      )

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
            data-uid='larger'
            data-testid='larger'
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
            data-uid='smaller'
            data-testid='smaller'
            style={{
              position: 'absolute',
              left: 10,
              top: 200,
              width: 20,
              height: 20,
              backgroundColor: '#FF0000',
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
        </div>
      `),
      )
    })

    it('Should insert a conditional into an absolute layout', async () => {
      const renderResult = await setupInsertTest(inputCode)

      FOR_TESTS_setNextGeneratedUids(['ddd'])

      const targetParentElement = renderResult.renderedDOM.getByTestId('larger')
      const targetParentElementBounds = targetParentElement.getBoundingClientRect()
      const targetPoint = {
        x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
        y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'no-drag-outline',
        'Conditional',
      )

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
            data-uid='larger'
            data-testid='larger'
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
                  left: 140,
                  top: 40,
                  width: 100,
                  height: 100,
                }}
                data-uid='ddd'
              />
            ) : (
              <div
                style={{
                  position: 'absolute',
                  left: 140,
                  top: 40,
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
            data-uid='smaller'
            data-testid='smaller'
            style={{
              position: 'absolute',
              left: 10,
              top: 200,
              width: 20,
              height: 20,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
      )
    })

    it('Should insert a fragment into an absolute layout', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetParentElement = renderResult.renderedDOM.getByTestId('larger')
      const targetParentElementBounds = targetParentElement.getBoundingClientRect()
      const targetPoint = {
        x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
        y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'no-drag-outline',
        'Fragment',
      )

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
            data-uid='larger'
            data-testid='larger'
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
                  left: 140,
                  top: 40,
                  width: 100,
                  height: 100,
                }}
                data-uid='ddd'
              />
            </React.Fragment>
          </div>
          <div
            data-uid='smaller'
            data-testid='smaller'
            style={{
              position: 'absolute',
              left: 10,
              top: 200,
              width: 20,
              height: 20,
              backgroundColor: '#FF0000',
            }}
          />
        </div>
      `),
      )
    })
  })

  describe('Dragging from the insert menu into a flex layout', () => {
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
        data-testid='ccc'
        style={{
          width: 100,
          height: 190,
          backgroundColor: '#FF0000',
          display: 'flex',
        }}
      />
    </div>
  `)

    it('Should insert a div into a flex layout at zero position', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetNextSibling = renderResult.renderedDOM.getByTestId('bbb')
      const targetNextSiblingBounds = targetNextSibling.getBoundingClientRect()
      // Drag close to the left edge of the target sibling
      const targetPoint = {
        x: targetNextSiblingBounds.x + 5,
        y: targetNextSiblingBounds.y + 5,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'show-drag-outline',
      )

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
            data-testid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
              display: 'flex',
            }}
          />
        </div>
      `),
      )
    })

    it('Should insert a wrapped element into a flex layout at zero position', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetNextSibling = renderResult.renderedDOM.getByTestId('bbb')
      const targetNextSiblingBounds = targetNextSibling.getBoundingClientRect()
      // Drag close to the left edge of the target sibling
      const targetPoint = {
        x: targetNextSiblingBounds.x + 5,
        y: targetNextSiblingBounds.y + 5,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'show-drag-outline',
        'Fragment',
      )

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
          <React.Fragment>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 100,
                height: 100,
                contain: 'layout',
              }}
              data-uid='ddd'
            />
          </React.Fragment>
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
            data-testid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
              display: 'flex',
            }}
          />
        </div>
      `),
      )
    })

    it('Should insert a div into a flex layout with absolute positioning', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetSibling = renderResult.renderedDOM.getByTestId('bbb')
      const targetSiblingBounds = targetSibling.getBoundingClientRect()
      // Drag close to the right edge of the target sibling
      const targetPoint = {
        x: targetSiblingBounds.x + targetSiblingBounds.width - 5,
        y: targetSiblingBounds.y + targetSiblingBounds.height - 5,
      }

      await startDraggingFromInsertMenuDivButtonToPoint(targetPoint, emptyModifiers, renderResult)
      // Tab once to switch from flex insert to abs
      await pressKey('Tab', { modifiers: emptyModifiers })
      await finishDraggingToPoint(targetPoint, emptyModifiers, renderResult)

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
            data-uid='ccc'
            data-testid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
              display: 'flex',
            }}
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 125,
              top: 125,
              width: 100,
              height: 100,
            }}
            data-uid='ddd'
          />
        </div>
      `),
      )
    })

    it('Should forcibly insert a div into a flex layout that does not provide bounds with absolute positioning', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetParent = renderResult.renderedDOM.getByTestId('ccc')
      const targetParentBounds = targetParent.getBoundingClientRect()

      const targetPoint = {
        x: targetParentBounds.x + targetParentBounds.width / 2,
        y: targetParentBounds.y + targetParentBounds.height / 2,
      }

      await startDraggingFromInsertMenuDivButtonToPoint(targetPoint, emptyModifiers, renderResult)
      // Tab once to switch from flex insert to forced abs
      await pressKey('Tab', { modifiers: emptyModifiers })
      await finishDraggingToPoint(targetPoint, emptyModifiers, renderResult)

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
            data-uid='ccc'
            data-testid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
              display: 'flex',
              contain: 'layout',
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 190,
                top: 45,
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

    it('Should insert a div into a flex layout at the next position', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetPrevSibling = renderResult.renderedDOM.getByTestId('bbb')
      const targetPrevSiblingBounds = targetPrevSibling.getBoundingClientRect()
      // Drag close to the right edge of the target sibling
      const targetPoint = {
        x: targetPrevSiblingBounds.x + targetPrevSiblingBounds.width - 5,
        y: targetPrevSiblingBounds.y + targetPrevSiblingBounds.height - 5,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'show-drag-outline',
      )

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
            data-testid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
              display: 'flex',
            }}
          />
        </div>
      `),
      )
    })

    it('Should insert a div into a child of a flex layout which provides absolute bounds', async () => {
      const renderResult = await setupInsertTest(inputCode)

      const targetParentElement = renderResult.renderedDOM.getByTestId('bbb')
      const targetParentElementBounds = targetParentElement.getBoundingClientRect()

      const targetPoint = {
        x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
        y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
      }

      await dragFromInsertMenuDivButtonToPoint(
        targetPoint,
        emptyModifiers,
        renderResult,
        'no-drag-outline',
      )

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
                left: 40,
                top: 40,
                width: 100,
                height: 100,
              }}
              data-uid='ddd'
            />
          </div>
          <div
            data-uid='ccc'
            data-testid='ccc'
            style={{
              width: 100,
              height: 190,
              backgroundColor: '#FF0000',
              display: 'flex',
            }}
          />
        </div>
      `),
      )
    })
  })

  it('can insert two images', async () => {
    const renderResult = await renderTestEditorWithCode(
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
        data-uid='larger'
        data-testid='larger'
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
        data-uid='smaller'
        data-testid='smaller'
        style={{
          position: 'absolute',
          left: 10,
          top: 200,
          width: 20,
          height: 20,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
  `),
      'await-first-dom-report',
    )

    const targetParentElement = renderResult.renderedDOM.getByTestId('larger')
    const targetParentElementBounds = targetParentElement.getBoundingClientRect()
    const targetPoint = {
      x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
      y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
    }

    await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

    FOR_TESTS_setNextGeneratedUid('ddd')

    await dragFromInsertMenuDivButtonToPoint(
      targetPoint,
      emptyModifiers,
      renderResult,
      'no-drag-outline',
      'img',
    )

    await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

    FOR_TESTS_setNextGeneratedUid('eee')

    await dragFromInsertMenuDivButtonToPoint(
      targetPoint,
      emptyModifiers,
      renderResult,
      'no-drag-outline',
      'img',
    )

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/larger',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/larger/ddd',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/larger/eee',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/smaller',
      ],
    )
  })
})
