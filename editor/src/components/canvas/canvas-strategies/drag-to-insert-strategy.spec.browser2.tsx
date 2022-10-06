import { FOR_TESTS_setNextGeneratedUid } from '../../../core/model/element-template-utils'
import { slightlyOffsetPointBecauseVeryWeirdIssue } from '../../../utils/utils.test-utils'
import { setRightMenuTab } from '../../editor/actions/action-creators'
import { RightMenuTab } from '../../editor/store/editor-state'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../ui-jsx.test-utils'

// FIXME These tests will probably start to fail if the insert menu becomes too long, at which point we may
// have to insert some mocking to restrict the available items there

async function setupInsertTest(inputCode: string): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  await renderResult.dispatch([setRightMenuTab(RightMenuTab.Insert)], false)

  const newUID = 'ddd'
  FOR_TESTS_setNextGeneratedUid(newUID)

  return renderResult
}

async function dragFromInsertMenuDivButtonToPoint(
  targetPoint: { x: number; y: number },
  renderResult: EditorRenderResult,
) {
  const insertButton = renderResult.renderedDOM.getByTestId('insert-item-div')
  const insertButtonBounds = insertButton.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = slightlyOffsetPointBecauseVeryWeirdIssue({
    x: insertButtonBounds.x + insertButtonBounds.width / 2,
    y: insertButtonBounds.y + insertButtonBounds.height / 2,
  })

  const endPoint = slightlyOffsetPointBecauseVeryWeirdIssue(targetPoint)

  mouseMoveToPoint(insertButton, startPoint)
  mouseDownAtPoint(insertButton, startPoint)
  mouseMoveToPoint(canvasControlsLayer, endPoint, { eventOptions: { buttons: 1 } })
  mouseUpAtPoint(canvasControlsLayer, endPoint)

  await renderResult.getDispatchFollowUpActionsFinished()
}

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

    await dragFromInsertMenuDivButtonToPoint(targetPoint, renderResult)

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

    await dragFromInsertMenuDivButtonToPoint(targetPoint, renderResult)

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
        style={{
          width: 100,
          height: 190,
          backgroundColor: '#FF0000',
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

    await dragFromInsertMenuDivButtonToPoint(targetPoint, renderResult)

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

  it('Should insert a div into a flex layout at the next position', async () => {
    const renderResult = await setupInsertTest(inputCode)

    const targetPrevSibling = renderResult.renderedDOM.getByTestId('bbb')
    const targetPrevSiblingBounds = targetPrevSibling.getBoundingClientRect()
    // Drag close to the right edge of the target sibling
    const targetPoint = {
      x: targetPrevSiblingBounds.x + targetPrevSiblingBounds.width - 5,
      y: targetPrevSiblingBounds.y + targetPrevSiblingBounds.height - 5,
    }

    await dragFromInsertMenuDivButtonToPoint(targetPoint, renderResult)

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

  it('Should insert a div into a child of a flex layout which provides absolute bounds', async () => {
    const renderResult = await setupInsertTest(inputCode)

    const targetParentElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetParentElementBounds = targetParentElement.getBoundingClientRect()

    const targetPoint = {
      x: targetParentElementBounds.x + targetParentElementBounds.width / 2,
      y: targetParentElementBounds.y + targetParentElementBounds.height / 2,
    }

    await dragFromInsertMenuDivButtonToPoint(targetPoint, renderResult)

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
