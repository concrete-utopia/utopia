/* eslint-disable jest/expect-expect */
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  canvasPoint,
  CanvasVector,
  offsetPoint,
  size,
  Size,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifiers, shiftModifier } from '../../../../utils/modifiers'
import { slightlyOffsetPointBecauseVeryWeirdIssue, wait } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import {
  EdgePosition,
  edgePosition,
  EdgePositionBottom,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTopLeft,
  EdgePositionTopRight,
} from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../../event-helpers.test-utils'

async function dragResizeControl(
  renderResult: EditorRenderResult,
  target: ElementPath,
  pos: EdgePosition,
  dragDelta: CanvasVector,
  modifiers?: Modifiers,
) {
  await renderResult.dispatch([selectComponents([target], false)], true)
  const resizeControl = renderResult.renderedDOM.getByTestId(`resize-control-${pos.x}-${pos.y}`)
  const resizeControlBounds = resizeControl.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = canvasPoint(
    slightlyOffsetPointBecauseVeryWeirdIssue({
      x: resizeControlBounds.x + resizeControlBounds.width / 2,
      y: resizeControlBounds.y + resizeControlBounds.height / 2,
    }),
  )

  const endPoint = offsetPoint(startPoint, dragDelta)

  await mouseMoveToPoint(resizeControl, startPoint)
  await mouseDownAtPoint(resizeControl, startPoint)
  await mouseMoveToPoint(canvasControlsLayer, endPoint, { eventOptions: { buttons: 1 }, modifiers })
  await mouseUpAtPoint(canvasControlsLayer, endPoint)

  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('Flex Resize with flex grow', () => {
  describe('When reaching the parent edge flexGrow is added', () => {
    it('resizing in a flex row from bottom-right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        'flex-start',
        'height: 85',
      )
    })
    it('resizing in a flex row from the right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        'flex-start',
        'height: 60',
      )
    })
    it('resizing in a flex column from bottom-right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: 10, y: 50 }),
        'column',
        'flex-start',
        'width: 60',
      )
    })
    it('resizing in a flex column from the bottom edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottom,
        canvasPoint({ x: 10, y: 50 }),
        'column',
        'flex-start',
        'width: 50',
      )
    })
    it('resizing in a centered flex row from bottom-right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        'center',
        'height: 85',
      )
    })
    it('resizing in a centered flex row from the right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        'center',
        'height: 60',
      )
    })
  })
  describe('Resizing an element with flexGrow adds flexBasis and removes flexGrow', () => {
    it('resizing in a flex row from bottom-right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: -60, y: -25 }),
        'row',
        'flex-start',
        'height: 60',
        'height: 35, width: 50',
      )
    })
    it('resizing in a flex row from the right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: -60, y: -25 }),
        'row',
        'flex-start',
        'height: 60',
        'height: 60, width: 50',
      )
    })
    it('resizing in a flex column from bottom-right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: -10, y: -50 }),
        'column',
        'flex-start',
        'width: 50',
        'width: 40, height: 60',
      )
    })
    it('resizing in a flex column from the bottom edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottom,
        canvasPoint({ x: -10, y: -50 }),
        'column',
        'flex-start',
        'width: 50',
        'width: 50, height: 60',
      )
    })
    it('resizing in a centered flex row from bottom-right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: -60, y: -25 }),
        'row',
        'center',
        'height: 60',
        'height: 35, width: 50',
      )
    })
    it('resizing in a centered flex row from the right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: -60, y: -25 }),
        'row',
        'center',
        'height: 60',
        'height: 60, width: 50',
      )
    })
  })
  describe('Resizing an element that`s not on the edge (not the first/last sibling)', () => {
    it('when an element has flexGrow resizing removes it', async () => {
      const initialStyle = {
        backgroundColor: '#aaaaaa33',
        height: 25,
        flexGrow: 1,
      }
      const expectedStyle = {
        backgroundColor: '#aaaaaa33',
        height: 25,
        width: 50,
      }
      await resizeTestOnNonEdgeSibling(
        EdgePositionRight,
        canvasPoint({ x: -60, y: 25 }),
        'row',
        'center',
        initialStyle,
        expectedStyle,
      )
    })
    it('when an element reaches the edge of its sibling flexGrow is not added', async () => {
      const initialStyle = {
        backgroundColor: '#aaaaaa33',
        height: 25,
        width: 60,
      }
      const expectedStyle = {
        backgroundColor: '#aaaaaa33',
        height: 25,
        width: 120,
      }
      await resizeTestOnNonEdgeSibling(
        EdgePositionRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        'center',
        initialStyle,
        expectedStyle,
      )
    })
  })
  describe('Resizing from the inner edge positions doesn`t snap, it keeps width and height', () => {
    it('resizing in a flex row from left edge of the last element (flex-start)', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(110, 35)
      await resizeTestKeepsWidthHeight(
        EdgePositionBottomLeft,
        canvasPoint({ x: -60, y: -25 }),
        'row',
        'flex-start',
        initialSize,
        expectedSize,
      )
    })
    it('resizing in a flex column from top edge of the last element (flex-start)', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(60, 110)
      await resizeTestKeepsWidthHeight(
        EdgePositionTopLeft,
        canvasPoint({ x: -10, y: -50 }),
        'column',
        'flex-start',
        initialSize,
        expectedSize,
      )
    })
    it('resizing in a flex row from right edge of the first element (flex-end)', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(110, 35)
      await resizeTestKeepsWidthHeight(
        EdgePositionTopRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        'flex-end',
        initialSize,
        expectedSize,
      )
    })
    it('resizing in a flex column from bottom edge of the first element (flex-end)', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(60, 110)
      await resizeTestKeepsWidthHeight(
        EdgePositionBottomRight,
        canvasPoint({ x: 10, y: 50 }),
        'column',
        'flex-end',
        initialSize,
        expectedSize,
      )
    })
  })
  describe('Resizing when the parent has hug x hug set', () => {
    it('In a flex row when reaching the parent edge width is updated, instead of adding flexGrow', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(110, 85)
      await resizeTestParentSizeMaxContent(
        EdgePositionBottomRight,
        canvasPoint({ x: 60, y: 25 }),
        'row',
        initialSize,
        expectedSize,
      )
    })
    it('In a flex column when reaching the parent edge width is updated, instead of adding flexGrow', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(60, 110)
      await resizeTestParentSizeMaxContent(
        EdgePositionBottomRight,
        canvasPoint({ x: 10, y: 50 }),
        'column',
        initialSize,
        expectedSize,
      )
    })
  })
})

async function resizeTestAddsFlexGrow(
  pos: EdgePosition,
  dragVector: CanvasVector,
  flexDirection: 'row' | 'column',
  justifyContent: 'flex-start' | 'center',
  widthOrHeight: string,
) {
  const inputCode = makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
          position: 'relative',
          display: 'flex',
          flexDirection: '${flexDirection}',
          justifyContent: '${justifyContent}',
          gap: 10,
          padding: 5,
        }}
      >
        <div
          data-uid='bbb'
          style={{
            width: 180,
            height: 180,
            backgroundColor: '#d3d3d3',
          }}
        />
        <div
          data-uid='ccc'
          style={{
            backgroundColor: '#FF0000',
            width: 80,
            height: 80,
          }}
        />
        <div
          data-uid='ddd'
          style={{
            backgroundColor: '#FF0000',
            width: 50,
            height: 60,
          }}
        />
      </div>
    `)

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ddd`)

  await dragResizeControl(renderResult, target, pos, dragVector)

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
          flexDirection: '${flexDirection}',
          justifyContent: '${justifyContent}',
          gap: 10,
          padding: 5,
        }}
      >
        <div
          data-uid='bbb'
          style={{
            width: 180,
            height: 180,
            backgroundColor: '#d3d3d3',
          }}
        />
        <div
          data-uid='ccc'
          style={{
            backgroundColor: '#FF0000',
            width: 80,
            height: 80,
          }}
        />
        <div
          data-uid='ddd'
          style={{
            backgroundColor: '#FF0000',
            ${widthOrHeight},
            flexGrow: 1,
          }}
        />
      </div>
      `),
  )
}

async function resizeTestRemovesFlexGrow(
  pos: EdgePosition,
  dragVector: CanvasVector,
  flexDirection: 'row' | 'column',
  justifyContent: 'flex-start' | 'center',
  widthOrHeight: string,
  resultWidthHeight: string,
) {
  const inputCode = makeTestProjectCodeWithSnippet(`
      <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        flexDirection: '${flexDirection}',
        justifyContent: '${justifyContent}',
        gap: 10,
        padding: 5,
      }}
    >
      <div
        data-uid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          backgroundColor: '#FF0000',
          width: 80,
          height: 80,
        }}
      />
      <div
        data-uid='ddd'
        style={{
          backgroundColor: '#FF0000',
          flexGrow: 1,
          ${widthOrHeight},
        }}
      />
    </div>
    `)

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ddd`)

  await dragResizeControl(renderResult, target, pos, dragVector)

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
          flexDirection: '${flexDirection}',
          justifyContent: '${justifyContent}',
          gap: 10,
          padding: 5,
        }}
      >
        <div
          data-uid='bbb'
          style={{
            width: 180,
            height: 180,
            backgroundColor: '#d3d3d3',
          }}
        />
        <div
          data-uid='ccc'
          style={{
            backgroundColor: '#FF0000',
            width: 80,
            height: 80,
          }}
        />
        <div
          data-uid='ddd'
          style={{
            backgroundColor: '#FF0000',
            ${resultWidthHeight}
          }}
        />
      </div>
      `),
  )
}

async function resizeTestOnNonEdgeSibling(
  pos: EdgePosition,
  dragVector: CanvasVector,
  flexDirection: 'row' | 'column',
  justifyContent: 'flex-start' | 'center',
  initialStyle: React.CSSProperties,
  expectedStyle: React.CSSProperties,
) {
  const inputCode = (targetStyle: React.CSSProperties) =>
    makeTestProjectCodeWithSnippet(`
      <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        flexDirection: '${flexDirection}',
        justifyContent: '${justifyContent}',
        gap: 10,
        padding: 5,
      }}
    >
      <div
        data-uid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ddd'
        style={${JSON.stringify(targetStyle)}}
      />
      <div
        data-uid='ccc'
        style={{
          backgroundColor: '#FF0000',
          width: 80,
          height: 80,
        }}
      />
    </div>
    `)

  const renderResult = await renderTestEditorWithCode(
    inputCode(initialStyle),
    'await-first-dom-report',
  )
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ddd`)

  await dragResizeControl(renderResult, target, pos, dragVector)

  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(inputCode(expectedStyle))
}

async function resizeTestKeepsWidthHeight(
  pos: EdgePosition,
  dragVector: CanvasVector,
  flexDirection: 'row' | 'column',
  justifyContent: 'flex-start' | 'flex-end',
  initialSize: Size,
  expectedSize: Size,
) {
  const targetElement = (targetSize: Size) => `<div
    data-uid='ddd'
    style={{
      backgroundColor: '#FF0000',
      width: ${targetSize.width},
      height: ${targetSize.height},
    }}
  />`

  const inputCode = (targetSize: Size) =>
    makeTestProjectCodeWithSnippet(`
      <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        flexDirection: '${flexDirection}',
        justifyContent: '${justifyContent}',
        gap: 10,
        padding: 5,
      }}
    >
      ${justifyContent === 'flex-end' ? targetElement(targetSize) : ''}
      <div
        data-uid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          backgroundColor: '#FF0000',
          width: 80,
          height: 80,
        }}
      />
      ${justifyContent === 'flex-start' ? targetElement(targetSize) : ''}
    </div>
    `)

  const renderResult = await renderTestEditorWithCode(
    inputCode(initialSize),
    'await-first-dom-report',
  )
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ddd`)

  await dragResizeControl(renderResult, target, pos, dragVector)

  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(inputCode(expectedSize))
}

async function resizeTestParentSizeMaxContent(
  pos: EdgePosition,
  dragVector: CanvasVector,
  flexDirection: 'row' | 'column',
  initialSize: Size,
  expectedSize: Size,
) {
  const inputCode = (targetSize: Size) =>
    makeTestProjectCodeWithSnippet(`
      <div
      data-uid='aaa'
      style={{
        width: 'max-content',
        height: 'max-content',
        backgroundColor: '#FFFFFF',
        position: 'relative',
        display: 'flex',
        flexDirection: '${flexDirection}',
        gap: 10,
        padding: 5,
      }}
    >
      <div
        data-uid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      />
      <div
        data-uid='ccc'
        style={{
          backgroundColor: '#FF0000',
          width: 80,
          height: 80,
        }}
      />
      <div
        data-uid='ddd'
        style={{
          backgroundColor: '#FF0000',
          width: ${targetSize.width},
          height: ${targetSize.height},
        }}
      />
    </div>
    `)

  const renderResult = await renderTestEditorWithCode(
    inputCode(initialSize),
    'await-first-dom-report',
  )
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ddd`)

  await dragResizeControl(renderResult, target, pos, dragVector)

  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(inputCode(expectedSize))
}
