/* eslint-disable jest/expect-expect */
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import { canvasPoint, CanvasVector, offsetPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifiers, shiftModifier } from '../../../../utils/modifiers'
import { slightlyOffsetPointBecauseVeryWeirdIssue } from '../../../../utils/utils.test-utils'
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
  EdgePositionBottomRight,
  EdgePositionRight,
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
        canvasPoint({ x: 70, y: 25 }),
        'row',
        'flex-start',
        'height: 85',
      )
    })
    it('resizing in a flex row from the right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: 70, y: 25 }),
        'row',
        'flex-start',
        'height: 60',
      )
    })
    it('resizing in a flex column from bottom-right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: 10, y: 60 }),
        'column',
        'flex-start',
        'width: 60',
      )
    })
    it('resizing in a flex column from the bottom edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottom,
        canvasPoint({ x: 10, y: 60 }),
        'column',
        'flex-start',
        'width: 50',
      )
    })
    it('resizing in a centered flex row from bottom-right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: 70, y: 25 }),
        'row',
        'center',
        'height: 85',
      )
    })
    it('resizing in a centered flex row from the right edge', async () => {
      await resizeTestAddsFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: 70, y: 25 }),
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
        canvasPoint({ x: -70, y: -25 }),
        'row',
        'flex-start',
        'height: 60',
        'height: 35, width: 50',
      )
    })
    it('resizing in a flex row from the right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: -70, y: -25 }),
        'row',
        'flex-start',
        'height: 60',
        'height: 60, width: 50',
      )
    })
    it('resizing in a flex column from bottom-right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: -10, y: -60 }),
        'column',
        'flex-start',
        'width: 50',
        'width: 40, height: 60',
      )
    })
    it('resizing in a flex column from the bottom edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottom,
        canvasPoint({ x: -10, y: -60 }),
        'column',
        'flex-start',
        'width: 50',
        'width: 50, height: 60',
      )
    })
    it('resizing in a centered flex row from bottom-right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionBottomRight,
        canvasPoint({ x: -70, y: -25 }),
        'row',
        'center',
        'height: 60',
        'height: 35, width: 50',
      )
    })
    it('resizing in a centered flex row from the right edge', async () => {
      await resizeTestRemovesFlexGrow(
        EdgePositionRight,
        canvasPoint({ x: -70, y: -25 }),
        'row',
        'center',
        'height: 60',
        'height: 60, width: 50',
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
