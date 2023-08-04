/* eslint-disable jest/expect-expect */
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasVector, Size } from '../../../../core/shared/math-utils'
import { canvasPoint, offsetPoint, size } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import { shiftModifier } from '../../../../utils/modifiers'
import { slightlyOffsetPointBecauseVeryWeirdIssue, wait } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import type { EdgePosition } from '../../canvas-types'
import {
  edgePosition,
  EdgePositionBottom,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
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
  describe('Resizing further than the snapping area adds width/height', () => {
    it('resizing in a flex row', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(130, 35) // width 110px to snap
      await resizeTestKeepsWidthHeight(
        EdgePositionTopRight,
        canvasPoint({ x: 80, y: 25 }),
        'row',
        'flex-start',
        initialSize,
        expectedSize,
      )
    })
    it('resizing in a flex column', async () => {
      const initialSize = size(50, 60)
      const expectedSize = size(50, 120) // height 110px to snap
      await resizeTestKeepsWidthHeight(
        EdgePositionBottom,
        canvasPoint({ x: 10, y: 60 }),
        'column',
        'flex-start',
        initialSize,
        expectedSize,
      )
    })
  })
})

describe('Resizing sets hug when size matches with children size', () => {
  describe('Resizing an element with children in a flex row', () => {
    it('Sets hug when sized as children', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: 100,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: 'max-content',
        height: 380,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTopLeft,
        canvasPoint({ x: 18, y: 10 }), // x: 20px to snap
        'row',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets width when the size doesn`t reach the snap threshold', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: 100,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: 86,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionLeft,
        canvasPoint({ x: 14, y: 10 }),
        'row',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets width when size is smaller than children size', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: 100,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: 70,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionLeft,
        canvasPoint({ x: 30, y: 10 }),
        'row',
        initialStyle,
        expectedStyle,
      )
    })
  })
  describe('Resizing an element with children in a flex column', () => {
    it('Sets hug when sized as children, resized element has flexDirection: row', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'row' as const,
        height: 60,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'row' as const,
        height: 'max-content',
        width: 380,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTopLeft,
        canvasPoint({ x: 10, y: 42 }), // y: 40px to snap
        'column',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets height when the size doesn`t reach the snap threshold, resized element has flexDirection: row', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'row' as const,
        height: 60,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'row' as const,
        height: 26,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTop,
        canvasPoint({ x: 10, y: 34 }),
        'column',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets height when size is smaller than children size, resized element has flexDirection: row', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'row' as const,
        height: 60,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'row' as const,
        height: 14,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTop,
        canvasPoint({ x: 10, y: 46 }),
        'column',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets hug when sized as children, resized element has flexDirection: column', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        height: 60,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        height: 'max-content',
        width: 380,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTopLeft,
        canvasPoint({ x: 10, y: 22 }), // y: 20px to snap
        'column',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets height when the size doesn`t reach the snap threshold, resized element has flexDirection: column', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        height: 60,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        height: 46,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTop,
        canvasPoint({ x: 10, y: 14 }),
        'column',
        initialStyle,
        expectedStyle,
      )
    })
    it('Sets height when size is smaller than children size, resized element has flexDirection: column', async () => {
      const initialStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        height: 60,
      }
      const expectedStyle = {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        height: 34,
      }
      await resizeTestWithChildrenSetHug(
        EdgePositionTop,
        canvasPoint({ x: 10, y: 26 }),
        'column',
        initialStyle,
        expectedStyle,
      )
    })
  })
})

it('trues up groups', async () => {
  const inputCode = formatTestProjectCode(
    makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'center',
          gap: 10,
        }}
      >
        <Group
          data-uid='group'
          style={{
            background: 'white',
          }}
          >
            <div
              data-uid='foo'
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
                background: 'red',
              }}
            />
            <div
              data-uid='bar'
              style={{
                position: 'absolute',
                left: 100,
                top: 100,
                width: 50,
                height: 50,
                background: 'blue',
              }}
            />
        </Group>
      </div>
    `),
  )

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/group`)
  await dragResizeControl(
    renderResult,
    target,
    EdgePositionBottomRight,
    canvasPoint({ x: 50, y: 25 }),
  )

  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
    formatTestProjectCode(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='aaa'
          style={{
            display: 'flex',
            flexDirection: 'row',
            justifyContent: 'center',
            gap: 10,
          }}
        >
          <Group
            data-uid='group'
            style={{
              background: 'white',
              width: 250,
              height: 200,
            }}
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 83,
                  height: 67,
                  background: 'red',
                }}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  left: 167,
                  top: 133,
                  width: 83,
                  height: 67,
                  background: 'blue',
                }}
              />
          </Group>
        </div>
      `),
    ),
  )
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

async function resizeTestWithChildrenSetHug(
  pos: EdgePosition,
  dragVector: CanvasVector,
  flexDirection: 'row' | 'column',
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
        gap: 10,
        padding: 5,
      }}
    >
      <div
        data-uid='ddd'
        style={${JSON.stringify(targetStyle)}}
      >
        <div
          style={{
            width: 40,
            height: 20,
            border: '1px solid rgb(0, 0, 0, 1)',
          }}
          data-uid='eee'
        />
        <div
          style={{
            width: 40,
            height: 20,
            border: '1px solid rgb(0, 0, 0, 1)',
          }}
          data-uid='fff'
        />
      </div>
      <div
        data-uid='bbb'
        style={{
          width: 180,
          height: 180,
          backgroundColor: '#d3d3d3',
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
