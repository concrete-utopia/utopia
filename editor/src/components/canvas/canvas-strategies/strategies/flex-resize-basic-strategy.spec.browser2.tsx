/* eslint-disable jest/expect-expect */
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import { canvasPoint, offsetPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import { shiftModifier } from '../../../../utils/modifiers'
import { slightlyOffsetPointBecauseVeryWeirdIssue } from '../../../../utils/utils.test-utils'
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
import { edgePosition, EdgePositionBottomRight, EdgePositionTopRight } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../../event-helpers.test-utils'
import { FLEX_RESIZE_STRATEGY_ID } from './flex-resize-strategy'

async function dragResizeControl(
  renderResult: EditorRenderResult,
  target: ElementPath,
  pos: EdgePosition,
  dragDelta: CanvasVector,
  modifiers?: Modifiers,
  shouldStrategyBeActive: boolean = true,
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

  if (shouldStrategyBeActive === true) {
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
      FLEX_RESIZE_STRATEGY_ID,
    )
  }

  await mouseUpAtPoint(canvasControlsLayer, endPoint)

  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('Flex Resize', () => {
  describe('Flex resize in row', () => {
    // Corner tests
    it('resizes a flex element from edgePosition 0, 0 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(0, 0), canvasPoint({ x: 15, y: 25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 0, 0 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(0, 0), canvasPoint({ x: 15, y: -25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 0, 0 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(0, 0), canvasPoint({ x: -15, y: 25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 0, 0 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(0, 0), canvasPoint({ x: -15, y: -25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(0, 1), canvasPoint({ x: 15, y: 25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(0, 1), canvasPoint({ x: 15, y: -25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(0, 1), canvasPoint({ x: -15, y: 25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(0, 1), canvasPoint({ x: -15, y: -25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(1, 0), canvasPoint({ x: 15, y: 25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(1, 0), canvasPoint({ x: 15, y: -25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(1, 0), canvasPoint({ x: -15, y: 25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(1, 0), canvasPoint({ x: -15, y: -25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(1, 1), canvasPoint({ x: 15, y: 25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(1, 1), canvasPoint({ x: 15, y: -25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(1, 1), canvasPoint({ x: -15, y: 25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(1, 1), canvasPoint({ x: -15, y: -25 }), 65, 165)
    })
    // Edge tests
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(0, 0.5), canvasPoint({ x: 15, y: 25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(0, 0.5), canvasPoint({ x: 15, y: -25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(0, 0.5), canvasPoint({ x: -15, y: 25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(0, 0.5), canvasPoint({ x: -15, y: -25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(0.5, 0), canvasPoint({ x: 15, y: 25 }), 80, 165)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(0.5, 0), canvasPoint({ x: 15, y: -25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(0.5, 0), canvasPoint({ x: -15, y: 25 }), 80, 165)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(0.5, 0), canvasPoint({ x: -15, y: -25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(1, 0.5), canvasPoint({ x: 15, y: 25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(1, 0.5), canvasPoint({ x: 15, y: -25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(1, 0.5), canvasPoint({ x: -15, y: 25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(1, 0.5), canvasPoint({ x: -15, y: -25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (15, 25)', async () => {
      await resizeTestRow(edgePosition(0.5, 1), canvasPoint({ x: 15, y: 25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (15, -25)', async () => {
      await resizeTestRow(edgePosition(0.5, 1), canvasPoint({ x: 15, y: -25 }), 80, 165)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (-15, 25)', async () => {
      await resizeTestRow(edgePosition(0.5, 1), canvasPoint({ x: -15, y: 25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (-15, -25)', async () => {
      await resizeTestRow(edgePosition(0.5, 1), canvasPoint({ x: -15, y: -25 }), 80, 165)
    })
  })

  describe('Flex resize in column', () => {
    // Corner tests
    it('resizes a flex element from edgePosition 0, 0 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(0, 0), canvasPoint({ x: 15, y: 25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 0, 0 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(0, 0), canvasPoint({ x: 15, y: -25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 0, 0 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(0, 0), canvasPoint({ x: -15, y: 25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 0, 0 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(0, 0), canvasPoint({ x: -15, y: -25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(0, 1), canvasPoint({ x: 15, y: 25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(0, 1), canvasPoint({ x: 15, y: -25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(0, 1), canvasPoint({ x: -15, y: 25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 0, 1 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(0, 1), canvasPoint({ x: -15, y: -25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(1, 0), canvasPoint({ x: 15, y: 25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(1, 0), canvasPoint({ x: 15, y: -25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(1, 0), canvasPoint({ x: -15, y: 25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 1, 0 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(1, 0), canvasPoint({ x: -15, y: -25 }), 65, 215)
    })
    // Edge tests
    it('resizes a flex element from edgePosition 1, 1 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(1, 1), canvasPoint({ x: 15, y: 25 }), 95, 215)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(1, 1), canvasPoint({ x: 15, y: -25 }), 95, 165)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(1, 1), canvasPoint({ x: -15, y: 25 }), 65, 215)
    })
    it('resizes a flex element from edgePosition 1, 1 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(1, 1), canvasPoint({ x: -15, y: -25 }), 65, 165)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(0, 0.5), canvasPoint({ x: 15, y: 25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(0, 0.5), canvasPoint({ x: 15, y: -25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(0, 0.5), canvasPoint({ x: -15, y: 25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 0, 0.5 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(0, 0.5), canvasPoint({ x: -15, y: -25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 0), canvasPoint({ x: 15, y: 25 }), 80, 165)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 0), canvasPoint({ x: 15, y: -25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 0), canvasPoint({ x: -15, y: 25 }), 80, 165)
    })
    it('resizes a flex element from edgePosition 0.5, 0 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 0), canvasPoint({ x: -15, y: -25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(1, 0.5), canvasPoint({ x: 15, y: 25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(1, 0.5), canvasPoint({ x: 15, y: -25 }), 95, 190)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(1, 0.5), canvasPoint({ x: -15, y: 25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 1, 0.5 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(1, 0.5), canvasPoint({ x: -15, y: -25 }), 65, 190)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (15, 25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 1), canvasPoint({ x: 15, y: 25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (15, -25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 1), canvasPoint({ x: 15, y: -25 }), 80, 165)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (-15, 25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 1), canvasPoint({ x: -15, y: 25 }), 80, 215)
    })
    it('resizes a flex element from edgePosition 0.5, 1 with drag vector (-15, -25)', async () => {
      await resizeTestColumn(edgePosition(0.5, 1), canvasPoint({ x: -15, y: -25 }), 80, 165)
    })
  })

  describe('when aspect ratio is locked', () => {
    describe('horizontal', () => {
      it('respects ratio (right)', async () => {
        await resizeWithModifiers(
          edgePosition(1, 0.5),
          canvasPoint({ x: 15, y: 20 }),
          { width: 100, height: 200 },
          { width: 115, height: 230 },
          shiftModifier,
        )
      })
      it('respects ratio (left)', async () => {
        await resizeWithModifiers(
          edgePosition(0, 0.5),
          canvasPoint({ x: 15, y: 20 }),
          { width: 100, height: 200 },
          { width: 85, height: 170 },
          shiftModifier,
        )
      })
    })
    describe('vertical', () => {
      it('respects ratio (bottom)', async () => {
        await resizeWithModifiers(
          edgePosition(0.5, 1),
          canvasPoint({ x: 20, y: 15 }),
          { width: 100, height: 200 },
          { width: 108, height: 215 },
          shiftModifier,
        )
      })
      it('respects ratio (top)', async () => {
        await resizeWithModifiers(
          edgePosition(0.5, 0),
          canvasPoint({ x: 20, y: 15 }),
          { width: 100, height: 200 },
          { width: 93, height: 185 },
          shiftModifier,
        )
      })
    })
    describe('diagonal', () => {
      it('respects ratio (br)', async () => {
        await resizeWithModifiers(
          edgePosition(1, 1),
          canvasPoint({ x: 15, y: 20 }),
          { width: 100, height: 200 },
          { width: 115, height: 230 },
          shiftModifier,
        )
      })
      it('respects ratio (tr)', async () => {
        await resizeWithModifiers(
          edgePosition(1, 0),
          canvasPoint({ x: 15, y: 20 }),
          { width: 100, height: 200 },
          { width: 115, height: 230 },
          shiftModifier,
        )
      })
      it('respects ratio (tl)', async () => {
        await resizeWithModifiers(
          edgePosition(0, 0),
          canvasPoint({ x: 15, y: 20 }),
          { width: 100, height: 200 },
          { width: 90, height: 180 },
          shiftModifier,
        )
      })
      it('respects ratio (bl)', async () => {
        await resizeWithModifiers(
          edgePosition(0, 1),
          canvasPoint({ x: 15, y: 20 }),
          { width: 100, height: 200 },
          { width: 110, height: 220 },
          shiftModifier,
        )
      })
    })
  })

  describe('groups', () => {
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
      const target = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/group`,
      )

      await dragResizeControl(
        renderResult,
        target,
        EdgePositionBottomRight,
        canvasPoint({ x: 20, y: 30 }),
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
                  width: 190,
                  height: 210,
                }}
                >
                  <div
                    data-uid='foo'
                    style={{
                      position: 'absolute',
                      left: 0,
                      top: 0,
                      width: 63,
                      height: 70,
                      background: 'red',
                    }}
                  />
                  <div
                    data-uid='bar'
                    style={{
                      position: 'absolute',
                      left: 127,
                      top: 140,
                      width: 63,
                      height: 70,
                      background: 'blue',
                    }}
                  />
              </Group>
            </div>
         `),
        ),
      )
    })
  })
})

describe('resize with flexBasis set', () => {
  it('updates flexBasis in a flex row', async () => {
    const testProject = makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        display: 'flex',
      }}
    >
      <div
        data-uid='bbb'
        style={{
          flexBasis: 80,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
    `)
    const renderResult = await renderTestEditorWithCode(testProject, 'await-first-dom-report')
    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)

    await dragResizeControl(
      renderResult,
      target,
      EdgePositionTopRight,
      canvasPoint({ x: 15, y: 20 }),
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          position: 'relative',
          display: 'flex',
        }}
      >
        <div
          data-uid='bbb'
          style={{
            flexBasis: 95,
            height: 170,
            backgroundColor: '#FF0000',
          }}
        />
      </div>`),
    )
  })
  it('updates flexBasis in a flex column', async () => {
    const testProject = makeTestProjectCodeWithSnippet(`
    <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        position: 'relative',
        display: 'flex',
        flexDirection: 'column'
      }}
    >
      <div
        data-uid='bbb'
        style={{
          flexBasis: 80,
          width: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
    `)
    const renderResult = await renderTestEditorWithCode(testProject, 'await-first-dom-report')
    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)

    await dragResizeControl(
      renderResult,
      target,
      EdgePositionTopRight,
      canvasPoint({ x: 15, y: 20 }),
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          position: 'relative',
          display: 'flex',
          flexDirection: 'column'
        }}
      >
        <div
          data-uid='bbb'
          style={{
            flexBasis: 60,
            width: 205,
            backgroundColor: '#FF0000',
          }}
        />
      </div>`),
    )
  })
})

async function resizeTestRow(
  pos: EdgePosition,
  dragVector: CanvasVector,
  expectedWidth: number,
  expextedHeight: number,
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
            width: 80,
            height: 190,
            backgroundColor: '#FF0000',
          }}
        />
        <div
          data-uid='ddd'
          style={{
            width: 50,
            height: 110,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
    `)

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)

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
            width: ${expectedWidth},
            height: ${expextedHeight},
            backgroundColor: '#FF0000',
          }}
        />
        <div
          data-uid='ddd'
          style={{
            width: 50,
            height: 110,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
      `),
  )
}

async function resizeTestColumn(
  pos: EdgePosition,
  dragVector: CanvasVector,
  expectedWidth: number,
  expextedHeight: number,
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
            width: 80,
            height: 190,
            backgroundColor: '#FF0000',
          }}
        />
        <div
          data-uid='ddd'
          style={{
            width: 50,
            height: 110,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
    `)

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)

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
            width: ${expectedWidth},
            height: ${expextedHeight},
            backgroundColor: '#FF0000',
          }}
        />
        <div
          data-uid='ddd'
          style={{
            width: 50,
            height: 110,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
      `),
  )
}

const resizeWithoutDimensions = async (
  pos: EdgePosition,
  dragVector: CanvasVector,
  initialDimensions: { width?: number; height?: number },
  expectDimensions: { width?: number; height?: number },
  shouldStrategyBeActive: boolean = true,
) => {
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
          style={
            ${JSON.stringify({ backgroundColor: '#f0f', ...initialDimensions })}
          }
        />
        <div
          data-uid='ddd'
          style={{
            width: 50,
            height: 110,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
    `)

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)

  await dragResizeControl(renderResult, target, pos, dragVector, undefined, shouldStrategyBeActive)

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
          style={
            ${JSON.stringify({ backgroundColor: '#f0f', ...expectDimensions })}
          }
        />
        <div
          data-uid='ddd'
          style={{
            width: 50,
            height: 110,
            backgroundColor: '#FF0000',
          }}
        />
      </div>
      `),
  )
}

async function resizeWithModifiers(
  pos: EdgePosition,
  dragVector: CanvasVector,
  initial: { width: number; height: number },
  expected: { width: number; height: number },
  modifiers: Modifiers,
) {
  const inputCode = makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          display: 'flex',
        }}
      >
        <div
          data-uid='ccc'
          style={{
            width: ${initial.width},
            height: ${initial.height},
            backgroundColor: '#09f',
          }}
        />
      </div>
    `)

  const renderResult = await renderTestEditorWithCode(inputCode, 'await-first-dom-report')
  const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)

  await dragResizeControl(renderResult, target, pos, dragVector, modifiers)

  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
    makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          display: 'flex',
        }}
      >
        <div
          data-uid='ccc'
          style={{
            width: ${expected.width},
            height: ${expected.height},
            backgroundColor: '#09f',
          }}
        />
      </div>
      `),
  )
}
