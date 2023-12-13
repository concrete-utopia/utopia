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
import { edgePosition } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseDownAtPoint, mouseMoveToPoint, mouseUpAtPoint } from '../../event-helpers.test-utils'
import { BASIC_RESIZE_STRATEGY_ID } from './basic-resize-strategy'

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
      BASIC_RESIZE_STRATEGY_ID,
    )
  }

  await mouseUpAtPoint(canvasControlsLayer, endPoint)

  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('Basic Resize', () => {
  // Corner tests
  it('resizes a flex element from edgePosition 0, 0 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(0, 0), canvasPoint({ x: 15, y: 25 }), 65, 165)
  })
  it('resizes a flex element from edgePosition 0, 0 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(0, 0), canvasPoint({ x: 15, y: -25 }), 65, 215)
  })
  it('resizes a flex element from edgePosition 0, 0 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(0, 0), canvasPoint({ x: -15, y: 25 }), 95, 165)
  })
  it('resizes a flex element from edgePosition 0, 0 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(0, 0), canvasPoint({ x: -15, y: -25 }), 95, 215)
  })
  it('resizes a flex element from edgePosition 0, 1 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(0, 1), canvasPoint({ x: 15, y: 25 }), 65, 215)
  })
  it('resizes a flex element from edgePosition 0, 1 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(0, 1), canvasPoint({ x: 15, y: -25 }), 65, 165)
  })
  it('resizes a flex element from edgePosition 0, 1 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(0, 1), canvasPoint({ x: -15, y: 25 }), 95, 215)
  })
  it('resizes a flex element from edgePosition 0, 1 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(0, 1), canvasPoint({ x: -15, y: -25 }), 95, 165)
  })
  it('resizes a flex element from edgePosition 1, 0 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(1, 0), canvasPoint({ x: 15, y: 25 }), 95, 165)
  })
  it('resizes a flex element from edgePosition 1, 0 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(1, 0), canvasPoint({ x: 15, y: -25 }), 95, 215)
  })
  it('resizes a flex element from edgePosition 1, 0 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(1, 0), canvasPoint({ x: -15, y: 25 }), 65, 165)
  })
  it('resizes a flex element from edgePosition 1, 0 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(1, 0), canvasPoint({ x: -15, y: -25 }), 65, 215)
  })
  it('resizes a flex element from edgePosition 1, 1 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(1, 1), canvasPoint({ x: 15, y: 25 }), 95, 215)
  })
  it('resizes a flex element from edgePosition 1, 1 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(1, 1), canvasPoint({ x: 15, y: -25 }), 95, 165)
  })
  it('resizes a flex element from edgePosition 1, 1 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(1, 1), canvasPoint({ x: -15, y: 25 }), 65, 215)
  })
  it('resizes a flex element from edgePosition 1, 1 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(1, 1), canvasPoint({ x: -15, y: -25 }), 65, 165)
  })
  // Edge tests
  it('resizes a flex element from edgePosition 0, 0.5 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(0, 0.5), canvasPoint({ x: 15, y: 25 }), 65, 190)
  })
  it('resizes a flex element from edgePosition 0, 0.5 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(0, 0.5), canvasPoint({ x: 15, y: -25 }), 65, 190)
  })
  it('resizes a flex element from edgePosition 0, 0.5 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(0, 0.5), canvasPoint({ x: -15, y: 25 }), 95, 190)
  })
  it('resizes a flex element from edgePosition 0, 0.5 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(0, 0.5), canvasPoint({ x: -15, y: -25 }), 95, 190)
  })
  it('resizes a flex element from edgePosition 0.5, 0 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(0.5, 0), canvasPoint({ x: 15, y: 25 }), 80, 165)
  })
  it('resizes a flex element from edgePosition 0.5, 0 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(0.5, 0), canvasPoint({ x: 15, y: -25 }), 80, 215)
  })
  it('resizes a flex element from edgePosition 0.5, 0 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(0.5, 0), canvasPoint({ x: -15, y: 25 }), 80, 165)
  })
  it('resizes a flex element from edgePosition 0.5, 0 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(0.5, 0), canvasPoint({ x: -15, y: -25 }), 80, 215)
  })
  it('resizes a flex element from edgePosition 1, 0.5 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(1, 0.5), canvasPoint({ x: 15, y: 25 }), 95, 190)
  })
  it('resizes a flex element from edgePosition 1, 0.5 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(1, 0.5), canvasPoint({ x: 15, y: -25 }), 95, 190)
  })
  it('resizes a flex element from edgePosition 1, 0.5 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(1, 0.5), canvasPoint({ x: -15, y: 25 }), 65, 190)
  })
  it('resizes a flex element from edgePosition 1, 0.5 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(1, 0.5), canvasPoint({ x: -15, y: -25 }), 65, 190)
  })
  it('resizes a flex element from edgePosition 0.5, 1 with drag vector (15, 25)', async () => {
    await resizeTest(edgePosition(0.5, 1), canvasPoint({ x: 15, y: 25 }), 80, 215)
  })
  it('resizes a flex element from edgePosition 0.5, 1 with drag vector (15, -25)', async () => {
    await resizeTest(edgePosition(0.5, 1), canvasPoint({ x: 15, y: -25 }), 80, 165)
  })
  it('resizes a flex element from edgePosition 0.5, 1 with drag vector (-15, 25)', async () => {
    await resizeTest(edgePosition(0.5, 1), canvasPoint({ x: -15, y: 25 }), 80, 215)
  })
  it('resizes a flex element from edgePosition 0.5, 1 with drag vector (-15, -25)', async () => {
    await resizeTest(edgePosition(0.5, 1), canvasPoint({ x: -15, y: -25 }), 80, 165)
  })
})

describe('when the element has missing dimensions', () => {
  describe('both missing', () => {
    describe('horizontal movement', () => {
      it('adds the width', async () => {
        await resizeWithoutDimensions(
          edgePosition(1, 0),
          canvasPoint({ x: 15, y: 0 }),
          {},
          { width: 415 },
          false,
        )
      })
    })

    describe('vertical movement', () => {
      it('adds the height', async () => {
        await resizeWithoutDimensions(
          edgePosition(0, 0),
          canvasPoint({ x: 0, y: 15 }),
          {},
          { height: 15 },
          false,
        )
      })
    })

    describe('diagonal movement', () => {
      it('adds both dimensions', async () => {
        await resizeWithoutDimensions(
          edgePosition(0, 0),
          canvasPoint({ x: 10, y: 15 }),
          {},
          { width: 390, height: 15 },
          false,
        )
      })
    })
  })

  describe('width missing', () => {
    describe('horizontal movement', () => {
      it('adds the width', async () => {
        await resizeWithoutDimensions(
          edgePosition(1, 0),
          canvasPoint({ x: 15, y: 0 }),
          {},
          { width: 415 },
          false,
        )
      })
    })

    describe('vertical movement', () => {
      it('adds the height, does not add the width', async () => {
        await resizeWithoutDimensions(
          edgePosition(0, 0),
          canvasPoint({ x: 0, y: 15 }),
          { height: 20 },
          { height: 5 },
        )
      })
    })

    describe('diagonal movement', () => {
      it('updates the height, and adds the width', async () => {
        await resizeWithoutDimensions(
          edgePosition(0, 0),
          canvasPoint({ x: 10, y: 15 }),
          { height: 20 },
          { height: 5, width: 390 },
        )
      })
    })
  })

  describe('height missing', () => {
    describe('horizontal movement', () => {
      it('updates only the width', async () => {
        await resizeWithoutDimensions(
          edgePosition(1, 0),
          canvasPoint({ x: 15, y: 0 }),
          { width: 15 },
          { width: 30 },
        )
      })
    })

    describe('vertical movement', () => {
      it('adds the height', async () => {
        await resizeWithoutDimensions(
          edgePosition(0, 0),
          canvasPoint({ x: 0, y: 15 }),
          { width: 15 },
          { width: 15, height: 15 },
          false,
        )
      })
    })

    describe('diagonal movement', () => {
      it('adds the height and updates width', async () => {
        await resizeWithoutDimensions(
          edgePosition(0, 0),
          canvasPoint({ x: 10, y: 15 }),
          { width: 15 },
          { width: 5, height: 15 },
        )
      })
    })
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

async function resizeTest(
  pos: EdgePosition,
  dragVector: CanvasVector,
  expectedWidth: number,
  expextedHeight: number,
  shouldStrategyBeActive: boolean = true,
) {
  const inputCode = makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
          position: 'relative',
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
  shouldStrategyBeActive: boolean = true,
) {
  const inputCode = makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{ width: '100%', height: '100%' }}
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

  await dragResizeControl(renderResult, target, pos, dragVector, modifiers, shouldStrategyBeActive)

  expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
    makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{ width: '100%', height: '100%' }}
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
