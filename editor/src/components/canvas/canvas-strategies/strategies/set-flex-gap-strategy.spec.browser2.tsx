/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkFlexGapHandlesPositionedCorrectly"] }] */
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  FlexGapControlHandleTestId,
  FlexGapControlTestId,
} from '../../controls/select-mode/flex-gap-control'
import { valueWithUnitAppropriatePrecision } from '../../controls/select-mode/controls-common'
import { mouseClickAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { shiftModifier } from '../../../../utils/modifiers'
import { FlexGapTearThreshold } from './set-flex-gap-strategy'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import { canvasPoint } from '../../../../core/shared/math-utils'
import { checkFlexGapHandlesPositionedCorrectly } from '../../controls/select-mode/flex-gap-control.test-utils'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import * as EP from '../../../../core/shared/element-path'

const DivTestId = 'mydiv'

describe('Flex gap strategy', () => {
  it('gap controls are not present when element has no children', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 167,
        top: 180,
        width: 557,
        height: 359,
        display: 'flex',
        gap: 10,
      }}
      data-uid='fac'
    ></div>
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControls = [
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlTestId),
      ...editor.renderedDOM.queryAllByTestId((testId) =>
        testId.startsWith(FlexGapControlHandleTestId),
      ),
    ]

    expect(gapControls).toEqual([])
  })
  it('gap controls are present when flex gap is not set on element and element has more than 1 child', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 167,
        top: 180,
        width: 557,
        height: 359,
        display: 'flex',
      }}
      data-uid='fac'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 187,
          height: 150,
          contain: 'layout',
        }}
        data-uid='a39'
      />
    </div>
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    const gapControls = [
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlTestId),
      ...editor.renderedDOM.queryAllByTestId((testId) =>
        testId.startsWith(FlexGapControlHandleTestId),
      ),
    ]

    expect(gapControls.length).toEqual(2)
  })
  it('gap controls are not present when flex gap is not set on element and element only has 1 child', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 167,
        top: 180,
        width: 557,
        height: 359,
        display: 'flex',
        gap: 42
      }}
      data-uid='fac'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
    </div>
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandles = editor.renderedDOM.queryAllByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )

    expect(gapControlHandles).toEqual([])
  })

  it('gap controls are not present when elements are wrapped', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithWrappedLayout(true),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControls = [
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlTestId),
      ...editor.renderedDOM.queryAllByTestId((testId) =>
        testId.startsWith(FlexGapControlHandleTestId),
      ),
    ]

    expect(gapControls).toEqual([])
  })

  it('gap controls are present when elements are not wrapped', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithWrappedLayout(false),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    const gapControls = editor.renderedDOM.queryAllByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )

    expect(gapControls.length).toEqual(3)
  })

  it('adjusts gap by dragging the handle', async () => {
    const initialGap = 42
    const dragDelta = 11

    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '${initialGap}px',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) + dragDelta,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    await mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '${initialGap + dragDelta}px',`, flexDirection: 'row' }),
    )
  })

  it('adjusts gap by dragging the handle, with shift held', async () => {
    const initialGap = 42
    const dragDelta = 11

    const coarseValue = valueWithUnitAppropriatePrecision(null, initialGap + dragDelta, 'coarse')

    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '${initialGap}px',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) + dragDelta,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    await mouseDragFromPointToPoint(gapControlHandle, center, endPoint, {
      modifiers: shiftModifier,
    })
    await editor.getDispatchFollowUpActionsFinished()

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '${coarseValue}px',`, flexDirection: 'row' }),
    )
  })

  it('adjusts gap by dragging the handle, in em units', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '2.7em',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) + 11,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    await mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '3.4em',`, flexDirection: 'row' }),
    )
  })

  it('adjusts gap in column by dragging the handle', async () => {
    const initialGap = 42
    const dragDelta = 11

    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '${initialGap}px',`, flexDirection: 'column' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2) + dragDelta,
    }

    await mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '${initialGap + dragDelta}px',`, flexDirection: 'column' }),
    )
  })

  it('cannot adjust gap value to be lower than 0', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '12px',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.getRenderedCanvas().getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    await mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId((testId) =>
      testId.startsWith(FlexGapControlHandleTestId),
    )
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) - 22,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    await mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '0px',`, flexDirection: 'row' }),
    )
  })

  it('can remove gap prop by dragging more than a set threshold', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '12px',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    await doGapResize(editor, canvasPoint({ x: FlexGapTearThreshold - 12 - 1, y: 0 }))

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithNoGap({ flexDirection: 'row' }),
    )
  })

  it('can remove gap prop by dragging more than a set threshold, in flex column', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '12px',`, flexDirection: 'column' }),
      'await-first-dom-report',
    )

    await doGapResize(editor, canvasPoint({ x: 0, y: FlexGapTearThreshold - 12 - 1 }))

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithNoGap({ flexDirection: 'column' }),
    )
  })

  it('can remove gap prop by dragging more than a set threshold, in flex row-reverse', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '12px',`, flexDirection: 'row-reverse' }),
      'await-first-dom-report',
    )

    await doGapResize(editor, canvasPoint({ x: -(FlexGapTearThreshold - 12 - 1), y: 0 }))

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithNoGap({ flexDirection: 'row-reverse' }),
    )
  })

  it('can remove gap prop by dragging more than a set threshold, in flex column-reverse', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '12px',`, flexDirection: 'column-reverse' }),
      'await-first-dom-report',
    )

    await doGapResize(editor, canvasPoint({ x: 0, y: -(FlexGapTearThreshold - 12 - 1) }))

    await checkFlexGapHandlesPositionedCorrectly(editor, DivTestId)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithNoGap({ flexDirection: 'column-reverse' }),
    )
  })

  it('the gap handles show up where expected in a series of cases', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGapExamples(),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString(`${BakedInStoryboardUID}/example-1`)])
    await checkFlexGapHandlesPositionedCorrectly(editor, 'example-1')

    await selectComponentsForTest(editor, [EP.fromString(`${BakedInStoryboardUID}/example-2`)])
    await checkFlexGapHandlesPositionedCorrectly(editor, 'example-2')

    await selectComponentsForTest(editor, [EP.fromString(`${BakedInStoryboardUID}/example-3`)])
    await checkFlexGapHandlesPositionedCorrectly(editor, 'example-3')

    await selectComponentsForTest(editor, [EP.fromString(`${BakedInStoryboardUID}/example-4`)])
    await checkFlexGapHandlesPositionedCorrectly(editor, 'example-4')

    await selectComponentsForTest(editor, [EP.fromString(`${BakedInStoryboardUID}/example-5`)])
    await checkFlexGapHandlesPositionedCorrectly(editor, 'example-5')

    await selectComponentsForTest(editor, [EP.fromString(`${BakedInStoryboardUID}/example-6`)])
    await checkFlexGapHandlesPositionedCorrectly(editor, 'example-6')
  })
})

interface GapTestCodeParams {
  flexDirection: string
  gap: string
}

function testCodeWithGapExamples(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <div
      data-uid={'example-1'}
      data-testid={'example-1'}
      style={{
        height: 'max-content',
        position: 'absolute',
        left: 50,
        top: 50,
        display: 'flex',
        flexDirection: 'column',
        width: 263,
        gap: 30,
        padding: '30px 15px 34px 15px',
        alignItems: 'flex-start',
        justifyContent: 'flex-start',
      }}
    >
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
    </div>
    <div
      data-uid={'example-2'}    
      data-testid={'example-2'}
      style={{
        height: 'max-content',
        position: 'absolute',
        left: 350,
        top: 50,
        display: 'flex',
        flexDirection: 'column',
        width: 263,
        gap: 30,
        alignItems: 'flex-start',
        justifyContent: 'flex-start',
      }}
    >
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
    </div>
    <div
      data-uid={'example-3'}    
      data-testid={'example-3'}
      style={{
        height: 'max-content',
        position: 'absolute',
        left: 50,
        top: 305,
        display: 'flex',
        flexDirection: 'column',
        width: 263,
        gap: 30,
        alignItems: 'center',
        justifyContent: 'flex-start',
      }}
    >
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
    </div>
    <div
      data-uid={'example-4'}    
      data-testid={'example-4'}
      style={{
        height: 'max-content',
        position: 'absolute',
        left: 400,
        top: 305,
        display: 'flex',
        flexDirection: 'column',
        width: 263,
        gap: 30,
        justifyContent: 'flex-start',
      }}
    >
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
        }}
      >
        absolute this
      </span>
    </div>
    <div
      data-uid={'example-5'}    
      data-testid={'example-5'}
      style={{
        height: 145,
        position: 'absolute',
        left: 50,
        top: 550,
        display: 'flex',
        flexDirection: 'row',
        width: 414,
        gap: 30,
        alignItems: 'flex-end',
        justifyContent: 'flex-start',
      }}
    >
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
    </div>
    <div
      data-uid={'example-6'}    
      data-testid={'example-6'}
      style={{
        height: 145,
        position: 'absolute',
        left: 500,
        top: 550,
        display: 'flex',
        flexDirection: 'row-reverse',
        width: 414,
        gap: 30,
        alignItems: 'flex-end',
        justifyContent: 'flex-start',
      }}
    >
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
      <span
        style={{
          backgroundColor: 'magenta',
          color: 'white',
          width: 'max-content',
        }}
      >
        absolute this
      </span>
    </div>
  </Storyboard>
)
`
}

function testCodeWithGap(params: GapTestCodeParams): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 167,
        top: 180,
        width: 557,
        height: 359,
        display: 'flex',
        flexDirection: '${params.flexDirection}',
        ${params.gap}
      }}
      data-uid='fac'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 187,
          height: 150,
          contain: 'layout',
        }}
        data-uid='a39'
      />
    </div>
  </Storyboard>
)
`
}

type NoGapParams = Omit<GapTestCodeParams, 'gap'>

function testCodeWithNoGap(params: NoGapParams): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 167,
        top: 180,
        width: 557,
        height: 359,
        display: 'flex',
        flexDirection: '${params.flexDirection}',
      }}
      data-uid='fac'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 187,
          height: 150,
          contain: 'layout',
        }}
        data-uid='a39'
      />
    </div>
  </Storyboard>
)
`
}

async function doGapResize(editor: EditorRenderResult, delta: CanvasPoint) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.getRenderedCanvas().getByTestId(DivTestId)
  const divBounds = div.getBoundingClientRect()

  await mouseClickAtPoint(canvasControlsLayer, {
    x: divBounds.x + 5,
    y: divBounds.y + 5,
  })

  const gapControlHandle = editor.renderedDOM.getByTestId((testId) =>
    testId.startsWith(FlexGapControlHandleTestId),
  )
  const gapControlBounds = gapControlHandle.getBoundingClientRect()

  const center = {
    x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
    y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
  }

  const endPoint = {
    x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) + delta.x,
    y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2) + delta.y,
  }

  await mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
  await editor.getDispatchFollowUpActionsFinished()
}

function projectWithWrappedLayout(wrap: boolean): string {
  return `import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='Storyboard'>
      <div
        data-testid='${DivTestId}'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          width: 307,
          height: 234,
          display: 'flex',
          flexWrap: '${wrap ? 'wrap' : 'nowrap'}',
          gap: 41,
          left: 43,
          top: 201,
        }}
        data-uid='cca'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 77,
            height: 51,
            contain: 'layout',
          }}
          data-uid='a58'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 51,
            height: 88,
            contain: 'layout',
          }}
          data-uid='47c'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 40,
            height: 59,
            contain: 'layout',
          }}
          data-uid='121'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 36,
            height: 52,
            contain: 'layout',
          }}
          data-uid='e7f'
        />
      </div>
    </Storyboard>
  )
  `
}
