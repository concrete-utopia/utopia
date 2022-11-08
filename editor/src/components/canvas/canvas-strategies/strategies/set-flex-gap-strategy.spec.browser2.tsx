import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  FlexGapControlHandleTestId,
  FlexGapControlTestId,
} from '../../controls/select-mode/flex-gap-control'
import { mouseClickAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

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
        backgroundColor: '#0091FFAA',
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
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControls = [
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlTestId),
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlHandleTestId),
    ]

    expect(gapControls).toEqual([])
  })
  it('gap controls are not present when flex gap is not set on element', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#0091FFAA',
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
          backgroundColor: '#0091FFAA',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
      <div
        style={{
          backgroundColor: '#0091FFAA',
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
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControls = [
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlTestId),
      ...editor.renderedDOM.queryAllByTestId(FlexGapControlHandleTestId),
    ]

    expect(gapControls).toEqual([])
  })
  it('gap controls are present when flex gap is set on element and element has children', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#0091FFAA',
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
          backgroundColor: '#0091FFAA',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
      <div
        style={{
          backgroundColor: '#0091FFAA',
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
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()
    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlContainer = editor.renderedDOM.getByTestId(FlexGapControlTestId)
    const gapControlHandles = editor.renderedDOM.queryAllByTestId(FlexGapControlHandleTestId)

    expect(gapControlContainer).toBeTruthy()
    expect(gapControlHandles.length).toEqual(1)
  })

  it('adjusts gap by dragging the handle', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '42px',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId(FlexGapControlHandleTestId)
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) + 11,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '53px',`, flexDirection: 'row' }),
    )
  })

  it('adjusts gap by dragging the handle, in em units', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '2.7em',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId(FlexGapControlHandleTestId)
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) + 11,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '3.4em',`, flexDirection: 'row' }),
    )
  })

  it('adjusts gap in column by dragging the handle', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '53px',`, flexDirection: 'column' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId(FlexGapControlHandleTestId)
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2) + 11,
    }

    mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '64px',`, flexDirection: 'column' }),
    )
  })

  it('cannot adjust gap value to be lower than 0', async () => {
    const editor = await renderTestEditorWithCode(
      testCodeWithGap({ gap: `gap: '12px',`, flexDirection: 'row' }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId(DivTestId)
    const divBounds = div.getBoundingClientRect()

    mouseClickAtPoint(canvasControlsLayer, {
      x: divBounds.x + 5,
      y: divBounds.y + 5,
    })

    const gapControlHandle = editor.renderedDOM.getByTestId(FlexGapControlHandleTestId)
    const gapControlBounds = gapControlHandle.getBoundingClientRect()

    const center = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2),
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    const endPoint = {
      x: Math.floor(gapControlBounds.x + gapControlBounds.width / 2) - 22,
      y: Math.floor(gapControlBounds.y + gapControlBounds.height / 2),
    }

    mouseDragFromPointToPoint(gapControlHandle, center, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      testCodeWithGap({ gap: `gap: '0px',`, flexDirection: 'row' }),
    )
  })
})

interface GapTestCodeParams {
  flexDirection: string
  gap: string
}

function testCodeWithGap(params: GapTestCodeParams): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${DivTestId}'
      style={{
        backgroundColor: '#0091FFAA',
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
          backgroundColor: '#0091FFAA',
          width: 102,
          height: 80,
          contain: 'layout',
        }}
        data-uid='fed'
      />
      <div
        style={{
          backgroundColor: '#0091FFAA',
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
