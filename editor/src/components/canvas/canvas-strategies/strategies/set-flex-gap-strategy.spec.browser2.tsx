import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  FlexGapControlHandleTestId,
  FlexGapControlTestId,
} from '../../controls/select-mode/flex-gap-control'
import { mouseClickAtPoint } from '../../event-helpers.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'

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
})
