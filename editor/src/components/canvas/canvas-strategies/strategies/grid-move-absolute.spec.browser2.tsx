import * as EP from '../../../../core/shared/element-path'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { offsetPoint, windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { Point } from '../../event-helpers.test-utils'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseMoveToPoint,
  mouseUpAtPoint,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

async function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  targetControlTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const endPoint = offsetPoint(startPoint, dragDelta)

  await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })

  const targetControl = renderResult.renderedDOM.getByTestId(targetControlTestId)

  await mouseDownAtPoint(targetControl, startPoint, { modifiers: modifiers })

  const delta: Point = {
    x: endPoint.x - startPoint.x,
    y: endPoint.y - startPoint.y,
  }
  await mouseMoveToPoint(
    targetControl,
    {
      x: endPoint.x,
      y: endPoint.y,
    },
    {
      modifiers: modifiers,
      eventOptions: {
        movementX: delta.x,
        movementY: delta.y,
        buttons: 1,
      },
    },
  )

  await mouseUpAtPoint(canvasControlsLayer, endPoint, {
    modifiers: modifiers,
  })
}

describe('grid move absolute strategy', () => {
  it('move an absolute element in a non-absolute grid', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb' data-testid='sb'>
    <div
      style={{
        height: 'max-content',
        position: 'absolute',
        left: 776,
        top: 6,
      }}
      data-uid='container'
      data-testid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          left: 100,
          top: 100,
          width: 1000,
          height: 1000,
          display: 'grid',
          gridTemplateColumns: '200px 200px 200px',
          gridTemplateRows: '200px 200px 200px',
          gridGap: 10,
          padding: 10,
        }}
        data-uid='grid'
        data-testid='grid'
      >
        <div
          style={{
            gridColumn: 1,
            gridRow: 1,
            backgroundColor: '#f0f',
            position: 'absolute',
            left: 500,
            top: 500,
            width: 79,
            height: 86,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      </div>
    </div>
  </Storyboard>
)`,
      'await-first-dom-report',
    )

    await dragElement(
      editor,
      'dragme',
      'grid-cell-sb/container/grid/dragme',
      windowPoint({ x: -200, y: -200 }),
      emptyModifiers,
    )

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb' data-testid='sb'>
    <div
      style={{
        height: 'max-content',
        position: 'absolute',
        left: 776,
        top: 6,
      }}
      data-uid='container'
      data-testid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          left: 100,
          top: 100,
          width: 1000,
          height: 1000,
          display: 'grid',
          gridTemplateColumns: '200px 200px 200px',
          gridTemplateRows: '200px 200px 200px',
          gridGap: 10,
          padding: 10,
        }}
        data-uid='grid'
        data-testid='grid'
      >
        <div
          style={{
            backgroundColor: '#f0f',
            position: 'absolute',
            width: 79,
            height: 86,
            gridColumn: 1,
            gridRow: 1,
            top: 300,
            left: 300,
          }}
          data-uid='dragme'
          data-testid='dragme'
        />
      </div>
    </div>
  </Storyboard>
)
`,
    )

    expect(1).toEqual(1)
  })
})
