import type { CSSProperties } from 'react'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'
import type { SimpleRectangle } from '../../..//core/shared/math-utils'
import { selectComponentsForTest } from '../../../utils/utils.test-utils'
import * as EP from '../../../core/shared/element-path'

interface TestGridOutlinesResult {
  top?: SimpleRectangle
  left?: SimpleRectangle
  bottom?: SimpleRectangle
  right?: SimpleRectangle
}

async function testGridOutlines(
  targetPath: string,
  attributes: CSSProperties,
): Promise<TestGridOutlinesResult> {
  const fullAttributes: CSSProperties = { backgroundColor: 'pink', ...attributes }
  let fullAttributesText: string = '{\n'
  for (const [key, value] of Object.entries(fullAttributes)) {
    fullAttributesText += `${key}: ${JSON.stringify(value)},\n`
  }
  fullAttributesText += '}'

  const projectCode = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 1000,
        height: 1000,
        position: 'absolute',
        left: 50,
        top: 100,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        style={{
          display: 'grid',
          gridTemplateRows: '100px 100px 100px',
          gridTemplateColumns: '100px 100px 100px',
          gridGap: 15,
          height: 400,
          width: 400,
          position: 'absolute',
          left: 0,
          top: 0,
          padding: '10px 20px 30px 40px'
        }}
        data-uid='grid'
      >
        <div
          style={
            ${fullAttributesText}
          }
          data-uid='grid-child'
          data-testid='grid-child'
        />
      </div>
      <div
        style={{
          position: 'absolute',
          left: 1000,
          top: 0
        }}
        data-uid='grid-2-outer'
      >
        <div
          style={{
            display: 'grid',
            gridTemplateRows: '100px 100px 100px',
            gridTemplateColumns: '100px 100px 100px',
            gridGap: 15,
            height: 400,
            width: 400,
            left: 0,
            top: 0,
            padding: '10px 20px 30px 40px'
          }}
          data-uid='grid-2'
        >
          <div
            style={
              ${fullAttributesText}
            }
            data-uid='grid-child-2'
            data-testid='grid-child-2'
          />
        </div>
      </div>
    </Scene>
  </Storyboard>
)
`
  const editor = await renderTestEditorWithCode(projectCode, 'await-first-dom-report')
  await selectComponentsForTest(editor, [EP.fromString(targetPath)])

  const topPinLine = editor.renderedDOM.queryByTestId('pin-line-top')
  const leftPinLine = editor.renderedDOM.queryByTestId('pin-line-left')
  const bottomPinLine = editor.renderedDOM.queryByTestId('pin-line-bottom')
  const rightPinLine = editor.renderedDOM.queryByTestId('pin-line-right')

  function toBoundingRect(element: HTMLElement): SimpleRectangle {
    const domRect = element.getBoundingClientRect()
    return {
      x: domRect.x,
      y: domRect.y,
      width: domRect.width,
      height: domRect.height,
    }
  }

  let result: TestGridOutlinesResult = {}
  if (topPinLine != null) {
    result.top = toBoundingRect(topPinLine)
  }
  if (leftPinLine != null) {
    result.left = toBoundingRect(leftPinLine)
  }
  if (bottomPinLine != null) {
    result.bottom = toBoundingRect(bottomPinLine)
  }
  if (rightPinLine != null) {
    result.right = toBoundingRect(rightPinLine)
  }
  return result
}

describe('Grid Pin Outlines', () => {
  it('pinned top and left, grid rows and columns fully specified and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and left, grid rows and columns fully specified and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 688.5, y: 315.5, width: 12, height: 1 },
      top: { x: 725.5, y: 285.5, width: 1, height: 5 },
    })
  })
  it('pinned top and right, grid rows and columns fully specified and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and right, grid rows and columns fully specified and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 777.5, y: 315.5, width: 12, height: 1 },
      top: { x: 751.5, y: 285.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and left, grid rows and columns fully specified and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and left, grid rows and columns fully specified and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 688.5, y: 355.5, width: 12, height: 1 },
      bottom: { x: 725.5, y: 381.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and right, grid rows and columns fully specified and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and right, grid rows and columns fully specified and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 777.5, y: 355.5, width: 12, height: 1 },
      bottom: { x: 751.5, y: 381.5, width: 1, height: 5 },
    })
  })

  it('pinned top and left, grid column end set to auto and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and left, grid column end set to auto and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 688.5, y: 315.5, width: 12, height: 1 },
      top: { x: 725.5, y: 285.5, width: 1, height: 5 },
    })
  })
  it('pinned top and right, grid column end set to auto and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and right, grid column end set to auto and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 807.5, y: 315.5, width: 12, height: 1 },
      top: { x: 781.5, y: 285.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and left, grid column end set to auto and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and left, grid column end set to auto and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 688.5, y: 355.5, width: 12, height: 1 },
      bottom: { x: 725.5, y: 381.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and right, grid column end set to auto and not absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and right, grid column end set to auto and absolutely positioned', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 807.5, y: 355.5, width: 12, height: 1 },
      bottom: { x: 781.5, y: 381.5, width: 1, height: 5 },
    })
  })

  it('pinned top and left, grid rows and columns fully specified and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and left, grid rows and columns fully specified and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 1418.5, y: 190.5, width: 12, height: 1 },
      top: { x: 1455.5, y: 160.5, width: 1, height: 5 },
    })
  })
  it('pinned top and right, grid rows and columns fully specified and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and right, grid rows and columns fully specified and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 1806.5, y: 190.5, width: 12, height: 1 },
      top: { x: 1781.5, y: 160.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and left, grid rows and columns fully specified and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and left, grid rows and columns fully specified and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 1418.5, y: 530.5, width: 12, height: 1 },
      bottom: { x: 1455.5, y: 555.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and right, grid rows and columns fully specified and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and right, grid rows and columns fully specified and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 1806.5, y: 530.5, width: 12, height: 1 },
      bottom: { x: 1781.5, y: 555.5, width: 1, height: 5 },
    })
  })
  it('pinned top and left, grid column end set to auto and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and left, grid column end set to auto and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 1418.5, y: 190.5, width: 12, height: 1 },
      top: { x: 1455.5, y: 160.5, width: 1, height: 5 },
    })
  })
  it('pinned top and right, grid column end set to auto and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and right, grid column end set to auto and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      top: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 1806.5, y: 190.5, width: 12, height: 1 },
      top: { x: 1781.5, y: 160.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and left, grid column end set to auto and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and left, grid column end set to auto and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      bottom: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 1418.5, y: 530.5, width: 12, height: 1 },
      bottom: { x: 1455.5, y: 555.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and right, grid column end set to auto and not absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and right, grid column end set to auto and absolutely positioned in non-absolute grid', async () => {
    const result = await testGridOutlines('storyboard/scene/grid-2-outer/grid-2/grid-child-2', {
      position: 'absolute',
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 'auto',
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 1806.5, y: 530.5, width: 12, height: 1 },
      bottom: { x: 1781.5, y: 555.5, width: 1, height: 5 },
    })
  })

  it('pinned top and left, grid rows and columns fully specified and not absolutely positioned, with a row start span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      top: 5,
      left: 12,
      gridRowStart: 'span 2',
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and left, grid rows and columns fully specified and absolutely positioned, with a row start span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      top: 5,
      left: 12,
      gridRowStart: 'span 2',
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 688.5, y: 190.5, width: 12, height: 1 },
      top: { x: 725.5, y: 160.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and right, grid rows and columns fully specified and not absolutely positioned, with a row start span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      bottom: 5,
      right: 12,
      gridRowStart: 'span 2',
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and right, grid rows and columns fully specified and absolutely positioned, with a row start span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      bottom: 5,
      right: 12,
      gridRowStart: 'span 2',
      gridColumnStart: 3,
      gridRowEnd: 2,
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 777.5, y: 240.5, width: 12, height: 1 },
      bottom: { x: 751.5, y: 266.5, width: 1, height: 5 },
    })
  })
  it('pinned top and left, grid rows and columns fully specified and not absolutely positioned, with a row end span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 'span 2',
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned top and left, grid rows and columns fully specified and absolutely positioned, with a row end span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      top: 5,
      left: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 'span 2',
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      left: { x: 688.5, y: 315.5, width: 12, height: 1 },
      top: { x: 725.5, y: 285.5, width: 1, height: 5 },
    })
  })
  it('pinned bottom and right, grid rows and columns fully specified and not absolutely positioned, with a row end span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 'span 2',
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({})
  })
  it('pinned bottom and right, grid rows and columns fully specified and absolutely positioned, with a row end span', async () => {
    const result = await testGridOutlines('storyboard/scene/grid/grid-child', {
      position: 'absolute',
      bottom: 5,
      right: 12,
      gridRowStart: 2,
      gridColumnStart: 3,
      gridRowEnd: 'span 2',
      gridColumnEnd: 3,
      width: 50,
      height: 50,
    })
    expect(result).toEqual({
      right: { x: 777.5, y: 470.5, width: 12, height: 1 },
      bottom: { x: 751.5, y: 496.5, width: 1, height: 5 },
    })
  })
})
