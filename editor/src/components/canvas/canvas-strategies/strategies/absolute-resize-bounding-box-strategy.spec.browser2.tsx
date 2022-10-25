import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { selectComponents } from '../../../editor/actions/action-creators'
import CanvasActions from '../../canvas-actions'
import { createInteractionViaMouse, updateInteractionViaMouse } from '../interaction-state'
import {
  canvasPoint,
  CanvasVector,
  windowPoint,
  WindowPoint,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers, Modifiers, shiftModifier } from '../../../../utils/modifiers'
import { ElementPath } from '../../../../core/shared/project-file-types'
import {
  EdgePositionBottomRight,
  EdgePosition,
  EdgePositionLeft,
  EdgePositionTopLeft,
} from '../../canvas-types'
import { wait } from '../../../../utils/utils.test-utils'
import { ControlDelay } from '../canvas-strategy-types'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import { mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'

function resizeElement(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
) {
  const canvasControl = renderResult.renderedDOM.queryByTestId(
    `resize-control-${edgePosition.x}-${edgePosition.y}`,
  )
  if (canvasControl == null) {
    return
  }

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  mouseDragFromPointWithDelta(canvasControl, startPoint, dragDelta, { modifiers: modifiers })
}

// no mouseup here! it starts the interaction and resizes with drag delta
async function startDragUsingActions(
  renderResult: EditorRenderResult,
  target: ElementPath,
  edgePosition: EdgePosition,
  dragDelta: CanvasVector,
) {
  await renderResult.dispatch([selectComponents([target], false)], true)
  const startInteractionSession = createInteractionViaMouse(zeroCanvasPoint, emptyModifiers, {
    type: 'RESIZE_HANDLE',
    edgePosition: edgePosition,
  })
  await renderResult.dispatch(
    [CanvasActions.createInteractionSession(startInteractionSession)],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
  await renderResult.dispatch(
    [
      CanvasActions.updateInteractionSession(
        updateInteractionViaMouse(startInteractionSession, 'DRAG', dragDelta, emptyModifiers, {
          type: 'RESIZE_HANDLE',
          edgePosition: edgePosition,
        }),
      ),
    ],
    false,
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

const doesOrNotSupportStyleCode = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const SupportsStyle = (props) => {
  return (
    <div
      style={{
        backgroundColor: 'teal',
        position: 'absolute',
        left: 0,
        top: 200,
        width: 100,
        height: 100,
        ...props.style,
      }}
      data-uid='supports-style-component'
      data-testid='supports-style-component'
    >
      DOES
    </div>
  )
}

export const DoesNotSupportStyle = (props) => {
  return (
    <div
      style={{
        backgroundColor: 'lightgreen',
        position: 'absolute',
        left: 100,
        top: 200,
        width: 100,
        height: 100,
      }}
      data-uid='does-not-support-style-component'
      data-testid='does-not-support-style-component'
    >
      DOES NOT
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 700,
        }}
        data-uid='containing-div'
      >
        <SupportsStyle
          data-uid='supports-style'
          data-testid='supports-style'
        />
        <DoesNotSupportStyle
          data-uid='does-not-support-style'
          data-testid='does-not-support-style'
        />
      </div>
    </Scene>
  </Storyboard>
)`

const projectDoesNotHonourSizeProperties = `
import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export const App2 = (props) => {
  return (
    <div
      style={{
        position: 'absolute',
        top: 5,
        left: 5,
        width: 350,
        height: 400,
      }}
      data-uid='aaa'
      data-testid='aaa'
    />
  )
}

export var App = (props) => {
  return (
    <App2
      data-uid='app2'
      style={{ left: 20, top: 20, width: 300, height: 400 }}
    />
  )
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`

const projectWithAspectRatioLockedElement = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 170,
        height: 430,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 215,
        height: 215,
        position: 'absolute',
        left: -69,
        top: 863,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`

function projectDoesHonourSizeProperties(width: number, height: number): string {
  return `import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'

export const App2 = (props) => {
  return (
    <div
      style={{
        position: 'absolute',
        top: props.style.top,
        left: props.style.left,
        width: props.style.width,
        height: props.style.height,
      }}
      data-uid='aaa'
      data-testid='aaa'
    />
  )
}

export var App = (props) => {
  return (
    <App2
      data-uid='app2'
      style={{ left: 20, top: 20, width: ${width}, height: ${height} }}
    />
  )
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App data-uid='${TestAppUID}' />
      </Scene>
    </Storyboard>
  )
}
`
}

describe('Absolute Resize Strategy', () => {
  it('resizes component instances that honour the size properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesHonourSizeProperties(300, 300),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['app2'])
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      projectDoesHonourSizeProperties(340, 275),
    )
  })
  it('does not resize a component instance that does not honour the size properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesNotHonourSizeProperties,
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['app2'])
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      projectDoesNotHonourSizeProperties,
    )
  })
  it('resizes absolute positioned element from bottom right edge', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 240, height: 95 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('resizes absolute element with snapping, `bbb` should snap to `ccc`', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 70, top: 30, width: 170, height: 140 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('resizes absolute element with aspect ratio locked, keeping aspect ratio', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['0cd', '7a0']])
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 13,
        top: 284,
        width: 383,
        height: 449,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 170,
        height: 430,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 215,
        height: 215,
        position: 'absolute',
        left: -69,
        top: 863,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('resizes absolute element with aspect ratio locked, disregarding aspect ratio if shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['0cd', '7a0']])
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 62,
        top: 284,
        width: 334,
        height: 449,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 170,
        height: 430,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 215,
        height: 215,
        position: 'absolute',
        left: -69,
        top: 863,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('images are automatically resized with aspect ratio locked, keeping aspect ratio', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['0cd', '62f']])
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 170,
        height: 430,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 238,
        height: 238,
        position: 'absolute',
        left: -92,
        top: 840,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('images are resized without regard to aspect ratio if shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['0cd', '62f']])
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 170,
        height: 430,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 186,
        height: 238,
        position: 'absolute',
        left: -40,
        top: 840,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection with aspect ratio locked, if any elements in the selection are aspect ratio locked', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const targets = [EP.elementPath([['0cd', '7a0']]), EP.elementPath([['0cd', '452']])]
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 24,
        top: 284,
        width: 372,
        height: 436,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 169,
        top: 817,
        width: 174,
        height: 440,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 215,
        height: 215,
        position: 'absolute',
        left: -69,
        top: 863,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection without aspect ratio locked, if any elements in the selection are aspect ratio locked but shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const targets = [EP.elementPath([['0cd', '7a0']]), EP.elementPath([['0cd', '452']])]
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 62,
        top: 284,
        width: 334,
        height: 436,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 192,
        top: 817,
        width: 156,
        height: 440,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 215,
        height: 215,
        position: 'absolute',
        left: -69,
        top: 863,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection with aspect ratio locked, if any elements in the selection are images', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const targets = [EP.elementPath([['0cd', '62f']]), EP.elementPath([['0cd', '452']])]
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 165,
        top: 804,
        width: 179,
        height: 453,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 227,
        height: 227,
        position: 'absolute',
        left: -91,
        top: 842,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection without aspect ratio locked, if any elements in the selection are images but shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithAspectRatioLockedElement,
      'await-first-dom-report',
    )

    const targets = [EP.elementPath([['0cd', '62f']]), EP.elementPath([['0cd', '452']])]
    const dragDelta = windowPoint({ x: 29, y: -23 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 363,
        height: 426,
      }}
      data-uid='7a0'
    />
    <div
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 186,
        top: 804,
        width: 158,
        height: 453,
      }}
      data-uid='452'
    />
    <img
      style={{
        width: 200,
        height: 227,
        position: 'absolute',
        left: -40,
        top: 842,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='62f'
    />
  </Storyboard>
)
`)
  })
  it('handles checking if a component supports the style property (for those that do)', async () => {
    const renderResult = await renderTestEditorWithCode(
      doesOrNotSupportStyleCode,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/containing-div/supports-style')
    const dragDelta = windowPoint({ x: 25, y: 25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    const supportsStyleDiv = renderResult.renderedDOM.getByTestId('supports-style-component')
    const supportsStyleRect = supportsStyleDiv.getBoundingClientRect()
    expect(supportsStyleRect.width).toEqual(125)
    expect(supportsStyleRect.height).toEqual(125)
  })
  it('handles checking if a component supports the style property (for those that do not)', async () => {
    const renderResult = await renderTestEditorWithCode(
      doesOrNotSupportStyleCode,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/containing-div/does-not-support-style')
    const dragDelta = windowPoint({ x: 25, y: 25 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    const supportsStyleDiv = renderResult.renderedDOM.getByTestId(
      'does-not-support-style-component',
    )
    const supportsStyleRect = supportsStyleDiv.getBoundingClientRect()
    expect(supportsStyleRect.width).toEqual(100)
    expect(supportsStyleRect.height).toEqual(100)
  })
})

describe('Absolute Resize Strategy Canvas Controls', () => {
  it('when an absolute positioned element is resized the parent outlines become visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, right: 160, bottom: 230 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const parentOutlineControlBeforeDrag =
      renderResult.renderedDOM.queryByTestId('parent-outlines-control')
    expect(parentOutlineControlBeforeDrag).toBeNull()
    const parentBoundsControlBeforeDrag =
      renderResult.renderedDOM.queryByTestId('parent-bounds-control')
    expect(parentBoundsControlBeforeDrag).toBeNull()

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await startDragUsingActions(renderResult, target, EdgePositionLeft, zeroCanvasPoint)

    await wait(ControlDelay + 10)
    const parentOutlineControl = renderResult.renderedDOM.getByTestId('parent-outlines-control')
    expect(parentOutlineControl).toBeDefined()
    const parentBoundsControl = renderResult.renderedDOM.getByTestId('parent-bounds-control')
    expect(parentBoundsControl).toBeDefined()
  })
  it('snap guidelines are visible when an absolute positioned element(bbb) is resized and snaps to its sibling (ccc)', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = canvasPoint({ x: 29, y: -23 }) // 'bbb' will snap to bottom right corner of 'ccc'

    await startDragUsingActions(renderResult, target, EdgePositionTopLeft, dragDelta)

    expect(renderResult.renderedDOM.getByTestId('guideline-0').style.display).toEqual('block')
    expect(renderResult.renderedDOM.getByTestId('guideline-1').style.display).toEqual('block')
  })
})
