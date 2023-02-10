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
import { emptyModifiers, Modifiers, shiftModifier } from '../../../../utils/modifiers'
import { ElementPath } from '../../../../core/shared/project-file-types'
import {
  EdgePositionBottomRight,
  EdgePosition,
  EdgePositionLeft,
  EdgePositionTopLeft,
  EdgePositionRight,
  EdgePositionBottom,
  EdgePositionTop,
} from '../../canvas-types'
import { wait } from '../../../../utils/utils.test-utils'
import { ControlDelay } from '../canvas-strategy-types'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { setFeatureEnabled } from '../../../../utils/feature-switches'
import { CSSProperties } from 'react'
import { MaxContent } from '../../../inspector/inspector-common'
import { ResizePointTestId } from '../../controls/select-mode/absolute-resize-control'

async function resizeElement(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
): Promise<void> {
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

  await mouseDragFromPointWithDelta(canvasControl, startPoint, dragDelta, { modifiers: modifiers })
}

// no mouseup here! it starts the interaction and resizes with drag delta
async function startDragUsingActions(
  renderResult: EditorRenderResult,
  target: ElementPath,
  edgePosition: EdgePosition,
  dragDelta: CanvasVector,
) {
  await renderResult.dispatch([selectComponents([target], false)], true)
  const startInteractionSession = createInteractionViaMouse(
    zeroCanvasPoint,
    emptyModifiers,
    {
      type: 'RESIZE_HANDLE',
      edgePosition: edgePosition,
    },
    'zero-drag-not-permitted',
  )
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
    <div data-uid='app-root'>
      <App2
        data-uid='app2'
        style={{
          left: 20,
          top: 20,
          width: ${width},
          height: ${height},
        }}
      />
    </div>
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

async function doDblClickTest(
  editor: EditorRenderResult,
  testId: string,
  verticalOffset: number = 30,
): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  await mouseClickAtPoint(canvasControlsLayer, divCorner)

  const nineBlockControlSegment = editor.renderedDOM.getByTestId(testId)

  await mouseClickAtPoint(
    nineBlockControlSegment,
    { x: 2, y: verticalOffset },
    { eventOptions: { detail: 2 } },
  )

  return div
}

const projectForEdgeDblClick = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <div
      data-testid='mydiv'
      style={{
        backgroundColor: '#3EA881FC',
        position: 'absolute',
        left: -231,
        top: 221,
        width: 637,
        display: 'flex',
        gap: 31,
        alignItems: 'center',
        justifyContent: 'center',
        height: 445,
      }}
    >
      <div
        style={{
          backgroundColor: '#E91C1CC4',
          width: 200,
          height: 192,
          contain: 'layout',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'flex-end',
          flexDirection: 'column',
        }}
      ></div>
      <div
        style={{
          backgroundColor: '#2C49C9B3',
          width: 73,
          height: 358,
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)
`

const projectForEdgeDblClickWithText = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <div
      data-testid='mydiv'
      style={{
        backgroundColor: '#3EA881FC',
        position: 'absolute',
        left: -231,
        top: 221,
        width: 637,
        display: 'flex',
        gap: 31,
        alignItems: 'center',
        justifyContent: 'center',
        height: 445,
      }}
    >
      hello there
    </div>
  </Storyboard>
)
`

const projectForEdgeDblClickWithPosition = (
  leftPos: CSSProperties['position'],
  rightPos: CSSProperties['position'],
) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <div
      data-testid='mydiv'
      style={{
        backgroundColor: '#3EA881FC',
        position: 'absolute',
        left: -231,
        top: 221,
        width: 637,
        display: 'flex',
        gap: 31,
        alignItems: 'center',
        justifyContent: 'center',
        height: 445,
      }}
    >
      <div
        style={{
          backgroundColor: '#E91C1CC4',
          width: 200,
          height: 192,
          contain: 'layout',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'flex-end',
          flexDirection: 'column',
          position: '${leftPos}'
        }}
      ></div>
      <div
        style={{
          backgroundColor: '#2C49C9B3',
          width: 73,
          height: 358,
          contain: 'layout',
          position: '${rightPos}'
        }}
      />
    </div>
  </Storyboard>
)
`

const projectForEdgeDblClickNoChildren = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <div
      data-testid='mydiv'
      style={{
        backgroundColor: '#3EA881FC',
        position: 'absolute',
        left: -231,
        top: 221,
        width: 637,
        display: 'flex',
        gap: 31,
        alignItems: 'center',
        justifyContent: 'center',
        height: 445,
      }}
    >
    </div>
  </Storyboard>
)
`

describe('Absolute Resize Strategy', () => {
  it('resizes component instances that honour the size properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesHonourSizeProperties(300, 300),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['app-root', 'app2'])
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await renderResult.dispatch([selectComponents([target], false)], true)

    expect(renderResult.renderedDOM.getByTestId('parent-resize-label')).toBeTruthy()

    await resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)
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
    await resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)

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
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
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
    await resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 240, height: 95 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('resizes absolute positioned element with missing position values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ left: 100, top: 100, width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const dragDelta = windowPoint({ x: 40, y: 50 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ left: 100, top: 100, width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: 160, height: 70, left: 40, top: 50 }}
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
            style={{ backgroundColor: '#aaaaaa33', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
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
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 70, top: 30, width: 170, height: 140 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('resizes absolute element with aspect ratio locked, keeping aspect ratio', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      data-uid='div-aspect-ratio-locked'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['storyboard', 'div-aspect-ratio-locked']])
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -42,
        top: 207,
        width: 375,
        height: 500,
      }}
      data-uid='div-aspect-ratio-locked'
    />
  </Storyboard>
)
`)
  })
  it('resizes absolute element with aspect ratio locked, disregarding aspect ratio if shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      data-uid='div-aspect-ratio-locked'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['storyboard', 'div-aspect-ratio-locked']])
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 133,
        top: 207,
        width: 200,
        height: 500,
      }}
      data-uid='div-aspect-ratio-locked'
    />
  </Storyboard>
)
`)
  })
  it('images are automatically resized with aspect ratio locked, keeping aspect ratio', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['storyboard', 'img-element']])
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        position: 'absolute',
        left: -42,
        top: 207,
        width: 375,
        height: 500,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
  </Storyboard>
)
`)
  })
  it('images are resized without regard to aspect ratio if shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const target = EP.elementPath([['storyboard', 'img-element']])
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents([target], false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        position: 'absolute',
        left: 133,
        top: 207,
        width: 200,
        height: 500,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection with aspect ratio locked, if any elements in the selection are aspect ratio locked', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      data-uid='div-aspect-ratio-locked'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 100,
        height: 200,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const targets = [
      EP.elementPath([['storyboard', 'div-aspect-ratio-locked']]),
      EP.elementPath([['storyboard', 'div-simple']]),
    ]
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -9,
        top: 207,
        width: 342,
        height: 456,
      }}
      data-uid='div-aspect-ratio-locked'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 152,
        top: 799,
        width: 114,
        height: 228,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection without aspect ratio locked, if any elements in the selection are aspect ratio locked but shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      data-uid='div-aspect-ratio-locked'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 100,
        height: 200,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const targets = [
      EP.elementPath([['storyboard', 'div-aspect-ratio-locked']]),
      EP.elementPath([['storyboard', 'div-simple']]),
    ]
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-aspect-ratio-locked
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 133,
        top: 207,
        width: 200,
        height: 456,
      }}
      data-uid='div-aspect-ratio-locked'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 227,
        top: 799,
        width: 67,
        height: 228,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection with aspect ratio locked, if any elements in the selection are images', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 100,
        height: 200,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const targets = [
      EP.elementPath([['storyboard', 'img-element']]),
      EP.elementPath([['storyboard', 'div-simple']]),
    ]
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -9,
        top: 207,
        width: 342,
        height: 456,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 152,
        top: 799,
        width: 114,
        height: 228,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`)
  })
  it('resizes multiselection without aspect ratio locked, if any elements in the selection are images but shift is held', async () => {
    const renderResult = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 33,
        top: 307,
        width: 300,
        height: 400,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 174,
        top: 827,
        width: 100,
        height: 200,
      }}
      data-uid='div-simple'
    />
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    const targets = [
      EP.elementPath([['storyboard', 'img-element']]),
      EP.elementPath([['storyboard', 'div-simple']]),
    ]
    const dragDelta = windowPoint({ x: 100, y: -100 })

    await renderResult.dispatch([selectComponents(targets, false)], true)
    await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, shiftModifier)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <img
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 133,
        top: 207,
        width: 200,
        height: 456,
      }}
      src='/editor/icons/favicons/favicon-128.png?hash=578b112672eaa052e413e1698446e7bf2729c2ad'
      data-uid='img-element'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 227,
        top: 799,
        width: 67,
        height: 228,
      }}
      data-uid='div-simple'
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
    await resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)
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
    await resizeElement(renderResult, dragDelta, EdgePositionBottomRight, emptyModifiers)
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
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, right: 160, bottom: 230 }}
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
    await startDragUsingActions(renderResult, target, EdgePositionLeft, canvasPoint({ x: 5, y: 5 }))

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
            style={{ backgroundColor: '#aaaaaa33', width: 70, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
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

describe('Double click on resize edge', () => {
  before(() => setFeatureEnabled('Nine block control', true))
  after(() => setFeatureEnabled('Nine block control', false))

  const edgeResizeControlTestId = (position: EdgePosition) =>
    `resize-control-${position.x}-${position.y}`

  it('double click left edge', async () => {
    const editor = await renderTestEditorWithCode(projectForEdgeDblClick, 'await-first-dom-report')
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionLeft))
    expect(div.style.height).toEqual('445px')
    expect(div.style.width).toEqual(MaxContent)
  })

  it('double click right edge', async () => {
    const editor = await renderTestEditorWithCode(projectForEdgeDblClick, 'await-first-dom-report')
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionRight))
    expect(div.style.height).toEqual('445px')
    expect(div.style.width).toEqual(MaxContent)
  })

  it('double click top edge', async () => {
    const editor = await renderTestEditorWithCode(projectForEdgeDblClick, 'await-first-dom-report')
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionTop))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual(MaxContent)
  })

  it('double click bottom edge', async () => {
    const editor = await renderTestEditorWithCode(projectForEdgeDblClick, 'await-first-dom-report')
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual(MaxContent)
  })

  it("not applicable when children don't participate in the layout", async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickWithPosition('absolute', 'absolute'),
      'await-first-dom-report',
    )
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual('445px')
  })

  it('not applicable when children are positioned absolute', async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickNoChildren,
      'await-first-dom-report',
    )
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual('445px')
  })

  it('not applicable when children are positioned sticky', async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickWithPosition('sticky', 'sticky'),
      'await-first-dom-report',
    )
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual('445px')
  })

  it('not applicable when children are positioned fixed', async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickWithPosition('fixed', 'fixed'),
      'await-first-dom-report',
    )
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual('445px')
  })

  it('`max-content` is applied to `height` when element only has text children', async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickWithText,
      'await-first-dom-report',
    )
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
    expect(div.style.width).toEqual('637px')
    expect(div.style.height).toEqual(MaxContent)
  })

  it('`max-content` is applied to `width` when element only has text children', async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickWithText,
      'await-first-dom-report',
    )
    const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionRight))
    expect(div.style.width).toEqual(MaxContent)
    expect(div.style.height).toEqual('445px')
  })
})

describe('double click on resize corner', () => {
  before(() => setFeatureEnabled('Nine block control', true))
  after(() => setFeatureEnabled('Nine block control', false))

  it('resizes to fit when resize corner is double clicked', async () => {
    const editor = await renderTestEditorWithCode(
      projectForEdgeDblClickWithText,
      'await-first-dom-report',
    )

    const view = await doDblClickTest(editor, ResizePointTestId(EdgePositionTopLeft), 1)

    expect(view.style.width).toEqual(MaxContent)
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual(MaxContent)
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
    expect(view.style.flex).toEqual('')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexGrow).toEqual('')
    expect(view.style.flexBasis).toEqual('')
  })
})
