import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  selectComponents,
  setConditionalOverriddenCondition,
} from '../../../editor/actions/action-creators'
import CanvasActions from '../../canvas-actions'
import { createInteractionViaMouse, updateInteractionViaMouse } from '../interaction-state'
import type { CanvasVector, Delta } from '../../../../core/shared/math-utils'
import { canvasPoint, windowPoint, zeroCanvasPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { altModifier, emptyModifiers, shiftModifier } from '../../../../utils/modifiers'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { EdgePosition } from '../../canvas-types'
import {
  EdgePositionBottomRight,
  EdgePositionLeft,
  EdgePositionTopLeft,
  EdgePositionRight,
  EdgePositionBottom,
  EdgePositionTop,
  EdgePositionBottomLeft,
} from '../../canvas-types'
import {
  expectElementWithTestIdNotToBeRendered,
  expectElementWithTestIdToBeRendered,
  expectElementWithTestIdToBeRenderedWithDisplayNone,
  selectComponentsForTest,
  wait,
} from '../../../../utils/utils.test-utils'
import { ControlDelay } from '../canvas-strategy-types'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../../core/model/scene-utils'
import {
  dispatchMouseEnterEventAtPoint,
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { CSSProperties } from 'react'
import { MaxContent } from '../../../inspector/inspector-common'
import {
  SizeLabelTestId,
  ResizePointTestId,
  AbsoluteResizeControlTestId,
} from '../../controls/select-mode/absolute-resize-control'
import type { FragmentLikeType } from './fragment-like-helpers'
import { AllFragmentLikeTypes } from './fragment-like-helpers'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  FragmentLikeElementUid,
} from './fragment-like-helpers.test-utils'
import { FOR_TESTS_setNextGeneratedUids } from '../../../../core/model/element-template-utils.test-utils'
import { isRight } from '../../../../core/shared/either'
import { ImmediateParentOutlinesTestId } from '../../controls/parent-outlines'
import { ImmediateParentBoundsTestId } from '../../controls/parent-bounds'
import { getResizeControl, resizeElement } from './absolute-resize.test-utils'
import {
  RESIZE_CONTROL_SAFE_GAP,
  SafeGapSmallElementSize,
  SmallElementSize,
} from '../../controls/bounding-box-hooks'
import { act } from 'react-dom/test-utils'
import { ComponentsHonouringPropsStylesProject } from './common-projects.test-utils'

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

const projectWithFragment = `import * as React from 'react'

const foo = true

export var storyboard = (
  <div data-uid='root'>
    <React.Fragment data-uid='fragment'>

    </React.Fragment>
  </div>
)
`

const projectDoesNotHonourSizeProperties = `
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
import { Scene, Storyboard, View, Group } from 'utopia-api'

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
      data-uid='div'
      data-testid='div'
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

  await mouseDoubleClickAtPoint(nineBlockControlSegment, { x: 2, y: verticalOffset })

  return div
}

async function doSnapDrag(
  editor: EditorRenderResult,
  delta: { x: number; y: number },
  edgePosition: EdgePosition,
  callback: () => Promise<void>,
  modifiers: Modifiers = emptyModifiers,
) {
  const canvasControl = editor.renderedDOM.getByTestId(
    `resize-control-${edgePosition.x}-${edgePosition.y}`,
  )

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  await mouseDragFromPointWithDelta(canvasControl, startPoint, delta, {
    modifiers: modifiers,
    midDragCallback: callback,
  })
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

const projectForMultiSelectResize = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 146,
        top: 118,
        width: 305,
        height: 233,
      }}
      data-uid='one'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 213,
        top: 377,
        width: 178,
        height: 179,
      }}
      data-uid='two'
    />
    <div
      style={{
        backgroundColor: '#00acff',
        position: 'absolute',
        left: 759,
        top: 155,
        width: 228,
        height: 254,
      }}
      data-uid='horizontal'
      data-testid='horizontal'
    />
    <div
      style={{
        backgroundColor: '#2b8f65',
        position: 'absolute',
        left: 70,
        top: 779,
        width: 267,
        height: 275,
      }}
      data-uid='vertical'
      data-testid='vertical'
    />
  </Storyboard>
)
`

const projectForCenterBasedResize = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 462,
        top: 122,
        width: 300,
        height: 177,
      }}
      data-uid='foo'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 91,
          top: 75,
          width: 118,
          height: 31,
        }}
        data-uid='bar'
      />
    </div>
  </Storyboard>
)
`

const projectWithGroupsForResize = (type: FragmentLikeType) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    ${getOpeningFragmentLikeTag(type)}
      <div
        style={{
          backgroundColor: '#00acff',
          position: 'absolute',
          left: 379.5,
          top: 94.5,
          width: 163,
          height: 184,
        }}
        data-uid='aac'
        data-label='eee'
      />
      <div
        style={{
          backgroundColor: '#ff0001',
          position: 'absolute',
          left: 980.5,
          top: 62.5,
          width: 306,
          height: 239,
        }}
        data-uid='aad'
        data-label='eee'
      />
      ${getClosingFragmentLikeTag(type)}
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 258.5,
        top: -193,
        width: 243,
        height: 195,
      }}
      data-uid='98d'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -431,
        top: 754.5,
        width: 447,
        height: 266,
      }}
      data-uid='5ce'
    />
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

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectElementWithTestIdNotToBeRendered", "expectElementWithTestIdToBeRendered"] }] */

describe('Absolute Resize Strategy', () => {
  it('the size label is not shown when an empty fragment is selected', async () => {
    const editor = await renderTestEditorWithCode(projectWithFragment, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('root/fragment')])
    const absoluteResizeControl = editor.renderedDOM.queryAllByTestId(SizeLabelTestId)
    expect(absoluteResizeControl).toEqual([])
  })
  it('sizeless element shows (Children) in size label', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='root'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
            }}
            data-uid='sizeless'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 179,
                top: 114,
                width: 188,
                height: 161,
              }}
              data-uid='child'
            />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [
      EP.appendNewElementPath(TestScenePath, ['root', 'sizeless']),
    ])
    const sizeLabel = await editor.renderedDOM.findByTestId(SizeLabelTestId)
    expect(sizeLabel.textContent).toEqual('(Children) 188 x 161')
  })
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
  it('resizes a component instance that honours the size properties by updating the instance', async () => {
    const renderResult = await renderTestEditorWithCode(
      ComponentsHonouringPropsStylesProject,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/mycompdiv')

    await renderResult.dispatch([selectComponents([target], false)], true)

    expect(renderResult.renderedDOM.getByTestId('parent-resize-label')).toBeTruthy()

    const dragDelta = windowPoint({ x: 0, y: 30 })
    await resizeElement(renderResult, dragDelta, EdgePositionBottom, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const MyCompDiv = (props) => {
  return (
    <div
      data-uid='mycompdivinnerdiv'
      data-testid='mycompdivinnerdiv'
      style={props.style}
    >
      MyCompDiv
    </div>
  )
}

export const MyCompFrag = (props) => {
  return (
    <>
      <div
        data-uid='mycompfraginnerdiv'
        data-testid='mycompfraginnerdiv'
        style={props.style}
      >
        MyCompFrag
      </div>
    </>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='scene'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 118,
        top: 172,
        width: 300,
        height: 300,
      }}
    >
      <MyCompDiv
        data-uid='mycompdiv'
        style={{
          position: 'absolute',
          width: 137,
          height: 121,
          left: 137,
          top: 36,
        }}
      />
      <MyCompFrag
        data-uid='mycompfrag'
        style={{
          position: 'absolute',
          width: 150,
          height: 60.5,
          left: 21,
          top: 36,
        }}
      />
      <div
        data-uid='regulardiv'
        data-testid='regulardiv'
        style={{
          position: 'absolute',
          backgroundColor: 'lightblue',
          left: 171,
          top: 109,
          height: 93,
          width: 108,
        }}
      >
        Regular Div
      </div>
    </div>
  </Storyboard>
)
`)
  })
  it('resizes a component instance that honours the size properties (inside a fragment) by updating the instance', async () => {
    const renderResult = await renderTestEditorWithCode(
      ComponentsHonouringPropsStylesProject,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/mycompfrag')

    await renderResult.dispatch([selectComponents([target], false)], true)

    expect(renderResult.renderedDOM.getByTestId('parent-resize-label')).toBeTruthy()

    const dragDelta = windowPoint({ x: 0, y: 30 })
    await resizeElement(renderResult, dragDelta, EdgePositionBottom, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const MyCompDiv = (props) => {
  return (
    <div
      data-uid='mycompdivinnerdiv'
      data-testid='mycompdivinnerdiv'
      style={props.style}
    >
      MyCompDiv
    </div>
  )
}

export const MyCompFrag = (props) => {
  return (
    <>
      <div
        data-uid='mycompfraginnerdiv'
        data-testid='mycompfraginnerdiv'
        style={props.style}
      >
        MyCompFrag
      </div>
    </>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='scene'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 118,
        top: 172,
        width: 300,
        height: 300,
      }}
    >
      <MyCompDiv
        data-uid='mycompdiv'
        style={{
          position: 'absolute',
          width: 137,
          height: 91.5,
          left: 137,
          top: 36,
        }}
      />
      <MyCompFrag
        data-uid='mycompfrag'
        style={{
          position: 'absolute',
          width: 150,
          height: 92,
          left: 21,
          top: 36,
        }}
      />
      <div
        data-uid='regulardiv'
        data-testid='regulardiv'
        style={{
          position: 'absolute',
          backgroundColor: 'lightblue',
          left: 171,
          top: 109,
          height: 93,
          width: 108,
        }}
      >
        Regular Div
      </div>
    </div>
  </Storyboard>
)
`)
  })
  it('resizes a regular div by updating it', async () => {
    const renderResult = await renderTestEditorWithCode(
      ComponentsHonouringPropsStylesProject,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/regulardiv')

    await renderResult.dispatch([selectComponents([target], false)], true)

    expect(renderResult.renderedDOM.getByTestId('parent-resize-label')).toBeTruthy()

    const dragDelta = windowPoint({ x: 0, y: 30 })
    await resizeElement(renderResult, dragDelta, EdgePositionBottom, emptyModifiers)
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const MyCompDiv = (props) => {
  return (
    <div
      data-uid='mycompdivinnerdiv'
      data-testid='mycompdivinnerdiv'
      style={props.style}
    >
      MyCompDiv
    </div>
  )
}

export const MyCompFrag = (props) => {
  return (
    <>
      <div
        data-uid='mycompfraginnerdiv'
        data-testid='mycompfraginnerdiv'
        style={props.style}
      >
        MyCompFrag
      </div>
    </>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='scene'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 118,
        top: 172,
        width: 300,
        height: 300,
      }}
    >
      <MyCompDiv
        data-uid='mycompdiv'
        style={{
          position: 'absolute',
          width: 137,
          height: 91.5,
          left: 137,
          top: 36,
        }}
      />
      <MyCompFrag
        data-uid='mycompfrag'
        style={{
          position: 'absolute',
          width: 150,
          height: 60.5,
          left: 21,
          top: 36,
        }}
      />
      <div
        data-uid='regulardiv'
        data-testid='regulardiv'
        style={{
          position: 'absolute',
          backgroundColor: 'lightblue',
          left: 171,
          top: 109,
          height: 123,
          width: 108,
        }}
      >
        Regular Div
      </div>
    </div>
  </Storyboard>
)
`)
  })
  it('does not resize a component instance that does not honour the size properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesNotHonourSizeProperties,
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['app2'])

    await renderResult.dispatch([selectComponents([target], false)], true)
    expect(getResizeControl(renderResult, EdgePositionBottomRight)).toBeNull()
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

    await renderResult.dispatch([selectComponents([target], false)], true)
    const resizeControl = getResizeControl(renderResult, EdgePositionBottomRight)
    expect(resizeControl).toBeNull()
  })
  it('percent values are rounded to 2 decimal places after resizing', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 92,
          top: 162,
          width: 430,
          height: 378,
        }}
        data-uid='aaa'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: '100%',
            height: '100%',
            contain: 'layout',
          }}
          data-uid='bbb'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)

    await renderResult.dispatch([selectComponents([target], false)], true)
    await resizeElement(renderResult, { x: -10, y: -10 }, EdgePositionBottomRight, emptyModifiers)
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 92,
          top: 162,
          width: 430,
          height: 378,
        }}
        data-uid='aaa'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: '97.67%',
            height: '97.35%',
            contain: 'layout',
          }}
          data-uid='bbb'
        />
      </div>
      `),
    )
  })
  describe('snap lines', () => {
    it('horizontal snap lines are shown when resizing a multiselection', async () => {
      const editor = await renderTestEditorWithCode(
        projectForMultiSelectResize,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/one'), EP.fromString('sb/two')])

      await doSnapDrag(editor, { x: 0, y: -147 }, EdgePositionBottomRight, async () => {
        expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(1)
        expect(
          editor.getEditorState().editor.canvas.controls.snappingGuidelines[0].guideline.type,
        ).toEqual('YAxisGuideline')
      })
    })
    it('vertical snap lines are shown when resizing a multiselection', async () => {
      const editor = await renderTestEditorWithCode(
        projectForMultiSelectResize,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/one'), EP.fromString('sb/two')])
      await doSnapDrag(editor, { x: -114, y: 0 }, EdgePositionBottomRight, async () => {
        expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(1)
        expect(
          editor.getEditorState().editor.canvas.controls.snappingGuidelines[0].guideline.type,
        ).toEqual('XAxisGuideline')
      })
    })
    it('both vertical and horizontal snap lines are shown when resizing a multiselection', async () => {
      const editor = await renderTestEditorWithCode(
        projectForMultiSelectResize,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/one'), EP.fromString('sb/two')])
      await doSnapDrag(editor, { x: -114, y: -147 }, EdgePositionBottomRight, async () => {
        expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(2)
        expect(
          editor.getEditorState().editor.canvas.controls.snappingGuidelines[0].guideline.type,
        ).toEqual('XAxisGuideline')
        expect(
          editor.getEditorState().editor.canvas.controls.snappingGuidelines[1].guideline.type,
        ).toEqual('YAxisGuideline')
      })
    })
    it('snap lines are not shown when the bounding box size is below the snapping threshold', async () => {
      const editor = await renderTestEditorWithCode(
        projectForMultiSelectResize,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/one'), EP.fromString('sb/two')])
      await doSnapDrag(editor, { x: -305, y: -147 }, EdgePositionBottomRight, async () => {
        // the snap drag along the x axis makes the contents smaller than the snap threshold, so no guidelines are shown
        expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(0)
      })
    })
    it('snap lines are not shown when the bounding box size is below the snapping threshold (center-based)', async () => {
      const editor = await renderTestEditorWithCode(
        projectForCenterBasedResize,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/foo/bar')])
      await doSnapDrag(
        editor,
        { x: 0, y: -14 },
        EdgePositionBottom,
        async () => {
          // the snap drag along the y axis makes the contents smaller than the snap threshold, so no guidelines are shown
          expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(
            0,
          )
        },
        altModifier,
      )
    })
    describe('snapping to pinned children', () => {
      it('container does not snap to flow child', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
            <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
              <div
                style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
                data-uid='bbb'
                data-testid='bbb'
              >
                <div style={{ width: 20, height: 20, backgroundColor: '#d0e5fc' }} data-uid='42c' />
              </div>
            </div>
          `),
          'await-first-dom-report',
        )

        const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

        await renderResult.dispatch([selectComponents([target], false)], true)
        await doSnapDrag(renderResult, { x: 0, y: 20 }, EdgePositionTop, async () => {
          // no guidelines are shown
          expect(
            renderResult.getEditorState().editor.canvas.controls.snappingGuidelines.length,
          ).toEqual(0)
        })

        await renderResult.getDispatchFollowUpActionsFinished()
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
            <div
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 70, width: 200, height: 100 }}
              data-uid='bbb'
              data-testid='bbb'
            >
              <div style={{ width: 20, height: 20, backgroundColor: '#d0e5fc' }} data-uid='42c' />
            </div>
          </div>
          `),
        )
      })
      it('container does not snap to flex child', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
            <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
              <div
                style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120, display: 'flex' }}
                data-uid='bbb'
                data-testid='bbb'
              >
                <div style={{ width: 20, height: 20, backgroundColor: '#d0e5fc' }} data-uid='42c' />
              </div>
            </div>
          `),
          'await-first-dom-report',
        )

        const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

        await renderResult.dispatch([selectComponents([target], false)], true)
        await doSnapDrag(renderResult, { x: 0, y: 20 }, EdgePositionTop, async () => {
          // no guidelines are shown
          expect(
            renderResult.getEditorState().editor.canvas.controls.snappingGuidelines.length,
          ).toEqual(0)
        })

        await renderResult.getDispatchFollowUpActionsFinished()
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
            <div
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 70, width: 200, height: 100, display: 'flex' }}
              data-uid='bbb'
              data-testid='bbb'
            >
              <div style={{ width: 20, height: 20, backgroundColor: '#d0e5fc' }} data-uid='42c' />
            </div>
          </div>
          `),
        )
      })
      ;(
        [
          [windowPoint({ y: 0, x: 10 }), EdgePositionLeft, 'left', 1],
          [windowPoint({ y: 10, x: 0 }), EdgePositionTop, 'top', 1],
          [windowPoint({ y: 10, x: 10 }), EdgePositionTopLeft, 'top left', 2],
          [windowPoint({ y: 0, x: -20 }), EdgePositionRight, 'right', 0],
          [windowPoint({ y: -20, x: 0 }), EdgePositionBottom, 'bottom', 0],
          [windowPoint({ y: -20, x: -20 }), EdgePositionBottomRight, 'bottom right', 0],
        ] as const
      ).forEach(([delta, edge, label, numberOfGuidelines]) => {
        it(`${numberOfGuidelines} snap lines shown when resizing container with absolte child pinned to bottom and right - dragging from ${label}`, async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(`
              <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
                <div
                  style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 50, height: 50 }}
                  data-uid='bbb'
                  data-testid='bbb'
                >
                  <div style={{ width: 20, height: 20, backgroundColor: '#d0e5fc', position: 'absolute', right: 20, bottom: 20, }} data-uid='42c' />
                </div>
              </div>
            `),
            'await-first-dom-report',
          )

          const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

          await renderResult.dispatch([selectComponents([target], false)], true)
          await doSnapDrag(renderResult, delta, edge, async () => {
            // guidelines are shown
            expect(
              renderResult.getEditorState().editor.canvas.controls.snappingGuidelines.length,
            ).toEqual(numberOfGuidelines)
          })
        })
      })
      ;(
        [
          [windowPoint({ y: 0, x: 20 }), EdgePositionLeft, 'left', 0],
          [windowPoint({ y: 20, x: 0 }), EdgePositionTop, 'top', 0],
          [windowPoint({ y: 20, x: 10 }), EdgePositionTopLeft, 'top left', 0],
          [windowPoint({ y: 0, x: -10 }), EdgePositionRight, 'right', 1],
          [windowPoint({ y: -10, x: 0 }), EdgePositionBottom, 'bottom', 1],
          [windowPoint({ y: -10, x: -10 }), EdgePositionBottomRight, 'bottom right', 2],
        ] as const
      ).forEach(([delta, edge, label, numberOfGuidelines]) => {
        it(`${numberOfGuidelines} snap lines shown when resizing container with absolte child pinned to top and left - dragging from ${label}`, async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(`
              <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
                <div
                  style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 50, height: 50 }}
                  data-uid='bbb'
                  data-testid='bbb'
                >
                  <div style={{ width: 20, height: 20, backgroundColor: '#d0e5fc', position: 'absolute', top: 20, left: 20, }} data-uid='42c' />
                </div>
              </div>
            `),
            'await-first-dom-report',
          )

          const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

          await renderResult.dispatch([selectComponents([target], false)], true)
          await doSnapDrag(renderResult, delta, edge, async () => {
            // guidelines are shown
            expect(
              renderResult.getEditorState().editor.canvas.controls.snappingGuidelines.length,
            ).toEqual(numberOfGuidelines)
          })
        })
      })
    })

    describe('groups', () => {
      AllFragmentLikeTypes.forEach((type) => {
        describe(`– ${type} parents`, () => {
          it('vertical snap lines are shown', async () => {
            const editor = await renderTestEditorWithCode(
              projectWithGroupsForResize(type),
              'await-first-dom-report',
            )

            if (type === 'conditional') {
              const path = Object.values(editor.getEditorState().editor.jsxMetadata).find((value) =>
                isRight(value.element) && value.element.value.type === 'JSX_CONDITIONAL_EXPRESSION'
                  ? value
                  : null,
              )!.elementPath
              await selectComponentsForTest(editor, [path])
            } else {
              await selectComponentsForTest(editor, [EP.fromString(`sb/fragment-like`)])
            }

            await doSnapDrag(editor, { x: -121, y: 0 }, EdgePositionBottomLeft, async () => {
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines.length,
              ).toEqual(1)
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines[0].guideline.type,
              ).toEqual('XAxisGuideline')
            })
          })
          it('horizontal snap lines are shown', async () => {
            const editor = await renderTestEditorWithCode(
              projectWithGroupsForResize(type),
              'await-first-dom-report',
            )

            if (type === 'conditional') {
              const path = Object.values(editor.getEditorState().editor.jsxMetadata).find((value) =>
                isRight(value.element) && value.element.value.type === 'JSX_CONDITIONAL_EXPRESSION'
                  ? value
                  : null,
              )!.elementPath
              await selectComponentsForTest(editor, [path])
            } else {
              await selectComponentsForTest(editor, [EP.fromString(`sb/fragment-like`)])
            }

            await doSnapDrag(editor, { x: -10, y: 453 }, EdgePositionBottomLeft, async () => {
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines.length,
              ).toEqual(1)
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines[0].guideline.type,
              ).toEqual('YAxisGuideline')
            })
          })
          it('both snap lines are shown', async () => {
            const editor = await renderTestEditorWithCode(
              projectWithGroupsForResize(type),
              'await-first-dom-report',
            )

            if (type === 'conditional') {
              const path = Object.values(editor.getEditorState().editor.jsxMetadata).find((value) =>
                isRight(value.element) && value.element.value.type === 'JSX_CONDITIONAL_EXPRESSION'
                  ? value
                  : null,
              )!.elementPath
              await selectComponentsForTest(editor, [path])
            } else {
              await selectComponentsForTest(editor, [EP.fromString(`sb/fragment-like`)])
            }

            await doSnapDrag(editor, { x: -121, y: 453 }, EdgePositionBottomLeft, async () => {
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines.length,
              ).toEqual(2)
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines[0].guideline.type,
              ).toEqual('XAxisGuideline')
              expect(
                editor.getEditorState().editor.canvas.controls.snappingGuidelines[1].guideline.type,
              ).toEqual('YAxisGuideline')
            })
          })
        })
      })
    })
  })
  describe('groups', () => {
    it('has a "Group" size label', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [selectComponents([EP.fromString('storyboard/group')], false)],
        true,
      )

      const label = await renderResult.renderedDOM.findByTestId(SizeLabelTestId)
      expect(label.textContent).toEqual('Group')
    })
    it('resizes groups correctly when dragging from top/left without existing left/top props (left)', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `),
        'await-first-dom-report',
      )

      const target = EP.fromString('storyboard/group')

      await renderResult.dispatch([selectComponents([target], false)], true)

      await resizeElement(renderResult, { x: -200, y: 0 }, EdgePositionLeft, emptyModifiers)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 350, height: 150, background: 'black', left: -200 }}>
                <div data-uid='foo' style={{ position:'absolute', width: 117, height: 50, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 117, height: 50, background: 'blue', top: 100, left: 233 }} />
              </Group>
            </Storyboard>
          )
        `),
      )
    })
    it('resizes groups correctly when dragging from top/left without existing left/top props (top)', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `),
        'await-first-dom-report',
      )

      const target = EP.fromString('storyboard/group')

      await renderResult.dispatch([selectComponents([target], false)], true)

      await resizeElement(renderResult, { x: 0, y: -200 }, EdgePositionTop, emptyModifiers)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 350, background: 'black', top: -200 }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 117, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 117, background: 'blue', top: 233, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `),
      )
    })
    it('resizes groups correctly when dragging from top/left without existing left/top props (top-left)', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `),
        'await-first-dom-report',
      )

      const target = EP.fromString('storyboard/group')

      await renderResult.dispatch([selectComponents([target], false)], true)

      await resizeElement(renderResult, { x: -100, y: -200 }, EdgePositionTopLeft, emptyModifiers)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 250, height: 350, background: 'black', left: -100, top: -200 }}>
                <div data-uid='foo' style={{ position:'absolute', width: 83, height: 117, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 83, height: 117, background: 'blue', top: 233, left: 167 }} />
              </Group>
            </Storyboard>
          )
        `),
      )
    })
    describe('parent resize', () => {
      /**
       * In the following tests, the input groups are deliberately _not_ trued up, to make sure the resize actually
       * does true them up even when coming from a misconfigured scenario.
       */
      it('when the group is not pinned and positioned absolutely', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: 100, top: 100, width: 200, height: 150 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 50, top: 33, height: 89, width: 100, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 30, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 30, background: 'blue', top: 33, left: 60 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/div')

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(renderResult, { x: -80, y: 0 }, EdgePositionBottomRight, emptyModifiers)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: 100, top: 100, width: 120, height: 150 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 50, top: 33, height: 89, width: 100, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 45, height: 42, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 45, height: 42, background: 'blue', top: 47, left: 55 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
        )
      })
      it('when the group is pinned horizontally', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: 100, top: 100, width: 200, height: 150 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 50, top: 33, height: 89, right: 60, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 56, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 56, background: 'blue', top: 33, left: 100 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/div')

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(renderResult, { x: -50, y: 0 }, EdgePositionBottomRight, emptyModifiers)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: 100, top: 100, width: 150, height: 150 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 50, top: 33, height: 89, right: 60, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 13, height: 56, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 13, height: 56, background: 'blue', top: 33, left: 27 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
        )

        await resizeElement(
          renderResult,
          { x: -150, y: 100 },
          EdgePositionBottomLeft,
          emptyModifiers,
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: -50, top: 100, width: 300, height: 250 }}>
                <Group data-uid='group' style={{ position: 'absolute', left: 50, top: 33, height: 89, right: 60, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 62, height: 56, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 62, height: 56, background: 'blue', top: 33, left: 128 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
        )
      })
      it('when the group is pinned vertically', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: 100, top: 100, width: 200, height: 150 }}>
                <Group data-uid='group' style={{ position: 'absolute', top: 50, bottom: 33, width: 89, left: 60, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 56, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 56, background: 'blue', top: 33, left: 100 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/div')

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(renderResult, { x: 0, y: -50 }, EdgePositionBottomRight, emptyModifiers)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: 100, top: 100, width: 200, height: 100 }}>
                <Group data-uid='group' style={{ position: 'absolute', top: 50, bottom: 33, width: 89, left: 60, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 30, height: 11, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 30, height: 11, background: 'blue', top: 6, left: 59 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
        )

        await resizeElement(
          renderResult,
          { x: -150, y: 100 },
          EdgePositionBottomLeft,
          emptyModifiers,
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <div data-uid='div' style={{ background: '#aaa', position: 'absolute', left: -50, top: 100, width: 350, height: 200 }}>
                <Group data-uid='group' style={{ position: 'absolute', top: 50, bottom: 33, width: 89, left: 60, background: 'white' }}>
                  <div data-uid='foo' style={{ position: 'absolute', width: 30, height: 76, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position: 'absolute', width: 30, height: 76, background: 'blue', top: 41, left: 59 }} />
                </Group>
              </div>
            </Storyboard>
          )
        `),
        )
      })
    })
    describe('text children', () => {
      function isBetween(n: number, min: number, max: number) {
        return min <= n && n <= max
      }

      it('does not resize text elements past their intrinsic size', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', left: 114, top: 115, width: 366, height: 308 }}>
                <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 118, height: 220 }} />
                <span data-uid='span' data-testid='span' style={{ position: 'absolute', wordBreak: 'break-word', left: 231, top: 258, width: 135, height: 50, backgroundColor: 'red', color: 'white' }}>
                  hello there
                </span>
              </Group>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/group')

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(
          renderResult,
          { x: -200, y: -200 },
          EdgePositionBottomRight,
          emptyModifiers,
        )

        const span = await renderResult.renderedDOM.findByTestId('span')
        expect(span.clientWidth).toEqual(68)
        expect(isBetween(span.clientHeight, 18, 19)).toBe(true)
      })
      it('does not resize text elements past their intrinsic size with padding', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', left: 114, top: 115, width: 366, height: 308 }}>
                <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 118, height: 220 }} />
                <span data-uid='span' data-testid='span' style={{ padding: 10, position: 'absolute', wordBreak: 'break-word', left: 231, top: 258, width: 135, height: 50, backgroundColor: 'red', color: 'white' }}>
                  hello there
                </span>
              </Group>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/group')

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(
          renderResult,
          { x: -200, y: -200 },
          EdgePositionBottomRight,
          emptyModifiers,
        )

        const span = await renderResult.renderedDOM.findByTestId('span')
        expect(span.clientWidth).toEqual(88)
        expect(isBetween(span.clientHeight, 37, 39)).toBe(true)
      })

      it('does not resize text elements past their intrinsic size when zoomed in', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', left: 114, top: 115, width: 366, height: 308 }}>
                <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 118, height: 220 }} />
                <span data-uid='span' data-testid='span' style={{ padding: 10, position: 'absolute', wordBreak: 'break-word', left: 231, top: 258, width: 135, height: 50, backgroundColor: 'red', color: 'white' }}>
                  hello there
                </span>
              </Group>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/group')

        await renderResult.dispatch(
          [selectComponents([target], false), CanvasActions.zoom(2)],
          true,
        )

        await resizeElement(
          renderResult,
          { x: -400, y: -400 },
          EdgePositionBottomRight,
          emptyModifiers,
        )

        const span = await renderResult.renderedDOM.findByTestId('span')
        expect(span.clientWidth).toEqual(88)
        expect(isBetween(span.clientHeight, 37, 39)).toBe(true)
      })
      it('does not resize text elements past their intrinsic size when zoomed out', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', left: 114, top: 115, width: 366, height: 308 }}>
                <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 118, height: 220 }} />
                <span data-uid='span' data-testid='span' style={{ padding: 10, position: 'absolute', wordBreak: 'break-word', left: 231, top: 258, width: 135, height: 50, backgroundColor: 'red', color: 'white' }}>
                  hello there
                </span>
              </Group>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/group')

        await renderResult.dispatch(
          [selectComponents([target], false), CanvasActions.zoom(0.5)],
          true,
        )

        await resizeElement(
          renderResult,
          { x: -100, y: -100 },
          EdgePositionBottomRight,
          emptyModifiers,
        )

        const span = await renderResult.renderedDOM.findByTestId('span')
        expect(span.clientWidth).toEqual(88)
        expect(isBetween(span.clientHeight, 37, 39)).toBe(true)
      })
      it('does not resize text elements past their intrinsic size for nested groups', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', left: 257, top: 97, width: 533, height: 438 }}>
                <Group data-uid='nested-group' style={{ position: 'absolute', left: 0, top: 153, width: 365, height: 285 }}>
                  <div data-uid='nested-div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 165, height: 165 }} />
                  <span data-uid='span' data-testid='span' style={{ position: 'absolute', wordBreak: 'break-word', left: 237, top: 233, width: 128, height: 52, backgroundColor: 'red', color: 'white' }}>
                    hello there
                  </span>
                </Group>
                <div data-uid='div' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 444, top: 0, width: 89, height: 104 }} />
              </Group>
            </Storyboard>
          )
        `),
          'await-first-dom-report',
        )

        const target = EP.fromString('storyboard/group')

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(
          renderResult,
          { x: -200, y: -200 },
          EdgePositionBottomRight,
          emptyModifiers,
        )

        const span = await renderResult.renderedDOM.findByTestId('span')
        expect(span.clientWidth).toEqual(80)
        expect(span.clientHeight).toEqual(28)
      })
    })
    describe('constrained children', () => {
      async function resizeTarget(
        input: string,
        target: ElementPath,
        delta: Delta,
        edgePosition: EdgePosition,
      ) {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(input),
          'await-first-dom-report',
        )

        await renderResult.dispatch([selectComponents([target], false)], true)

        await resizeElement(renderResult, delta, edgePosition, emptyModifiers)

        return getPrintedUiJsCode(renderResult.getEditorState())
      }

      const tests: {
        name: string
        input: string
        target: ElementPath
        edgePosition: EdgePosition
        delta: Delta
        want: string
      }[] = [
        {
          name: 'respects min width for a constraint on left',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} data-constraints={['left']} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 100, height: 50, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 33, height: 17, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 0, height: 17, background: 'blue', top: 33, left: 100 }} data-constraints={['left']} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on left and width',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} data-constraints={['left', 'width']} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 50, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 17, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 17, background: 'blue', top: 33, left: 100 }} data-constraints={['left', 'width']} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on right',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} data-constraints={['right']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 100, height: 50, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 0, height: 17, background: 'red', top: 0, left: 0 }} data-constraints={['right']} />
                <div data-uid='bar' style={{ position:'absolute', width: 33, height: 17, background: 'blue', top: 33, left: 67 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on right and width',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} data-constraints={['right', 'width']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 50, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 50, height: 17, background: 'red', top: 0, left: 0 }} data-constraints={['right', 'width']} />
                <div data-uid='bar' style={{ position:'absolute', width: 50, height: 17, background: 'blue', top: 33, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on top',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} data-constraints={['top']} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 100, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 17, height: 33, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 17, height: 0, background: 'blue', top: 100, left: 33 }} data-constraints={['top']} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on top and height',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} data-constraints={['top', 'height']} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 150, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 17, height: 50, background: 'red', top: 0, left: 0 }} />
                <div data-uid='bar' style={{ position:'absolute', width: 17, height: 50, background: 'blue', top: 100, left: 33 }} data-constraints={['top', 'height']} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on bottom',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} data-constraints={['bottom']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 100, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 17, height: 0, background: 'red', top: 0, left: 0 }} data-constraints={['bottom']} />
                <div data-uid='bar' style={{ position:'absolute', width: 17, height: 33, background: 'blue', top: 67, left: 33 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on bottom and height',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} data-constraints={['bottom', 'height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 150, background: 'black' }}>
                <div data-uid='foo' style={{ position:'absolute', width: 17, height: 50, background: 'red', top: 0, left: 0 }} data-constraints={['bottom', 'height']} />
                <div data-uid='bar' style={{ position:'absolute', width: 17, height: 50, background: 'blue', top: 100, left: 33 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on width',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50 }} data-constraints={['width']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 50, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 17, height: 17, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 17, background: 'cyan', top: 17, left: 0 }} data-constraints={['width']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 17, height: 17, background: 'blue', top: 33, left: 33 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on height',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50 }} data-constraints={['height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 50, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 17, height: 17, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 17, height: 50, background: 'cyan', top: 0, left: 17 }} data-constraints={['height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 17, height: 17, background: 'blue', top: 33, left: 33 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on both width and height',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50 }} data-constraints={['width', 'height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 50, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 17, height: 17, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 0, left: 0 }} data-constraints={['width', 'height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 17, height: 17, background: 'blue', top: 33, left: 33 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on both left and right',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50, right: 50 }} data-constraints={['left', 'right']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 50, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 17, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 17, background: 'cyan', top: 17, left: 50, right: 50 }} data-constraints={['left', 'right']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 17, background: 'blue', top: 33, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'respects min width for a constraint on both top and bottom',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50, bottom: 50 }} data-constraints={['top', 'bottom']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: -200, y: -100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 50, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 17, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 17, height: 50, background: 'cyan', top: 50, left: 17, bottom: 50 }} data-constraints={['top', 'bottom']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 17, height: 50, background: 'blue', top: 100, left: 33 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'locks max height for a constraint on both top and bottom and height',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50, bottom: 50 }} data-constraints={['top', 'bottom', 'height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: 200, y: 100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 350, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 117, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 117, height: 50, background: 'cyan', top: 50, left: 117, bottom: 50 }} data-constraints={['top', 'bottom', 'height']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 117, height: 50, background: 'blue', top: 100, left: 233 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
        {
          name: 'locks max width for a constraint on both left and right and width',
          input: `
            import * as React from 'react'
            import { Storyboard, Group } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='storyboard'>
                <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 150, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 50, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 50, background: 'cyan', top: 50, left: 50, right: 50 }} data-constraints={['left', 'right', 'width']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 50, background: 'blue', top: 100, left: 100 }} />
                </Group>
              </Storyboard>
            )
          `,
          target: EP.fromString('storyboard/group'),
          edgePosition: EdgePositionBottomRight,
          delta: { x: 200, y: 100 },
          want: `
          import * as React from 'react'
          import { Storyboard, Group } from 'utopia-api'

          export var storyboard = (
            <Storyboard data-uid='storyboard'>
              <Group data-uid='group' style={{ position: 'absolute', width: 150, height: 250, background: 'black' }}>
                  <div data-uid='foo' style={{ position:'absolute', width: 50, height: 83, background: 'red', top: 0, left: 0 }} />
                  <div data-uid='baz' style={{ position: 'absolute', width: 50, height: 83, background: 'cyan', top: 83, left: 50, right: 50 }} data-constraints={['left', 'right', 'width']} />
                  <div data-uid='bar' style={{ position:'absolute', width: 50, height: 83, background: 'blue', top: 167, left: 100 }} />
              </Group>
            </Storyboard>
          )
        `,
        },
      ]

      for (let idx = 0; idx < tests.length; idx++) {
        const tt = tests[idx]
        it(`(${idx + 1}) ${tt.name}`, async () => {
          const got = await resizeTarget(tt.input, tt.target, tt.delta, tt.edgePosition)
          expect(got).toEqual(formatTestProjectCode(tt.want))
        })
      }
    })
  })
})

describe('Absolute Resize Strategy Canvas Controls', () => {
  it('when an absolute positioned element is selected the bounding box shows', async () => {
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

    expectElementWithTestIdNotToBeRendered(renderResult, AbsoluteResizeControlTestId([]))

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await renderResult.dispatch([selectComponents([target], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    expectElementWithTestIdToBeRendered(renderResult, AbsoluteResizeControlTestId([target]))
  })

  it('when a condition is overriden to one without an element, the bounding box disappears (when the conditional has siblings)', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          {
            // @utopia/uid=conditional
            true ? (
              <div
                style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, right: 160, bottom: 230 }}
                data-uid='bbb'
                data-testid='bbb'
              />
            ) : null
          }
          <div />
        </div>
      `),
      'await-first-dom-report',
    )

    expectElementWithTestIdNotToBeRendered(renderResult, AbsoluteResizeControlTestId([]))

    const conditional = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])
    await renderResult.dispatch([selectComponents([conditional], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const childOfConditional = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional', 'bbb'])

    expectElementWithTestIdNotToBeRendered(renderResult, AbsoluteResizeControlTestId([conditional]))
    expectElementWithTestIdToBeRendered(
      renderResult,
      AbsoluteResizeControlTestId([childOfConditional]),
    )

    await renderResult.dispatch([setConditionalOverriddenCondition(conditional, false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()
    expectElementWithTestIdNotToBeRendered(
      renderResult,
      AbsoluteResizeControlTestId([childOfConditional]),
    )

    expectElementWithTestIdNotToBeRendered(
      renderResult,
      AbsoluteResizeControlTestId([childOfConditional]),
    )
    expectElementWithTestIdNotToBeRendered(renderResult, AbsoluteResizeControlTestId([conditional]))
  })

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

    expectElementWithTestIdNotToBeRendered(renderResult, ImmediateParentOutlinesTestId([]))
    expectElementWithTestIdNotToBeRendered(renderResult, ImmediateParentBoundsTestId([]))
    expectElementWithTestIdNotToBeRendered(renderResult, AbsoluteResizeControlTestId([]))

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await startDragUsingActions(renderResult, target, EdgePositionLeft, canvasPoint({ x: 5, y: 5 }))

    await wait(ControlDelay + 10)
    expectElementWithTestIdToBeRendered(renderResult, ImmediateParentOutlinesTestId([target]))
    expectElementWithTestIdToBeRendered(renderResult, ImmediateParentBoundsTestId([target]))
    expectElementWithTestIdToBeRendered(renderResult, AbsoluteResizeControlTestId([target]))
  })

  describe('when a fragment-like element is resized the parent outlines become visible', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`resizing a ${type}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
          <div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, right: 170, bottom: 240 }} data-uid='container'>
            ${getOpeningFragmentLikeTag(type)}
              <div
                style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, right: 160, bottom: 230 }}
                data-uid='bbb'
                data-testid='bbb'
              />
              ${getClosingFragmentLikeTag(type)}
          </div>
          `),
          'await-first-dom-report',
        )

        expectElementWithTestIdNotToBeRendered(renderResult, ImmediateParentOutlinesTestId([]))
        expectElementWithTestIdNotToBeRendered(renderResult, ImmediateParentBoundsTestId([]))
        expectElementWithTestIdNotToBeRendered(renderResult, AbsoluteResizeControlTestId([]))

        const target = EP.appendNewElementPath(TestScenePath, ['container', FragmentLikeElementUid])
        await startDragUsingActions(
          renderResult,
          target,
          EdgePositionLeft,
          canvasPoint({ x: 5, y: 5 }),
        )

        await wait(ControlDelay + 10)
        expectElementWithTestIdToBeRendered(renderResult, ImmediateParentOutlinesTestId([target]))
        expectElementWithTestIdToBeRendered(renderResult, ImmediateParentBoundsTestId([target]))
        // FIXME Does this imply these tests are actually broken, or that this was the actual expectation all along?
        expectElementWithTestIdToBeRenderedWithDisplayNone(
          renderResult,
          AbsoluteResizeControlTestId([target]),
        )
      })
    })
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

  it('can resize from near the size label', async () => {
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

    const canvasControl = getResizeControl(renderResult, EdgePositionBottom)
    if (canvasControl == null) {
      throw new Error(`Could not find canvas control.`)
    }

    const resizeCornerBounds = canvasControl.getBoundingClientRect()
    const startPoint = windowPoint({
      x: resizeCornerBounds.x + resizeCornerBounds.width / 2,
      y: resizeCornerBounds.y + resizeCornerBounds.height - 1,
    })

    const endPoint = {
      x: startPoint.x + dragDelta.x,
      y: startPoint.y + dragDelta.y,
    }

    await mouseDragFromPointToPoint(canvasControl, startPoint, endPoint, { realMouseDown: true })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 95 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
})

describe('Double click on resize edge', () => {
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

  describe('grids', () => {
    const project = ({
      rows: rows,
      columns: columns,
    }: {
      rows: string
      columns: string
    }) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='sc'
    >
      <div
        style={{
          display: 'grid',
          gridTemplateRows: '${rows}',
          gridTemplateColumns: '${columns}',
          backgroundColor: '#0074ff',
          position: 'absolute',
          left: 135,
          top: 55,
          width: 400,
          height: 400,
          opacity: '30%',
        }}
        data-uid='grid'
        data-testid='mydiv'
      >
        <img
          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'
          alt='Utopia logo'
          style={{
            objectFit: 'contain',
            display: 'inline-block',
            width: 100,
            height: 100,
            gridColumnStart: 2,
            gridColumnEnd: 2,
            gridRowStart: 2,
            gridRowEnd: 2,
          }}
          data-uid='smyramid'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
    it('removes width when right edge is clicked', async () => {
      const editor = await renderTestEditorWithCode(
        project({ rows: '66px 66px 66px 66px', columns: '50px 81px 96px 85px' }),
        'await-first-dom-report',
      )
      const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionRight))
      expect(div.style.width).toEqual('') // width is removed
      expect(div.style.height).toEqual('400px')
    })
    it('removes height when bottom edge is clicked', async () => {
      const editor = await renderTestEditorWithCode(
        project({ rows: '66px 66px 66px 66px', columns: '50px 81px 96px 85px' }),
        'await-first-dom-report',
      )
      const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
      expect(div.style.width).toEqual('400px')
      expect(div.style.height).toEqual('') // height is removed
    })
    it("isn't applicable when the selected grid uses fr along the affected axis", async () => {
      const editor = await renderTestEditorWithCode(
        project({ rows: '66px 1fr 66px 66px', columns: '50px 81px 96px 85px' }),
        'await-first-dom-report',
      )
      const div = await doDblClickTest(editor, edgeResizeControlTestId(EdgePositionBottom))
      expect(div.style.width).toEqual('400px')
      expect(div.style.height).toEqual('400px')
    })
  })
})

describe('double click on resize corner', () => {
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

async function makeResizeInGroupProject(
  type: FragmentLikeType,
  targets: Array<ElementPath>,
): Promise<string> {
  FOR_TESTS_setNextGeneratedUids([
    'skip1',
    'skip2',
    'skip3',
    'skip4',
    'skip5',
    'skip6',
    'skip7',
    'skip8',
    'skip9',
    'skip10',
    'fragment-like',
  ])

  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='aaa'
      >
        ${getOpeningFragmentLikeTag(type)}
          <View
            style={{
              backgroundColor: '#aaaaaa33',
              contain: 'layout',
              position: 'absolute',
              width: 80,
              height: 100,
              left: 40,
              top: 50,
            }}
            data-uid='ccc'
            data-testid='ccc'
          />
          <View
            style={{
              backgroundColor: '#aaaaaa33',
              contain: 'layout',
              position: 'absolute',
              width: 130,
              height: 120,
              left: 170,
              top: 70,
            }}
            data-uid='ddd'
          />
          ${getClosingFragmentLikeTag(type)}
        <View
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            width: 40,
            height: 40,
            left: 30,
            top: 330,
          }}
          data-uid='xxx'
        />
      </div>
    `),
    'await-first-dom-report',
  )

  await renderResult.getDispatchFollowUpActionsFinished()

  const initialCode = getPrintedUiJsCode(renderResult.getEditorState())

  const dragDelta = windowPoint({ x: -30, y: -30 })

  await renderResult.dispatch([selectComponents(targets, false)], true)
  await resizeElement(renderResult, dragDelta, EdgePositionTopLeft, emptyModifiers)
  await renderResult.getDispatchFollowUpActionsFinished()

  const result = getPrintedUiJsCode(renderResult.getEditorState())
  renderResult.renderedDOM.unmount()

  // make sure something actually changed in the project
  expect(result).not.toEqual(initialCode)

  return result
}

describe('Absolute Resize Group-like behaviors', () => {
  AllFragmentLikeTypes.forEach((type) => {
    describe(`group-like ${type} element`, () => {
      it('resizing a group is the same as multiselect resizing the children', async () => {
        const groupResizeResult = await makeResizeInGroupProject(type, [
          EP.appendNewElementPath(TestScenePath, ['aaa', 'fragment-like']),
        ])
        const multiselectResult = await makeResizeInGroupProject(type, [
          EP.appendNewElementPath(TestScenePath, ['aaa', 'fragment-like', 'inner-fragment', 'ccc']),
          EP.appendNewElementPath(TestScenePath, ['aaa', 'fragment-like', 'inner-fragment', 'ddd']),
        ])

        expect(groupResizeResult).toEqual(multiselectResult)
      })
    })
  })
})

describe('Absolute Resize Control', () => {
  describe('safe gap', () => {
    it('Resize control is placed on small elements outside of the draggable frame area, with a safe gap', async () => {
      const width = SafeGapSmallElementSize
      const height = SafeGapSmallElementSize
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: ${width}, height: ${height} }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
        'await-first-dom-report',
      )

      const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
      await selectComponentsForTest(renderResult, [target])

      const resizeControlTop = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionTop.x}-${EdgePositionTop.y}`,
      )
      expect(resizeControlTop.style.transform).toEqual('translate(0px, -5px)')
      expect(resizeControlTop.style.top).toEqual('')
      expect(resizeControlTop.style.left).toEqual('')
      expect(resizeControlTop.style.width).toEqual(`${width + RESIZE_CONTROL_SAFE_GAP * 2}px`)
      expect(resizeControlTop.style.height).toEqual('5px')

      const resizeControlRight = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionRight.x}-${EdgePositionRight.y}`,
      )
      expect(resizeControlRight.style.transform).toEqual('translate(0px, 0px)')
      expect(resizeControlRight.style.top).toEqual('')
      expect(resizeControlRight.style.left).toEqual(`${width + RESIZE_CONTROL_SAFE_GAP * 2}px`)
      expect(resizeControlRight.style.width).toEqual('5px')
      expect(resizeControlRight.style.height).toEqual(`${height + RESIZE_CONTROL_SAFE_GAP * 2}px`)

      const resizeControlBottom = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionBottom.x}-${EdgePositionBottom.y}`,
      )
      expect(resizeControlBottom.style.transform).toEqual('translate(0px, 0px)')
      expect(resizeControlBottom.style.top).toEqual(`${height + RESIZE_CONTROL_SAFE_GAP * 2}px`)
      expect(resizeControlBottom.style.left).toEqual('')
      expect(resizeControlBottom.style.width).toEqual(`${width + RESIZE_CONTROL_SAFE_GAP * 2}px`)
      expect(resizeControlBottom.style.height).toEqual('5px')

      const resizeControlLeft = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionLeft.x}-${EdgePositionLeft.y}`,
      )
      expect(resizeControlLeft.style.transform).toEqual('translate(-5px, 0px)')
      expect(resizeControlLeft.style.top).toEqual('')
      expect(resizeControlLeft.style.left).toEqual('')
      expect(resizeControlLeft.style.width).toEqual('5px')
      expect(resizeControlLeft.style.height).toEqual(`${height + RESIZE_CONTROL_SAFE_GAP * 2}px`)
    })
    it("Doesn't show the safe gap during resize after threshold is passed", async () => {
      const width = SmallElementSize
      const height = SmallElementSize
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: ${width}, height: ${height} }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
        'await-first-dom-report',
      )

      const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
      await selectComponentsForTest(renderResult, [target])

      const bottomRightHandle = await renderResult.renderedDOM.findByTestId(`resize-control-1-1`)
      const topRightHandle = await renderResult.renderedDOM.findByTestId(`resize-control-1-0`)
      const topLeftHandle = await renderResult.renderedDOM.findByTestId(`resize-control-0-0`)
      const bottomLeftHandle = await renderResult.renderedDOM.findByTestId(`resize-control-0-1`)

      await mouseDownAtPoint(bottomRightHandle, { x: 2, y: 2 })
      await mouseMoveToPoint(bottomRightHandle, { x: 10, y: 0 })

      const threshold = 2 // px

      expect(bottomRightHandle.parentElement?.style.visibility).toEqual('visible')
      expect(topRightHandle.parentElement?.style.visibility).toEqual('hidden')
      expect(topLeftHandle.parentElement?.style.visibility).toEqual('hidden')
      expect(bottomLeftHandle.parentElement?.style.visibility).toEqual('hidden')

      const resizeControlTop = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionTop.x}-${EdgePositionTop.y}`,
      )
      expect(resizeControlTop.style.transform).toEqual('translate(0px, -5px)')
      expect(resizeControlTop.style.top).toEqual('')
      expect(resizeControlTop.style.left).toEqual('')
      expect(resizeControlTop.style.width).toEqual(`${width + 10 - threshold}px`)
      expect(resizeControlTop.style.height).toEqual('5px')

      const resizeControlRight = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionRight.x}-${EdgePositionRight.y}`,
      )
      expect(resizeControlRight.style.transform).toEqual('translate(-5px, 0px)')
      expect(resizeControlRight.style.top).toEqual('')
      expect(resizeControlRight.style.left).toEqual(`${width + 10 - threshold}px`)
      expect(resizeControlRight.style.width).toEqual('10px')
      expect(resizeControlRight.style.height).toEqual(`${height - threshold}px`)

      const resizeControlBottom = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionBottom.x}-${EdgePositionBottom.y}`,
      )
      expect(resizeControlBottom.style.transform).toEqual('translate(0px, 0px)')
      expect(resizeControlBottom.style.top).toEqual(`${height - 2}px`)
      expect(resizeControlBottom.style.left).toEqual('')
      expect(resizeControlBottom.style.width).toEqual(`${width + 10 - threshold}px`)
      expect(resizeControlBottom.style.height).toEqual('5px')

      const resizeControlLeft = renderResult.renderedDOM.getByTestId(
        `resize-control-${EdgePositionLeft.x}-${EdgePositionLeft.y}`,
      )
      expect(resizeControlLeft.style.transform).toEqual('translate(-5px, 0px)')
      expect(resizeControlLeft.style.top).toEqual('')
      expect(resizeControlLeft.style.left).toEqual('')
      expect(resizeControlLeft.style.width).toEqual('10px')
      expect(resizeControlLeft.style.height).toEqual(`${height - threshold}px`)
    })
  })
  it('Resize control on non-small elements extend into the draggable frame area', async () => {
    const width = SmallElementSize + 1
    const height = SmallElementSize + 1
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: ${width}, height: ${height} }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await selectComponentsForTest(renderResult, [target])

    const resizeControlTop = renderResult.renderedDOM.getByTestId(
      `resize-control-${EdgePositionTop.x}-${EdgePositionTop.y}`,
    )
    expect(resizeControlTop.style.transform).toEqual('translate(0px, -5px)')
    expect(resizeControlTop.style.top).toEqual('')
    expect(resizeControlTop.style.left).toEqual('')
    expect(resizeControlTop.style.width).toEqual(`${width}px`)
    expect(resizeControlTop.style.height).toEqual('10px')

    const resizeControlRight = renderResult.renderedDOM.getByTestId(
      `resize-control-${EdgePositionRight.x}-${EdgePositionRight.y}`,
    )
    expect(resizeControlRight.style.transform).toEqual('translate(-5px, 0px)')
    expect(resizeControlRight.style.top).toEqual('')
    expect(resizeControlRight.style.left).toEqual(`${width}px`)
    expect(resizeControlRight.style.width).toEqual('10px')
    expect(resizeControlRight.style.height).toEqual(`${height}px`)

    const resizeControlBottom = renderResult.renderedDOM.getByTestId(
      `resize-control-${EdgePositionBottom.x}-${EdgePositionBottom.y}`,
    )
    expect(resizeControlBottom.style.transform).toEqual('translate(0px, -5px)')
    expect(resizeControlBottom.style.top).toEqual(`${height}px`)
    expect(resizeControlBottom.style.left).toEqual('')
    expect(resizeControlBottom.style.width).toEqual(`${width}px`)
    expect(resizeControlBottom.style.height).toEqual('10px')

    const resizeControlLeft = renderResult.renderedDOM.getByTestId(
      `resize-control-${EdgePositionLeft.x}-${EdgePositionLeft.y}`,
    )
    expect(resizeControlLeft.style.transform).toEqual('translate(-5px, 0px)')
    expect(resizeControlLeft.style.top).toEqual('')
    expect(resizeControlLeft.style.left).toEqual('')
    expect(resizeControlLeft.style.width).toEqual('10px')
    expect(resizeControlLeft.style.height).toEqual(`${height}px`)
  })
  it('dims the size label on hover', async () => {
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

    await renderResult.dispatch([selectComponents([target], false)], true)
    const sizeLabel = renderResult.renderedDOM.getByTestId(SizeLabelTestId)
    const sizeLabelBounds = sizeLabel.getBoundingClientRect()
    act(() => {
      dispatchMouseEnterEventAtPoint({ x: sizeLabelBounds.x + 2, y: sizeLabelBounds.y + 2 })
    })

    const { opacity } = sizeLabel.style
    expect(opacity).toEqual('0.075')
  })
})
