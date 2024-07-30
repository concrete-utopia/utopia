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
import { selectComponents } from '../../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import type { WindowPoint } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier, emptyModifiers, shiftModifier } from '../../../../utils/modifiers'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import { SceneLabelTestID } from '../../controls/select-mode/scene-label'
import {
  keyUp,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
  mouseUpAtPoint,
} from '../../event-helpers.test-utils'
import { ImmediateParentOutlinesTestId } from '../../controls/parent-outlines'
import {
  expectElementWithTestIdNotToBeRendered,
  expectElementWithTestIdToBeRendered,
  selectComponentsForTest,
} from '../../../../utils/utils.test-utils'
import { ImmediateParentBoundsTestId } from '../../controls/parent-bounds'
import { AllFragmentLikeTypes, FragmentLikeType } from './fragment-like-helpers'
import {
  getOpeningFragmentLikeTag,
  getClosingFragmentLikeTag,
  FragmentLikeElementUid,
  InnerFragmentId,
} from './fragment-like-helpers.test-utils'
import { getDomRectCenter } from '../../../../core/shared/dom-utils'
import { cartesianProduct } from '../../../../core/shared/array-utils'
import { NO_OP } from '../../../../core/shared/utils'
import { MoveReorderReparentIndicatorID } from '../../controls/select-mode/strategy-indicator'
import { CanvasToolbarEditButtonID } from '../../../editor/canvas-toolbar'
import { ComponentsHonouringPropsStylesProject } from './common-projects'

async function dragByPixels(
  editor: EditorRenderResult,
  delta: WindowPoint,
  testid: string,
  modifiers: Modifiers = emptyModifiers,
) {
  const targetElement = editor.renderedDOM.getByTestId(testid)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const targetElementCenter = windowPoint(getDomRectCenter(targetElementBounds))
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

  await mouseDragFromPointWithDelta(canvasControlsLayer, targetElementCenter, delta, {
    modifiers,
    midDragCallback: async () => {
      NO_OP()
    },
  })
}

async function dragElement(
  canvasControlsLayer: HTMLElement,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  midDragCallback?: () => Promise<void>,
): Promise<void> {
  await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers,
    midDragCallback,
  })
}

const projectDoesNotHonourPositionProperties = `
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
      data-uid='div'
      data-testid='div'
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

function projectDoesHonourPositionProperties(left: number, top: number): string {
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
    <div
      data-uid='app-root'
      style={{ width: '100%', height: '100%' }}
    >
      <App2
        data-uid='app2'
        style={{
          left: ${left},
          top: ${top},
          width: 300,
          height: 400,
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

function positioningFromCss(css: CSSStyleDeclaration) {
  return { left: css.left, top: css.top }
}

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectElementWithTestIdNotToBeRendered", "expectElementWithTestIdToBeRendered"] }] */

describe('Absolute Move Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </View>
    `,
      ),
      'await-first-dom-report',
    )

    const initialEditorCode = getPrintedUiJsCode(editor.getEditorState())

    const targetElement = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
    )

    await selectComponentsForTest(editor, [targetElement])

    await dragByPixels(editor, windowPoint({ x: 1, y: 1 }), 'bbb')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditorCode)
  })

  describe('pinned move', () => {
    it('works with a TL pinned absolute element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </View>
  `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </View>
`,
        ),
      )
    })
    it('works with a TL pinned absolute element with px values', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: 50, width: 250, height: 300 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
  `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </View>
`,
        ),
      )
    })
    it('works with a TL pinned absolute element with px values, with disabled snapping', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: 50, width: 250, height: 300 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '66px', top: 66, width: 250, height: 300 }}
            data-uid='ccc'
          />
        </View>
      `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb', cmdModifier)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '66px', top: 66, width: 250, height: 300 }}
            data-uid='ccc'
          />
        </View>
    `,
        ),
      )
    })
    it('works with a RB pinned absolute element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 50, bottom: 50, width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
        `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 35, bottom: 35, width: 250, height: 300 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </View>
`,
        ),
      )
    })
    it('works with a TLRB pinned absolute element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, right: 100, bottom: 50, width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
        `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, right: 85, bottom: 35, width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
`,
        ),
      )
    })

    it('overrides expressions + fires toast', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50 + 5, top: 50 + (props.top ?? 0), width: 250, height: 300 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </View>
      `,
        ),
        'await-first-dom-report',
      )

      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 70, top: 65, width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
          `,
        ),
      )

      expect(editor.getEditorState().editor.toasts.length).toBe(1)
    })

    it('works with percentages', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '25%', top: '0%', width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
        `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '28.75%', top: '3.75%', width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
        `,
        ),
      )
    })
  })
  describe('Axis locked move', () => {
    it('works with a TL pinned absolute element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </View>
  `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 10, y: 20 }), 'bbb', shiftModifier)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 70, width: 250, height: 300 }}
        data-uid='bbb'
        data-testid='bbb'
      />
    </View>
`,
        ),
      )
    })
    it('works with a TLRB pinned absolute element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, bottom: 250, right: 200 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
        `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 25, y: 10 }), 'bbb', shiftModifier)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 75, top: 50, bottom: 250, right: 175 }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </View>
        `,
        ),
      )
    })
    it('works with a TL pinned absolute element with child', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            >
              <View
                style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
                data-uid='ccc'
              />
            </View>
          </View>
        `,
        ),
        'await-first-dom-report',
      )
      const targetElement = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
      )

      await selectComponentsForTest(editor, [targetElement])

      await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `
          <View style={{ ...(props.style || {}) }} data-uid='aaa'>
            <View
              style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
              data-uid='bbb'
              data-testid='bbb'
            >
              <View
                style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
                data-uid='ccc'
              />
            </View>
          </View>
`,
        ),
      )
    })
  })

  it('moves a component instance that honours the position properties by updating the instance', async () => {
    const renderResult = await renderTestEditorWithCode(
      ComponentsHonouringPropsStylesProject,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/mycompdiv')

    await renderResult.dispatch([selectComponents([target], false)], true)

    await dragByPixels(renderResult, windowPoint({ x: 15, y: 15 }), 'mycompdivinnerdiv')
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
          left: 150,
          top: 51,
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
  it('moves a component instance that honours the position properties (inside a fragment) by updating the instance', async () => {
    const renderResult = await renderTestEditorWithCode(
      ComponentsHonouringPropsStylesProject,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/mycompfrag')

    await renderResult.dispatch([selectComponents([target], false)], true)

    await dragByPixels(renderResult, windowPoint({ x: 15, y: 15 }), 'mycompfraginnerdiv')
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
          left: 36,
          top: 52,
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
  it('moves a regular div by updating it', async () => {
    const renderResult = await renderTestEditorWithCode(
      ComponentsHonouringPropsStylesProject,
      'await-first-dom-report',
    )

    const target = EP.fromString('storyboard/scene/regulardiv')

    await renderResult.dispatch([selectComponents([target], false)], true)

    const dragDelta = windowPoint({ x: 0, y: 30 })
    await dragByPixels(renderResult, windowPoint({ x: 15, y: 15 }), 'regulardiv')
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
          left: 186,
          top: 128,
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

  it('moves component instances that honour the position properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesHonourPositionProperties(20, 20),
      'await-first-dom-report',
    )
    const targetElement = renderResult.renderedDOM.getByTestId('div')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers, async () => {
      const moveReorderReparentIndicator = renderResult.renderedDOM.getByTestId(
        MoveReorderReparentIndicatorID,
      )
      expect(moveReorderReparentIndicator.innerText).toEqual('Moving Absolute Elements')
      const toolbarEditButtonImage = renderResult.renderedDOM.getByTestId(
        `${CanvasToolbarEditButtonID}-icon`,
      )
      expect(toolbarEditButtonImage.getAttribute('src')).toEqual(expect.stringContaining('moveabs')) // the toolbar shows the Absolute Move icon
    })

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      projectDoesHonourPositionProperties(60, -5),
    )
  })
  it('moves component instances that honour the position properties, selecting the element if the first mousedown uses cmd', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesHonourPositionProperties(20, 20),
      'await-first-dom-report',
    )
    const targetElement = renderResult.renderedDOM.getByTestId('div')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const endPoint = windowPoint({ x: targetElementBounds.x + 45, y: targetElementBounds.y - 20 })

    await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseMoveToPoint(canvasControlsLayer, endPoint, { eventOptions: { buttons: 1 } })
    await mouseUpAtPoint(canvasControlsLayer, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      projectDoesHonourPositionProperties(60, -5),
    )
  })
  it('releasing cmd mid drag does not break the interaction', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesHonourPositionProperties(20, 20),
      'await-first-dom-report',
    )
    const targetElement = renderResult.renderedDOM.getByTestId('div')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const midPoint = windowPoint({ x: targetElementBounds.x + 25, y: targetElementBounds.y - 10 })
    const endPoint = windowPoint({ x: targetElementBounds.x + 45, y: targetElementBounds.y - 20 })

    await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseMoveToPoint(canvasControlsLayer, midPoint, {
      modifiers: cmdModifier,
      eventOptions: { buttons: 1 },
    })
    await keyUp('Meta')
    await mouseMoveToPoint(canvasControlsLayer, endPoint, { eventOptions: { buttons: 1 } })
    await mouseUpAtPoint(canvasControlsLayer, endPoint)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      projectDoesHonourPositionProperties(60, -5),
    )
  })
  it('does not move a component instance that does not honour the position properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectDoesNotHonourPositionProperties,
      'await-first-dom-report',
    )
    const targetElement = renderResult.renderedDOM.getByTestId('div')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(projectDoesNotHonourPositionProperties),
    )
  })
  it('moves absolute positioned element', async () => {
    const startX = 40
    const startY = 50
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              startX + dragDelta.x
            }, top: ${startY + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })

  it('moves absolute positioned element even if it has a static parent', async () => {
    const startX = 40
    const startY = 50
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              startX + dragDelta.x
            }, top: ${startY + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })

  it('moves absolute positioned scene', async () => {
    const startX = 40
    const startY = 50
    const renderResult = await renderTestEditorWithCode(
      `
        import * as React from 'react'
        import {
          Scene,
          Storyboard,
        } from 'utopia-api'
        export var App = (props) => {
          return (
            <div data-uid='aaa' />
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='${BakedInStoryboardUID}'>
              <Scene
                style={{ position: 'absolute', left: ${startX}, top: ${startY}, width: 375, height: 812 }}
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
      `,
      'await-first-dom-report',
    )

    const sceneLabel = renderResult.renderedDOM.getByTestId(SceneLabelTestID)
    const sceneLabelBounds = sceneLabel.getBoundingClientRect()

    const startPoint = windowPoint({ x: sceneLabelBounds.x + 5, y: sceneLabelBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(sceneLabel, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import {
          Scene,
          Storyboard,
        } from 'utopia-api'
        export var App = (props) => {
          return (
            <div data-uid='aaa' />
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='${BakedInStoryboardUID}'>
              <Scene
                style={{ position: 'absolute', left: ${startX + dragDelta.x}, top: ${
        startY + dragDelta.y
      }, width: 375, height: 812 }}
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
    `),
    )
  })
  it('moves selected element even when it is covered with a non-selected one', async () => {
    const startX = 40
    const startY = 50
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      true,
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              startX + dragDelta.x
            }, top: ${startY + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX}, top: ${startY}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })
  it('works with TL pinned absolute elements in multiselection with descendant', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
            data-uid='bbb'
            data-testid='bbb'
          >
            <View
              style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
              data-uid='ccc'
            />
          </View>
        </View>
    `,
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`),
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb/ccc`),
    ])

    // await wait(10000)

    await dragByPixels(editor, windowPoint({ x: 15, y: 15 }), 'bbb')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 65, top: 65, width: 250, height: 300 }}
            data-uid='bbb'
            data-testid='bbb'
          >
            <View
              style={{ backgroundColor: '#0091FFAB', position: 'absolute', left: 100, top: 100, width: 250, height: 300 }}
              data-uid='ccc'
            />
          </View>
        </View>
    `,
      ),
    )
  })
  it('moves multiselection when dragging one of the selected elements', async () => {
    const startX1 = 40
    const startY1 = 50
    const startX2 = 80
    const startY2 = 100
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${startX1}, top: ${startY1}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
          ],
          false,
        ),
      ],
      true,
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              startX1 + dragDelta.x
            }, top: ${startY1 + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${
              startX2 + dragDelta.x
            }, top: ${startY2 + dragDelta.y}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })
  it('moves multiselection when dragging a non-selected element inside the selection area', async () => {
    const startX1 = 40
    const startY1 = 50
    const startX2 = 122
    const startY2 = 50
    const startX3 = 122
    const startY3 = 130
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${startX1}, top: ${startY1}, width: 60, height: 60 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 60, height: 60 }}
            data-uid='ccc'
            data-testid='ccc'
          />
          <div
            style={{ backgroundColor: 'green', position: 'absolute', left: ${startX3}, top: ${startY3}, width: 60, height: 60 }}
            data-uid='ddd'
            data-testid='ddd'
          />
          
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'ddd']),
          ],
          false,
        ),
      ],
      true,
    )

    const targetElement = renderResult.renderedDOM.getByTestId('ccc')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 41, y: -26 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              startX1 + dragDelta.x
            }, top: ${startY1 + dragDelta.y}, width: 60, height: 60 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 60, height: 60 }}
            data-uid='ccc'
            data-testid='ccc'
          />
          <div
            style={{ backgroundColor: 'green', position: 'absolute', left: ${
              startX3 + dragDelta.x
            }, top: ${startY3 + dragDelta.y}, width: 60, height: 60 }}
            data-uid='ddd'
            data-testid='ddd'
          />
        </div>
      `),
    )
  })
  it('ignores selection when dragging element outside of the selection area', async () => {
    const startX1 = 0
    const startY1 = 0
    const startX2 = 210
    const startY2 = 220
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${startX1}, top: ${startY1}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])], false)],
      true,
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 50, y: targetElementBounds.y + 50 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              startX1 + dragDelta.x
            }, top: ${startY1 + dragDelta.y}, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: 'red', position: 'absolute', left: ${startX2}, top: ${startY2}, width: 200, height: 120 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </div>
      `),
    )
  })

  it('fills in missing props of absolute positioned element', async () => {
    const parentMargin = 100
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          style={{ left: 50, top: 50, height: 150, width: 150, margin: ${parentMargin}}}
          data-uid='ccc'
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'absolute',
              width: 200,
              height: 300,
              backgroundColor: '#d3d3d3',
            }}
          />
        </div>
      </div>
      `),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: 25 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          position: 'absolute',
          width: '100%',
          height: '100%',
          backgroundColor: '#FFFFFF',
        }}
      >
        <div
          style={{ left: 50, top: 50, height: 150, width: 150, margin: ${parentMargin} }}
          data-uid='ccc'
        >
          <div
            data-uid='bbb'
            data-testid='bbb'
            style={{
              position: 'absolute',
              width: 200,
              height: 300,
              backgroundColor: '#d3d3d3',
              left: ${parentMargin + dragDelta.x},
              top: ${parentMargin + dragDelta.y},
            }}
          />
        </div>
      </div>
      `),
    )
  })
  it('moves absolute element with snapping, `bbb` should snap to `ccc`', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', width: 100, height: 30 }}
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

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 9, y: -23 }) // 'bbb' will snap to bottom edge and middle of 'ccc'

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 30, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  it('moves absolute element without snapping while pressing cmd `bbb` should not snap to `ccc`', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', width: 100, height: 30 }}
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

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 9, y: -23 })

    await dragElement(canvasControlsLayer, startPoint, dragDelta, cmdModifier)

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#aaaaaa33', width: 100, height: 30 }}
            data-uid='ccc'
          />
          <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 49, top: 27, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
    )
  })
  AllFragmentLikeTypes.forEach((type) => {
    it(`element with ${type} parent snaps to sibling within the ${type} parent`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 100,
          top: -7,
          width: 459,
          height: 217,
        }}
        data-uid='container'
      >
        ${getOpeningFragmentLikeTag(type)}
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 45,
              top: 77,
              width: 79,
              height: 119,
            }}
            data-uid='982'
            data-testid='sibling'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 157,
              top: 136,
              width: 36,
              height: 44,
            }}
            data-uid='drag-me'
            data-testid='drag-me'
          />
        ${getClosingFragmentLikeTag(type)}
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 265,
            top: 38,
            width: 88,
            height: 129,
          }}
          data-uid='e5a'
        />
      </div>`),
        'await-first-dom-report',
      )

      const siblingElement = editor.renderedDOM.getByTestId('sibling')
      const siblingBounds = siblingElement.getBoundingClientRect()

      const targetElement = editor.renderedDOM.getByTestId('drag-me')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const targetElementCenter = windowPoint(getDomRectCenter(targetElementBounds))
      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/${FragmentLikeElementUid}/${InnerFragmentId}/drag-me`,
        ),
      ])

      const delta = siblingBounds.top - targetElementBounds.top

      const dragDelta = windowPoint({ x: -1, y: delta })

      await mouseDragFromPointWithDelta(canvasControlsLayer, targetElementCenter, dragDelta, {
        midDragCallback: async () => {
          expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(
            1,
          )
        },
      })
    })
  })

  AllFragmentLikeTypes.forEach((type) => {
    it(`element with ${type} parent snaps to elements outside the ${type} parent`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 100,
        top: -7,
        width: 459,
        height: 217,
      }}
      data-uid='container'
    >
      ${getOpeningFragmentLikeTag(type)}
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 45,
            top: 77,
            width: 79,
            height: 119,
          }}
          data-uid='982'
          data-testid='sibling'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 157,
            top: 136,
            width: 36,
            height: 44,
          }}
          data-uid='drag-me'
          data-testid='drag-me'
        />
      ${getClosingFragmentLikeTag(type)}
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 265,
          top: 38,
          width: 88,
          height: 129,
        }}
        data-uid='element-outside'
        data-testid='element-outside'
      />
    </div>`),
        'await-first-dom-report',
      )

      const outsideElement = editor.renderedDOM.getByTestId('element-outside')
      const outsideElementBounds = outsideElement.getBoundingClientRect()

      const targetElement = editor.renderedDOM.getByTestId('drag-me')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const targetElementCenter = windowPoint(getDomRectCenter(targetElementBounds))
      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/${FragmentLikeElementUid}/${InnerFragmentId}/drag-me`,
        ),
      ])

      const delta = outsideElementBounds.top - targetElementBounds.top

      const dragDelta = windowPoint({ x: -1, y: delta })

      await mouseDragFromPointWithDelta(canvasControlsLayer, targetElementCenter, dragDelta, {
        midDragCallback: async () => {
          expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(
            1,
          )
        },
      })
    })
  })

  cartesianProduct(AllFragmentLikeTypes, AllFragmentLikeTypes).forEach(([outer, inner]) => {
    it(`element in ${inner}, nested in ${outer} parents snaps to elements in ${outer}`, async () => {
      const innerFragmentLikeUid = 'inner-fragment-like-uid'
      const innerFragmentUid = 'inner-fragment-like-fragment-uid'
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(` <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 176,
          top: 191,
          width: 462,
          height: 242,
        }}
        data-uid='container'
      >
        ${getOpeningFragmentLikeTag(outer)}
          ${getOpeningFragmentLikeTag(inner, {
            outerUid: innerFragmentLikeUid,
            innerUid: innerFragmentUid,
          })}
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 60,
                top: 75,
                width: 76,
                height: 111,
              }}
              data-uid='df3'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 182,
                top: 141,
                width: 33,
                height: 61,
              }}
              data-uid='drag-me'
              data-testid='drag-me'
            />
            ${getClosingFragmentLikeTag(inner)}
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 286,
              top: 51,
              width: 39,
              height: 159,
            }}
            data-uid='86d'
            data-testid='element-outside'
          />
        ${getClosingFragmentLikeTag(outer)}
      </div>`),
        'await-first-dom-report',
      )

      const outsideElement = editor.renderedDOM.getByTestId('element-outside')
      const outsideElementBounds = outsideElement.getBoundingClientRect()

      const targetElement = editor.renderedDOM.getByTestId('drag-me')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const targetElementCenter = windowPoint(getDomRectCenter(targetElementBounds))
      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/${FragmentLikeElementUid}/${InnerFragmentId}/${innerFragmentLikeUid}/${innerFragmentUid}/drag-me`,
        ),
      ])

      const delta = outsideElementBounds.top - targetElementBounds.top

      const dragDelta = windowPoint({ x: -1, y: delta })

      await mouseDragFromPointWithDelta(canvasControlsLayer, targetElementCenter, dragDelta, {
        midDragCallback: async () => {
          expect(editor.getEditorState().editor.canvas.controls.snappingGuidelines.length).toEqual(
            1,
          )
        },
      })
    })
  })
})

describe('Absolute Move Strategy Canvas Controls', () => {
  it('when an absolute positioned element is started to be moved parent outlines become visible', async () => {
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

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    await dragElement(
      canvasControlsLayer,
      startPoint,
      windowPoint({ x: 5, y: 5 }),
      emptyModifiers,
      async () => {
        expectElementWithTestIdToBeRendered(renderResult, ImmediateParentOutlinesTestId([target]))
        expectElementWithTestIdToBeRendered(renderResult, ImmediateParentBoundsTestId([target]))
      },
    )
  })

  describe('when a fragment-like element is moved, parent outlines become visible', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`moving a ${type}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
          <div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, right: 170, bottom: 240 }} data-uid='container' data-label='container'>
            ${getOpeningFragmentLikeTag(type)}
            <div
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 100, height: 20 }}
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

        const target = EP.appendNewElementPath(TestScenePath, [
          'container',
          FragmentLikeElementUid,
          InnerFragmentId,
        ])
        await selectComponentsForTest(renderResult, [target])

        const targetElement = renderResult.renderedDOM.getByTestId('bbb')
        const targetElementBounds = targetElement.getBoundingClientRect()
        const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

        const startPoint = windowPoint({
          x: targetElementBounds.x + 5,
          y: targetElementBounds.y + 5,
        })

        await dragElement(
          canvasControlsLayer,
          startPoint,
          windowPoint({ x: 5, y: 5 }),
          emptyModifiers,
          async () => {
            expectElementWithTestIdToBeRendered(
              renderResult,
              ImmediateParentOutlinesTestId([target]),
            )
            expectElementWithTestIdToBeRendered(renderResult, ImmediateParentBoundsTestId([target]))
          },
        )
      })
    })
  })

  it('when an absolute positioned element is selected the pin lines are visible', async () => {
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

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      true,
    )

    const pinLineLeft = renderResult.renderedDOM.getByTestId('pin-line-left')
    expect(pinLineLeft).toBeDefined()
    const pinLineTop = renderResult.renderedDOM.getByTestId('pin-line-top')
    expect(pinLineTop).toBeDefined()
    const pinLineRight = renderResult.renderedDOM.getByTestId('pin-line-right')
    expect(pinLineRight).toBeDefined()
    const pinLineBottom = renderResult.renderedDOM.getByTestId('pin-line-bottom')
    expect(pinLineBottom).toBeDefined()
  })

  it('the snap guidelines are visible when an absolute positioned element(bbb) is dragged and snaps to its sibling (ccc)', async () => {
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

    const dragDelta = windowPoint({ x: 29, y: -23 }) // 'bbb' will snap to bottom right corner of 'ccc'

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers, async () => {
      expect(renderResult.renderedDOM.getByTestId('guideline-0').style.display).toEqual('block')
      expect(renderResult.renderedDOM.getByTestId('guideline-1').style.display).toEqual('block')
    })
  })

  it('the xmarks are visible when the an absolute positioned element(bbb) is dragged and snaps to its sibling (ccc)', async () => {
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

    const targetElement = renderResult.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 29, y: -23 }) // 'bbb' will snap to bottom right corner of 'ccc'
    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers, async () => {
      expect(positioningFromCss(renderResult.renderedDOM.getByTestId('xmark-0').style)).toEqual({
        left: '67.5px',
        top: '-2.5px',
      })
      expect(positioningFromCss(renderResult.renderedDOM.getByTestId('xmark-1').style)).toEqual({
        left: '67.5px',
        top: '27.5px',
      })
      expect(positioningFromCss(renderResult.renderedDOM.getByTestId('xmark-2').style)).toEqual({
        left: '-2.5px',
        top: '27.5px',
      })
      expect(positioningFromCss(renderResult.renderedDOM.getByTestId('xmark-3').style)).toEqual({
        left: '67.5px',
        top: '147.5px',
      })
      expect(positioningFromCss(renderResult.renderedDOM.getByTestId('xmark-4').style)).toEqual({
        left: '267.5px',
        top: '27.5px',
      })
    })
  })
})

describe('Absolute Move Strategy Group-like behaviors', () => {
  it('an unstyled div wrapping two absolute positioned divs', async () => {
    function makeTestProject(dragDelta: { x: number; y: number }) {
      return makeTestProjectCodeWithSnippet(`
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='aaa'
      >
        <div data-uid='bbb' data-testid='bbb'>
          <View
            style={{
              backgroundColor: '#aaaaaa33',
              contain: 'layout',
              position: 'absolute',
              width: 80,
              height: 100,
              left: ${40 + dragDelta.x},
              top: ${50 + dragDelta.y},
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
              left: ${170 + dragDelta.x},
              top: ${70 + dragDelta.y},
            }}
            data-uid='ddd'
          />
        </div>
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
    `)
    }

    const renderResult = await renderTestEditorWithCode(
      makeTestProject({ x: 0, y: 0 }),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      true,
    )

    // we are dragging aaa/bbb, but we start the drag _over_ 'aaa/bbb/ccc', as aaa/bbb has no intrinsic size
    const targetElement = renderResult.renderedDOM.getByTestId('ccc')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(makeTestProject(dragDelta))
  })
})
