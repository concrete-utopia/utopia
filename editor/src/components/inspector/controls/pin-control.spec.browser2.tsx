/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "makeTestCase"] }] */
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import { RegisteredCanvasStrategies } from '../../canvas/canvas-strategies/canvas-strategies'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import {
  TestAppUID,
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithCode,
} from '../../canvas/ui-jsx.test-utils'
import * as EP from '../../../core/shared/element-path'
import { selectComponentsForTest, wait } from '../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import { getRectCenter } from '../../../core/shared/math-utils'
import { getDomRectCenter } from '../../../core/shared/dom-utils'

function makeTestProjectCode(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group, Rectangle } from 'utopia-api'


  export var App = (props) => {
    return (
      ${componentInnards}
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
  return formatTestProjectCode(code)
}

function makeTestProjectCodeWithoutUIDs(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group, Rectangle } from 'utopia-api'


  export var App = (props) => {
    return (
      ${componentInnards}
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
        >
          <App
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`
  return formatTestProjectCode(code)
}

interface TestCase {
  baseProject: string
  actionChange: (renderResult: EditorRenderResult) => Promise<void>
  expectedProject: string
}

describe('Group child constraints', () => {
  function makeTestCase(testCase: TestCase): () => Promise<void> {
    return async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode(testCase.baseProject),
        'await-first-dom-report',
        RegisteredCanvasStrategies,
        { 'Simplified Layout Section': true },
      )

      await testCase.actionChange(editor)
      await editor.getDispatchFollowUpActionsFinished()

      // Check the expected code.
      expect(formatTestProjectCode(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState()))).toEqual(
        makeTestProjectCodeWithoutUIDs(testCase.expectedProject),
      )
    }
  }
  describe('width constraint', () => {
    it.only(
      'toggling with left and right set',
      makeTestCase({
        baseProject: `<Group
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 300,
        left: 150,
        top: 100,
        right: 300,
        width: 500,
      }}
    >
      <div
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          height: 100,
          position: 'absolute',
          left: 150,
          top: 100,
          right: 50,
        }}
      />
    </Group>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardVariableName}/${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:group/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])
          console.log('selectedViews', renderResult.getEditorState().editor.selectedViews.length)
          const pinWidthButton = await renderResult.renderedDOM.findByTestId(
            'pin-width-control-button',
          )
          const pinWidthButtonBounds = pinWidthButton.getBoundingClientRect()
          const pinWidthButtonCenter = getDomRectCenter(pinWidthButtonBounds)
          await mouseClickAtPoint(pinWidthButton, pinWidthButtonCenter)
          console.log('bleh')
        },
        expectedProject: `<Group
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 300,
        left: 150,
        top: 100,
        right: 300,
        width: 500,
      }}
    >
      <div
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          height: 100,
          position: 'absolute',
          left: 150,
          top: 100,
          right: 50,
        }}
      />
    </Group>`,
      }),
    )
  })
})
