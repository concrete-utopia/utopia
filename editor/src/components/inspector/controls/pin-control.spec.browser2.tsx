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
import {
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import { getDomRectCenter } from '../../../core/shared/dom-utils'
import type { Modifiers } from '../../../utils/modifiers'
import { cmdModifier } from '../../../utils/modifiers'

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

// Re-enable these tests when reactivating pin toggling for the new world
xdescribe('Group child constraints', () => {
  function makeTestCase(testCase: TestCase): () => Promise<void> {
    return async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode(testCase.baseProject),
        'await-first-dom-report',
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
    it(
      'toggling with left and right set',
      makeTestCase({
        baseProject: `<Group
      data-uid={'group'}
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 300,
        left: 150,
        top: 100,
        width: 500,
      }}
    >
      <div
        data-uid={'target-div'}
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          height: 100,
          left: 150,
          top: 100,
          right: 50,
        }}
      />
    </Group>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:group/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          const pinWidthButton = await renderResult.renderedDOM.findByTestId(
            'pin-width-control-button',
          )
          const pinWidthButtonBounds = pinWidthButton.getBoundingClientRect()
          const pinWidthButtonCenter = getDomRectCenter(pinWidthButtonBounds)
          await mouseClickAtPoint(pinWidthButton, pinWidthButtonCenter)
        },
        expectedProject: `<Group
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 100,
        left: 300,
        top: 200,
        width: 300,
      }}
    >
      <div
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          height: 100,
          left: 0,
          top: 0,
          right: 0,
        }}
        data-constraints={['width']}
      />
    </Group>`,
      }),
    )
    it(
      'toggling with left and right set with data-constraints already set',
      makeTestCase({
        baseProject: `<Group
      data-uid={'group'}
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 100,
        left: 300,
        top: 200,
        width: 300,
      }}
    >
      <div
        data-uid={'target-div'}
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          height: 100,
          left: 0,
          top: 0,
          right: 0,
        }}
        data-constraints={['width']}
      />
    </Group>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:group/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          const pinWidthButton = await renderResult.renderedDOM.findByTestId(
            'pin-width-control-button',
          )
          const pinWidthButtonBounds = pinWidthButton.getBoundingClientRect()
          const pinWidthButtonCenter = getDomRectCenter(pinWidthButtonBounds)
          await mouseClickAtPoint(pinWidthButton, pinWidthButtonCenter)
        },
        expectedProject: `<Group
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 100,
        left: 300,
        top: 200,
        width: 300,
      }}
    >
      <div
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          height: 100,
          left: 0,
          top: 0,
          right: 0,
        }}
      />
    </Group>`,
      }),
    )
  })
  describe('height constraint', () => {
    it(
      'toggling with top and bottom set',
      makeTestCase({
        baseProject: `<Group
      data-uid={'group'}
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 300,
        left: 150,
        top: 100,
        width: 500,
      }}
    >
      <div
        data-uid={'target-div'}
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          left: 100,
          width: 150,
          top: 100,
          bottom: 50,
        }}
      />
    </Group>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:group/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          const pinHeightButton = await renderResult.renderedDOM.findByTestId(
            'pin-height-control-button',
          )
          const pinHeightButtonBounds = pinHeightButton.getBoundingClientRect()
          const pinHeightButtonCenter = getDomRectCenter(pinHeightButtonBounds)
          await mouseClickAtPoint(pinHeightButton, pinHeightButtonCenter)
        },
        expectedProject: `<Group
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 150,
        left: 250,
        top: 200,
        width: 150,
      }}
    >
      <div
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          left: 0,
          width: 150,
          top: 0,
          bottom: 0,
        }}
        data-constraints={['height']}
      />
    </Group>`,
      }),
    )
    it(
      'toggling with top and bottom set with data-constraints already set',
      makeTestCase({
        baseProject: `<Group
      data-uid={'group'}
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 150,
        left: 250,
        top: 200,
        width: 150,
      }}
    >
      <div
        data-uid={'target-div'}
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          left: 0,
          width: 150,
          top: 0,
          bottom: 0,
        }}
        data-constraints={['height']}
      />
    </Group>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:group/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          const pinHeightButton = await renderResult.renderedDOM.findByTestId(
            'pin-height-control-button',
          )
          const pinHeightButtonBounds = pinHeightButton.getBoundingClientRect()
          const pinHeightButtonCenter = getDomRectCenter(pinHeightButtonBounds)
          await mouseClickAtPoint(pinHeightButton, pinHeightButtonCenter)
        },
        expectedProject: `<Group
      style={{
        backgroundColor: 'lightblue',
        position: 'absolute',
        height: 150,
        left: 250,
        top: 200,
        width: 150,
      }}
    >
      <div
        data-testid='target-div'
        style={{
          backgroundColor: 'lightgrey',
          position: 'absolute',
          left: 0,
          width: 150,
          top: 0,
          bottom: 0,
        }}
      />
    </Group>`,
      }),
    )
  })
})

describe('Frame child constraints', () => {
  function makeTestCase(testCase: TestCase): () => Promise<void> {
    return async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode(testCase.baseProject),
        'await-first-dom-report',
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
    it(
      'If width is not active, we interpret it as Left + Width',
      makeTestCase({
        baseProject: `<div
          data-uid={'root'}
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-uid={'target-div'}
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              height: 100,
              left: 150,
              top: 100,
              right: 50,
            }}
          />
        </div>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          await clickOnPin(renderResult, 'pin-width-control-button')
        },
        expectedProject: `<div
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              height: 100,
              top: 100,
              left: 150,
              width: 300,
            }}
          />
        </div>`,
      }),
    )

    it(
      'If width is already active, clicking on it is a no-op',
      makeTestCase({
        baseProject: `<div
          data-uid={'root'}
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-uid={'target-div'}
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              right: 150,
              top: 100,
              width: 300,
              height: 100,
            }}
          />
        </div>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          await clickOnPin(renderResult, 'pin-width-control-button')
        },
        expectedProject: `<div
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              right: 150,
              top: 100,
              width: 300,
              height: 100,
            }}
          />
        </div>`,
      }),
    )
  })

  describe('bottom pin', () => {
    it(
      'clicking bottom pin with top and height set sets Bottom and Height',
      makeTestCase({
        baseProject: `<div
          data-uid={'root'}
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-uid={'target-div'}
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              left: 150,
              top: 100,
              height: 100,
              width: 500,
            }}
          />
        </div>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          await clickOnPin(renderResult, 'pin-control-catcher-pin-bottom')
        },
        expectedProject: `<div
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              left: 150,
              width: 500,
              bottom: 100,
              height: 100,
            }}
          />
        </div>`,
      }),
    )

    it(
      'clicking bottom pin bottom and height set is a no-op',
      makeTestCase({
        baseProject: `<div
          data-uid={'root'}
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-uid={'target-div'}
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              left: 150,
              width: 500,
              bottom: 100,
              height: 100,
            }}
          />
        </div>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          await clickOnPin(renderResult, 'pin-control-catcher-pin-bottom')
        },
        expectedProject: `<div
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              left: 150,
              width: 500,
              bottom: 100,
              height: 100,
            }}
          />
        </div>`,
      }),
    )

    it(
      'cmd + clicking bottom pin with top and height set sets Top and Bottom',
      makeTestCase({
        baseProject: `<div
          data-uid={'root'}
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-uid={'target-div'}
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              left: 150,
              top: 100,
              height: 100,
              width: 500,
            }}
          />
        </div>`,
        actionChange: async function (renderResult: EditorRenderResult): Promise<void> {
          const targetPath = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/target-div`,
          )
          await selectComponentsForTest(renderResult, [targetPath])

          await clickOnPin(renderResult, 'pin-control-catcher-pin-bottom', {
            modifiers: cmdModifier,
          })
        },
        expectedProject: `<div
          style={{
            backgroundColor: 'lightblue',
            position: 'absolute',
            height: 300,
            left: 150,
            top: 100,
            width: 500,
          }}
        >
          <div
            data-testid='target-div'
            style={{
              backgroundColor: 'lightgrey',
              position: 'absolute',
              left: 150,
              width: 500,
              top: 100,
              bottom: 100,
            }}
          />
        </div>`,
      }),
    )
  })
})
async function clickOnPin(
  renderResult: EditorRenderResult,
  pinTestId: string,
  options: {
    modifiers?: Modifiers
  } = {},
) {
  const pinWidthButton = await renderResult.renderedDOM.findByTestId(pinTestId)
  const pinWidthButtonBounds = pinWidthButton.getBoundingClientRect()
  const pinWidthButtonCenter = getDomRectCenter(pinWidthButtonBounds)
  await mouseClickAtPoint(pinWidthButton, pinWidthButtonCenter, options)
}
