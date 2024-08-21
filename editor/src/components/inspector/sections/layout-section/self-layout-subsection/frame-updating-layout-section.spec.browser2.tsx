/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "makeTestCase"] }] */

import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../../core/model/scene-utils'
import type { MaybeInfinityLocalRectangle } from '../../../../../core/shared/math-utils'
import { localRectangle } from '../../../../../core/shared/math-utils'
import type { EditorRenderResult } from '../../../../canvas/ui-jsx.test-utils'
import {
  TestAppUID,
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithCode,
} from '../../../../canvas/ui-jsx.test-utils'
import * as EP from '../../../../../core/shared/element-path'
import { selectComponentsForTest } from '../../../../../utils/utils.test-utils'
import { RegisteredCanvasStrategies } from '../../../../canvas/canvas-strategies/canvas-strategies'
import { act, fireEvent } from '@testing-library/react'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
  pressKey,
} from '../../../../canvas/event-helpers.test-utils'
import { getDomRectCenter } from '../../../../../core/shared/dom-utils'
import { getFixedHugDropdownId } from '../../../fill-hug-fixed-control'

async function updateInputValue(
  renderResult: EditorRenderResult,
  controlToUpdateTestID: string,
  newValue: string,
): Promise<void> {
  const controlToUpdate = renderResult.renderedDOM.getByTestId(controlToUpdateTestID)
  const controlToUpdateBounds = controlToUpdate.getBoundingClientRect()
  await act(async () => {
    await mouseClickAtPoint(controlToUpdate, getDomRectCenter(controlToUpdateBounds))
    fireEvent.change(controlToUpdate, { target: { value: newValue } })
    fireEvent.blur(controlToUpdate)
  })
}

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
  expectedFrames: { [key: string]: MaybeInfinityLocalRectangle | null }
  expectedProject: string
  expectedFixedHugDropdownWidthValue: string
  expectedFixedHugDropdownHeightValue: string
}

describe('Frame updating layout section', () => {
  function makeTestCase(testCase: TestCase) {
    return async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCode(testCase.baseProject),
        'await-first-dom-report',
      )

      await testCase.actionChange(editor)
      await editor.getDispatchFollowUpActionsFinished()

      const widthDropdown = await editor.renderedDOM.findByTestId(
        getFixedHugDropdownId('width') + '-popuplist',
      )
      expect(widthDropdown.textContent).toEqual(testCase.expectedFixedHugDropdownWidthValue)

      const heightDropdown = await editor.renderedDOM.findByTestId(
        getFixedHugDropdownId('height') + '-popuplist',
      )
      expect(heightDropdown.textContent).toEqual(testCase.expectedFixedHugDropdownHeightValue)

      // Check the expected frames.
      const metadataMap = editor.getEditorState().editor.jsxMetadata
      for (const [path, expectedFrame] of Object.entries(testCase.expectedFrames)) {
        const metadataForElement = metadataMap[path]
        expect(metadataForElement).not.toBeNull()
        expect(metadataForElement).not.toBeUndefined()
        const actualLocalFrame = metadataForElement.localFrame
        expect(actualLocalFrame).toEqual(expectedFrame)
      }

      // Check the expected code.
      expect(formatTestProjectCode(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState()))).toEqual(
        makeTestProjectCodeWithoutUIDs(testCase.expectedProject),
      )
    }
  }

  describe('Left control', () => {
    it(
      'scrubbing the left control label',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          const scrubLabel = await renderResult.renderedDOM.findByTestId(
            `frame-left-number-input-mouse-down-handler`,
          )
          const scrubLabelBounds = scrubLabel.getBoundingClientRect()
          const scrubLabelCenter = getDomRectCenter(scrubLabelBounds)
          const scrubLabelEndPoint = { x: scrubLabelCenter.x + 100, y: scrubLabelCenter.y }
          await mouseDragFromPointToPoint(scrubLabel, scrubLabelCenter, scrubLabelEndPoint)
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 140,
              y: 100,
              width: 200,
              height: 300,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 140,
                top: 100,
                width: 200,
                height: 300,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with a single element selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the left field.
          await updateInputValue(renderResult, `frame-left-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 110,
              y: 100,
              width: 200,
              height: 300,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 110,
                top: 100,
                width: 200,
                height: 300,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with single element with percentage value selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: '25%',
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the left field.
          await updateInputValue(renderResult, `frame-left-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 110,
              y: 100,
              width: 200,
              height: 300,
            }),
        },
        expectedProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: '13.75%',
                top: 100,
                width: 200,
                height: 300,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with multiple elements selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 300,
                width: 25,
                height: 35,
              }}
              data-uid={'rectangle-2'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`,
            ),
          ])

          // Change the left field.
          await updateInputValue(renderResult, `frame-left-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 110,
              y: 100,
              width: 200,
              height: 300,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`]:
            localRectangle({
              x: 110,
              y: 300,
              width: 25,
              height: 35,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 110,
                top: 100,
                width: 200,
                height: 300,
              }}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 110,
                top: 300,
                width: 25,
                height: 35,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with nested Groups when setting value directly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 75,
                width: 100,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-left-number-input`, '100')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 100,
              y: 0,
              width: 200,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 100,
              y: 75,
              width: 100,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 50,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 50,
              y: 50,
              width: 50,
              height: 50,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 0,
                width: 200,
                height: 175,
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 100,
                  height: 75,
                }}
              />
              <Group
                style={{
                  position: 'absolute',
                  left: 100,
                  top: 75,
                  width: 100,
                  height: 100,
                }}
              >
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                  }}
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 50,
                    top: 50,
                    width: 50,
                    height: 50,
                  }}
                />
              </Group>
            </Group>
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )

    it(
      'with nested Groups that are not trued up when setting value directly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 50,
                top: 75,
                width: 50,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 25,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 25,
                  top: 50,
                  width: 25,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-left-number-input`, '100')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 100,
              y: 0,
              width: 200,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 100,
              y: 75,
              width: 100,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 50,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 50,
              y: 50,
              width: 50,
              height: 50,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 0,
                width: 200,
                height: 175,
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 100,
                  height: 75,
                }}
              />
              <Group
                style={{
                  position: 'absolute',
                  left: 100,
                  top: 75,
                  width: 100,
                  height: 100,
                }}
              >
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                  }}
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 50,
                    top: 50,
                    width: 50,
                    height: 50,
                  }}
                />
              </Group>
            </Group>
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )
  })

  describe('Top control', () => {
    it(
      'with a single element selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-top-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 110,
              width: 200,
              height: 300,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 110,
                width: 200,
                height: 300,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with a single element with percentage value selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: '25%',
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-top-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 110,
              width: 200,
              height: 300,
            }),
        },
        expectedProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: '12.22%',
                width: 200,
                height: 300,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with multiple elements selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 300,
                width: 25,
                height: 35,
              }}
              data-uid={'rectangle-2'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-top-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 110,
              width: 200,
              height: 300,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`]:
            localRectangle({
              x: 400,
              y: 110,
              width: 25,
              height: 35,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 110,
                width: 200,
                height: 300,
              }}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 110,
                width: 25,
                height: 35,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with nested Groups when setting value indirectly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              bottom: 200,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 75,
                width: 100,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-top-number-input`, '125')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 0,
              y: 125,
              width: 200,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 100,
              y: 75,
              width: 100,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 50,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 50,
              y: 50,
              width: 50,
              height: 50,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Group
              style={{
                position: 'absolute',
                left: 0,
                bottom: 100,
                width: 200,
                height: 175,
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 100,
                  height: 75,
                }}
              />
              <Group
                style={{
                  position: 'absolute',
                  left: 100,
                  top: 75,
                  width: 100,
                  height: 100,
                }}
              >
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                  }}
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 50,
                    top: 50,
                    width: 50,
                    height: 50,
                  }}
                />
              </Group>
            </Group>
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )

    it(
      'with nested Groups when setting the Top for a group child directly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 75,
                width: 100,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/outer-group-child`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-top-number-input`, '175')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 0,
              y: 75,
              width: 200,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/outer-group-child`]:
            localRectangle({
              x: 0,
              y: 100,
              width: 100,
              height: 75,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 100,
              y: 0,
              width: 100,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 50,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 50,
              y: 50,
              width: 50,
              height: 50,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Group
              style={{
                position: 'absolute',
                left: 0,
                top: 75,
                width: 200,
                height: 175,
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 100,
                  width: 100,
                  height: 75,
                }}
              />
              <Group
                style={{
                  position: 'absolute',
                  left: 100,
                  top: 0,
                  width: 100,
                  height: 100,
                }}
              >
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 50,
                    height: 50,
                  }}
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 50,
                    top: 50,
                    width: 50,
                    height: 50,
                  }}
                />
              </Group>
            </Group>
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )
  })

  describe('Width control', () => {
    it(
      'with a single element selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the width field.
          await updateInputValue(renderResult, `frame-width-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 110,
              height: 300,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 110,
                height: 300,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with single element with percentage value selected (height is 100%) when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: '25%',
                height: '100%',
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the width field.
          await updateInputValue(renderResult, `frame-width-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 110,
              height: 900,
            }),
        },
        expectedProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: '13.75%',
                height: '100%',
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Scaled',
        expectedFixedHugDropdownHeightValue: 'Fill',
      }),
    )

    it(
      'with multiple elements selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 300,
                width: 25,
                height: 35,
              }}
              data-uid={'rectangle-2'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`,
            ),
          ])

          // Change the width field.
          await updateInputValue(renderResult, `frame-width-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 110,
              height: 300,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`]:
            localRectangle({
              x: 400,
              y: 300,
              width: 110,
              height: 35,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 110,
                height: 300,
              }}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 300,
                width: 110,
                height: 35,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with nested Groups when setting value directly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 75,
                width: 100,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-width-number-input`, '400')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 400,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 200,
              y: 75,
              width: 200,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 100,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 100,
              y: 50,
              width: 100,
              height: 50,
            }),
        },
        expectedProject: `<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
      >
        <Group
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: 400,
            height: 175,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 75,
            }}
          />
          <Group
            style={{
              position: 'absolute',
              left: 200,
              top: 75,
              width: 200,
              height: 100,
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 50,
              }}
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 100,
                top: 50,
                width: 100,
                height: 50,
              }}
            />
          </Group>
        </Group>
      </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )

    it(
      'with nested Groups that are not trued up because the user messed with their code when setting value directly they also become trued up',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 50,
                top: 75,
                width: 50,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 25,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 25,
                  top: 50,
                  width: 25,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-width-number-input`, '400')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 400,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 200,
              y: 75,
              width: 200,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 100,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 100,
              y: 50,
              width: 100,
              height: 50,
            }),
        },
        expectedProject: `<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
      >
        <Group
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: 400,
            height: 175,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 75,
            }}
          />
          <Group
            style={{
              position: 'absolute',
              left: 200,
              top: 75,
              width: 200,
              height: 100,
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 50,
              }}
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 100,
                top: 50,
                width: 100,
                height: 50,
              }}
            />
          </Group>
        </Group>
      </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )

    it(
      'with nested Groups when setting the inner group width directly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 75,
                width: 100,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-width-number-input`, '200')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 300,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 100,
              y: 75,
              width: 200,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 100,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 100,
              y: 50,
              width: 100,
              height: 50,
            }),
        },
        expectedProject: `<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
      >
        <Group
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: 300,
            height: 175,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 100,
              height: 75,
            }}
          />
          <Group
            style={{
              position: 'absolute',
              left: 100,
              top: 75,
              width: 200,
              height: 100,
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 50,
              }}
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 100,
                top: 50,
                width: 100,
                height: 50,
              }}
            />
          </Group>
        </Group>
      </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )

    it(
      'with nested Groups, the outer Group has left-right props, no width prop. setting value indirectly',
      makeTestCase({
        baseProject: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid={'root-div'}
        >
          <Group
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              right: 200,
              height: 175,
            }}
            data-uid='outer-group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 75,
              }}
              data-uid='outer-group-child'
            />
            <Group
              style={{
                position: 'absolute',
                left: 100,
                top: 75,
                width: 100,
                height: 100,
              }}
              data-uid='inner-group'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 50,
                  height: 50,
                }}
                data-uid='inner-child-2'
              />
            </Group>
          </Group>
        </div>`,
        actionChange: async (renderResult) => {
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`,
            ),
          ])

          // Change the top field.
          await updateInputValue(renderResult, `frame-width-number-input`, '400')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 400,
              height: 175,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group`]:
            localRectangle({
              x: 200,
              y: 75,
              width: 200,
              height: 100,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-1`]:
            localRectangle({
              x: 0,
              y: 0,
              width: 100,
              height: 50,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/outer-group/inner-group/inner-child-2`]:
            localRectangle({
              x: 100,
              y: 50,
              width: 100,
              height: 50,
            }),
        },
        expectedProject: `<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
      >
        <Group
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            right: 0,
            height: 175,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 75,
            }}
          />
          <Group
            style={{
              position: 'absolute',
              left: 200,
              top: 75,
              width: 200,
              height: 100,
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 100,
                height: 50,
              }}
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 100,
                top: 50,
                width: 100,
                height: 50,
              }}
            />
          </Group>
        </Group>
      </div>`,
        expectedFixedHugDropdownWidthValue: 'Hug',
        expectedFixedHugDropdownHeightValue: 'Hug',
      }),
    )
  })

  describe('Height control', () => {
    it(
      'with single element selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the height field.
          await updateInputValue(renderResult, `frame-height-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 200,
              height: 110,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 110,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )

    it(
      'with single element with percentage value selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: '25%',
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the height field.
          await updateInputValue(renderResult, `frame-height-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 200,
              height: 110,
            }),
        },
        expectedProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: '12.22%',
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Scaled',
      }),
    )

    it(
      'with single element with percentage value selected (width is 100%) when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: '100%',
                height: '25%',
              }}
              data-uid={'rectangle-1'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
          ])

          // Change the height field.
          await updateInputValue(renderResult, `frame-height-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 800,
              height: 110,
            }),
        },
        expectedProject: `<div
            style={{
              position: 'absolute',
              height: 900,
              width: 800,
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: '100%',
                height: '12.22%',
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fill',
        expectedFixedHugDropdownHeightValue: 'Scaled',
      }),
    )

    it(
      'with multiple elements selected when setting value directly',
      makeTestCase({
        baseProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid={'root-div'}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 300,
              }}
              data-uid={'rectangle-1'}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 300,
                width: 25,
                height: 35,
              }}
              data-uid={'rectangle-2'}
            />
          </div>`,
        actionChange: async (renderResult) => {
          // Select the rectangle.
          await selectComponentsForTest(renderResult, [
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`,
            ),
            EP.fromString(
              `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`,
            ),
          ])

          // Change the height field.
          await updateInputValue(renderResult, `frame-height-number-input`, '110')
        },
        expectedFrames: {
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-1`]:
            localRectangle({
              x: 90,
              y: 100,
              width: 200,
              height: 110,
            }),
          [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/rectangle-2`]:
            localRectangle({
              x: 400,
              y: 300,
              width: 25,
              height: 110,
            }),
        },
        expectedProject: `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 90,
                top: 100,
                width: 200,
                height: 110,
              }}
            />
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 400,
                top: 300,
                width: 25,
                height: 110,
              }}
            />
          </div>`,
        expectedFixedHugDropdownWidthValue: 'Fixed',
        expectedFixedHugDropdownHeightValue: 'Fixed',
      }),
    )
  })
})
