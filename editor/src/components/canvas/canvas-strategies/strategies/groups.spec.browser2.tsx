/* eslint jest/expect-expect: ["warn", { "assertFunctionNames": ["expect", "assertStylePropsSet"] }] */
import { getSimpleAttributeAtPath } from '../../../../core/model/element-metadata-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import { getDomRectCenter } from '../../../../core/shared/dom-utils'
import { forceRight } from '../../../../core/shared/either'
import { right } from '../../../../core/shared/either'
import { foldEither } from '../../../../core/shared/either'
import { fromString } from '../../../../core/shared/element-path'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { create } from '../../../../core/shared/property-path'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers } from '../../../../utils/modifiers'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { EdgePositionBottomRight, EdgePositionTopLeft } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { formatTestProjectCode } from '../../ui-jsx.test-utils'
import { TestAppUID } from '../../ui-jsx.test-utils'
import { TestSceneUID } from '../../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { resizeElement } from './absolute-resize.test-utils'
import { changeInspectorNumberControl } from '../../../inspector/common/inspector.test-utils'
import { ParentOutlinesTestIdSuffix } from '../../controls/parent-outlines'
import { ParentBoundsTestIdSuffix } from '../../controls/parent-bounds'

const GroupPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group`

function makeCodeSnippetForGroups(code: string) {
  return `
  <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
    ${code}
  </div>
`
}

function makeGroupTestProjectCode(componentInnards: string): string {
  const code = `
  import * as React from 'react'
  import { Scene, Storyboard, View, Group } from 'utopia-api'

  export const DoesNotHonourPositionSize = (props) => {
    return <div 
      style={{
        backgroundColor: 'red',
        position: 'absolute',
        top: 0,
        left: 0,
        width: 100,
        height: 100,
      }}
      data-testid='not-honour-position-size'
      data-uid='not-honour-position-size'
    />
  }

  export const DoesNotHonourPosition = (props) => {
    return <div 
      style={{
        backgroundColor: 'red',
        position: 'absolute',
        top: 0,
        left: 0,
        width: props.style.width,
        height: props.style.height,
      }}
      data-testid='not-honour-position'
      data-uid='not-honour-position'
    />
  }
  
  export const DoesNotHonourSize = (props) => {
    return <div 
      style={{
        backgroundColor: 'red',
        position: 'absolute',
        top: props.style.top,
        left: props.style.left,
        width: 100,
        height: 100,
      }}
      data-testid='not-honour-size'
      data-uid='not-honour-size'
    />
  }

  export const HonoursPositionSize = (props) => {
    return <div 
      style={{
        backgroundColor: 'red',
        position: 'absolute',
        top: props.style.top,
        left: props.style.left,
        width: props.style.width,
        height: props.style.height,
      }}
      data-testid='honours-position-size'
      data-uid='honours-position-size'
    />
  }

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

async function renderProjectWithGroup(code: string) {
  const editor = await renderTestEditorWithCode(
    makeGroupTestProjectCode(makeCodeSnippetForGroups(code)),
    'await-first-dom-report',
  )

  return editor
}

async function dragByPixels(
  editor: EditorRenderResult,
  delta: { x: number; y: number },
  testid: string,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
    staggerMoveEvents?: boolean
    midDragCallback?: () => Promise<void>
    skipMouseUp?: boolean
  } = {},
) {
  const targetElement = editor.renderedDOM.getByTestId(testid)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const targetElementCenter = getDomRectCenter(targetElementBounds)
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

  await mouseDragFromPointWithDelta(canvasControlsLayer, targetElementCenter, delta, options)
}

function assertStylePropsSet(
  editor: EditorRenderResult,
  elementPathStr: string,
  stylePropsObjToMatch: Record<string, any>,
) {
  const foundElement = forceNotNull(
    `couldn't find element ${elementPathStr}`,
    foldEither(
      () => null,
      (element) => (element.type === 'JSX_ELEMENT' ? element : null),
      editor.getEditorState().editor.jsxMetadata[elementPathStr].element,
    ),
  )

  for (const key in stylePropsObjToMatch) {
    const expectedValue = stylePropsObjToMatch[key]
    const actualValue = forceRight(
      getSimpleAttributeAtPath(right(foundElement.props), create('style', key)),
      `couldn't find style prop ${key}`,
    )
    if (actualValue !== expectedValue) {
      throw new Error(
        `assertStylePropsSet equality

props.style.${key} on element ${elementPathStr}:

Expected: ${JSON.stringify(expectedValue)}
Received: ${JSON.stringify(actualValue)}`,
      )
    }
    expect(actualValue).toEqual(expectedValue)
  }
}

function checkThatParentOutlinesAndBoundsNotPresent(
  editorRenderResult: EditorRenderResult,
): () => Promise<void> {
  return async () => {
    // Check that there are no parent outlines.
    const outlines = editorRenderResult.renderedDOM.queryAllByTestId((content) => {
      return content.endsWith(ParentOutlinesTestIdSuffix)
    })
    expect(outlines).toHaveLength(0)
    // Check that there are no parent bounds.
    const bounds = editorRenderResult.renderedDOM.queryAllByTestId((content) => {
      return content.endsWith(ParentBoundsTestIdSuffix)
    })
    expect(bounds).toHaveLength(0)
  }
}

describe('Groups behaviors', () => {
  describe('Absolute Positioned Groups', () => {
    describe('Various Group Configurations', () => {
      it('single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')
      })

      it('multiple children with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('nothing wrong with this group!', async () => {
        const editor = await renderProjectWithGroup(`
          <Group
            data-testid='group'
            style={{
              backgroundColor: '#d3d3d3',
              contain: 'layout',
            }}
          >
            <View
              style={{
                backgroundColor: '#E80000',
                position: 'absolute',
                left: 0.5,
                top: 0,
                width: 50,
                height: 50,
              }}
              data-uid='dea'
            />
            <View
              style={{
                backgroundColor: '#FF0000',
                position: 'absolute',
                left: 248,
                top: 167,
                width: 150,
                height: 50,
              }}
              data-uid='c0f'
            />
            <View
              style={{
                backgroundColor: '#FF0000',
                position: 'absolute',
                left: 150,
                top: 12,
                width: 150,
                height: 50,
              }}
              data-uid='909'
            />
            <View
              style={{
                backgroundColor: '#FF0000',
                position: 'absolute',
                left: 0,
                top: 210,
                width: 150,
                height: 50,
              }}
              data-uid='e2f'
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('398px')
        expect(groupDiv.style.height).toBe('260px')
      })

      it('group pinned right,bottom with multiple children with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', right: 50, bottom: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('single child with OFFSET top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('150px')
        expect(groupDiv.style.height).toBe('150px')
      })

      it('children with bottom,right,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                bottom: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 100,
                bottom: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('child with top,left,bottom,right pins, no width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div
              data-testid='child'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                right: 100,
                bottom: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')
        expect(groupDiv.style.width).toBe('150px')
        expect(groupDiv.style.height).toBe('150px')

        const childDiv = editor.renderedDOM.getByTestId('child')
        // notice that the child ends up with zero width and height because it was set to auto
        expect(childDiv.getBoundingClientRect().width).toBe(0)
        expect(childDiv.getBoundingClientRect().height).toBe(0)
      })

      it('child with top,left,bottom,right, width, height (!!!!!!) pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                right: 100,
                bottom: 100,
                width: 50,
                height: 50,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('children with nested Fragments', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <>
              <div 
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 100,
                  left: 100,
                  width: 100,
                  height: 100,
                }}
              />
            </>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })
      it('children with nested Groups', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
            }}
          />
          <Group style={{position: 'absolute', left: 100, top: 100}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })
      it('nested group with bottom,right,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              bottom: 100,
              right: 100,
              width: 100,
              height: 100,
            }}
          />
          <Group style={{position: 'absolute', bottom: 0, right: 0}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('nested group with top,left,right,bottom pins', async () => {
        const editor = await renderProjectWithGroup(`
            <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
              <div 
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
              <Group 
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 100,
                  left: 100,
                  right: 50,
                  bottom: 50,
                }}
              >
                <div 
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    top: 0,
                    left: 0,
                    width: 100,
                    height: 100,
                  }}
                />
              </Group>
            </Group>
          `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('250px')
        expect(groupDiv.style.height).toBe('250px')
      })

      it('IGNORED: Any percentage pins are treated as zero', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: '50%',
                height: '50%',
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')
      })

      it('IGNORED: group with static child', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                // position: 'absolute', // this is static!
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')
      })
    })

    describe('Group Resize', () => {
      it('group with width/height prop, single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('group with no size prop, single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('nested sized groups with single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <Group data-uid='group-inner' data-testid='group-inner' style={{position: 'absolute', width: 100, height: 100, left: 0, top: 0}}>
              <div 
                data-uid='child-1'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('nested unsized groups with single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <Group data-uid='group-inner' data-testid='group-inner' style={{position: 'absolute', left: 0, top: 0}}>
              <div 
                data-uid='child-1'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('multiple children with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: 125,
            top: 125,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: 150,
            top: 150,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('group pinned right,bottom with multiple children with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', right: 50, bottom: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: undefined,
            top: undefined,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: 125,
            top: 125,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: undefined,
            top: undefined,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: 150,
            top: 150,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('single child with OFFSET top,left,width,height results in a trued up group', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('150px')
        expect(groupDiv.style.height).toBe('150px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('133px')
          expect(groupDiv.style.height).toBe('133px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 117,
            top: 117,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 133,
            height: 133,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('183px')
          expect(groupDiv.style.height).toBe('183px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 67,
            top: 67,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 183,
            height: 183,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('sized group with children with bottom,right,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 200, height: 200, left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                bottom: 0,
                width: 100,
                height: 100,
              }}
            />
            <div
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 100,
                bottom: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 100 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 250,
            height: 300,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: undefined,
            top: undefined,
            width: 125,
            height: 150,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: undefined,
            top: undefined,
            width: 125,
            height: 150,
            right: 125,
            bottom: 150,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('350px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 300,
            height: 350,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: undefined,
            top: undefined,
            width: 150,
            height: 175,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: undefined,
            top: undefined,
            width: 150,
            height: 175,
            right: 150,
            bottom: 175,
          })
        }
      })

      it('Zero-sized child with top,left,bottom,right pins, no width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div
              data-testid='child-1'
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                right: 100,
                bottom: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')
        expect(groupDiv.style.width).toBe('150px')
        expect(groupDiv.style.height).toBe('150px')

        const childDiv = editor.renderedDOM.getByTestId('child-1')
        // notice that the child ends up with zero width and height because it was set to auto
        expect(childDiv.getBoundingClientRect().width).toBe(0)
        expect(childDiv.getBoundingClientRect().height).toBe(0)

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('0px')
          expect(groupDiv.style.height).toBe('0px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 117,
            top: 117,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
        }

        // resizing top left
        {
          // now that our Group is 0x0 sized, resizing can't fix it, just offset it. I'm leaving this test as a documentation of this behavior
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('0px')
          expect(groupDiv.style.height).toBe('0px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 67,
            top: 67,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
        }
      })

      it('child with top,left,bottom,right, width, height (!!!!!!) pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                right: 100,
                bottom: 100,
                width: 50,
                height: 50,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 100, y: 150 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('75px')
          expect(groupDiv.style.height).toBe('88px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 125,
            top: 138,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
            width: 75,
            height: 88,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('125px')
          expect(groupDiv.style.height).toBe('138px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 75,
            top: 88,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
            width: 125,
            height: 138,
          })
        }
      })

      it('children with nested Fragments', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <React.Fragment data-uid='fragment'>
              <React.Fragment data-uid='fragment-2'>
                <div 
                  data-uid='child-2'
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    top: 100,
                    left: 100,
                    width: 100,
                    height: 100,
                  }}
                />
              </React.Fragment>
            </React.Fragment>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/fragment/fragment-2/child-2`, {
            left: 125,
            top: 125,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/fragment/fragment-2/child-2`, {
            left: 150,
            top: 150,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('children with nested Groups', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            data-uid='child-1'
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
            }}
          />
          <Group data-uid='inner-group' style={{position: 'absolute', left: 100, top: 100}}>
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: 125,
            top: 125,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: 150,
            top: 150,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('nested group with bottom,right,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            data-uid='child-1'
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              bottom: 100,
              right: 100,
              width: 100,
              height: 100,
            }}
          />
          <Group data-uid='inner-group' style={{position: 'absolute', bottom: 0, right: 0}}>
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: undefined,
            top: undefined,
            width: 125,
            height: 125,
            right: 125,
            bottom: 125,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: undefined,
            top: undefined,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 125,
            height: 125,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: undefined,
            top: undefined,
            width: 150,
            height: 150,
            right: 150,
            bottom: 150,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: undefined,
            top: undefined,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('nested group with top,left,right,bottom pins', async () => {
        const editor = await renderProjectWithGroup(`
            <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
              <div 
                data-uid='child-1'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
              <Group 
                data-uid='inner-group'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 100,
                  left: 100,
                  right: 50,
                  bottom: 50,
                }}
              >
                <div 
                  data-uid='child-2'
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    top: 0,
                    left: 0,
                    width: 100,
                    height: 100,
                  }}
                />
              </Group>
            </Group>
          `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('250px')
        expect(groupDiv.style.height).toBe('250px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 100 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('240px')
          expect(groupDiv.style.height).toBe('280px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 120,
            height: 140,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: 120,
            top: 140,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 120,
            height: 140,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('290px')
          expect(groupDiv.style.height).toBe('330px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 145,
            height: 165,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: 145,
            top: 165,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 145,
            height: 165,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('Sized group with a percentage pinned child', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 200, height: 250, left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: '50%',
                height: '60%',
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('250px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 100, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('300px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 300,
            height: 300,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 120,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: 150,
            top: 120,
            width: '50%',
            height: '60%',
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('350px')
          expect(groupDiv.style.height).toBe('350px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 350,
            height: 350,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 175,
            height: 140,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-2`, {
            left: 175,
            top: 140,
            width: '50%',
            height: '60%',
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('IGNORED: group with static child', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                // position: 'absolute', // this is static!
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: undefined,
            left: 0,
            top: 0,
            width: 100,
            height: 100,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: undefined,
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('IGNORED: group with child that does not honour position or size props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <DoesNotHonourPositionSize 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('IGNORED: group with child that does not honour position props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <DoesNotHonourPosition
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }
      })
      it('IGNORED: group with child that does not honour size props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <DoesNotHonourSize 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }
      })
      it('group with child that does honour position and size props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <HonoursPositionSize
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers)

          expect(groupDiv.style.width).toBe('150px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 150,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            position: 'absolute',
            left: 0,
            top: 0,
            width: 200,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
        }
      })
    })

    describe('Moving one Child', () => {
      it('IGNORED: moving a child within a group that does not honour position or size props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <DoesNotHonourPositionSize 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'not-honour-position-size', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 100,
          height: 100,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
      })

      it('IGNORED: moving a child inside the group that does not honour position props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <DoesNotHonourPosition
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'not-honour-position', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 100,
          height: 100,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
      })
      it('IGNORED: moving a child inside the group that does not honour size props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <DoesNotHonourSize 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'not-honour-size', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 100,
          height: 100,
          right: undefined,
          bottom: undefined,
        })

        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 100,
          top: 100,
          width: 100,
          height: 100,
        })
      })
      it('moving a child inside the group that does honour position and size props', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <HonoursPositionSize
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'honours-position-size', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 200,
          height: 200,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 100,
          top: 100,
          width: 100,
          height: 100,
        })
      })

      it('moving a child inside the group move the child and offsets the group and all group children so they stay within the bounds', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-2`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'child-2', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 0,
          right: 100,
          width: 25,
          height: 25,
        })
      })

      it('moving a child inside a right,bottom pinned group move the child and offsets the group and all group children so they stay within the bounds', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', right: 150, bottom: 150}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-2`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'child-2', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          right: 50,
          bottom: 50,
          width: 300,
          height: 300,
          left: undefined,
          top: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 0,
          right: 100,
          width: 25,
          height: 25,
        })
      })

      it('moving a child inside a group without pins move the child and offsets the group and all group children so they stay within the bounds', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{ position: 'absolute' }}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-2`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'child-2', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          width: 300,
          height: 300,
          left: undefined,
          top: undefined,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 0,
          right: 100,
          width: 25,
          height: 25,
        })
      })

      it('moving a child to expand top-left', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 150, top: 150}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: -100, y: -100 }, 'child-1', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 100,
          right: 0,
          width: 25,
          height: 25,
        })
      })

      it('moving a child in a right,bottom group to expand top-left', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', right: 50, bottom: 50}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: -100, y: -100 }, 'child-1', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          right: 50,
          bottom: 50,
          width: 300,
          height: 300,
          left: undefined,
          top: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 100,
          right: 0,
          width: 25,
          height: 25,
        })
      })

      it('moving a top-right pinned child to expand top-right', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 25, top: 150}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-3`)])

        await dragByPixels(editor, { x: 100, y: -100 }, 'child-3', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 25,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 100,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 150,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 0,
          right: 0,
          width: 25,
          height: 25,
        })
      })

      it('group with width/height prop gets updated in the editor', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group'
            data-testid='group' 
            style={{
              position: 'absolute', 
              left: 50,
              top: 50,
              width: 200,
              height: 200,
            }}
          >
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-2`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'child-2', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 0,
          right: 100,
          width: 25,
          height: 25,
        })
      })

      it('children offset behavior works with nested Fragments', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 150, top: 150}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <React.Fragment data-uid='fragment'>
              <div 
                data-uid='child-2'
                data-testid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 150,
                  left: 150,
                  width: 50,
                  height: 50,
                }}
              />
              <div 
                data-uid='child-3'
                data-testid='child-3'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  right: 0,
                  top: 0,
                  width: 25,
                  height: 25,
                }}
              />
              </React.Fragment>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: -100, y: -100 }, 'child-1', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/fragment/child-2`, {
          left: 250,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/fragment/child-3`, {
          top: 100,
          right: 0,
          width: 25,
          height: 25,
        })
      })

      it('Group with Fragments, moving the Fragment child works', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 150, top: 150}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <React.Fragment data-uid='fragment'>
              <React.Fragment data-uid='fragment-2'>
                <div 
                  data-uid='child-2'
                  data-testid='child-2'
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    top: 150,
                    left: 150,
                    width: 50,
                    height: 50,
                  }}
                />
                <div 
                  data-uid='child-3'
                  data-testid='child-3'
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    right: 0,
                    top: 0,
                    width: 25,
                    height: 25,
                  }}
                />
                </React.Fragment>
              </React.Fragment>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [
          fromString(`${GroupPath}/fragment/fragment-2/child-2`),
        ])

        await dragByPixels(editor, { x: 25, y: 50 }, 'child-2')

        expect(groupDiv.style.width).toBe('225px')
        expect(groupDiv.style.height).toBe('250px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 150,
          top: 150,
          width: 225, // this is important that we are adding width / height to the Group here
          height: 250,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/fragment/fragment-2/child-2`, {
          left: 175,
          top: 200,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/fragment/fragment-2/child-3`, {
          top: 0,
          right: 25,
          width: 25,
          height: 25,
        })
      })

      it('works with nested Groups', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' data-testid='inner-group' style={{position: 'absolute', left: 150, top: 0}}>
              <div 
                data-uid='child-2'
                data-testid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 150,
                  left: 0,
                  width: 50,
                  height: 50,
                }}
              />
              <div 
                data-uid='child-3'
                data-testid='child-3'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  right: 0,
                  top: 0,
                  width: 25,
                  height: 25,
                }}
              />
              </Group>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        const innerGroupDiv = editor.renderedDOM.getByTestId('inner-group')

        expect(innerGroupDiv.style.width).toBe('50px')
        expect(innerGroupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/inner-group/child-2`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'child-2', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        expect(innerGroupDiv.style.width).toBe('125px')
        expect(innerGroupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
          left: 175,
          top: 0,
          width: 125,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
          left: 75,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/inner-group/child-3`, {
          top: 0,
          right: 100,
          width: 25,
          height: 25,
        })
      })

      it('works with nested Groups that have explicit size', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50, width: 200, height: 200}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' data-testid='inner-group' style={{position: 'absolute', left: 150, top: 0, width: 50, height: 200}}>
              <div 
                data-uid='child-2'
                data-testid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 150,
                  left: 0,
                  width: 50,
                  height: 50,
                }}
              />
              <div 
                data-uid='child-3'
                data-testid='child-3'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  right: 0,
                  top: 0,
                  width: 25,
                  height: 25,
                }}
              />
              </Group>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        const innerGroupDiv = editor.renderedDOM.getByTestId('inner-group')

        expect(innerGroupDiv.style.width).toBe('50px')
        expect(innerGroupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/inner-group/child-2`)])

        await dragByPixels(editor, { x: 100, y: 100 }, 'child-2', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')

        expect(innerGroupDiv.style.width).toBe('125px')
        expect(innerGroupDiv.style.height).toBe('300px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 50,
          top: 50,
          width: 300,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
          left: 175,
          top: 0,
          width: 125,
          height: 300,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
          left: 75,
          top: 250,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/inner-group/child-3`, {
          top: 0,
          right: 100,
          width: 25,
          height: 25,
        })
      })

      it('an accidental static child disables all Group-like features', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' 
            style={{
              position: 'absolute', 
              left: 150, 
              top: 150, 
              width: 200, 
              height: 200
            }}
          >
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
            <div 
              data-uid='static-child'
              data-testid='static-child'
              style={{
                backgroundColor: 'red',
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-1`)])

        await dragByPixels(editor, { x: -45, y: -45 }, 'child-1', {
          midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
        })

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        assertStylePropsSet(editor, `${GroupPath}`, {
          left: 150,
          top: 150,
          width: 200,
          height: 200,
          right: undefined,
          bottom: undefined,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-1`, {
          left: -45,
          top: -45,
          width: 100,
          height: 100,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-2`, {
          left: 150,
          top: 150,
          width: 50,
          height: 50,
        })
        assertStylePropsSet(editor, `${GroupPath}/child-3`, {
          top: 0,
          right: 0,
          width: 25,
          height: 25,
        })
      })

      it('No Snapping to Parent Group: a single child of group does not snap to the group starting bounds', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <Group data-uid='group-inner' data-testid='group-inner' style={{position: 'absolute', width: 100, height: 100, left: 0, top: 0}}>
              <div 
                data-uid='child-1'
                data-testid='child-1'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        {
          await selectComponentsForTest(editor, [fromString(`${GroupPath}/group-inner/child-1`)])

          // drag towards the top left, but keep mouse down
          await dragByPixels(editor, { x: -15, y: -15 }, 'child-1', {
            midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
            skipMouseUp: true,
          })

          // drag back towards bottom right, but one pixel short
          await dragByPixels(editor, { x: 14, y: 14 }, 'child-1', {
            midDragCallback: checkThatParentOutlinesAndBoundsNotPresent(editor),
          })

          // prove that no snapping has been applied and the element is one pixel to the top and left

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('100px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 49,
            top: 49,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }
      })
    })
  })
})

describe('Inspector Pins section for group child', () => {
  it('changing the Left pin of a group child trues up the group', async () => {
    const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 100, top: 100, width: 200, height: 200}}>
            <div
              data-uid='child-1'
              data-testid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              data-uid='child-2'
              data-testid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 150,
                left: 150,
                width: 50,
                height: 50,
              }}
            />
            <div 
              data-uid='child-3'
              data-testid='child-3'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                top: 0,
                width: 25,
                height: 25,
              }}
            />
          </Group>
        `)
    const groupDiv = editor.renderedDOM.getByTestId('group')

    expect(groupDiv.style.width).toBe('200px')
    expect(groupDiv.style.height).toBe('200px')

    await selectComponentsForTest(editor, [fromString(`${GroupPath}/child-2`)])

    await changeInspectorNumberControl(editor, 'position-top-number-input', '250')

    expect(groupDiv.style.width).toBe('200px')
    expect(groupDiv.style.height).toBe('300px')

    assertStylePropsSet(editor, `${GroupPath}`, {
      top: 100,
      left: 100,
      width: 200,
      height: 300,
      right: undefined,
      bottom: undefined,
    })
    assertStylePropsSet(editor, `${GroupPath}/child-1`, {
      left: 0,
      top: 0,
      width: 100,
      height: 100,
    })
    assertStylePropsSet(editor, `${GroupPath}/child-2`, {
      left: 150,
      top: 250,
      width: 50,
      height: 50,
    })
    assertStylePropsSet(editor, `${GroupPath}/child-3`, {
      top: 0,
      right: 0,
      width: 25,
      height: 25,
    })
  })
})
