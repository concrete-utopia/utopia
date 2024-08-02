/* eslint jest/expect-expect: ["warn", { "assertFunctionNames": ["expect", "assertStylePropsSet"] }] */
import { screen } from '@testing-library/react'
import { getSimpleAttributeAtPath } from '../../../../core/model/element-metadata-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import { safeIndex } from '../../../../core/shared/array-utils'
import { getDomRectCenter } from '../../../../core/shared/dom-utils'
import { foldEither, forceRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { fromString } from '../../../../core/shared/element-path'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { create } from '../../../../core/shared/property-path'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers } from '../../../../utils/modifiers'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { changeInspectorNumberControl } from '../../../inspector/common/inspector.test-utils'
import { EditorFixProblemsButtonTestId } from '../../../inspector/editor-contract-section'
import { EdgePositionBottomRight, EdgePositionTopLeft } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { ParentBoundsTestIdSuffix } from '../../controls/parent-bounds'
import { ParentOutlinesTestIdSuffix } from '../../controls/parent-outlines'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  TestAppUID,
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetWithoutUIDs,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { resizeElement } from './absolute-resize.test-utils'

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
  describe('Static Groups', () => {
    it('group is not `position: absolute`', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
              <Group data-uid='group'>
                <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
              </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )
      const groupPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`,
      )
      const metadataBefore = renderResult.getEditorState().editor.jsxMetadata
      const groupMetadataBefore = forceNotNull(
        'Should be able to find metadata for group.',
        metadataBefore[EP.toString(groupPath)],
      )
      expect(groupMetadataBefore.globalFrame).toEqual({
        x: 0,
        y: 0,
        width: 150,
        height: 150,
      })

      const orangePath = EP.appendToPath(groupPath, 'qux')
      await selectComponentsForTest(renderResult, [orangePath])
      await resizeElement(renderResult, { x: 70, y: 90 }, EdgePositionBottomRight, emptyModifiers)

      const metadataAfter = renderResult.getEditorState().editor.jsxMetadata
      const groupMetadataAfter = forceNotNull(
        'Should be able to find metadata for group.',
        metadataAfter[EP.toString(groupPath)],
      )
      expect(groupMetadataAfter.globalFrame).toEqual({
        x: 0,
        y: 0,
        width: 220,
        height: 240,
      })
      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
          <div>
            <Group style={{ width: 220, height: 240 }}>
              <div style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
              <div style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
              <div style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
              <div style={{ position: 'absolute', top: 100, left: 100, width: 120, height: 140, background: 'orange' }} />
            </Group>
          </div>
        `),
      )
    })
    it('group is in a flex container', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root' style={{ display: 'flex', flexDirection: 'row' }}>
              <Group data-uid='group'>
                <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
              </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )
      const groupPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`,
      )
      const metadataBefore = renderResult.getEditorState().editor.jsxMetadata
      const groupMetadataBefore = forceNotNull(
        'Should be able to find metadata for group.',
        metadataBefore[EP.toString(groupPath)],
      )
      expect(groupMetadataBefore.globalFrame).toEqual({
        x: 0,
        y: 0,
        width: 150,
        height: 150,
      })

      const orangePath = EP.appendToPath(groupPath, 'qux')
      await selectComponentsForTest(renderResult, [orangePath])
      await resizeElement(renderResult, { x: 70, y: 90 }, EdgePositionBottomRight, emptyModifiers)

      const metadataAfter = renderResult.getEditorState().editor.jsxMetadata
      const groupMetadataAfter = forceNotNull(
        'Should be able to find metadata for group.',
        metadataAfter[EP.toString(groupPath)],
      )
      expect(groupMetadataAfter.globalFrame).toEqual({
        x: 0,
        y: 0,
        width: 220,
        height: 240,
      })
      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
          <div style={{ display: 'flex', flexDirection: 'row' }}>
            <Group style={{ width: 220, height: 240 }}>
              <div style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
              <div style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
              <div style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
              <div style={{ position: 'absolute', top: 100, left: 100, width: 120, height: 140, background: 'orange' }} />
            </Group>
          </div>
        `),
      )
    })
  })

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
            <Group data-testid='group' style={{ position: 'absolute', left: 50, top: 50, width: 250, height: 250 }}>
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

      it('group with conditionals', async () => {
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
            {
              true ? (
                <div style={{ position: 'absolute', left: 0, top: 0, width: 300, height: 300, background: 'blue' }} />
              ) : null
            }
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')
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

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

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
            width: 200,
            height: 200,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

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
            width: 250,
            height: 250,
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

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('350px')

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
            right: 0,
            bottom: 0,
            width: 300,
            height: 350,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('350px')
          expect(groupDiv.style.height).toBe('400px')

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
            right: 0,
            bottom: 0,
            width: 350,
            height: 400,
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
            <Group data-uid='group' data-testid='group' style={{ position: 'absolute', left: 50, top: 50, width: 250, height: 250 }}>
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

          expect(groupDiv.style.width).toBe('300px')
          expect(groupDiv.style.height).toBe('350px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 300,
            height: 350,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 150,
            height: 175,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: 150,
            top: 175,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 150,
            height: 175,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers)

          expect(groupDiv.style.width).toBe('350px')
          expect(groupDiv.style.height).toBe('400px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 350,
            height: 400,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 175,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group`, {
            left: 175,
            top: 200,
            width: undefined,
            height: undefined,
            right: 0,
            bottom: 0,
          })
          assertStylePropsSet(editor, `${GroupPath}/inner-group/child-2`, {
            left: 0,
            top: 0,
            width: 175,
            height: 200,
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

          const toasts = editor.getEditorState().editor.toasts
          expect(toasts).toHaveLength(1)
          const firstToast = safeIndex(toasts, 0)
          expect(firstToast?.id).toEqual('percentage-pin-replaced')

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
            width: 150,
            height: 180,
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
            width: 175,
            height: 210,
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

    describe('Group Resize With Constrained Children', () => {
      it('group with width/height prop, single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              data-constraints={['width', 'height']}
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
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('150px')
              expect(groupDiv.style.height).toBe('150px')
            },
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
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('150px')
              expect(groupDiv.style.height).toBe('150px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('100px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
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
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('sizeless group, single child with top,left,width,height pins, constrained width height', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              data-constraints={['width', 'height']}
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
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('100px') // TODO for a later PR: the group, or at least the canvas controls should resize during the interaction, but it can't because it doesn't have its own size prop
              expect(groupDiv.style.height).toBe('100px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('100px')

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
            width: 100,
            height: 100,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('100px') // here too
              expect(groupDiv.style.height).toBe('100px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('100px')

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
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('single Group child with top,left,width,height,right,bottom !!! pins, constrained left, top. The unconstrained pins resize relative to their ratio.', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              data-constraints={['left', 'top']}
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
                right: 50,
                bottom: 50
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
          await resizeElement(editor, { x: 60, y: 60 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('210px')
              expect(groupDiv.style.height).toBe('210px')
            },
          })

          expect(groupDiv.style.width).toBe('210px')
          expect(groupDiv.style.height).toBe('210px')

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
            width: 210, // so this is interesting here, the original constraints were 100 width, 50 right. resizing by 60 split the resize 40-20 towards the width
            height: 210,
          })
        }
      })

      it('nested sized groups with single child with top,left,width,height pins, the inner group is constrained width', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50, width: 100, height: 100}}>
            <Group data-constraints={['width']} data-uid='group-inner' data-testid='group-inner' style={{position: 'absolute', left: 0, top: 0, width: 100, height: 100}}>
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
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('150px')
              expect(groupDiv.style.height).toBe('150px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('150px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 100,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: 100,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 100,
            height: 150,
            right: undefined,
            bottom: undefined,
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('150px')
              expect(groupDiv.style.height).toBe('200px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 100,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner`, {
            left: 0,
            top: 0,
            width: 100,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/group-inner/child-1`, {
            left: 0,
            top: 0,
            width: 100,
            height: 200,
            right: undefined,
            bottom: undefined,
          })
        }
      })

      it('Zero-sized child with top,left,bottom,right pins, no width,height pins, constrained left,top,bottom,right, sized Group', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50, width: 150, height: 150}}>
            <div
              data-testid='child-1'
              data-uid='child-1'
              data-constraints={['left', 'top', 'right', 'bottom']}
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
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('200px')
              expect(groupDiv.style.height).toBe('200px')
            },
          })

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 100,
            top: 100,
            width: 200,
            height: 200,
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
          // now the child's left,top,right,bottom are 0, so it doesn't matter if they are marked as Constrained or not: 0 would be equivalent to a constraint anyways, so this is a simple resize
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('250px')
              expect(groupDiv.style.height).toBe('250px')
            },
          })

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 250,
            height: 250,
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

      it('Zero-sized child with top,left,bottom,right pins, existing but ZERO width,height pins, constrained left,top,bottom,right, sized Group', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50, width: 150, height: 150}}>
            <div
              data-testid='child-1'
              data-uid='child-1'
              data-constraints={['left', 'top', 'right', 'bottom']}
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                width: 0,
                height: 0,
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
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('200px')
              expect(groupDiv.style.height).toBe('200px')
            },
          })

          expect(groupDiv.style.width).toBe('200px')
          expect(groupDiv.style.height).toBe('200px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 100,
            top: 100,
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
            right: 0,
            bottom: 0,
          })
        }

        // resizing top left
        {
          // now the child's left,top,right,bottom are 0, so it doesn't matter if they are marked as Constrained or not: 0 would be equivalent to a constraint anyways, so this is a simple resize
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('250px')
              expect(groupDiv.style.height).toBe('250px')
            },
          })

          expect(groupDiv.style.width).toBe('250px')
          expect(groupDiv.style.height).toBe('250px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 50,
            top: 50,
            width: 250,
            height: 250,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 250,
            height: 250,
            right: 0,
            bottom: 0,
          })
        }
      })

      xit('Zero-sized child with top,left,bottom,right pins, no width,height pins, constrained left,top,bottom,right, SIZELESS Group', async () => {
        // This is a documentation of a missing feature.
        // It can happen that a Group + Child doesn't have enough Tentpole Points to correctly spread the Group to a given size and position
        // In this case, you can try to resize the parent Group, but nothing will happen on the canvas, because the code tries to change the Group's width/height (doesn't exist and we are not adding it)
        // and it also tries to change the child's width and height (which are not printed to the code either)

        // we should either make the missing points explicit on the child, or on the group

        // there are other examples for this problem, if you try to move a tentpole child towards negative Top and the group doesn't have an explicit Top point, nothing will happen

        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div
              data-testid='child-1'
              data-uid='child-1'
              data-constraints={['left', 'top', 'right', 'bottom']}
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
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('200px')
              expect(groupDiv.style.height).toBe('200px')
            },
          })

          expect(groupDiv.style.width).toBe('50px')
          expect(groupDiv.style.height).toBe('50px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 100,
            top: 100,
            width: undefined,
            height: undefined,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 50, // THIS IS THE KEY! we are not smart enough to know that without this explicit width here, nothing good will happen
            height: 50,
            right: 0,
            bottom: 0,
          })
        }

        // resizing top left
        {
          // now the child's left,top,right,bottom are 0, so it doesn't matter if they are marked as Constrained or not: 0 would be equivalent to a constraint anyways, so this is a simple resize
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('100px')
              expect(groupDiv.style.height).toBe('100px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('100px')

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
            width: 100, // badly missing
            height: 100,
            right: 0,
            bottom: 0,
          })
        }
      })

      it('group with width/height prop, single child with top,left and hugging width/height', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', width: 100, height: 100, left: 50, top: 50}}>
            <div
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 'max-content',
                height: 'max-content',
              }}
            >
              <div data-uid='inner' style={{ width: 100, height: 100 }} />
            </div>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')

        // Resizing bottom right
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: 50, y: 50 }, EdgePositionBottomRight, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('150px')
              expect(groupDiv.style.height).toBe('150px')
            },
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
            width: 'max-content',
            height: 'max-content',
          })
        }

        // resizing top left
        {
          await selectComponentsForTest(editor, [fromString(GroupPath)])
          await resizeElement(editor, { x: -50, y: -50 }, EdgePositionTopLeft, emptyModifiers, {
            midDragCallback: async () => {
              expect(groupDiv.style.width).toBe('150px')
              expect(groupDiv.style.height).toBe('150px')
            },
          })

          expect(groupDiv.style.width).toBe('100px')
          expect(groupDiv.style.height).toBe('100px')

          assertStylePropsSet(editor, `${GroupPath}`, {
            left: 0,
            top: 0,
            width: 100,
            height: 100,
            right: undefined,
            bottom: undefined,
          })
          assertStylePropsSet(editor, `${GroupPath}/child-1`, {
            left: 0,
            top: 0,
            width: 'max-content',
            height: 'max-content',
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

    await changeInspectorNumberControl(editor, 'frame-top-number-input', '250')

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

describe('Autofix problems', () => {
  it('fixes a child with percent pins (wh)', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: 10, top: 10, width: 264, height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: '50%', height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group/child2`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes a group child with percentage pin (right)', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', top: 0, right: "10%", width: 132, height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group/child2`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 10, top: 10, width: 264.5, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute',  top: 0, width: 132, height: 59, left: 133 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes a group child with percentage pin (right, left set)', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, right: "20%", width: 132, height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group/child2`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, width: 132, height: 59, top: 0 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes a child with non absolute position', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'relative', left: 162, top: 13, width: 132, height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group/child2`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes a group with percent pins', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: 10, top: 10, width: '50%', height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes a group with children with problems', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: "10%", top: 10, width: "50%", height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: '50%', height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'relative', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 40, top: 10, width: 262, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 100, height: 59 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes multiple children with problems', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<Group data-uid='group' style={{ position: 'absolute', left: 10, top: 10, width: 264, height: 132, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: '50%', height: 59 }} />
					<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'relative', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group/child2`),
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/group/child3`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<Group style={{ position: 'absolute', left: 10, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
				</Group>
			</div>
		`),
      ),
    )
  })
  it('fixes a group with percent pins inside another element', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<div data-uid='container' style={{ position: 'absolute', left: 25, top: 25, width: 300, height: 300 }}>
					<Group data-uid='group' style={{ position: 'absolute', left: '20%', top: 10, width: '50%', height: 132, backgroundColor: 'white' }}>
						<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
						<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
						<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
					</Group>
				</div>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/container/group`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<div style={{ position: 'absolute', left: 25, top: 25, width: 300, height: 300 }}>
					<Group style={{ position: 'absolute', left: 60, top: 10, width: 294, height: 132, backgroundColor: 'white' }}>
						<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
						<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
						<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
					</Group>
				</div>
			</div>
		`),
      ),
    )
  })
  it('fixes a group with percent pins inside another element (right)', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
				<div data-uid='container' style={{ position: 'absolute', left: 25, top: 25, width: 600, height: 300 }}>
					<Group data-uid='group' style={{ position: 'absolute', right: '20%', top: 10, width: '50%', height: 132, backgroundColor: 'white' }}>
						<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
						<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
						<div data-uid='child3' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
					</Group>
				</div>
			</div>
		`),
      ),
      'await-first-dom-report',
    )

    await selectComponentsForTest(renderResult, [
      EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/container/group`),
    ])

    const button = await screen.findByTestId(EditorFixProblemsButtonTestId)
    await mouseClickAtPoint(button, { x: 1, y: 1 })

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
			<div>
				<div style={{ position: 'absolute', left: 25, top: 25, width: 600, height: 300 }}>
					<Group style={{ position: 'absolute', top: 10, width: 294, height: 132, backgroundColor: 'white', left: 186 }}>
						<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 102, height: 59 }}  />
						<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 162, top: 13, width: 132, height: 59 }} />
						<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 45, top: 73, width: 102, height: 59 }} />
					</Group>
				</div>
			</div>
		`),
      ),
    )
  })
})
