import * as EP from '../../core/shared/element-path'
import { FOR_TESTS_setNextGeneratedUid } from '../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { selectComponents } from '../editor/actions/meta-actions'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { openContextMenuAndClickOnItem, pressKey } from './event-helpers.test-utils'
import type { EditorRenderResult } from './ui-jsx.test-utils'
import {
  renderTestEditorWithCode,
  makeTestProjectCodeWithSnippet,
  TestSceneUID,
  TestAppUID,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippetWithoutUIDs,
  formatTestProjectCode,
  getPrintedUiJsCode,
} from './ui-jsx.test-utils'
import { shiftCmdModifier } from '../../utils/modifiers'
import type { ElementPath } from '../../core/shared/project-file-types'
import { forceNotNull } from '../../core/shared/optional-utils'
import {
  filtered,
  fromTypeGuard,
  notNull,
  traverseArray,
} from '../../core/shared/optics/optic-creators'
import type { CanvasRectangle, MaybeInfinityCanvasRectangle } from '../../core/shared/math-utils'
import { isFiniteRectangle } from '../../core/shared/math-utils'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { applyCommandsAction } from '../editor/actions/action-creators'
import {
  convertFragmentToGroup,
  convertFrameToGroup,
} from './canvas-strategies/strategies/group-conversion-helpers'

const notNullFiniteCanvasRectangleOptic = notNull<MaybeInfinityCanvasRectangle>().compose(
  fromTypeGuard(isFiniteRectangle),
)

function getElementGlobalFrame(renderResult: EditorRenderResult, path: string): CanvasRectangle {
  const metadata = renderResult.getEditorState().editor.jsxMetadata
  const targetMetadata = forceNotNull('Metadata for target should exist.', metadata[path])
  return unsafeGet(notNullFiniteCanvasRectangleOptic, targetMetadata.globalFrame)
}

describe('Groups', () => {
  describe('removes padding and margin appropriately', () => {
    const paddingAndMarginSnippet = `
      <div
        data-uid='root'
      >
        <div
          style={{
            backgroundColor: 'blue',
            position: 'absolute',
            left: 20,
            top: 74,
            width: 358,
            height: 380,
            paddingLeft: 50,
            paddingTop: 50,
            paddingBottom: 50,
            paddingRight: 50,
          }}
          data-uid='div'
        >
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: -7,
              top: 20,
              width: 106,
              height: 95,
              margin: 80,
            }}
            data-uid='div-rect-1'
          />
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 127,
              top: 134,
              width: 110,
              height: 132,
              margin: 80,
            }}
            data-uid='div-rect-2'
          />
        </div>
        <React.Fragment data-uid='fragment'>
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 350,
              top: 20,
              width: 106,
              height: 95,
              margin: 80,
            }}
            data-uid='fragment-rect-1'
          />
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 427,
              top: 134,
              width: 110,
              height: 132,
              margin: 80,
            }}
            data-uid='fragment-rect-2'
          />
        </React.Fragment>
      </div>
`
    it('works when converting fragment to group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(paddingAndMarginSnippet),
        'await-first-dom-report',
      )
      const targetPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/fragment`,
      )
      await renderResult.dispatch(selectComponents([targetPath], false), true)

      const rect1Path = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/div/div-rect-1`
      const rect2Path = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/div/div-rect-2`
      const rect1FrameBefore = getElementGlobalFrame(renderResult, rect1Path)
      const rect2FrameBefore = getElementGlobalFrame(renderResult, rect2Path)

      // Trigger change to group.
      const editorState = renderResult.getEditorState().editor
      await renderResult.dispatch(
        [
          applyCommandsAction(
            convertFragmentToGroup(
              editorState.jsxMetadata,
              editorState.elementPathTree,
              editorState.allElementProps,
              targetPath,
            ),
          ),
        ],
        true,
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      const rect1FrameAfter = getElementGlobalFrame(renderResult, rect1Path)
      const rect2FrameAfter = getElementGlobalFrame(renderResult, rect2Path)

      const expectedFrames = {
        rect1: rect1FrameBefore,
        rect2: rect2FrameBefore,
      }
      const actualFrames = {
        rect1: rect1FrameAfter,
        rect2: rect2FrameAfter,
      }
      expect(actualFrames).toEqual(expectedFrames)

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div>
        <div
          style={{
            backgroundColor: 'blue',
            position: 'absolute',
            left: 20,
            top: 74,
            width: 358,
            height: 380,
            paddingLeft: 50,
            paddingTop: 50,
            paddingBottom: 50,
            paddingRight: 50,
          }}
        >
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: -7,
              top: 20,
              width: 106,
              height: 95,
              margin: 80,
            }}
          />
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 127,
              top: 134,
              width: 110,
              height: 132,
              margin: 80,
            }}
          />
        </div>
        <Group
          style={{
            position: 'absolute',
            top: 100,
            left: 430,
            width: 187,
            height: 246,
          }}
        >
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 106,
              height: 95,
            }}
          />
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 77,
              top: 114,
              width: 110,
              height: 132,
            }}
          />
        </Group>
      </div>
`,
        ),
      )
    })
    it('works when converting frame to group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(paddingAndMarginSnippet),
        'await-first-dom-report',
      )
      const targetPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/div`,
      )
      await renderResult.dispatch(selectComponents([targetPath], false), true)
      await renderResult.getDispatchFollowUpActionsFinished()

      const rect1Path = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/div/div-rect-1`
      const rect2Path = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/div/div-rect-2`
      const rect1FrameBefore = getElementGlobalFrame(renderResult, rect1Path)
      const rect2FrameBefore = getElementGlobalFrame(renderResult, rect2Path)

      // Trigger the change to group.
      const editorState = renderResult.getEditorState().editor
      await renderResult.dispatch(
        [
          applyCommandsAction(
            convertFrameToGroup(
              editorState.jsxMetadata,
              editorState.elementPathTree,
              editorState.allElementProps,
              targetPath,
            ),
          ),
        ],
        true,
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      const rect1FrameAfter = getElementGlobalFrame(renderResult, rect1Path)
      const rect2FrameAfter = getElementGlobalFrame(renderResult, rect2Path)

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div>
        <Group
          style={{
            position: 'absolute',
            left: 93,
            top: 174,
            width: 244,
            height: 246,
          }}
        >
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 106,
              height: 95,
            }}
          />
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 134,
              top: 114,
              width: 110,
              height: 132,
            }}
          />
        </Group>
        <React.Fragment>
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 350,
              top: 20,
              width: 106,
              height: 95,
              margin: 80,
            }}
          />
          <div
            style={{
              backgroundColor: '#FF69B4AB',
              position: 'absolute',
              left: 427,
              top: 134,
              width: 110,
              height: 132,
              margin: 80,
            }}
          />
        </React.Fragment>
      </div>
`,
        ),
      )

      const expectedFrames = {
        rect1: rect1FrameBefore,
        rect2: rect2FrameBefore,
      }
      const actualFrames = {
        rect1: rect1FrameAfter,
        rect2: rect2FrameAfter,
      }
      expect(actualFrames).toEqual(expectedFrames)
    })
  })
  describe('wrap in group', () => {
    it('removes margins from the children and takes account of padding on the parent', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
              backgroundColor: '#FFFFFF',
              padding: 200,
            }}
            data-uid='div'
          >
            <div
              style={{
                backgroundColor: '#FF69B4AB',
                left: 0,
                top: 0,
                width: 106,
                height: 95,
                margin: 80,
              }}
              data-uid='div-rect-1'
              data-testid='div-rect-1'
            />
            <div
              style={{
                backgroundColor: '#FF69B4AB',
                left: 134,
                top: 114,
                width: 110,
                height: 132,
                marginLeft: 80,
                marginTop: 90
              }}
              data-uid='div-rect-2'
              data-testid='div-rect-2'
            />
          </div>`,
        ),
        'await-first-dom-report',
      )

      const divRect1PathBefore = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:div/div-rect-1`,
      )
      const divRect2PathBefore = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:div/div-rect-2`,
      )
      const testValuePaths = [divRect1PathBefore, divRect2PathBefore]

      const rect1FrameBefore = getElementGlobalFrame(renderResult, EP.toString(divRect1PathBefore))
      const rect2FrameBefore = getElementGlobalFrame(renderResult, EP.toString(divRect2PathBefore))

      // Wrap them in a Group.
      await renderResult.dispatch(selectComponents(testValuePaths, false), true)
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('div-rect-1')
      const elementBounds = element.getBoundingClientRect()
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
              backgroundColor: '#FFFFFF',
              padding: 200,
            }}
          >
            <Group
              style={{
                position: 'absolute',
                left: 280,
                top: 280,
                width: 110,
                height: 317,
              }}
            >
              <div
                style={{
                  backgroundColor: '#FF69B4AB',
                  left: 0,
                  top: 0,
                  width: 106,
                  height: 95,
                  position: 'absolute',
                }}
                data-testid='div-rect-1'
              />
              <div
                style={{
                  backgroundColor: '#FF69B4AB',
                  left: 0,
                  top: 185,
                  width: 110,
                  height: 132,
                  position: 'absolute',
                }}
                data-testid='div-rect-2'
              />
            </Group>
          </div>`,
        ),
      )

      const metadataKeys = Object.keys(renderResult.getEditorState().editor.jsxMetadata)
      const divRect1PathAfter = unsafeGet(
        traverseArray<string>().compose(filtered((p) => p.endsWith('div-rect-1'))),
        metadataKeys,
      )
      const divRect2PathAfter = unsafeGet(
        traverseArray<string>().compose(filtered((p) => p.endsWith('div-rect-2'))),
        metadataKeys,
      )
      const rect1FrameAfter = getElementGlobalFrame(renderResult, divRect1PathAfter)
      const rect2FrameAfter = getElementGlobalFrame(renderResult, divRect2PathAfter)

      const expectedFrames = {
        rect1: rect1FrameBefore,
        rect2: rect2FrameBefore,
      }
      const actualFrames = {
        rect1: rect1FrameAfter,
        rect2: rect2FrameAfter,
      }
      expect(actualFrames).toEqual(expectedFrames)
    })
    it('works inside a conditional on an element', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-uid='then-div'
                   data-testid='then-div'
                 />
               ) : 'Test' 
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional/then-div`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('then-div')
      const elementBounds = element.getBoundingClientRect()
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <Group style={{ position: 'absolute', left: 154, top: 134, width: 150, height: 150}}>
                   <div
                     style={{
                       height: 150,
                       width: 150,
                       position: 'absolute',
                       left: 0,
                       top: 0,
                       backgroundColor: 'lightblue',
                     }}
                     data-testid='then-div'
                   />
                 </Group>
               ) : (
                'Test' 
               )
             }
           </div>`,
        ),
      )
    })

    it('works on an element', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             <div
               style={{
                 height: 150,
                 width: 150,
                 position: 'absolute',
                 left: 154,
                 top: 134,
                 backgroundColor: 'lightblue',
               }}
               data-uid='target-div'
               data-testid='target-div'
             />
           </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/target-div`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('target-div')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/group`),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             <Group style={{ position: 'absolute', left: 154, top: 134, width: 150, height: 150 }}>
               <div
                 style={{
                   height: 150,
                   width: 150,
                   position: 'absolute',
                   left: 0,
                   top: 0,
                   backgroundColor: 'lightblue',
                 }}
                 data-testid='target-div'
               />
             </Group>
           </div>`,
        ),
      )
    })

    it('works for single flex child', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
              data-uid='flex-row'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
                data-uid='child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 50,
                  contain: 'layout',
                }}
                data-uid='child-2'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 130,
                  height: 100,
                  contain: 'layout',
                }}
                data-uid='child-3'
              />
            </div>
           </div>`,
        ),
        'await-first-dom-report',
      )

      const child1Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/child-1`,
      )

      await renderResult.dispatch(selectComponents([child1Path], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('child-1')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/group`),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
            >
              <Group
                style={{
                  contain: 'layout',
                  width: 100,
                  height: 100,
                }}
              >
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 100,
                    height: 100,
                    contain: 'layout',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                  }}
                  data-testid='child-1'
                />
              </Group>
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 50,
                  contain: 'layout',
                }}
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 130,
                  height: 100,
                  contain: 'layout',
                }}
              />
            </div>
          </div>`,
        ),
      )
    })

    it('works for two flex children', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
              data-uid='flex-row'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
                data-uid='child-1'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 50,
                  contain: 'layout',
                }}
                data-testid='child-2'
                data-uid='child-2'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 130,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-3'
                data-uid='child-3'
              />
            </div>
           </div>`,
        ),
        'await-first-dom-report',
      )

      const child2Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/child-2`,
      )
      const child3Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/child-3`,
      )

      await renderResult.dispatch(selectComponents([child2Path, child3Path], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('child-2')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/group`),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
              />
              <Group
                style={{
                  contain: 'layout',
                  width: 250,
                  height: 100,
                }}
              >
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 100,
                    height: 50,
                    contain: 'layout',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                  }}
                  data-testid='child-2'
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 130,
                    height: 100,
                    contain: 'layout',
                    position: 'absolute',
                    left: 120,
                    top: 0,
                  }}
                  data-testid='child-3'
                />
              </Group>
            </div>
          </div>`,
        ),
      )
    })

    it('works for two flex children wrapped in a Fragment, if the Fragment is selected', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
              data-uid='flex-row'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
                data-uid='child-1'
              />
              <React.Fragment data-uid='fragment'>
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 100,
                    height: 50,
                    contain: 'layout',
                  }}
                  data-testid='child-2'
                  data-uid='child-2'
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 130,
                    height: 100,
                    contain: 'layout',
                  }}
                  data-testid='child-3'
                  data-uid='child-3'
                />
              </React.Fragment>
            </div>
           </div>`,
        ),
        'await-first-dom-report',
      )

      const fragmentPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/fragment`,
      )

      await renderResult.dispatch(selectComponents([fragmentPath], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('child-2')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        { x: elementBounds.x + 10, y: elementBounds.y + 10 },
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/group`),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
              />
              <Group
                style={{
                  contain: 'layout',
                  width: 250,
                  height: 100,
                }}
              >
                <React.Fragment>
                  <div
                    style={{
                      backgroundColor: '#aaaaaa33',
                      width: 100,
                      height: 50,
                      contain: 'layout',
                      position: 'absolute',
                      left: 0,
                      top: 0,
                    }}
                    data-testid='child-2'
                  />
                  <div
                    style={{
                      backgroundColor: '#aaaaaa33',
                      width: 130,
                      height: 100,
                      contain: 'layout',
                      position: 'absolute',
                      left: 120,
                      top: 0,
                    }}
                    data-testid='child-3'
                  />
                </React.Fragment>
              </Group>
            </div>
          </div>`,
        ),
      )
    })

    it('works for two flex children wrapped in a Fragment, if the two Fragment children are selected', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
              data-uid='flex-row'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
                data-uid='child-1'
              />
              <React.Fragment data-uid='fragment'>
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 100,
                    height: 50,
                    contain: 'layout',
                  }}
                  data-testid='child-2'
                  data-uid='child-2'
                />
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    width: 130,
                    height: 100,
                    contain: 'layout',
                  }}
                  data-testid='child-3'
                  data-uid='child-3'
                />
              </React.Fragment>
            </div>
           </div>`,
        ),
        'await-first-dom-report',
      )

      const child2Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/fragment/child-2`,
      )
      const child3Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/fragment/child-3`,
      )

      await renderResult.dispatch(selectComponents([child2Path, child3Path], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('child-2')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        { x: elementBounds.x + 10, y: elementBounds.y + 10 },
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/flex-row/fragment/group`,
        ),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 50,
                top: 50,
                width: 'max-content',
                height: 'max-content',
                display: 'flex',
                flexDirection: 'row',
                gap: 20,
                padding: '20px',
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-testid='child-1'
              />
              <React.Fragment>
                <Group
                  style={{
                    contain: 'layout',
                    width: 250,
                    height: 100,
                  }}
                >
                  <div
                    style={{
                      backgroundColor: '#aaaaaa33',
                      width: 100,
                      height: 50,
                      contain: 'layout',
                      position: 'absolute',
                      left: 0,
                      top: 0,
                    }}
                    data-testid='child-2'
                  />
                  <div
                    style={{
                      backgroundColor: '#aaaaaa33',
                      width: 130,
                      height: 100,
                      contain: 'layout',
                      position: 'absolute',
                      left: 120,
                      top: 0,
                    }}
                    data-testid='child-3'
                  />
                </Group>
              </React.Fragment>
            </div>
          </div>`,
        ),
      )
    })

    it('works if a Fragment is selected', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
            <React.Fragment data-uid='fragment'>
              <div
                style={{
                  height: 150,
                  width: 150,
                  position: 'absolute',
                  left: 154,
                  top: 134,
                  backgroundColor: 'lightblue',
                }}
                data-uid='target-div'
                data-testid='target-div'
              />
            </React.Fragment>
          </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/fragment`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('target-div')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        { x: elementBounds.x + 10, y: elementBounds.y + 10 },
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/group`),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
          <div style={{ ...props.style }}>
            <Group style={{ position: 'absolute', left: 154, top: 134, width: 150, height: 150 }}>
              <React.Fragment>
                <div
                  style={{
                    height: 150,
                    width: 150,
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    backgroundColor: 'lightblue',
                  }}
                  data-testid='target-div'
                />
              </React.Fragment>
            </Group>
          </div>`),
      )
    })

    it('wrap in Group for multiselect can turn a slot empty', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-uid='then-div'
                   data-testid='then-div'
                 />
               ) : 'Test' 
             }
             <div
                style={{
                height: 50,
                  width: 50,
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  backgroundColor: 'lightblue',
                }}
                data-uid='child-2'
                data-testid='child-2'
              />
           </div>`,
        ),
        'await-first-dom-report',
      )

      const child1Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional/then-div`,
      )
      const child2Path = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/child-2`,
      )

      await renderResult.dispatch(selectComponents([child1Path, child2Path], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('then-div')
      const elementBounds = element.getBoundingClientRect()
      FOR_TESTS_setNextGeneratedUid('group')
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/group`),
      ])

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
            {
              // @utopia/uid=conditional
              [].length === 0 ? (null) : 'Test' 
            }
            <Group
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 304,
                height: 284,
              }}
            >
              <div
                style={{
                  height: 150,
                  width: 150,
                  position: 'absolute',
                  left: 154,
                  top: 134,
                  backgroundColor: 'lightblue',
                }}
                data-testid='then-div'
              />
              <div
                  style={{
                  height: 50,
                  width: 50,
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  backgroundColor: 'lightblue',
                }}
                data-testid='child-2'
              />
            </Group>
          </div>`,
        ),
      )
    })
  })

  describe('ungroup', () => {
    async function ungroup(renderResult: EditorRenderResult, paths: ElementPath[]) {
      await renderResult.dispatch(selectComponents(paths, false), true)
      await pressKey('g', { modifiers: shiftCmdModifier })
    }

    it('can ungroup a single group', async () => {
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

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`),
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
            </div>
        `),
        ),
      )
    })
    it('can ungroup multiple groups', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group1' style={{ position: 'absolute', top: 0, left: 0}}>
                    <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
                <Group data-uid='group2' style={{ position: 'absolute', top: 120, left: 120}}>
                    <div data-uid='one' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='two' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='three' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='four' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group1`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group2`),
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                <div data-uid='one' style={{ position: 'absolute', top: 120, left: 120, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='two' style={{ position: 'absolute', top: 120, left: 220, width: 50, height: 50, background: 'red' }} />
                <div data-uid='three' style={{ position: 'absolute', top: 220, left: 120, width: 50, height: 50, background: 'green' }} />
                <div data-uid='four' style={{ position: 'absolute', top: 220, left: 220, width: 50, height: 50, background: 'orange' }} />
            </div>
        `),
        ),
      )
    })
    it('can ungroup multiple groups as part as a mixed multiselect', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group1' style={{ position: 'absolute', top: 0, left: 0}}>
                    <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
                <div data-uid='also-this' />
                <Group data-uid='group2' style={{ position: 'absolute', top: 120, left: 120}}>
                    <div data-uid='one' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='two' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='three' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='four' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group1`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/also-this`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group2`),
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                <div data-uid='one' style={{ position: 'absolute', top: 120, left: 120, width: 50, height: 50, background: 'blue' }} />
                <div data-uid='two' style={{ position: 'absolute', top: 120, left: 220, width: 50, height: 50, background: 'red' }} />
                <div data-uid='three' style={{ position: 'absolute', top: 220, left: 120, width: 50, height: 50, background: 'green' }} />
                <div data-uid='four' style={{ position: 'absolute', top: 220, left: 220, width: 50, height: 50, background: 'orange' }} />
            </div>
        `),
        ),
      )
    })
    it('when ungrouping a group the selection is set to the inner elements', async () => {
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

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`),
      ])

      const selection = [...renderResult.getEditorState().editor.selectedViews].sort(
        comparePathStrings,
      )
      expect(selection).toEqual(
        [
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/foo`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/bar`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/baz`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/qux`),
        ].sort(comparePathStrings),
      )
    })
    it('when ungrouping a group the selection is set to the inner elements (multiselect)', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group1' style={{ position: 'absolute', top: 0, left: 0}}>
                    <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
                <Group data-uid='group2' style={{ position: 'absolute', top: 120, left: 120}}>
                    <div data-uid='one' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='two' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='three' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='four' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group1`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group2`),
      ])

      const selection = [...renderResult.getEditorState().editor.selectedViews].sort(
        comparePathStrings,
      )
      expect(selection).toEqual(
        [
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/foo`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/bar`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/baz`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/qux`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/one`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/two`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/three`),
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/four`),
        ].sort(comparePathStrings),
      )
    })

    it("ungrouping that isn't a group, but is inside a group, trues up the group (without siblings)", async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group' style={{ position: 'absolute', top: 0, left: 0 }}>
                    <div data-uid='unwrap-me' style={{ position: 'absolute', top: 0, left: 0, padding: 10 }}>
                        <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                        <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                        <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                        <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                    </div>
                </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group/unwrap-me`),
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group' style={{ position: 'absolute', top: 0, left: 0 }}>
                    <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 100, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                </Group>
            </div>
        `),
        ),
      )
    })
    it("ungrouping that isn't a group, but is inside a group, trues up the group (with siblings)", async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group' style={{ position: 'absolute', top: 0, left: 0, background: 'pink' }}>
                    <div data-uid='unwrap-me' style={{ position: 'absolute', top: 0, left: 0, width: 193, height: 180, padding: 10 }}>
                        <div data-uid='foo' style={{ position: 'absolute', top: 10, left: 10, width: 50, height: 50, background: 'blue' }} />
                        <div data-uid='bar' style={{ position: 'absolute', top: 10, left: 100, width: 50, height: 50, background: 'red' }} />
                        <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 10, width: 50, height: 50, background: 'green' }} />
                        <div data-uid='qux' style={{ position: 'absolute', top: 100, left: 100, width: 50, height: 50, background: 'orange' }} />
                    </div>
                    <div data-uid='another-div' style={{ position: 'absolute', left: 200, top: 250, width: 50, height: 50, background: 'black' }} />
                </Group>
            </div>
        `),
        ),
        'await-first-dom-report',
      )

      await ungroup(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group/unwrap-me`),
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='root'>
                <Group data-uid='group' style={{ position: 'absolute', top: 10, left: 10, background: 'pink' }}>
                    <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 90, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='baz' style={{ position: 'absolute', top: 90, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='qux' style={{ position: 'absolute', top: 90, left: 90, width: 50, height: 50, background: 'orange' }} />
                    <div data-uid='another-div' style={{ position: 'absolute', left: 190, top: 240, width: 50, height: 50, background: 'black', }} />
                </Group>
            </div>
        `),
        ),
      )
    })
  })
})

function comparePathStrings(a: ElementPath, b: ElementPath): number {
  return EP.toString(a).localeCompare(EP.toString(b))
}
