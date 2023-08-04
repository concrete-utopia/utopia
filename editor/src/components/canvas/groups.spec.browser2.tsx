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

describe('Groups', () => {
  describe('wrap in group', () => {
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

    it('shows Toast if a conditional expression is selected', async () => {
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
                  data-uid='target-div'
                  data-testid='target-div'
                />
              ) : 'Test' 
            }
           </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a Group.
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('target-div')
      const elementBounds = element.getBoundingClientRect()
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Group Selection',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.getEditorState().editor.toasts.length).toBe(1)
      expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
        'Only simple JSX Elements can be wrapped into Groups for now 🙇',
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
    xit("ungrouping that isn't a group, but is inside a group, trues up the group (with siblings)", async () => {
      // TODO un-xit this test when the related trueup bug is fixed
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
                <Group data-uid='group' style={{ position: 'absolute', top: 10, left: 10, background: 'pink', width: 240, height: 290 }}>
                    <div data-uid='foo' style={{ position: 'absolute', top: 0, left: 0, width: 50, height: 50, background: 'blue' }} />
                    <div data-uid='bar' style={{ position: 'absolute', top: 0, left: 94, width: 50, height: 50, background: 'red' }} />
                    <div data-uid='baz' style={{ position: 'absolute', top: 93, left: 0, width: 50, height: 50, background: 'green' }} />
                    <div data-uid='qux' style={{ position: 'absolute', top: 93, left: 94, width: 50, height: 50, background: 'orange' }} />
                    <div data-uid='another-div' style={{ position: 'absolute', left: 200, top: 250, width: 50, height: 50, background: 'black', }} />
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
