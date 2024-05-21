import * as EP from '../../../core/shared/element-path'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import type { EditorRenderResult } from '../../../components/canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../../components/canvas/ui-jsx.test-utils'
import {
  applyCommandsAction,
  clearSelection,
  deleteSelected,
  deleteView,
  selectComponents,
  setLeftMenuTab,
  truncateHistory,
  undo,
  unwrapElements,
} from './action-creators'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ElementPaste } from '../action-types'
import type { InsertionPath } from '../store/insertion-path'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
  wrapInFragmentAndAppendElements,
} from '../store/insertion-path'
import { getElementFromRenderResult } from './actions.test-utils'
import {
  expectNoAction,
  expectSingleUndoNSaves,
  searchInFloatingMenu,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../../utils/utils.test-utils'
import {
  firePasteEvent,
  keyDown,
  MockClipboardHandlers,
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDragFromPointWithDelta,
  openContextMenuAndClickOnItem,
  pressKey,
} from '../../canvas/event-helpers.test-utils'
import { cmdModifier, shiftCmdModifier } from '../../../utils/modifiers'
import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../../../core/model/element-template-utils.test-utils'
import {
  createModifiedProject,
  createTestProjectWithMultipleFiles,
} from '../../../sample-projects/sample-project-utils.test-utils'
import {
  LeftMenuTab,
  navigatorEntryToKey,
  PlaygroundFilePath,
  StoryboardFilePath,
} from '../store/editor-state'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { windowPoint } from '../../../core/shared/math-utils'
import { assertNever } from '../../../core/shared/utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { maybeConditionalExpression } from '../../../core/model/conditionals'
import {
  PropsPreservedPasteHerePostActionChoiceId,
  PropsPreservedPastePostActionChoiceId,
  PropsReplacedPastePostActionChoiceId,
  PropsReplacedPasteHerePostActionChoiceId,
} from '../../canvas/canvas-strategies/post-action-options/post-action-paste'
import { getDomRectCenter } from '../../../core/shared/dom-utils'
import { FloatingPostActionMenuTestId } from '../../canvas/controls/select-mode/post-action-menu'
import { safeIndex } from '../../../core/shared/array-utils'
import { updateSelectedViews } from '../../canvas/commands/update-selected-views-command'

async function deleteFromScene(
  inputSnippet: string,
  targets: ElementPath[],
): Promise<{ code: string; selection: ElementPath[] }> {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(inputSnippet),
    'await-first-dom-report',
  )
  await renderResult.dispatch([selectComponents(targets, true)], true)
  await renderResult.dispatch([deleteSelected()], true)

  return {
    code: getPrintedUiJsCode(renderResult.getEditorState()),
    selection: renderResult.getEditorState().editor.selectedViews,
  }
}

function makeTargetPath(suffix: string): ElementPath {
  return EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:${suffix}`)
}

describe('actions', () => {
  describe('DELETE_SELECTED', () => {
    const tests: {
      name: string
      input: string
      targets: ElementPath[]
      wantCode: string
      wantSelection: ElementPath[]
    }[] = [
      {
        name: 'delete selected element',
        input: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        targets: [makeTargetPath('aaa/bbb')],
        wantCode: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: 'delete multiple elements',
        input: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
    </View>
    `,
        targets: [makeTargetPath('aaa/bbb'), makeTargetPath('aaa/ddd')],
        wantCode: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: 'delete empty fragments (single fragment)',
        input: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <React.Fragment data-uid='000'>
        <View
          style={{ background: '#f90', width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </React.Fragment>
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
    </View>
    `,
        targets: [makeTargetPath('aaa/000/ccc')],
        wantCode: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: "don't delete fragments if not empty",
        input: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <React.Fragment data-uid='000'>
        <View
          style={{ background: '#f90', width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
        <View
          style={{ background: '#90f', width: 50, height: 50 }}
          data-uid='eee'
          data-testid='eee'
        />
      </React.Fragment>
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
    </View>
    `,
        targets: [makeTargetPath('aaa/000/eee')],
        wantCode: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <React.Fragment>
        <View
          style={{ background: '#f90', width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </React.Fragment>
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('aaa/000/ccc')],
      },
      {
        name: 'delete empty fragments (multiple targets)',
        input: `
    <View data-uid='xxx'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <React.Fragment data-uid='000'>
        <View
          style={{ background: '#f90', width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
        <View
          style={{ background: '#90f', width: 50, height: 50 }}
          data-uid='eee'
          data-testid='eee'
        />
      </React.Fragment>
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
      <React.Fragment data-uid='001'>
        <View
          style={{ background: '#0f9', width: 50, height: 50 }}
          data-uid='fff'
          data-testid='fff'
        />
        <View
          style={{ background: '#9f0', width: 50, height: 50 }}
          data-uid='ggg'
          data-testid='ggg'
        />
      </React.Fragment>
    </View>
    `,
        targets: [
          makeTargetPath('xxx/000/eee'),
          makeTargetPath('xxx/001/fff'),
          makeTargetPath('xxx/001/ggg'),
        ],
        wantCode: `
    <View data-uid='xxx'>
      <View
        style={{ background: '#09f', width: 50, height: 50 }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <React.Fragment>
        <View
          style={{ background: '#f90', width: 50, height: 50 }}
          data-uid='ccc'
          data-testid='ccc'
        />
      </React.Fragment>
      <View
        style={{ background: '#f09', width: 50, height: 50 }}
        data-uid='ddd'
        data-testid='ddd'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('xxx/000/ccc'), makeTargetPath('xxx')],
      },
      {
        name: 'delete map expression',
        input: `
    <View data-uid='aaa'>
      {
        // @utopia/uid=6bb
        [0,1,2,3].map(() => (<View
          style={{ background: '#09f', width: 50, height: 50 }}
          data-uid='bbb'
          data-testid='bbb'
        />))
      }
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        targets: [makeTargetPath('aaa/6bb')],
        wantCode: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: 'delete expression',
        input: `
    <View data-uid='aaa'>
      {
        // @utopia/uid=d16
        (() => <View
          style={{ background: '#09f', width: 50, height: 50 }}
          data-uid='bbb'
          data-testid='bbb'
        />)()
      }
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        targets: [makeTargetPath('aaa/d16')],
        wantCode: `
    <View data-uid='aaa'>
      <View
        style={{ background: '#f90', width: 50, height: 50 }}
        data-uid='ccc'
        data-testid='ccc'
      />
    </View>
    `,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: 'delete group child selects next sibling',
        input: `
          <View data-uid='view'>
            <Group data-uid='group'>
              <div data-uid='child1' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 0, background: 'blue' }} />
              <div data-uid='child2' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 40, background: 'blue' }} />
              <div data-uid='child3' style={{ position: 'absolute', width: 10, height: 10, top: 40, left: 0, background: 'blue' }} />
              <div data-uid='child4' style={{ position: 'absolute', width: 10, height: 10, top: 40, left: 40, background: 'blue' }} />
            </Group>
          </View>
        `,
        targets: [makeTargetPath('view/group/child3')],
        wantCode: `
          <View data-uid='view'>
            <Group data-uid='group'>
              <div data-uid='child1' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 0, background: 'blue' }} />
              <div data-uid='child2' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 40, background: 'blue' }} />
              <div data-uid='child4' style={{ position: 'absolute', width: 10, height: 10, top: 40, left: 40, background: 'blue' }} />
            </Group>
          </View>
        `,
        wantSelection: [makeTargetPath('view/group/child1')],
      },
      // {
      //   name: 'delete group child selects next sibling (multiple selection)',
      //   input: `
      //     <View data-uid='view'>
      //       <Group data-uid='group'>
      //         <div data-uid='child1' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 0, background: 'blue' }} />
      //         <div data-uid='child2' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 40, background: 'blue' }} />
      //         <div data-uid='child3' style={{ position: 'absolute', width: 10, height: 10, top: 40, left: 0, background: 'blue' }} />
      //         <div data-uid='child4' style={{ position: 'absolute', width: 10, height: 10, top: 40, left: 40, background: 'blue' }} />
      //       </Group>
      //       <div data-uid='foo'>
      //         <div data-uid='bar' />
      //       </div>
      //     </View>
      //   `,
      //   targets: [makeTargetPath('view/group/child3'), makeTargetPath('view/foo/bar')],
      //   wantCode: `
      //     <View data-uid='view'>
      //       <Group data-uid='group'>
      //         <div data-uid='child1' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 0, background: 'blue' }} />
      //         <div data-uid='child2' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 40, background: 'blue' }} />
      //         <div data-uid='child4' style={{ position: 'absolute', width: 10, height: 10, top: 40, left: 40, background: 'blue' }} />
      //       </Group>
      //       <div data-uid='foo' style={{ width: 400, height: 0, position: 'absolute', left: 0, top: 50 }} />
      //     </View>
      //   `,
      //   wantSelection: [makeTargetPath('view/group/child1'), makeTargetPath('view/foo')],
      // },
      // {
      //   name: 'delete last group child deletes the group',
      //   input: `
      //     <View data-uid='view'>
      //       <Group data-uid='group'>
      //         <div data-uid='child1' style={{ position: 'absolute', width: 10, height: 10, top: 0, left: 0, background: 'blue' }} />
      //       </Group>
      //     </View>
      //   `,
      //   targets: [makeTargetPath('view/group/child1')],
      //   wantCode: `
      //     <View data-uid='view' style={{ width: 400, height: 10, position: 'absolute', left: 0, top: 0 }} />
      //   `,
      //   wantSelection: [makeTargetPath('view')],
      // },
      // {
      //   name: 'recursively delete empty parents when groups or fragments',
      //   input: `
      //   <div data-uid='root'>
      //     <Group data-uid='g1'>
      //       <Group data-uid='g2'>
      //         <React.Fragment data-uid='f1'>
      //           <div data-uid='child' />
      //         </React.Fragment>
      //       </Group>
      //     </Group>
      //   </div>
      //   `,
      //   targets: [makeTargetPath(`root/g1/g2/f1/child`)],
      //   wantCode: `
      //     <div data-uid='root' style={{ width: 400, height: 0, position: 'absolute', left: 0, top: 0 }} />
      //   `,
      //   wantSelection: [makeTargetPath(`root`)],
      // },
      {
        name: 'recursively delete empty parents when groups or fragments and stops',
        input: `
        <div data-uid='root'>
          <Group data-uid='g1'>
            <div data-uid='stop-here' />
            <Group data-uid='g2'>
              <React.Fragment data-uid='f1'>
                <div data-uid='child' />
              </React.Fragment>
            </Group>
          </Group>
        </div>
        `,
        targets: [makeTargetPath(`root/g1/g2/f1/child`)],
        wantCode: `
          <div data-uid='root'>
            <Group data-uid='g1'>
              <div data-uid='stop-here' />
            </Group>
          </div>
        `,
        wantSelection: [makeTargetPath(`root/g1/stop-here`)],
      },
      // {
      //   name: 'recursively delete empty parents when groups or fragments with multiselect',
      //   input: `
      //   <div data-uid='root'>
      //     <Group data-uid='g1'>
      //       <div data-uid='delete-me' />
      //       <Group data-uid='g2'>
      //         <React.Fragment data-uid='f1'>
      //           <div data-uid='child' />
      //         </React.Fragment>
      //       </Group>
      //     </Group>
      //   </div>
      //   `,
      //   targets: [makeTargetPath(`root/g1/g2/f1/child`), makeTargetPath(`root/g1/delete-me`)],
      //   wantCode: `
      //     <div data-uid='root' style={{ width: 400, height: 0, position: 'absolute', left: 0, top: 0 }} />
      //   `,
      //   wantSelection: [makeTargetPath(`root`)],
      // },
    ]
    tests.forEach((tt, idx) => {
      it(`(${idx + 1}) ${tt.name}`, async () => {
        const got = await deleteFromScene(tt.input, tt.targets)
        expect(got.code).toEqual(makeTestProjectCodeWithSnippet(tt.wantCode))
        expect(got.selection).toEqual(tt.wantSelection)
      })
    })

    it('can delete render props from an element in a map expression', async () => {
      const editor = await renderTestEditorWithModel(
        createModifiedProject({
          [StoryboardFilePath]: `import * as React from 'react'
        import * as Utopia from 'utopia-api'
        import { Storyboard, Scene } from 'utopia-api'
        
        export function Card({ header, children }) {
          return (
            <div data-uid='dbc'>
              <h2 data-uid='4b3'>{header}</h2>
              {children}
            </div>
          )
        }
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              style={{
                width: 521,
                height: 266,
                position: 'absolute',
                left: 717,
                top: 597,
                backgroundColor: 'white',
              }}
              data-uid='scene'
              data-testid='scene'
              commentId='sce'
            >
              {
                // @utopia/uid=map
                ["test"].map(() => (
                  <Card
                    data-uid='card'
                    header={<h3 data-uid='render-prop-element'>woot</h3>}
                  >
                    <p data-uid='child-element'>Card contents</p>
                  </Card>
                ))
              }
            </Scene>
          </Storyboard>
        )
        
      `,
          ['/utopia/components.utopia.js']: `import { Card } from './storyboard'

          const Components = {
            '/utopia/storyboard': {
              Card: {
                component: Card,
                properties: {
                  header: {
                    control: 'jsx',
                  },
                },
                variants: [],
              },
            },
          }
          
          export default Components          
      `,
        }),
        'await-first-dom-report',
      )

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/map',
        'regular-sb/scene/map/card~~~1',
        'render-prop-sb/scene/map/card~~~1/prop-label-header-header',
        'render-prop-value-sb/scene/map/card~~~1/render-prop-element-header',
        'render-prop-sb/scene/map/card~~~1/prop-label-children-children',
        'regular-sb/scene/map/card~~~1/child-element',
      ])

      const renderPropEntry = editor.renderedDOM.getByTestId(
        'NavigatorItemTestId-renderpropvalue_sb/scene/map/card~~~1/render_prop_element_header',
      )

      await mouseClickAtPoint(renderPropEntry, { x: 2, y: 2 })

      await pressKey('Backspace')

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/map',
        'regular-sb/scene/map/card~~~1',
        'render-prop-sb/scene/map/card~~~1/prop-label-header-header',
        'slot_sb/scene/map/card~~~1/prop-label-header',
        'render-prop-sb/scene/map/card~~~1/prop-label-children-children',
        'regular-sb/scene/map/card~~~1/child-element',
      ])
    })
  })

  describe('PASTE_JSX_ELEMENTS', () => {
    const clipboardMock = new MockClipboardHandlers().mock()

    async function runPaste(editor: EditorRenderResult) {
      const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      await clipboardMock.pasteDone
      await editor.getDispatchFollowUpActionsFinished()
    }

    type PasteTest = {
      name: string
      startingCode: string
      elements: (renderResult: EditorRenderResult) => Array<ElementPaste>
      pasteInto: InsertionPath
      want: string
      generatesSaveCount?: number
    }
    const tests: Array<PasteTest> = [
      {
        name: 'a single element',
        startingCode: `
        <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['aaa'])),
        want: `
        <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            <div data-uid='aad' style={{ top: 0, left: 0, position: 'absolute' }}>foo</div>
        </div>
		`,
      },
      {
        name: 'multiple elements',
        startingCode: `
        <div data-uid='aaa' style={{ lineHeight: '20px' }}>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            <div data-uid='ddd'>baz</div>
        </div>
		`,
        elements: (renderResult) => {
          const fooPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
          const barPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, fooPath),
              originalElementPath: fooPath,
              importsToAdd: {},
            },
            {
              element: getElementFromRenderResult(renderResult, barPath),
              originalElementPath: barPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['aaa'])),
        want: `
        <div data-uid='aaa' style={{ lineHeight: '20px' }}>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            <div data-uid='ddd'>baz</div>
            <div data-uid='aad' style={{ top: 0, left: 0, position: 'absolute' }}>foo</div>
            <div data-uid='aah' style={{ top: 20, left: 0, position: 'absolute' }}>bar</div>
        </div>
		`,
      },
      {
        name: 'a fragment',
        startingCode: `
        <div data-uid='root' style={{ height: 90 }}>
            <div data-uid='aaa'>
                <div data-uid='bbb' style={{ height: 10 }}>foo</div>
                <div data-uid='ccc' style={{ height: 10 }}>bar</div>
            </div>
            <React.Fragment data-uid='dbc'>
                <div data-uid='ddd' style={{ height: 10 }}>hello</div>
                <div data-uid='eee' style={{ height: 10 }}>there</div>
            </React.Fragment>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'aaa'])),
        want: `
		    <div data-uid='root' style={{ height: 90 }}>
            <div data-uid='aaa'>
            <div data-uid='bbb' style={{ height: 10 }}>
              foo
            </div>
            <div data-uid='ccc' style={{ height: 10 }}>
              bar
            </div>
            <React.Fragment>
              <div
                data-uid='aaf'
                style={{
                  height: 10,
                  position: 'absolute',
                  top: 20,
                  left: 0,
                }}
              >
                hello
              </div>
              <div
                data-uid='aal'
                style={{
                  height: 10,
                  position: 'absolute',
                  top: 30,
                  left: 0,
                }}
              >
                there
              </div>
            </React.Fragment>
            </div>
            <React.Fragment>
                <div data-uid='ddd' style={{ height: 10 }}>hello</div>
                <div data-uid='eee' style={{ height: 10 }}>there</div>
            </React.Fragment>
        </div>
		`,
      },
      {
        name: 'an empty fragment',
        startingCode: `
        <div data-uid='root'>
            <div data-uid='aaa'>
                <div data-uid='bbb'>foo</div>
                <div data-uid='ccc'>bar</div>
            </div>
            <React.Fragment data-uid='dbc'></React.Fragment>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'aaa'])),
        want: `
        <div data-uid='root'>
            <div data-uid='aaa'>
                <div data-uid='bbb'>foo</div>
                <div data-uid='ccc'>bar</div>
                <React.Fragment />
            </div>
            <React.Fragment />
        </div>
		`,
      },
      {
        name: 'a conditional',
        startingCode: `
		<div data-uid='root' style={{ height: 90 }}>
            <div data-uid='aaa' style={{ height: 50 }}>
              <div data-uid='bbb'>foo</div>
              <div data-uid='ccc'>bar</div>
            </div>
            {
                // @utopia/uid=conditional
                true ? (
                    <div data-uid='ddd' style={{ height: 10 }}>true</div>
                ): (
                    <div data-uid='eee' style={{ height: 10 }}>false</div>
                )
            }
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'conditional'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'aaa'])),
        want: `
		<div data-uid='root' style={{ height: 90 }}>
            <div data-uid='aaa' style={{ height: 50 }}>
              <div data-uid='bbb'>foo</div>
              <div data-uid='ccc'>bar</div>
              {
                // @utopia/uid=conditional
                true ? (
                  <div
                    data-uid='aag'
                    style={{
                      height: 10,
                      position: 'absolute',
                      top: 50,
                      left: 0,
                    }}
                  >
                    true
                  </div>
                ) : (
                  <div data-uid='aam' style={{ height: 10 }}>false</div>
                )
              }
            </div>
            {
                // @utopia/uid=conditional
                true ? (
                    <div data-uid='ddd' style={{ height: 10 }}>true</div>
                ): (
                    <div data-uid='eee' style={{ height: 10 }}>false</div>
                )
            }
        </div>
		`,
      },
      {
        name: 'an element inside a fragment',
        startingCode: `
        <div data-uid='root'>
            <React.Fragment data-uid='dbc'>
                <div data-uid='aaa' style={{ height: 10 }}>foo</div>
            </React.Fragment>
            <div data-uid='bbb' style={{ height: 10 }}>bar</div>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])),
        want: `
        <div data-uid='root'>
              <React.Fragment>
                <div data-uid='aaa' style={{ height: 10 }}>
                  foo
                </div>
                <div
                  data-uid='aaf'
                  style={{
                    height: 10,
                    top: 10,
                    left: 0,
                    position: 'absolute',
                  }}
                >
                  bar
                </div>
              </React.Fragment>
              <div data-uid='bbb' style={{ height: 10 }}>
                bar
              </div>
            </div>
		`,
      },
      {
        name: 'multiple elements inside a fragment',
        startingCode: `
        <div data-uid='root' style={{ height: 50 }}>
            <React.Fragment data-uid='dbc'>
                <div data-uid='aaa' style={{ height: 10 }}>foo</div>
            </React.Fragment>
            <div data-uid='bbb' style={{ height: 10 }}>bar</div>
            <div data-uid='ccc' style={{ height: 10 }}>baz</div>
        </div>
		`,
        elements: (renderResult) => {
          const fooPath = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          const barPath = EP.appendNewElementPath(TestScenePath, ['root', 'ccc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, fooPath),
              originalElementPath: fooPath,
              importsToAdd: {},
            },
            {
              element: getElementFromRenderResult(renderResult, barPath),
              originalElementPath: barPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])),
        want: `
        <div data-uid='root' style={{ height: 50 }}>
        <React.Fragment>
          <div data-uid='aaa' style={{ height: 10 }}>foo</div>
          <div
          data-uid='aaf'
          style={{
            height: 10,
            top: 10,
            left: 0,
            position: 'absolute',
          }}
          >
            bar
          </div>
          <div
            data-uid='aal'
            style={{
              height: 10,
              top: 20,
              left: 0,
              position: 'absolute',
            }}
          >
            baz
          </div>
        </React.Fragment>
        <div data-uid='bbb' style={{ height: 10 }}>
          bar
        </div>
        <div data-uid='ccc' style={{ height: 10 }}>
          baz
        </div>
      </div>
		`,
      },
      {
        name: 'an element inside an empty conditional branch (true)',
        startingCode: `
        <div data-uid='root'>
        	{
                // @utopia/uid=conditional
                true ? null : <div data-uid='aaa'>foo</div>
            }
            <div data-uid='bbb'>bar</div>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'true-case',
          replaceWithSingleElement(),
        ),
        want: `
        <div data-uid='root'>
            {
              // @utopia/uid=conditional
              true ? (
                <div
                  data-uid='aad'
                  style={{
                    top: 0,
                    left: 0,
                    position: 'absolute',
                  }}
                >
                  bar
                </div>
              ) : (
                <div data-uid='aaa'>foo</div>
              )
            }
            <div data-uid='bbb'>bar</div>
        </div>
		`,
      },
      {
        name: 'an element inside an empty conditional branch (false)',
        startingCode: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? <div data-uid='aaa' style={{ height: 10 }}>foo</div> : null
            }
            <div data-uid='bbb' style={{ height: 10 }}>bar</div>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'false-case',
          replaceWithSingleElement(),
        ),
        want: `
        <div data-uid='root'>
        {
          // @utopia/uid=conditional
          true ? (
            <div data-uid='aaa' style={{ height: 10 }}>
              foo
            </div>
          ) : (
            <div
              data-uid='aaf'
              style={{
                height: 10,
                top: 10,
                left: 0,
                position: 'absolute',
              }}
            >
              bar
            </div>
          )
        }
        <div data-uid='bbb' style={{ height: 10 }}>
          bar
        </div>
      </div>

		`,
      },
      {
        name: 'multiple elements into an empty conditional branch (true)',
        startingCode: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? null : <div data-uid='aaa' style={{ height: 10 }}>foo</div>
            }
            <div data-uid='bbb' style={{ height: 10 }}>bar</div>
            <div data-uid='ccc' style={{ height: 10 }}>baz</div>
        </div>
		`,
        elements: (renderResult) => {
          const barPath = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          const bazPath = EP.appendNewElementPath(TestScenePath, ['root', 'ccc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, barPath),
              originalElementPath: barPath,
              importsToAdd: {},
            },
            {
              element: getElementFromRenderResult(renderResult, bazPath),
              originalElementPath: bazPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'true-case',
          replaceWithSingleElement(),
        ),
        want: `
        <div data-uid='root'>
        {
          // @utopia/uid=conditional
          true ? (
            <React.Fragment>
              <div
                data-uid='aaf'
                style={{
                  height: 10,
                  top: 0,
                  left: 0,
                  position: 'absolute',
                }}
              >
                bar
              </div>
              <div
                data-uid='aal'
                style={{
                  height: 10,
                  top: 10,
                  left: 0,
                  position: 'absolute',
                }}
              >
                baz
              </div>
            </React.Fragment>
          ) : (
            <div data-uid='aaa' style={{ height: 10 }}>
              foo
            </div>
          )
        }
        <div data-uid='bbb' style={{ height: 10 }}>
          bar
        </div>
        <div data-uid='ccc' style={{ height: 10 }}>
          baz
        </div>
      </div>
		`,
      },
      {
        name: 'multiple elements into an empty conditional branch (false)',
        startingCode: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? <div data-uid='aaa' style={{ height: 10 }}>foo</div> : null
            }
            <div data-uid='bbb' style={{ height: 10 }}>bar</div>
            <div data-uid='ccc' style={{ height: 10 }}>baz</div>
        </div>
		`,
        elements: (renderResult) => {
          const barPath = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          const bazPath = EP.appendNewElementPath(TestScenePath, ['root', 'ccc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, barPath),
              originalElementPath: barPath,
              importsToAdd: {},
            },
            {
              element: getElementFromRenderResult(renderResult, bazPath),
              originalElementPath: bazPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'false-case',
          wrapInFragmentAndAppendElements('wrapper-fragment'),
        ),
        want: `
        <div data-uid='root'>
          {
            // @utopia/uid=conditional
            true ? (
              <div data-uid='aaa' style={{ height: 10 }}>
                foo
              </div>
            ) : (
              <React.Fragment>
                <div
                  data-uid='aaf'
                  style={{
                    height: 10,
                    top: 10,
                    left: 0,
                    position: 'absolute',
                  }}
                >
                  bar
                </div>
                <div
                  data-uid='aal'
                  style={{
                    height: 10,
                    top: 20,
                    left: 0,
                    position: 'absolute',
                  }}
                >
                  baz
                </div>
              </React.Fragment>
            )
          }
        <div data-uid='bbb' style={{ height: 10 }}>
          bar
        </div>
        <div data-uid='ccc' style={{ height: 10 }}>
          baz
        </div>
      </div>
		`,
      },
      {
        name: 'a fragment inside an empty conditional branch',
        startingCode: `
        <div data-uid='root' style={{lineHeight: '20px'}}>
            {
                // @utopia/uid=conditional
                true ? null : <div data-uid='aaa'>foo</div>
            }
            <React.Fragment data-uid='dbc'>
            	<div data-uid='bbb'>bar</div>
                <div data-uid='ccc'>baz</div>
            </React.Fragment>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'true-case',
          replaceWithSingleElement(),
        ),
        want: `
        <div data-uid='root' style={{lineHeight: '20px'}}>
            {
                // @utopia/uid=conditional
                true ? (
                    <React.Fragment>
                    	<div
                        data-uid='aad'
                        style={{
                          position: 'absolute',
                          top: 0,
                          left: 0,
                        }}
                      >
                        bar
                      </div>
                      <div
                        data-uid='aah'
                        style={{
                          position: 'absolute',
                          top: 20,
                          left: 0,
                        }}
                      >
                        baz
                      </div>
                    </React.Fragment>
                ) : <div data-uid='aaa'>foo</div>
            }
            <React.Fragment>
                <div data-uid='bbb'>bar</div>
                <div data-uid='ccc'>baz</div>
            </React.Fragment>
        </div>
		`,
      },
      {
        name: 'multiple fragments inside an empty conditional branch',
        startingCode: `
        <div data-uid='root' style={{lineHeight: '20px'}}>
            {
                // @utopia/uid=conditional
                true ? null : <div data-uid='aaa'>foo</div>
            }
            <React.Fragment data-uid='dbc'>
            	<div data-uid='bbb'>bar</div>
                <div data-uid='ccc'>baz</div>
            </React.Fragment>
            <React.Fragment data-uid='c69'>
                <div data-uid='ddd'>qux</div>
                <div data-uid='eee'>waldo</div>
            </React.Fragment>
        </div>
		`,
        elements: (renderResult) => {
          const firstPath = EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])
          const secondPath = EP.appendNewElementPath(TestScenePath, ['root', 'c69'])
          return [
            {
              element: getElementFromRenderResult(renderResult, firstPath),
              originalElementPath: firstPath,
              importsToAdd: {},
            },
            {
              element: getElementFromRenderResult(renderResult, secondPath),
              originalElementPath: secondPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'true-case',
          replaceWithSingleElement(),
        ),
        want: `
      <div data-uid='root' style={{lineHeight: '20px'}}>
      {
        // @utopia/uid=conditional
        true ? (
          <React.Fragment>
            <React.Fragment>
              <div
              data-uid='aad'
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
              }}
            >
              bar
            </div>
            <div
              data-uid='aah'
              style={{
                position: 'absolute',
                top: 20,
                left: 0,
              }}
            >
              baz
            </div>
          </React.Fragment>
          <React.Fragment>
            <div
              data-uid='aam'
              style={{
                position: 'absolute',
                top: 40,
                left: 0,
              }}
            >
              qux
            </div>
            <div
              data-uid='aaq'
              style={{
                position: 'absolute',
                top: 60,
                left: 0,
              }}
            >
              waldo
            </div>
          </React.Fragment>
          </React.Fragment>
        ) : <div data-uid='aaa'>foo</div>
      }
      <React.Fragment>
        <div data-uid='bbb'>bar</div>
        <div data-uid='ccc'>baz</div>
      </React.Fragment>
      <React.Fragment>
        <div data-uid='ddd'>qux</div>
        <div data-uid='eee'>waldo</div>
      </React.Fragment>
    </div>
		`,
      },
      {
        name: 'an active conditional branch',
        startingCode: `
    <div data-uid='root'>
      {
        // @utopia/uid=conditional
        true ? <div data-uid='aaa'>foo</div> : null
      }
    </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'conditional', 'aaa'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root'])),
        want: `
      <div data-uid='root'>
        {
          // @utopia/uid=conditional
          true ? <div data-uid='aaa'>foo</div> : null
        }
        <div data-uid='aad' style={{ top: 0, left: 0, position: 'absolute' }}>foo</div>
      </div>
		`,
      },
      {
        name: 'a flex container',
        startingCode: `
        <React.Fragment data-uid='fragment'>
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 230,
          top: 207,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 52.5,
          padding: '27px 69px',
        }}
        data-uid='flex-container'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 90,
            height: 71,
            contain: 'layout',
          }}
          data-uid='717'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 48,
            height: 79,
            contain: 'layout',
          }}
          data-uid='ca7'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 60,
            height: 101,
            contain: 'layout',
          }}
          data-uid='b41'
        />
      </div>
      <div
        style={{
          backgroundColor: '#b2a0cf',
          position: 'absolute',
          left: 346,
          top: 408,
          width: 162,
          height: 67,
        }}
        data-uid='element-to-paste'
      />
      </React.Fragment>
		`,
        elements: (renderResult) => {
          const path = EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:fragment/element-to-paste`,
          )
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: childInsertionPath(
          EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:fragment/flex-container`,
          ),
        ),
        want: `
        <React.Fragment>
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 230,
          top: 207,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 52.5,
          padding: '27px 69px',
        }}
        data-uid='flex-container'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 90,
            height: 71,
            contain: 'layout',
          }}
          data-uid='717'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 48,
            height: 79,
            contain: 'layout',
          }}
          data-uid='ca7'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 60,
            height: 101,
            contain: 'layout',
          }}
          data-uid='b41'
        />
        <div
          style={{
            backgroundColor: '#b2a0cf',
            contain: 'layout',
            width: 162,
            height: 67,
          }}
          data-uid='ele'
        />
      </div>
      <div
        style={{
          backgroundColor: '#b2a0cf',
          position: 'absolute',
          left: 346,
          top: 408,
          width: 162,
          height: 67,
        }}
        data-uid='element-to-paste'
      />
      </React.Fragment>
		`,
      },
      {
        name: 'a conditional clause with an element that doesnt support children',
        startingCode: `
        <div data-uid='root' style={{ height: 100 }}>
          {
            // @utopia/uid=conditional
            true ? <img data-uid='aaa' style={{ height: 20 }} /> : null
          }
          <div data-uid='bbb' style={{ height: 20 }}>bar</div>
          <div data-uid='ccc' style={{ height: 20 }}>baz</div>
        </div>
        `,
        elements: (renderResult) => {
          const path1 = EP.appendNewElementPath(TestScenePath, ['root', 'bbb'])
          const path2 = EP.appendNewElementPath(TestScenePath, ['root', 'ccc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path1),
              originalElementPath: path1,
              importsToAdd: {},
            },
            {
              element: getElementFromRenderResult(renderResult, path2),
              originalElementPath: path2,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: conditionalClauseInsertionPath(
          EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
          'true-case',
          wrapInFragmentAndAppendElements('wrapper-fragment'),
        ),
        want: `
        <div data-uid='root' style={{ height: 100 }}>
        {
          // @utopia/uid=conditional
          true ? (
            <React.Fragment>
              <div
                data-uid='aaf'
                style={{
                  height: 20,
                  top: -10,
                  left: 0,
                  position: 'absolute',
                }}
              >
                bar
              </div>
              <div
                data-uid='aal'
                style={{
                  height: 20,
                  top: 10,
                  left: 0,
                  position: 'absolute',
                }}
              >
                baz
              </div>
              <img data-uid='aaa' style={{ height: 20 }} />
            </React.Fragment>
          ) : null
        }
        <div data-uid='bbb' style={{ height: 20 }}>
          bar
        </div>
        <div data-uid='ccc' style={{ height: 20 }}>
          baz
        </div>
      </div>
		`,
      },
      {
        name: 'into a group',
        startingCode: `
            <div data-uid='root'>
              <div data-uid='foo' style={{ width: 50, height: 50, background: 'blue', position: 'absolute', left: 200, top: 200 }} />
              <Group data-uid='group' style={{ background: 'yellow' }}>
                <div data-uid='bar' style={{ width: 10, height: 10, background: 'red', position: 'absolute', top: 0, left: 0 }} />
                <div data-uid='baz' style={{ width: 10, height: 10, background: 'red', position: 'absolute', top: 100, left: 20 }} />
              </Group>
            </div>
          `,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'foo'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        generatesSaveCount: 2,
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'group'])),
        want: `
            <div data-uid='root'>
              <div data-uid='foo' style={{ width: 50, height: 50, background: 'blue', position: 'absolute', left: 200, top: 200 }} />
              <Group data-uid='group' style={{ background: 'yellow' }}>
                <div data-uid='bar' style={{ width: 10, height: 10, background: 'red', position: 'absolute', top: 0, left: 0 }} />
                <div data-uid='baz' style={{ width: 10, height: 10, background: 'red', position: 'absolute', top: 100, left: 20 }} />
                <div data-uid='aai' style={{ width: 50, height: 50, background: 'blue', position: 'absolute', left: 200, top: 200 }} />
              </Group>
            </div>
          `,
      },
    ]
    tests.forEach((test, i) => {
      it(`${i + 1}/${tests.length} ${test.name}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(test.startingCode),
          'await-first-dom-report',
        )

        const copiedPaths = test.elements(renderResult).map((e) => e.originalElementPath)
        await selectComponentsForTest(renderResult, copiedPaths)

        await pressKey('c', { modifiers: cmdModifier })

        if (test.pasteInto.type === 'CHILD_INSERTION') {
          await selectComponentsForTest(renderResult, [test.pasteInto.intendedParentPath])
        } else if (test.pasteInto.type === 'CONDITIONAL_CLAUSE_INSERTION') {
          const conditional = maybeConditionalExpression(
            MetadataUtils.findElementByElementPath(
              renderResult.getEditorState().editor.jsxMetadata,
              test.pasteInto.intendedParentPath,
            ),
          )!

          const targetUid =
            test.pasteInto.clause === 'true-case'
              ? conditional.whenTrue.uid
              : test.pasteInto.clause === 'false-case'
              ? conditional.whenFalse.uid
              : assertNever(test.pasteInto.clause)

          const targetPath = EP.appendToPath(test.pasteInto.intendedParentPath, targetUid)
          await selectComponentsForTest(renderResult, [targetPath])
        } else {
          assertNever(test.pasteInto)
        }

        const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

        await expectSingleUndoNSaves(renderResult, test.generatesSaveCount ?? 2, async () => {
          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()
        })

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(test.want),
        )
      })
    })

    it('can copy-paste end-to-end', async () => {
      const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='bbb'>
            <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
            <div data-uid='ddd' style={{width: 60, height: 60}} />
          </div>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [makeTargetPath('aaa/bbb')])
      await pressKey('c', { modifiers: cmdModifier })

      await selectComponentsForTest(renderResult, [makeTargetPath('aaa')])

      const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
      await renderResult.getDispatchFollowUpActionsFinished()
      await pressKey('Esc')

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`<div
            data-uid='aaa'
            style={{ contain: 'layout', width: 300, height: 300 }}
          >
            <div data-uid='bbb'>
              <div
                data-uid='ccc'
                style={{
                  position: 'absolute',
                  left: 20,
                  top: 50,
                  bottom: 150,
                  width: 100,
                }}
              />
              <div
                data-uid='ddd'
                style={{ width: 60, height: 60 }}
              />
            </div>
            <div data-uid='aar' style={{ top: 0, left: 0, position: 'absolute' }}>
              <div
                data-uid='aai'
                style={{
                  position: 'absolute',
                  left: 20,
                  top: 50,
                  bottom: 150,
                  width: 100,
                }}
              />
              <div
                data-uid='aao'
                style={{ width: 60, height: 60 }}
              />
            </div>
          </div>
  `),
      )
    })

    it('can copy-paste an expression end-to-end', async () => {
      const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          {
            // @utopia/uid=d54
            (() => (<div data-uid='bbb'>
              <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
              <div data-uid='ddd' style={{width: 60, height: 60}} />
            </div>))()
          }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [makeTargetPath('aaa/d54')])
      await pressKey('c', { modifiers: cmdModifier })

      await selectComponentsForTest(renderResult, [makeTargetPath('aaa')])

      const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
      await renderResult.getDispatchFollowUpActionsFinished()
      await pressKey('Esc')

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`<div
            data-uid='aaa'
            style={{ contain: 'layout', width: 300, height: 300 }}
          >
            {
              // @utopia/uid=d54
              (() => (
                <div data-uid='bbb'>
                  <div
                    data-uid='ccc'
                    style={{
                      position: 'absolute',
                      left: 20,
                      top: 50,
                      bottom: 150,
                      width: 100,
                    }}
                  />
                  <div
                    data-uid='ddd'
                    style={{ width: 60, height: 60 }}
                  />
                </div>
              ))()
            }
            {
              // @utopia/uid=d54
              (() => (
                <div data-uid='aas'>
                  <div
                    data-uid='aaj'
                    style={{
                      position: 'absolute',
                      left: 20,
                      top: 50,
                      bottom: 150,
                      width: 100,
                    }}
                  />
                  <div
                    data-uid='aap'
                    style={{ width: 60, height: 60 }}
                  />
                </div>
              ))()
            }
          </div>
  `),
      )
    })

    it('copy-paste into an expression pastes as sibling', async () => {
      const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='bbb'>
            {(() => (<div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />))()}
          </div>
          <div data-uid='ddd' style={{width: 60, height: 60}} />
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [makeTargetPath('aaa/ddd')])
      await pressKey('c', { modifiers: cmdModifier })

      const childrenOfBBB = MetadataUtils.getChildrenOrdered(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.elementPathTree,
        makeTargetPath('aaa/bbb'),
      )

      await selectComponentsForTest(renderResult, [childrenOfBBB[0].elementPath])

      const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
      await renderResult.getDispatchFollowUpActionsFinished()
      await pressKey('Esc')

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`<div
            data-uid='aaa'
            style={{ contain: 'layout', width: 300, height: 300 }}
          >
            <div data-uid='bbb'>
              {(() => (
                <div
                  data-uid='ccc'
                  style={{
                    position: 'absolute',
                    left: 20,
                    top: 50,
                    bottom: 150,
                    width: 100,
                  }}
                />
              ))()}  
              <div
                data-uid='aaf'
                style={{
                  width: 60,
                  height: 60,
                  top: 0,
                  left: 0,
                  position: 'absolute',
                }}
               />
            </div>
            <div
              data-uid='ddd'
              style={{ width: 60, height: 60 }}
            />
          </div>
  `),
      )
    })

    it('pasting a fragment into a different file imports React', async () => {
      const editor = await renderTestEditorWithModel(
        createTestProjectWithMultipleFiles({
          [StoryboardFilePath]: `
            import * as React from 'react'
            import { Scene, Storyboard } from 'utopia-api'
            import { Playground } from '/src/playground.js'
            
            export var storyboard = (
              <Storyboard data-uid='sb'>
                <Scene
                  style={{
                    width: 700,
                    height: 759,
                    position: 'absolute',
                    left: 212,
                    top: 128,
                  }}
                  data-label='Playground'
                  data-uid='scene-1'
                >
                  <Playground style={{}} data-uid='playground' />
                </Scene>
                <Scene
                  style={{
                    position: 'absolute',
                    left: 201.5,
                    top: 125,
                    width: 325,
                    height: 350,
                  }}
                  data-uid='scene-2'
                >
                  <React.Fragment data-uid='fragment'>
                    <div
                      style={{
                        backgroundColor: '#aaaaaa33',
                        position: 'absolute',
                        left: 37.5,
                        top: 64,
                        width: 204,
                        height: 67,
                      }}
                      data-uid='fc-1'
                    />
                    <div
                      style={{
                        backgroundColor: '#aaaaaa33',
                        position: 'absolute',
                        left: 37.5,
                        top: 148,
                        width: 204,
                        height: 54,
                      }}
                      data-uid='fc-2'
                    />
                  </React.Fragment>
                </Scene>
              </Storyboard>
            )
            `,
          [PlaygroundFilePath]: `            
            export var Playground = () => {
              return (
                <div
                  style={{
                    height: '100%',
                    width: '100%',
                    contain: 'layout',
                  }}
                  data-uid='pg-root'
                >
                  <div
                    style={{
                      height: 300,
                      position: 'absolute',
                      width: 300,
                      left: 154,
                      top: 134,
                      backgroundColor: '#ff7262',
                    }}
                    data-uid='pg-container'
                  />
                </div>
              )
            }
            
            `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/scene-2/fragment')])

      await pressKey('c', { modifiers: cmdModifier })

      await selectComponentsForTest(editor, [
        EP.fromString('sb/scene-1/playground:pg-root/pg-container'),
      ])

      const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

      FOR_TESTS_setNextGeneratedUids(['child1', 'child2', 'parent'])

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
      await editor.getDispatchFollowUpActionsFinished()

      await pressKey('Esc')
      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(editor.getEditorState(), PlaygroundFilePath))
        .toEqual(`import * as React from 'react'
export var Playground = () => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='pg-root'
    >
      <div
        style={{
          height: 300,
          position: 'absolute',
          width: 300,
          left: 154,
          top: 134,
          backgroundColor: '#ff7262',
        }}
        data-uid='pg-container'
      >
        <React.Fragment>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 48,
              top: 81,
              width: 204,
              height: 67,
            }}
            data-uid='fc-'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 48,
              top: 165,
              width: 204,
              height: 54,
            }}
            data-uid='aao'
          />
        </React.Fragment>
      </div>
    </div>
  )
}
`)
    })

    it('pasting back into original parent pastes into the right position', async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -65,
        top: 221,
        width: 451,
        height: 439,
      }}
      data-uid='container'
      data-testid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 11,
          top: 11,
          width: 202,
          height: 223,
        }}
        data-uid='child'
      />
    </div>
  </Storyboard>
)
`,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString(`sb/container/child`)])
      await pressKey('c', { modifiers: cmdModifier })

      await selectComponentsForTest(editor, [EP.fromString(`sb/container`)])
      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('container')
      const divBounds = div.getBoundingClientRect()
      const divCorner = {
        x: divBounds.x + 5,
        y: divBounds.y + 4,
      }

      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        divCorner,
        windowPoint({ x: 300, y: 300 }),
      )

      const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
      await editor.getDispatchFollowUpActionsFinished()

      await pressKey('Esc')
      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 235,
        top: 521,
        width: 451,
        height: 439,
      }}
      data-uid='container'
      data-testid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 11,
          top: 11,
          width: 202,
          height: 223,
        }}
        data-uid='child'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 125,
          top: 108,
          width: 202,
          height: 223,
        }}
        data-uid='chi'
      />
    </div>
  </Storyboard>
)
`)
    })

    describe('pasting component instances', () => {
      const project = `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      
      const App = (props) => (
        <div style={{ ...props.style }} data-uid='app-root'>
          <ThisComponent data-uid='component-1' />
          <ThisComponent data-uid='component-2' />
        </div>
      )
      
      const ThisComponent = (props) => (
        <div style={{ ...props.style }} data-uid='custom-root'>
          <div data-uid='hello-1' data-testid='target'>
            Hello there 1!
          </div>
          <div data-uid='aap'>Hello there 2!</div>
          <div data-uid='aat'>Hello there 3!</div>
        </div>
      )
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='scene'
            style={{ width: 432, height: 356, left: 98, top: 36 }}
          >
            <App
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 34,
                top: 31,
                width: 346,
                height: 223,
              }}
              data-uid='app'
            />
          </Scene>
        </Storyboard>
      )
      `

      it('cannot paste a component instance into its own definition', async () => {
        const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

        /**
         * Opening the component in the navigator isn't strictly necessary, since simply selecting the target paths is enough,
         * but it makes the tests less synthetic and might be useful for debugging this test
         */
        await mouseDoubleClickAtPoint(editor.renderedDOM.getAllByText('ThisComponent')[0], {
          x: 2,
          y: 2,
        })
        expect(
          editor.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:app-root',
          'regular-sb/scene/app:app-root/component-1',
          'regular-sb/scene/app:app-root/component-1:custom-root',
          'regular-sb/scene/app:app-root/component-1:custom-root/hello-1',
          'regular-sb/scene/app:app-root/component-1:custom-root/aap',
          'regular-sb/scene/app:app-root/component-1:custom-root/aat',
          'regular-sb/scene/app:app-root/component-2',
        ])

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:app-root/component-2')])

        await pressKey('c', { modifiers: cmdModifier })

        await selectComponentsForTest(editor, [
          EP.fromString('sb/scene/app:app-root/component-1:custom-root/hello-1'),
        ])

        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')
        firePasteEvent(canvasRoot)

        await clipboardMock.pasteDone
        await editor.getDispatchFollowUpActionsFinished()

        await pressKey('Esc')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.toasts.length).toEqual(1)
        expect(editor.getEditorState().editor.toasts[0].message).toEqual(
          'Cannot insert component instance into component definition',
        )
      })

      it('cannot paste a component instance into its own definition, transitively', async () => {
        const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

        await mouseDoubleClickAtPoint(editor.renderedDOM.getAllByText('ThisComponent')[0], {
          x: 2,
          y: 2,
        })

        expect(
          editor.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:app-root',
          'regular-sb/scene/app:app-root/component-1',
          'regular-sb/scene/app:app-root/component-1:custom-root',
          'regular-sb/scene/app:app-root/component-1:custom-root/hello-1',
          'regular-sb/scene/app:app-root/component-1:custom-root/aap',
          'regular-sb/scene/app:app-root/component-1:custom-root/aat',
          'regular-sb/scene/app:app-root/component-2',
        ])

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app')])

        await pressKey('c', { modifiers: cmdModifier })

        await selectComponentsForTest(editor, [
          EP.fromString('sb/scene/app:app-root/component-1:custom-root/hello-1'),
        ])

        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')
        firePasteEvent(canvasRoot)

        await clipboardMock.pasteDone
        await editor.getDispatchFollowUpActionsFinished()

        await pressKey('Esc')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.toasts.length).toEqual(1)
        expect(editor.getEditorState().editor.toasts[0].message).toEqual(
          'Cannot insert component instance into component definition',
        )
      })

      it('cannot paste a component instance into its own definition, via a Scene', async () => {
        const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

        await mouseDoubleClickAtPoint(editor.renderedDOM.getAllByText('ThisComponent')[0], {
          x: 2,
          y: 2,
        })

        expect(
          editor.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:app-root',
          'regular-sb/scene/app:app-root/component-1',
          'regular-sb/scene/app:app-root/component-1:custom-root',
          'regular-sb/scene/app:app-root/component-1:custom-root/hello-1',
          'regular-sb/scene/app:app-root/component-1:custom-root/aap',
          'regular-sb/scene/app:app-root/component-1:custom-root/aat',
          'regular-sb/scene/app:app-root/component-2',
        ])

        await selectComponentsForTest(editor, [EP.fromString('sb/scene')])

        await pressKey('c', { modifiers: cmdModifier })

        await selectComponentsForTest(editor, [
          EP.fromString('sb/scene/app:app-root/component-1:custom-root/hello-1'),
        ])

        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')
        firePasteEvent(canvasRoot)

        await clipboardMock.pasteDone
        await editor.getDispatchFollowUpActionsFinished()

        await pressKey('Esc')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.toasts.length).toEqual(1)
        expect(editor.getEditorState().editor.toasts[0].message).toEqual(
          'Cannot insert component instance into component definition',
        )
      })
    })

    describe('repeated paste', () => {
      async function pasteNTimes(editor: EditorRenderResult, n: number) {
        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

        for (let counter = 0; counter < n; counter += 1) {
          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await editor.getDispatchFollowUpActionsFinished()

          clipboardMock.resetDoneSignal()
        }
      }

      it('repeated paste in autolayout', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid='root'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 38,
                top: 12,
                width: 802,
                height: 337,
                display: 'flex',
                flexDirection: 'row',
                padding: '74px 42px 74px 42px',
                gap: 12,
              }}
              data-uid='container'
            >
              <div
                style={{
                  backgroundColor: '#0075ff',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-uid='div'
              />
              <div
                style={{
                  backgroundColor: '#0075ff',
                  width: 50,
                  height: 50,
                }}
                data-uid='last'
              />
            </div>
          </div>`),
          'await-first-dom-report',
        )

        const targetPath = makeTargetPath('root/container/div')

        await selectComponentsForTest(editor, [targetPath])
        await pressKey('c', { modifiers: cmdModifier })

        await pasteNTimes(editor, 4)

        await pressKey('Esc')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/div',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aag',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aai',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aak',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aam',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/last',
        ])
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 38,
          top: 12,
          width: 802,
          height: 337,
          display: 'flex',
          flexDirection: 'row',
          padding: '74px 42px 74px 42px',
          gap: 12,
        }}
        data-uid='container'
      >
        <div
          style={{
            backgroundColor: '#0075ff',
            width: 100,
            height: 100,
            contain: 'layout',
          }}
          data-uid='div'
        />
        <div
          style={{
            backgroundColor: '#0075ff',
            contain: 'layout',
            width: 100,
            height: 100,
          }}
          data-uid='aag'
        />
        <div
          style={{
            backgroundColor: '#0075ff',
            contain: 'layout',
            width: 100,
            height: 100,
          }}
          data-uid='aai'
        />
        <div
          style={{
            backgroundColor: '#0075ff',
            contain: 'layout',
            width: 100,
            height: 100,
          }}
          data-uid='aak'
        />
        <div
          style={{
            backgroundColor: '#0075ff',
            contain: 'layout',
            width: 100,
            height: 100,
          }}
          data-uid='aam'
        />
        <div
          style={{
            backgroundColor: '#0075ff',
            width: 50,
            height: 50,
          }}
          data-uid='last'
        />
      </div>
    </div>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='utopia-storyboard-uid'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='scene-aaa'
      >
        <App
          data-uid='app-entity'
          style={{
            position: 'absolute',
            bottom: 0,
            left: 0,
            right: 0,
            top: 0,
          }}
        />
      </Scene>
    </Storyboard>
  )
}
`)
      })
      it('repeatedly pasting an absolute element onto the storyboard', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div
          style={{
            backgroundColor: '#92bad2',
            position: 'absolute',
            left: 199,
            top: 225,
            width: 463,
            height: 460,
          }}
          data-uid='sb'
        >
          <div
            style={{
              backgroundColor: '#f8d0b7',
              position: 'absolute',
              left: 37,
              top: 293,
              width: 106,
              height: 113,
            }}
            data-uid='container'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 185,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='ccc'
          />
        </div>`),
          'await-first-dom-report',
        )

        const targetPath = makeTargetPath('sb/ccc')

        await selectComponentsForTest(renderResult, [targetPath])
        await pressKey('c', { modifiers: cmdModifier })

        await selectComponentsForTest(renderResult, [])

        await pasteNTimes(renderResult, 4)

        await pressKey('Esc')
        await renderResult.getDispatchFollowUpActionsFinished()

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/ccc',
          'regular-utopia-storyboard-uid/aai',
          'regular-utopia-storyboard-uid/aak',
          'regular-utopia-storyboard-uid/aam',
          'regular-utopia-storyboard-uid/aao',
        ])
        expect(getPrintedUiJsCode(renderResult.getEditorState()))
          .toEqual(`import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        backgroundColor: '#92bad2',
        position: 'absolute',
        left: 199,
        top: 225,
        width: 463,
        height: 460,
      }}
      data-uid='sb'
    >
      <div
        style={{
          backgroundColor: '#f8d0b7',
          position: 'absolute',
          left: 37,
          top: 293,
          width: 106,
          height: 113,
        }}
        data-uid='container'
      />
      <div
        style={{
          backgroundColor: '#da82c9',
          position: 'absolute',
          left: 185,
          top: 33,
          width: 244,
          height: 208,
        }}
        data-uid='ccc'
      />
    </div>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='utopia-storyboard-uid'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='scene-aaa'
      >
        <App
          data-uid='app-entity'
          style={{
            position: 'absolute',
            bottom: 0,
            left: 0,
            right: 0,
            top: 0,
          }}
        />
      </Scene>
      <div
        style={{
          backgroundColor: '#da82c9',
          position: 'absolute',
          left: 598,
          top: 316,
          width: 244,
          height: 208,
        }}
        data-uid='aai'
      />
      <div
        style={{
          backgroundColor: '#da82c9',
          position: 'absolute',
          left: 852,
          top: 316,
          width: 244,
          height: 208,
        }}
        data-uid='aak'
      />
      <div
        style={{
          backgroundColor: '#da82c9',
          position: 'absolute',
          left: 1106,
          top: 316,
          width: 244,
          height: 208,
        }}
        data-uid='aam'
      />
      <div
        style={{
          backgroundColor: '#da82c9',
          position: 'absolute',
          left: 1360,
          top: 316,
          width: 244,
          height: 208,
        }}
        data-uid='aao'
      />
    </Storyboard>
  )
}
`)
      })

      it('repeatedly pasting an absolute element into a container', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div
          style={{
            backgroundColor: '#92bad2',
            position: 'absolute',
            left: 199,
            top: 225,
            width: 463,
            height: 460,
          }}
          data-uid='sb'
        >
          <div
            style={{
              backgroundColor: '#f8d0b7',
              position: 'absolute',
              left: 37,
              top: 293,
              width: 106,
              height: 113,
            }}
            data-uid='container'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 185,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='ccc'
          />
        </div>`),
          'await-first-dom-report',
        )

        const targetPath = makeTargetPath('sb/ccc')

        await selectComponentsForTest(renderResult, [targetPath])
        await pressKey('c', { modifiers: cmdModifier })

        await selectComponentsForTest(renderResult, [targetPath])

        await pasteNTimes(renderResult, 4)

        await pressKey('Esc')
        await renderResult.getDispatchFollowUpActionsFinished()

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/ccc',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aai',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aak',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aam',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aao',
        ])

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`<div
          style={{
            backgroundColor: '#92bad2',
            position: 'absolute',
            left: 199,
            top: 225,
            width: 463,
            height: 460,
          }}
          data-uid='sb'
        >
          <div
            style={{
              backgroundColor: '#f8d0b7',
              position: 'absolute',
              left: 37,
              top: 293,
              width: 106,
              height: 113,
            }}
            data-uid='container'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 185,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='ccc'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 439,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='aai'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 693,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='aak'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 947,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='aam'
          />
          <div
            style={{
              backgroundColor: '#da82c9',
              position: 'absolute',
              left: 1201,
              top: 33,
              width: 244,
              height: 208,
            }}
            data-uid='aao'
          />
        </div>`),
        )
      })
    })

    describe('paste next to multiselection', () => {
      it('paste next to multiselection of flex elements', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid='root'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 38,
                top: 12,
                width: 802,
                height: 337,
                display: 'flex',
                flexDirection: 'row',
                padding: '74px 42px 74px 42px',
                gap: 12,
              }}
              data-uid='container'
            >
              <div
                style={{
                  backgroundColor: '#0075ff',
                  width: 100,
                  height: 100,
                  contain: 'layout',
                }}
                data-uid='div'
              />
              <div
                style={{
                  backgroundColor: '#0075ff',
                  width: 50,
                  height: 50,
                }}
                data-uid='last'
              />
            </div>
          </div>`),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [
          makeTargetPath('root/container/div'),
          makeTargetPath('root/container/last'),
        ])
        await pressKey('c', { modifiers: cmdModifier })

        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

        firePasteEvent(canvasRoot)

        // Wait for the next frame
        await clipboardMock.pasteDone
        await editor.getDispatchFollowUpActionsFinished()

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 38,
            top: 12,
            width: 802,
            height: 337,
            display: 'flex',
            flexDirection: 'row',
            padding: '74px 42px 74px 42px',
            gap: 12,
          }}
          data-uid='container'
        >
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 100,
              height: 100,
              contain: 'layout',
            }}
            data-uid='div'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 50,
              height: 50,
            }}
            data-uid='last'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              contain: 'layout',
              width: 100,
              height: 100,
            }}
            data-uid='aag'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 50,
              height: 50,
            }}
            data-uid='las'
          />
        </div>
      </div>`),
        )
      })

      it('paste next to multiselection of absolute elements', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid='root'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 38,
              top: 12,
              width: 802,
              height: 337,
              padding: '74px 42px 74px 42px',
            }}
            data-uid='container'
          >
            <div
              style={{
                backgroundColor: '#0075ff',
                width: 100,
                height: 100,
                contain: 'layout',
                position: 'absolute',
                left: 42,
                top: 74,
              }}
              data-uid='div'
            />
            <div
              style={{
                backgroundColor: '#0075ff',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 154,
                top: 74,
              }}
              data-uid='last'
            />
          </div>
        </div>`),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [
          makeTargetPath('root/container/div'),
          makeTargetPath('root/container/last'),
        ])
        await pressKey('c', { modifiers: cmdModifier })

        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

        firePasteEvent(canvasRoot)

        // Wait for the next frame
        await clipboardMock.pasteDone
        await editor.getDispatchFollowUpActionsFinished()

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 38,
            top: 12,
            width: 802,
            height: 337,
            padding: '74px 42px 74px 42px',
          }}
          data-uid='container'
        >
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 100,
              height: 100,
              contain: 'layout',
              position: 'absolute',
              left: 42,
              top: 74,
            }}
            data-uid='div'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 154,
              top: 74,
            }}
            data-uid='last'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 100,
              height: 100,
              contain: 'layout',
              position: 'absolute',
              left: 214,
              top: 74,
            }}
            data-uid='aaj'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 326,
              top: 74,
            }}
            data-uid='las'
          />
        </div>
      </div>
`),
        )
      })
    })

    describe('paste into a conditional', () => {
      describe('root', () => {
        it('pastes the element below the conditional', async () => {
          const testCode = `
              <div data-uid='root'>
                {
                  // @utopia/uid=conditional
                  true ? <div data-uid='aaa' /> : null
                }
                <div data-uid='bbb'>foo</div>
              </div>
            `
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(testCode),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [makeTargetPath('root/bbb')])
          await pressKey('c', { modifiers: cmdModifier })

          await selectComponentsForTest(renderResult, [makeTargetPath('root/conditional')])

          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          await pressKey('Esc')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
                <div data-uid='root'>
                  {
                    // @utopia/uid=conditional
                    true ? <div data-uid='aaa' /> : null
                  }
                  <div data-uid='bbb'>foo</div>
                  <div data-uid='aad' style={{ top: 0, left: 0, position: 'absolute' }}>foo</div>
                </div>
              `),
          )
        })
      })
      describe('non-empty branch', () => {
        it(`when it supports children, it's inserted as a child`, async () => {
          const testCode = `
              <div data-uid='root'>
                {
                  // @utopia/uid=conditional
                  true ? <div data-uid='aaa' /> : null
                }
                <div data-uid='bbb'>foo</div>
              </div>
            `
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(testCode),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [makeTargetPath('root/bbb')])
          await pressKey('c', { modifiers: cmdModifier })

          await selectComponentsForTest(renderResult, [makeTargetPath('root/conditional/aaa')])

          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          await pressKey('Esc')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
                <div data-uid='root'>
                  {
                    // @utopia/uid=conditional
                    true ? (
                      <div data-uid='aaa'>
                        <div data-uid='aad' style={{top: 0, left: 0, position: 'absolute'}}>foo</div>
                      </div>
                    ) : null
                  }
                  <div data-uid='bbb'>foo</div>
                </div>
              `),
          )
        })
        it(`when it does not support children, it's wrapped in a fragment`, async () => {
          const testCode = `
              <div data-uid='root' style={{ height: 50 }}>
                {
                  // @utopia/uid=conditional
                  true ? <img data-uid='aaa' style={{ width: 100, height: 100 }} src='https://placekitten.com/100/100' /> : null
                }
                <div data-uid='bbb' style={{ height: 10 }}>foo</div>
              </div>
            `
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(testCode),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [makeTargetPath('root/bbb')])
          await pressKey('c', { modifiers: cmdModifier })

          await selectComponentsForTest(renderResult, [makeTargetPath('root/conditional/aaa')])

          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          await pressKey('Esc')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
            <div data-uid='root' style={{ height: 50 }}>
            {
              // @utopia/uid=conditional
              true ? (
                <React.Fragment>
                  <div
                    data-uid='aaf'
                    style={{
                      height: 10,
                      top: 45,
                      left: 0,
                      position: 'absolute',
                    }}
                  >
                    foo
                  </div>
                  <img
                    data-uid='aaa'
                    style={{ width: 100, height: 100 }}
                    src='https://placekitten.com/100/100'
                  />
                </React.Fragment>
              ) : null
            }
            <div data-uid='bbb' style={{ height: 10 }}>
              foo
            </div>
          </div>

              `),
          )
        })
      })
      describe('empty branch', () => {
        it(`replaces the slot`, async () => {
          const testCode = `
              <div data-uid='root'>
                {
                  // @utopia/uid=conditional
                  true ? <div data-uid='aaa' /> : null
                }
                <div data-uid='bbb'>foo</div>
              </div>
            `
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(testCode),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [makeTargetPath('root/bbb')])
          await pressKey('c', { modifiers: cmdModifier })

          await selectComponentsForTest(renderResult, [makeTargetPath('root/conditional/d84')])

          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          await pressKey('Esc')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
                <div data-uid='root'>
                  {
                    // @utopia/uid=conditional
                    true ? (
                      <div data-uid='aaa' />
                    ) : (
                      <div
                        data-uid='aad'
                        style={{
                          top: 0,
                          left: 0,
                          position: 'absolute',
                        }}
                      >
                        foo
                      </div>
                    )
                  }
                  <div data-uid='bbb'>foo</div>
                </div>
              `),
          )
        })

        it('applies flex props if the conditional is in a flex container', async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(`<div
            style={{
              position: 'relative',
              display: 'flex',
              flexDirection: 'column',
              gap: 114.5,
              width: 'max-content',
              height: 'max-content',
            }}
            data-uid='root'
          >
            {
              // @utopia/uid=conditional
              true ? null : null
            }
            <img
              style={{
                width: 100,
                height: 100,
                position: 'absolute',
                top: 120,
                left: 0,
              }}
              data-uid='bbb'
              data-testid='bbb'
              src='/editor/utopia-logo-white-fill.png?hash=d7275eef10f8344f4b52a4f5ba1c92e698186d61'
            />
          </div>`),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, [makeTargetPath('root/bbb')])
          await pressKey('x', { modifiers: cmdModifier })

          await selectComponentsForTest(renderResult, [makeTargetPath('root/conditional/d84')])

          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          // Wait for the next frame
          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          await pressKey('Esc')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(`
                <div
                  style={{
                    position: 'relative',
                    display: 'flex',
                    flexDirection: 'column',
                    gap: 114.5,
                    width: 'max-content',
                    height: 'max-content',
                  }}
                  data-uid='root'
                >
                  {
                    // @utopia/uid=conditional
                    true ? (
                      <img
                        style={{
                          contain: 'layout',
                          width: 100,
                          height: 100,
                        }}
                        data-uid='bbb'
                        data-testid='bbb'
                        src='/editor/utopia-logo-white-fill.png?hash=d7275eef10f8344f4b52a4f5ba1c92e698186d61'
                      />
                    ) : null
                  }
                </div>
              `),
          )

          const img = renderResult.renderedDOM.getByTestId('bbb')
          const { top, left, position } = img.style
          expect({ top, left, position }).toEqual({ left: '', top: '', position: '' })
        })
      })
      describe('pasting an element creates new layout properties for the new parent layout', () => {
        const copyPasteLayoutTestCases: Array<{
          name: string
          input: string
          targets: Array<ElementPath>
          result: string
        }> = [
          {
            name: `paste an absolute element into a flex layout`,
            input: `<div data-uid='root'>
            <div data-uid='bbb' style={{position: 'absolute', width: 50, height: 40, top: 30, left: 20}}>Hello!</div>
            <div data-uid='ccc' style={{display: 'flex'}}></div>
          </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='root'>
              <div data-uid='bbb' style={{position: 'absolute', width: 50, height: 40, top: 30, left: 20}}>Hello!</div>
          <div data-uid='ccc' style={{display: 'flex'}}>
            <div data-uid='aai' style={{contain: 'layout', width: 50, height: 40}}>Hello!</div>
          </div>
        </div>`,
          },
          {
            name: `paste an absolute element with % values into a flex layout`,
            input: `<div data-uid='root'>
              <div data-uid='bbb' style={{position: 'absolute', width: '50%', height: '20%', top: 30, left: 20}}>Hello!</div>
              <div data-uid='ccc' style={{display: 'flex'}}></div>
            </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='root'>
              <div data-uid='bbb' style={{position: 'absolute', width: '50%', height: '20%', top: 30, left: 20}}>Hello!</div>
              <div data-uid='ccc' style={{display: 'flex'}}>
                <div data-uid='aai' style={{contain: 'layout', width: 200, height: 80}}>Hello!</div>
              </div>
            </div>`,
          },
          {
            name: `paste a flex child with px size into a flex layout`,
            input: `<div data-uid='root'>
              <div data-uid='bbb' style={{display: 'flex', flexDirection: 'column'}}>
                <div data-uid='ddd' style={{width: 50, flexBasis: 60}}>Hello!</div>
              </div>
              <div data-uid='ccc' style={{display: 'flex', flexDirection: 'row'}}></div>
            </div>`,
            targets: [makeTargetPath('root/bbb/ddd')],
            result: `<div data-uid='root'>
              <div data-uid='bbb' style={{display: 'flex', flexDirection: 'column'}}>
                <div data-uid='ddd' style={{width: 50, flexBasis: 60}}>Hello!</div>
              </div>
              <div data-uid='ccc' style={{display: 'flex', flexDirection: 'row'}}>
                <div data-uid='aaf' style={{width: 50, height: 60}}>Hello!</div>
              </div>
            </div>`,
          },
          {
            name: `paste a flex child with flexGrow into a flex layout`,
            input: `<div data-uid='root'>
              <div data-uid='bbb' style={{display: 'flex', flexDirection: 'column', padding: '10px'}}>
                <div data-uid='ddd' style={{flexGrow: 1}}>
                  <div data-uid='eee' style={{width:20, height: 20}}/>
                </div>
              </div>
              <div data-uid='ccc' style={{display: 'flex', flexDirection: 'row'}}></div>
            </div>`,
            targets: [makeTargetPath('root/bbb/ddd')],
            result: `<div data-uid='root'>
              <div data-uid='bbb' style={{display: 'flex', flexDirection: 'column', padding: '10px'}}>
                <div data-uid='ddd' style={{flexGrow: 1}}>
                  <div data-uid='eee' style={{width:20, height: 20}}/>
                </div>
              </div>
              <div data-uid='ccc' style={{display: 'flex', flexDirection: 'row'}}>
                <div data-uid='aaj' style={{width: 380, height: 20}}>
                  <div data-uid='aae' style={{width:20, height: 20}}/>
                </div>
              </div>
            </div>`,
          },
          {
            name: `paste a flex child into a flow layout`,
            input: `<div data-uid='root'>
            <div data-uid='bbb' style={{ display: 'flex', padding: 15 }}>
              <div data-uid='ddd' style={{ height: '100%', flexGrow: 1 }}>
                <div data-uid='eee' style={{ width: 20, height: 20 }}/>
              </div>
            </div>
            <div data-uid='ccc' style={{ contain: 'layout' }}></div>
          </div>`,
            targets: [makeTargetPath('root/bbb/ddd')],
            result: `<div data-uid='root'>
              <div data-uid='bbb' style={{ display: 'flex', padding: 15 }}>
                <div data-uid='ddd' style={{ height: '100%', flexGrow: 1 }}>
                  <div data-uid='eee' style={{ width: 20, height: 20 }}/>
                </div>
              </div>
              <div data-uid='ccc' style={{contain: 'layout'}}>
                <div data-uid='aak' style={{ height: 20, top: -10, left: 15, position: 'absolute' }}>
                  <div data-uid='aae' style={{ width: 20, height: 20 }}/>
                </div>
              </div>
            </div>`,
          },
          {
            name: 'paste an element into an absolute layout',
            input: `    <div
            style={{
              backgroundColor: '#92bad2',
              position: 'absolute',
              left: 199,
              top: 225,
              width: 463,
              height: 460,
            }}
            data-uid="root"
          >
            <div
              style={{
                backgroundColor: '#da82c9',
                position: 'absolute',
                left: 185,
                top: 33,
                width: 244,
                height: 208,
              }}
              data-uid="ccc"
            />
            <div
              style={{
                backgroundColor: '#f8d0b7',
                position: 'absolute',
                left: 37,
                top: 42,
                width: 106,
                height: 113,
              }}
              data-uid="source"
            />
          </div>`,
            targets: [makeTargetPath('root/source')],
            result: ` <div
            style={{
              backgroundColor: '#92bad2',
              position: 'absolute',
              left: 199,
              top: 225,
              width: 463,
              height: 460,
            }}
            data-uid="root"
          >
            <div
              style={{
                backgroundColor: '#da82c9',
                position: 'absolute',
                left: 185,
                top: 33,
                width: 244,
                height: 208,
              }}
              data-uid="ccc"
            >
              <div
                style={{
                  backgroundColor: '#f8d0b7',
                  position: 'absolute',
                  left: 69,
                  top: 9,
                  width: 106,
                  height: 113,
                }}
                data-uid="sou"
              />
            </div>
            <div
              style={{
                backgroundColor: '#f8d0b7',
                position: 'absolute',
                left: 37,
                top: 42,
                width: 106,
                height: 113,
              }}
              data-uid="source"
            />
          </div>`,
          },
          {
            name: 'paste an element into an absolute layout - element will be centered',
            input: `    <div
            style={{
              backgroundColor: '#92bad2',
              position: 'absolute',
              left: 199,
              top: 225,
              width: 463,
              height: 460,
            }}
            data-uid="root"
          >
            <div
              style={{
                backgroundColor: '#da82c9',
                position: 'absolute',
                left: 185,
                top: 33,
                width: 244,
                height: 208,
              }}
              data-uid="ccc"
            />
            <div
              style={{
                backgroundColor: '#f8d0b7',
                position: 'absolute',
                left: 37,
                top: 290,
                width: 106,
                height: 113,
              }}
              data-uid="source"
            />
          </div>`,
            targets: [makeTargetPath('root/source')],
            result: ` <div
            style={{
              backgroundColor: '#92bad2',
              position: 'absolute',
              left: 199,
              top: 225,
              width: 463,
              height: 460,
            }}
            data-uid="root"
          >
            <div
              style={{
                backgroundColor: '#da82c9',
                position: 'absolute',
                left: 185,
                top: 33,
                width: 244,
                height: 208,
              }}
              data-uid="ccc"
            >
              <div
                style={{
                  backgroundColor: '#f8d0b7',
                  position: 'absolute',
                  left: 69,
                  top: 48,
                  width: 106,
                  height: 113,
                }}
                data-uid="sou"
              />
            </div>
            <div
              style={{
                backgroundColor: '#f8d0b7',
                position: 'absolute',
                left: 37,
                top: 290,
                width: 106,
                height: 113,
              }}
              data-uid="source"
            />
          </div>`,
          },
          {
            name: 'paste an absolute element into a flow layout - element will be absolute',
            input: `<div data-uid='root'>
              <div data-uid='ccc' style={{ contain: 'layout' }}>
                <div data-uid='ddd' style={{ position: 'absolute', top: 10, left: 10 }}>hi</div>
                <div data-uid='eee' style={{ width: 20, height: 20 }}/>
              </div>
              <div data-uid='bbb' style={{ position: 'absolute', top: 20, left: 50, contain: 'layout' }}>hello</div>
            </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='root'>
              <div data-uid='ccc' style={{ contain: 'layout' }}>
                <div data-uid='ddd' style={{ position: 'absolute', top: 10, left: 10 }}>hi</div>
                <div data-uid='eee' style={{ width: 20, height: 20 }}/>
                <div data-uid='aah' style={{ position: 'absolute', top: 20, left: 50, contain: 'layout' }}>hello</div>
              </div>
              <div data-uid='bbb' style={{ position: 'absolute', top: 20, left: 50, contain: 'layout' }}>hello</div>
            </div>`,
          },
          {
            name: 'trying to paste a div into a span is not allowed',
            input: `<div data-uid='root' style={{ lineHeight: '20px' }}>
                <span data-uid='ccc'>hi</span>
                <div data-uid='bbb' style={{ width: 50, height: 50, contain: 'layout' }} />
              </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='root' style={{ lineHeight: '20px' }}>
                <span data-uid='ccc'>hi</span>
                <div data-uid='bbb' style={{ width: 50, height: 50, contain: 'layout' }} />
                <div data-uid='aaf' style={{ width: 50, height: 50, contain: 'layout', top: 20, left: 0, position: 'absolute' }} />
              </div>`,
          },
          {
            name: 'it is possible to paste a h1 element into a span',
            input: `<div data-uid='root'>
                <span data-uid='ccc'>hi</span>
                <h1 data-uid='bbb'>hello</h1>
              </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='root'>
                <span data-uid='ccc'>
                  hi<h1 data-uid='aac'>hello</h1>
                </span>
                <h1 data-uid='bbb'>hello</h1>
              </div>`,
          },
          {
            name: 'paste 2 absolute elements - elements will keep their position to each other',
            input: `<div data-uid='root' style={{ contain: 'layout', width: '100%', height: '100%'}}>
              <div data-uid='ccc' style={{ contain: 'layout', position: 'absolute', top: 100, left: 100, height: 100, width: 100 }}>
                <div data-uid='ddd' style={{ position: 'absolute', top: 10, left: 10 }}>hi</div>
              </div>
              <div data-uid='hello' style={{ position: 'absolute', top: 20, left: 50, contain: 'layout' }}>hello</div>
              <div data-uid='bello' style={{ position: 'absolute', top: 30, left: 30, contain: 'layout' }}>bello</div>
            </div>`,
            targets: [makeTargetPath('root/hello'), makeTargetPath('root/bello')],
            result: `<div data-uid='root' style={{ contain: 'layout', width: '100%', height: '100%'}}>
              <div data-uid='ccc' style={{ contain: 'layout', position: 'absolute', top: 100, left: 100, height: 100, width: 100 }}>
                <div data-uid='ddd' style={{ position: 'absolute', top: 10, left: 10 }}>hi</div>
                <div data-uid='hel' style={{ position: 'absolute', top: 36, left: 44, contain: 'layout' }}>hello</div>
                <div data-uid='bel' style={{ position: 'absolute', top: 46, left: 24, contain: 'layout' }}>bello</div>
              </div>
              <div data-uid='hello' style={{ position: 'absolute', top: 20, left: 50, contain: 'layout' }}>hello</div>
              <div data-uid='bello' style={{ position: 'absolute', top: 30, left: 30, contain: 'layout' }}>bello</div>
            </div>`,
          },
        ]

        copyPasteLayoutTestCases.forEach((tt, idx) => {
          it(`(${idx + 1}) [copy] ${tt.name}`, async () => {
            const renderResult = await renderTestEditorWithCode(
              makeTestProjectCodeWithSnippet(tt.input),
              'await-first-dom-report',
            )
            await selectComponentsForTest(renderResult, tt.targets)
            await pressKey('c', { modifiers: cmdModifier })

            await selectComponentsForTest(renderResult, [makeTargetPath('root/ccc')])

            const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

            firePasteEvent(canvasRoot)

            // Wait for the next frame
            await clipboardMock.pasteDone
            await renderResult.getDispatchFollowUpActionsFinished()

            await pressKey('Esc')
            await renderResult.getDispatchFollowUpActionsFinished()

            expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
              makeTestProjectCodeWithSnippet(tt.result),
            )
          })
        })

        const cutPasteLayoutTestCases: Array<{
          name: string
          input: string
          targets: Array<ElementPath>
          result: string
        }> = [
          {
            name: `paste an absolute element into a flex layout`,
            input: `<div data-uid='root'>
            <div data-uid='bbb' style={{position: 'absolute', width: 50, height: 40, top: 30, left: 20}}>Hello!</div>
            <div data-uid='ccc' style={{display: 'flex'}}></div>
          </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='root'>
          <div data-uid='ccc' style={{display: 'flex'}}>
            <div data-uid='bbb' style={{contain: 'layout', width: 50, height: 40}}>Hello!</div>
          </div>
        </div>`,
          },
          {
            name: 'paste an element into an absolute layout',
            input: `    <div
            style={{
              backgroundColor: '#92bad2',
              position: 'absolute',
              left: 199,
              top: 225,
              width: 463,
              height: 460,
            }}
            data-uid="root"
          >
            <div
              style={{
                backgroundColor: '#da82c9',
                position: 'absolute',
                left: 185,
                top: 33,
                width: 244,
                height: 208,
              }}
              data-uid="ccc"
            />
            <div
              style={{
                backgroundColor: '#f8d0b7',
                position: 'absolute',
                left: 37,
                top: 42,
                width: 106,
                height: 113,
              }}
              data-uid="source"
            />
          </div>`,
            targets: [makeTargetPath('root/source')],
            result: ` <div
            style={{
              backgroundColor: '#92bad2',
              position: 'absolute',
              left: 199,
              top: 225,
              width: 463,
              height: 460,
            }}
            data-uid="root"
          >
            <div
              style={{
                backgroundColor: '#da82c9',
                position: 'absolute',
                left: 185,
                top: 33,
                width: 244,
                height: 208,
              }}
              data-uid="ccc"
            >
              <div
                style={{
                  backgroundColor: '#f8d0b7',
                  position: 'absolute',
                  left: 69,
                  top: 9,
                  width: 106,
                  height: 113,
                }}
                data-uid="source"
              />
            </div>
          </div>`,
          },
        ]

        cutPasteLayoutTestCases.forEach((tt, idx) => {
          it(`(${idx + 1}) [cut] ${tt.name}`, async () => {
            const renderResult = await renderTestEditorWithCode(
              makeTestProjectCodeWithSnippet(tt.input),
              'await-first-dom-report',
            )
            await selectComponentsForTest(renderResult, tt.targets)
            await pressKey('x', { modifiers: cmdModifier })

            await selectComponentsForTest(renderResult, [makeTargetPath('root/ccc')])

            const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

            firePasteEvent(canvasRoot)

            // Wait for the next frame
            await clipboardMock.pasteDone
            await renderResult.getDispatchFollowUpActionsFinished()

            await pressKey('Esc')
            await renderResult.getDispatchFollowUpActionsFinished()

            expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
              makeTestProjectCodeWithSnippet(tt.result),
            )
          })
        })

        const copyPasteToStoryboardTestCases: Array<{
          name: string
          input: string
          targets: Array<ElementPath>
          result: string
        }> = [
          {
            name: `paste an absolute element into the storyboard`,
            input: `<div data-uid='root'>
                <div data-uid='bbb' style={{position: 'absolute', width: 50, height: 40, top: 30, left: 20}}>Hello!</div>
              </div>`,
            targets: [makeTargetPath('root/bbb')],
            result: `<div data-uid='aai' style={{position: 'absolute', width: 50, height: 40, top: 400, left: 695}}>Hello!</div>`,
          },
          {
            name: `paste a flex child into the storyboard`,
            input: `<div data-uid='root'>
                <div data-uid='bbb' style={{ display: 'flex', padding: 15 }}>
                  <div data-uid='ddd' style={{ height: '100%', flexGrow: 1 }}>
                    <div data-uid='eee' style={{ width: 20, height: 20 }}/>
                  </div>
                </div>
              </div>`,
            targets: [makeTargetPath('root/bbb/ddd')],
            result: `<div data-uid='aak' style={{ height: 20, top: 410, left: 535, position: 'absolute' }}>
                <div data-uid='aae' style={{ width: 20, height: 20 }}/>
              </div>`,
          },
          {
            name: 'paste 2 absolute elements to the storyboard - elements will keep their position to each other',
            input: `<div data-uid='root' style={{ contain: 'layout', width: '100%', height: '100%'}}>
              <div data-uid='hello' style={{ position: 'absolute', top: 20, left: 50, contain: 'layout', height: 20 }}>hello</div>
              <div data-uid='bello' style={{ position: 'absolute', top: 30, left: 30, contain: 'layout', height: 20 }}>bello</div>
            </div>`,
            targets: [makeTargetPath('root/hello'), makeTargetPath('root/bello')],
            result: `<div data-uid='hel' style={{ position: 'absolute', top: 405, left: 714, contain: 'layout', height: 20 }}>hello</div>
            <div data-uid='bel' style={{ position: 'absolute', top: 415, left: 694, contain: 'layout', height: 20 }}>bello</div>`,
          },
        ]

        copyPasteToStoryboardTestCases.forEach((tt, idx) => {
          it(`(${idx + 1}) ${tt.name}`, async () => {
            const renderResult = await renderTestEditorWithCode(
              makeTestProjectCodeWithSnippet(tt.input),
              'await-first-dom-report',
            )
            await selectComponentsForTest(renderResult, tt.targets)
            await pressKey('c', { modifiers: cmdModifier })

            await selectComponentsForTest(renderResult, [EP.fromString(BakedInStoryboardUID)])

            const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

            firePasteEvent(canvasRoot)

            // Wait for the next frame
            await clipboardMock.pasteDone
            await renderResult.getDispatchFollowUpActionsFinished()

            await pressKey('Esc')
            await renderResult.getDispatchFollowUpActionsFinished()

            expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
              formatTestProjectCode(`
                import * as React from 'react'
                import { Scene, Storyboard, View, Group } from 'utopia-api'
              
                export var App = (props) => {
                  return (${tt.input})
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
                      ${tt.result}
                    </Storyboard>
                  )
                }
              `),
            )
          })
        })
      })
    })

    describe('paste into a fragment', () => {
      const template = (innards: string) => `import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'
      
      const App = () => {
        return (
          ${innards} 
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            style={{
              width: 311,
              height: 313,
              position: 'absolute',
              left: 301,
              top: 169,
            }}
            data-label='Playground'
            data-uid='scene'
          >
            <App data-uid='app' />
          </Scene>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 109,
              top: 351,
              width: 36,
              height: 35,
            }}
            data-uid='paste-me'
            data-testid='paste-me'
          />
        </Storyboard>
      )
      `

      it('into a fragment with flex layout', async () => {
        const editor = await renderTestEditorWithCode(
          template(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 29,
          top: 9,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 28.75,
          padding: '17.5px 13.5px',
        }}
        data-uid='root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 54,
            height: 42,
            contain: 'layout',
          }}
          data-uid='227'
        />
        <React.Fragment data-uid="paste-here">
          <div
            style={{
              width: 50,
              height: 50,
              backgroundColor: '#cee5ff',
              contain: 'layout',
            }}
            data-uid='aak'
          />
          <div
            style={{
              width: 50,
              height: 50,
              backgroundColor: '#cee5ff',
              contain: 'layout',
            }}
            data-uid='8d3'
          />
        </React.Fragment>
      </div>`),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/paste-me')])

        await pressKey('x', { modifiers: cmdModifier })
        await editor.getDispatchFollowUpActionsFinished()

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root/paste-here')])

        await runPaste(editor)

        const pastedElement = editor.renderedDOM.getByTestId('paste-me')
        const { top, left, position, width, height } = pastedElement.style
        expect({ top, left, position, width, height }).toEqual({
          height: '35px',
          left: '',
          position: '',
          top: '',
          width: '36px',
        })
      })

      it('into a fragment with absolute layout', async () => {
        const editor = await renderTestEditorWithCode(
          template(`<React.Fragment data-uid="root">
          <div
            style={{
              backgroundColor: '#dd416f',
              position: 'absolute',
              left: 22,
              top: 34,
              width: 73,
              height: 70,
            }}
            data-uid='182'
          />
          <div
            style={{
              backgroundColor: '#6c3e80',
              position: 'absolute',
              left: 193,
              top: 34,
              width: 65,
              height: 70,
            }}
            data-uid='0bd'
          />
        </React.Fragment>`),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/paste-me')])

        await pressKey('x', { modifiers: cmdModifier })
        await editor.getDispatchFollowUpActionsFinished()

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root')])

        await runPaste(editor)

        const pastedElement = editor.renderedDOM.getByTestId('paste-me')
        const { top, left, position, width, height } = pastedElement.style
        expect({ top, left, position, width, height }).toEqual({
          height: '35px',
          left: '122px',
          position: 'absolute',
          top: '52px',
          width: '36px',
        })
      })
    })

    describe('pasting fragments with children', () => {
      const Child1TestId = 'child-1'
      const Child2TestId = 'child-2'
      const template = (innards: string) => `import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'
      
      const App = () => {
        return (
          ${innards} 
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            style={{
              width: 311,
              height: 313,
              position: 'absolute',
              left: 301,
              top: 169,
            }}
            data-label='Playground'
            data-uid='scene'
          >
            <App data-uid='app' />
          </Scene>
          <React.Fragment data-uid='fragment'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: -475,
                top: 571,
                width: 110,
                height: 112,
              }}
              data-uid='738'
              data-testid='${Child1TestId}'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: -294,
                top: 571,
                width: 100,
                height: 112,
              }}
              data-uid='f49'
              data-testid='${Child2TestId}'
            />
        </React.Fragment>
        </Storyboard>
      )
      `

      // only the props of the fragemnt's children are asserted in the following tests, since
      // previous tests already establish that pasting an element into a container with children
      // works as intended

      it('paste into an absolute layout', async () => {
        const editor = await renderTestEditorWithCode(
          template(`
          <div
            data-uid='root'
            style={{
              width: '100%',
              height: '100%',
              position: 'relative',
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 47,
                width: 311,
                height: 189,
              }}
              data-uid='container'
            />
          </div>
        `),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/fragment')])
        await pressKey('x', { modifiers: cmdModifier })
        await editor.getDispatchFollowUpActionsFinished()

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root/container')])

        await runPaste(editor)

        {
          const { position, top, left, width, height } =
            editor.renderedDOM.getByTestId(Child1TestId).style
          expect({ position, top, left, width, height }).toEqual({
            height: '112px',
            left: '15px',
            position: 'absolute',
            top: '39px',
            width: '110px',
          })
        }

        {
          const { position, top, left, width, height } =
            editor.renderedDOM.getByTestId(Child2TestId).style
          expect({ position, top, left, width, height }).toEqual({
            height: '112px',
            left: '196px',
            position: 'absolute',
            top: '39px',
            width: '100px',
          })
        }
      })

      it('paste into a flex layout', async () => {
        const editor = await renderTestEditorWithCode(
          template(`
          <div
            data-uid='root'
            style={{
              width: '100%',
              height: '100%',
              position: 'relative',
            }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 47,
                width: 311,
                height: 189,
                display: 'flex',
                padding: 25,
                gap: 20,
                alignItems: 'center',
                justifyContent: 'center',
              }}
              data-uid='container'
            />
          </div>
        `),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/fragment')])
        await pressKey('x', { modifiers: cmdModifier })
        await editor.getDispatchFollowUpActionsFinished()

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root/container')])

        await runPaste(editor)

        {
          const { position, top, left, width, height } =
            editor.renderedDOM.getByTestId(Child1TestId).style
          expect({ position, top, left, width, height }).toEqual({
            height: '112px',
            left: '',
            position: '',
            top: '',
            width: '110px',
          })
        }

        {
          const { position, top, left, width, height } =
            editor.renderedDOM.getByTestId(Child2TestId).style
          expect({ position, top, left, width, height }).toEqual({
            height: '112px',
            left: '',
            position: '',
            top: '',
            width: '100px',
          })
        }
      })

      it('elements with relative sizing and pins are converted to visual size, with pins removed', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
        import { Scene, Storyboard } from 'utopia-api'
        const App = () => {
          return (
            <div
              data-uid='root'
              style={{
                width: '100%',
                height: '100%',
                position: 'relative',
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 0,
                  top: 47,
                  width: 311,
                  height: 189,
                  padding: 25,
                }}
                data-uid='container'
              />
            </div>
          )
        }
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              style={{
                width: 311,
                height: 313,
                position: 'absolute',
                left: 301,
                top: 169,
              }}
              data-label='Playground'
              data-uid='scene'
            >
              <App data-uid='app' />
            </Scene>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: -169,
                top: 286,
                width: 352,
                height: 145,
              }}
              data-uid='outer'
            >
              <React.Fragment data-uid='fragment'>
                <div
                  style={{
                    backgroundColor: '#ff487e',
                    position: 'absolute',
                    left: '11.42045454546%',
                    right: 221,
                    height: '33%',
                    top: 48.5,
                  }}
                  data-uid='aaa'
                  data-testid='${Child1TestId}'
                />
                <div
                  style={{
                    backgroundColor: '#42ddcf',
                    width: '20%',
                    position: 'absolute',
                    top: 30,
                    left: 188,
                    bottom: '29%',
                  }}
                  data-uid='0bd'
                  data-testid='${Child2TestId}'
                />
              </React.Fragment>
            </div>
          </Storyboard>
        )
        `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/outer/fragment')])
        await pressKey('x', { modifiers: cmdModifier })
        await editor.getDispatchFollowUpActionsFinished()

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root/container')])

        await runPaste(editor)

        {
          const { position, top, left, width, height } =
            editor.renderedDOM.getByTestId(Child1TestId).style
          expect({ position, top, left, width, height }).toEqual({
            height: '48px',
            left: '46px',
            position: 'absolute',
            top: '118.5px',
            width: '91px',
          })
        }

        {
          const { position, top, left, width, height } =
            editor.renderedDOM.getByTestId(Child2TestId).style
          expect({ position, top, left, width, height }).toEqual({
            height: '73px',
            left: '194px',
            position: 'absolute',
            top: '100px',
            width: '70.5px',
          })
        }
      })
    })

    describe('Paste to Replace', () => {
      const pasteToReplaceTestCases: Array<{
        name: string
        input: string
        copyTargets: Array<ElementPath>
        pasteTargets: Array<ElementPath>
        expectedSelectedViews: Array<ElementPath> | null
        result: string
      }> = [
        {
          name: `paste to replace an absolute element`,
          input: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black'}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='ddd' style={{position: 'absolute', width: 50, height: 40, top: 100, left: 100}}>
                <div data-uid='eee'>Hi!</div>
              </div>
            </div>`,
          copyTargets: [makeTargetPath('root/bbb')],
          pasteTargets: [makeTargetPath('root/ddd')],
          expectedSelectedViews: [makeTargetPath('root/aai')],
          result: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black'}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='aai' style={{backgroundColor: 'lavender', outline: '1px solid black', top: 100, left: 100, position: 'absolute', }}>
                <span data-uid='aac'>Hello!</span>
              </div>
            </div>`,
        },
        {
          name: `paste to replace a flex child`,
          input: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 50, height: 20}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='ddd' style={{position: 'absolute', top: 100, left: 100, display: 'flex'}}>
                <div data-uid='eee'/>
                <div data-uid='fff'>
                  <div data-uid='ggg'>Hi!</div>
                </div>
                <div data-uid='hhh'/>
              </div>
            </div>`,
          copyTargets: [makeTargetPath('root/bbb')],
          pasteTargets: [makeTargetPath('root/ddd/fff')],
          expectedSelectedViews: [makeTargetPath('root/ddd/aak')],
          result: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 50, height: 20}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='ddd' style={{position: 'absolute', top: 100, left: 100, display: 'flex'}}>
                <div data-uid='eee'/>
                <div data-uid='aak' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 50, height: 20}}>
                  <span data-uid='aac'>Hello!</span>
                </div>
                <div data-uid='hhh'/>
              </div>
            </div>`,
        },
        {
          name: `paste to replace an absolute element with multiselection`,
          input: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='ddd' style={{position: 'absolute', width: 50, height: 40, top: 100, left: 100}}>
                <div data-uid='eee'>Hi!</div>
              </div>
              <div data-uid='fff'>
                <div data-uid='ggg' style={{position: 'absolute', top: 40, left: 40, backgroundColor: 'plum', outline: '1px solid white'}}>
                  <span data-uid='hhh' style={{color: 'white'}}>second element</span>
                </div>
              </div>
            </div>`,
          copyTargets: [makeTargetPath('root/bbb'), makeTargetPath('root/fff/ggg')],
          pasteTargets: [makeTargetPath('root/ddd')],
          expectedSelectedViews: [makeTargetPath('root/aak'), makeTargetPath('root/aaz')],
          result: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='aak' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40, top: 100, left: 100, position: 'absolute' }}>
                <span data-uid='aac'>Hello!</span>
              </div>
              <div data-uid='aaz' style={{position: 'absolute', top: 140, left: 140, backgroundColor: 'plum', outline: '1px solid white'}}>
                  <span data-uid='aaq' style={{color: 'white'}}>second element</span>
                </div>
              <div data-uid='fff'>
                <div data-uid='ggg' style={{position: 'absolute', top: 40, left: 40, backgroundColor: 'plum', outline: '1px solid white'}}>
                  <span data-uid='hhh' style={{color: 'white'}}>second element</span>
                </div>
              </div>
            </div>`,
        },
        {
          name: `paste to replace multiselected absolute elements with multiselected absolute elements`,
          input: `<div data-uid='root'>
            <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
              <span data-uid='ccc'>Hello!</span>
            </div>
            <div data-uid='ddd' style={{position: 'absolute', width: 50, height: 40, top: 100, left: 100}}>
              <div data-uid='eee'>Hi!</div>
            </div>
            <div data-uid='fff' style={{position: 'absolute', top: 40, left: 40, backgroundColor: 'plum', outline: '1px solid white'}}>
              <span data-uid='ggg' style={{color: 'white'}}>second element</span>
            </div>
            <div data-uid='jjj' style={{position: 'absolute', width: 50, height: 40, top: 200, left: 200}}>
              <div data-uid='kkk'>Hi!</div>
            </div>
          </div>`,
          copyTargets: [makeTargetPath('root/bbb'), makeTargetPath('root/fff')],
          pasteTargets: [makeTargetPath('root/jjj'), makeTargetPath('root/ddd')],
          expectedSelectedViews: [
            makeTargetPath('root/aak'),
            makeTargetPath('root/aaz'),
            makeTargetPath('root/aau'),
            makeTargetPath('root/abl'),
          ],
          result: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='aau' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40, top: 100, left: 100, position: 'absolute' }}>
                <span data-uid='aaf'>Hello!</span>
              </div>
              <div data-uid='abl' style={{position: 'absolute', top: 140, left: 140, backgroundColor: 'plum', outline: '1px solid white'}}>
                <span data-uid='abc' style={{color: 'white'}}>second element</span>
              </div>
              <div data-uid='fff' style={{position: 'absolute', top: 40, left: 40, backgroundColor: 'plum', outline: '1px solid white'}}>
                <span data-uid='ggg' style={{color: 'white'}}>second element</span>
              </div>
              <div data-uid='aak' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40, top: 200, left: 200, position: 'absolute' }}>
                <span data-uid='aac'>Hello!</span>
              </div>
              <div data-uid='aaz' style={{position: 'absolute', top: 240, left: 240, backgroundColor: 'plum', outline: '1px solid white'}}>
                  <span data-uid='aaq' style={{color: 'white'}}>second element</span>
                </div>
            </div>`,
        },
        {
          name: `paste to replace an absolute element in a conditional clause`,
          input: `<div data-uid='root'>
            <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black'}}>
              <span data-uid='ccc'>Hello!</span>
            </div>
            {
              //@utopia/uid=cond
              false
              ? null
              : (
                <div data-uid='ddd' style={{position: 'absolute', width: 50, height: 40, top: 100, left: 100}}>
                  <div data-uid='eee'>Hi!</div>
                </div>
              )
            }
          </div>`,
          copyTargets: [makeTargetPath('root/bbb')],
          pasteTargets: [makeTargetPath('root/cond/ddd')],
          expectedSelectedViews: [makeTargetPath('root/cond/aai')],
          result: `<div data-uid='root'>
            <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black'}}>
              <span data-uid='ccc'>Hello!</span>
            </div>
            {
              //@utopia/uid=cond
              false
              ? null
              : (
                <div data-uid='aai' style={{backgroundColor: 'lavender', outline: '1px solid black', top: 100, left: 100, position: 'absolute'}}>
                  <span data-uid='aac'>Hello!</span>
                </div>
              )
            }
          </div>`,
        },
        {
          name: `paste to replace an absolute element in a conditional clause with multiselection`,
          input: `<div data-uid='root'>
            <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
              <span data-uid='ccc'>Hello!</span>
            </div>
            {
              //@utopia/uid=cond
              false
              ? null
              : (
                <div data-uid='ddd' style={{position: 'absolute', width: 50, height: 40, top: 100, left: 100}}>
                  <div data-uid='eee'>Hi!</div>
                </div>
              )
            }
            <div data-uid='fff'>
              <div data-uid='ggg' style={{position: 'absolute', top: 40, left: 40, backgroundColor: 'plum', outline: '1px solid white'}}>
                <span data-uid='hhh' style={{color: 'white'}}>second element</span>
              </div>
            </div>
          </div>`,
          copyTargets: [makeTargetPath('root/bbb'), makeTargetPath('root/fff/ggg')],
          pasteTargets: [makeTargetPath('root/cond/ddd')],
          expectedSelectedViews: null,
          result: `<div data-uid='root'>
            <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
              <span data-uid='ccc'>Hello!</span>
            </div>
            {
              //@utopia/uid=cond
              false
              ? null
              : (
                <React.Fragment>
                  <div data-uid='aak' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40, top: 100, left: 100, position: 'absolute' }}>
                    <span data-uid='aac'>Hello!</span>
                  </div>
                  <div data-uid='aaz' style={{position: 'absolute', top: 140, left: 140, backgroundColor: 'plum', outline: '1px solid white'}}>
                      <span data-uid='aaq' style={{color: 'white'}}>second element</span>
                    </div>
                </React.Fragment>
              )
            }
            <div data-uid='fff'>
              <div data-uid='ggg' style={{position: 'absolute', top: 40, left: 40, backgroundColor: 'plum', outline: '1px solid white'}}>
                <span data-uid='hhh' style={{color: 'white'}}>second element</span>
              </div>
            </div>
          </div>`,
        },
      ]

      pasteToReplaceTestCases.forEach((tt, idx) => {
        it(`(${idx + 1}) ${tt.name}`, async () => {
          const renderResult = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(tt.input),
            'await-first-dom-report',
          )
          await selectComponentsForTest(renderResult, tt.copyTargets)
          await pressKey('c', { modifiers: cmdModifier })

          await selectComponentsForTest(renderResult, tt.pasteTargets)
          await pressKey('v', { modifiers: shiftCmdModifier })

          // Wait for the next frame
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            makeTestProjectCodeWithSnippet(tt.result),
          )
          if (tt.expectedSelectedViews != null) {
            expect(renderResult.getEditorState().editor.selectedViews).toEqual(
              tt.expectedSelectedViews,
            )
          }
        })
      })
    })

    describe('pasting with props replaced', () => {
      it('copy pasting element with code in props', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          const App = () => {
            const width = 44
            const height = 33

            const hello = () => console.log("Hello world!")

            const colors = { backgroundColor: '#cee5ff' }

            return (
              <div
                style={{
                  position: 'absolute',
                  width: width,
                  height: height,
                  top: 100,
                  left: 100,
                  ...colors
                }}
                onClick={hello}
                data-uid='root'
              />
            )
          }
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 200,
                  height: 300,
                  position: 'absolute',
                  left: 212,
                  top: 128,
                }}
                data-label='Playground'
                data-uid='scene'
              >
                <App data-uid='app' />
              </Scene>
            </Storyboard>
          )
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        await runPaste(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = () => {
  const width = 44
  const height = 33

  const hello = () => console.log('Hello world!')

  const colors = { backgroundColor: '#cee5ff' }

  return (
    <div
      style={{
        position: 'absolute',
        width: width,
        height: height,
        top: 100,
        left: 100,
        ...colors,
      }}
      onClick={hello}
      data-uid='root'
    />
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
    <div
      style={{
        position: 'absolute',
        width: 44,
        height: 33,
        top: 404,
        left: 698,
        backgroundColor: '#cee5ff',
      }}
      onClick={undefined}
      data-uid='roo'
    />
  </Storyboard>
)
`)
      })

      it('copy element with code in child and grandchild', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          const App = () => {
            const width = 44
            const height = 33

            const hello = () => console.log("Hello world!")

            const grandParentLabel = "grandParent"
            const parentLabel = "parent"

            return (
              <div data-label={grandParentLabel} data-uid="root">
                <div data-label={parentLabel} onClick={hello} data-uid="parent">
                  <div
                    style={{
                      position: 'absolute',
                      width: width,
                      height: height,
                      top: 100,
                      left: 100,
                      backgroundColor: '#cee5ff',
                    }}
                    onClick={hello}
                    data-uid='child'
                  />
                </div>
              </div>
            )
          }
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 200,
                  height: 300,
                  position: 'absolute',
                  left: 212,
                  top: 128,
                }}
                data-label='Playground'
                data-uid='scene'
              >
                <App data-uid='app' />
              </Scene>
            </Storyboard>
          )
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        await runPaste(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = () => {
  const width = 44
  const height = 33

  const hello = () => console.log('Hello world!')

  const grandParentLabel = 'grandParent'
  const parentLabel = 'parent'

  return (
    <div data-label={grandParentLabel} data-uid='root'>
      <div
        data-label={parentLabel}
        onClick={hello}
        data-uid='parent'
      >
        <div
          style={{
            position: 'absolute',
            width: width,
            height: height,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          onClick={hello}
          data-uid='child'
        />
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
    <div
      data-label='grandParent'
      data-uid='roo'
      style={{ top: 420, left: 620, position: 'absolute' }}
    >
      <div
        data-label='parent'
        onClick={undefined}
        data-uid='par'
      >
        <div
          style={{
            position: 'absolute',
            width: 44,
            height: 33,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          onClick={undefined}
          data-uid='chi'
        />
      </div>
    </div>
  </Storyboard>
)
`)
      })

      it('copy element wrapped in fragment', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          const App = () => {
            const width = 44
            const height = 33

            const hello = () => console.log("Hello world!")

            return (
              <React.Fragment data-uid="root">
                <React.Fragment>
                  <div
                    style={{
                      position: 'absolute',
                      width: width,
                      height: height,
                      top: 100,
                      left: 100,
                      backgroundColor: '#cee5ff',
                    }}
                    onClick={hello}
                    data-uid='child'
                  />
                </React.Fragment>
              </React.Fragment>
            )
          }
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 200,
                  height: 300,
                  position: 'absolute',
                  left: 212,
                  top: 128,
                }}
                data-label='Playground'
                data-uid='scene'
              >
                <App data-uid='app' />
              </Scene>
            </Storyboard>
          )
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        await runPaste(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = () => {
  const width = 44
  const height = 33

  const hello = () => console.log('Hello world!')

  return (
    <React.Fragment>
      <React.Fragment>
        <div
          style={{
            position: 'absolute',
            width: width,
            height: height,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          onClick={hello}
          data-uid='child'
        />
      </React.Fragment>
    </React.Fragment>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
    <React.Fragment>
      <React.Fragment>
        <div
          style={{
            position: 'absolute',
            width: 44,
            height: 33,
            top: 404,
            left: 698,
            backgroundColor: '#cee5ff',
          }}
          onClick={undefined}
          data-uid='chi'
        />
      </React.Fragment>
    </React.Fragment>
  </Storyboard>
)
`)
      })

      it('copy conditional with code in the true branch', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
            import { Scene, Storyboard } from 'utopia-api'
            
            const App = () => {
              const width = 44
              const height = 33
            
              const hello = () => console.log('Hello world!')
            
              return (
                <div
                  style={{
                    position: 'absolute',
                    width: 100,
                    height: 100,
                    top: 90,
                    left: 90,
                  }}
                  data-uid='root'
                >
                  {
                    // @utopia/uid=cond
                    true ? (
                     <div
                      style={{
                        backgroundColor: '#aaaaaa33',
                        position: 'absolute',
                        left: 6,
                        top: 6,
                        width: width,
                        height: height,
                      }}
                      onClick={hello}
                      data-uid='child'
                    />
                  ) : null}
                </div>
              )
            }
            
            export var storyboard = (
              <Storyboard data-uid='sb'>
                <Scene
                  style={{
                    width: 200,
                    height: 300,
                    position: 'absolute',
                    left: 212,
                    top: 128,
                  }}
                  data-label='Playground'
                  data-uid='scene'
                >
                  <App data-uid='app' />
                </Scene>
              </Storyboard>
            )            
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root/cond`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        await runPaste(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = () => {
  const width = 44
  const height = 33

  const hello = () => console.log('Hello world!')

  return (
    <div
      style={{
        position: 'absolute',
        width: 100,
        height: 100,
        top: 90,
        left: 90,
      }}
      data-uid='root'
    >
      {
        // @utopia/uid=cond
        true ? (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 6,
              top: 6,
              width: width,
              height: height,
            }}
            onClick={hello}
            data-uid='child'
          />
        ) : null
      }
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
    {
      // @utopia/uid=cond
      true ? (
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 698,
            top: 404,
            width: 44,
            height: 33,
          }}
          onClick={undefined}
          data-uid='chi'
        />
      ) : null
    }
  </Storyboard>
)
`)
      })

      it('copy conditional with code in the false branch', async () => {
        /**
         * The gotcha here is that the false branch only has metadata if
         * the conditional is toggled to display the false branch
         */
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
            import { Scene, Storyboard } from 'utopia-api'
            
            const App = () => {
              const width = 44
              const height = 33
            
              const hello = () => console.log('Hello world!')
            
              return (
                <div
                  style={{
                    position: 'absolute',
                    width: 100,
                    height: 100,
                    top: 90,
                    left: 90,
                  }}
                  data-uid='root'
                >
                  {
                    // @utopia/uid=cond
                    // @utopia/conditional=false
                    true ? null : (
                      <div
                        style={{
                          backgroundColor: '#aaaaaa33',
                          position: 'absolute',
                          left: 6,
                          top: 6,
                          width: width,
                          height: height,
                        }}
                        onClick={hello}
                        data-uid='child'
                      />
                    )
                  }
                </div>
              )
            }
            
            export var storyboard = (
              <Storyboard data-uid='sb'>
                <Scene
                  style={{
                    width: 200,
                    height: 300,
                    position: 'absolute',
                    left: 212,
                    top: 128,
                  }}
                  data-label='Playground'
                  data-uid='scene'
                >
                  <App data-uid='app' />
                </Scene>
              </Storyboard>
            )            
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root/cond`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        await runPaste(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = () => {
  const width = 44
  const height = 33

  const hello = () => console.log('Hello world!')

  return (
    <div
      style={{
        position: 'absolute',
        width: 100,
        height: 100,
        top: 90,
        left: 90,
      }}
      data-uid='root'
    >
      {
        // @utopia/uid=cond
        // @utopia/conditional=false
        true ? null : (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 6,
              top: 6,
              width: width,
              height: height,
            }}
            onClick={hello}
            data-uid='child'
          />
        )
      }
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
    {
      // @utopia/uid=cond
      // @utopia/conditional=false
      true ? null : (
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 698,
            top: 404,
            width: 44,
            height: 33,
          }}
          onClick={undefined}
          data-uid='chi'
        />
      )
    }
  </Storyboard>
)
`)
      })
    })

    describe('toggling to pasting with props preserved', () => {
      it('copy element with code in child and grandchild', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          const width = 88
          
          const App = () => {
            const width = 44

            return (
              <div data-uid="root">
                <div data-uid="parent">
                  <div
                    style={{
                      position: 'absolute',
                      width: width,
                      height: 33,
                      top: 100,
                      left: 100,
                      backgroundColor: '#cee5ff',
                    }}
                    data-uid='child'
                  />
                </div>
              </div>
            )
          }
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 200,
                  height: 300,
                  position: 'absolute',
                  left: 212,
                  top: 128,
                }}
                data-label='Playground'
                data-uid='scene'
              >
                <App data-uid='app' />
              </Scene>
            </Storyboard>
          )
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

        firePasteEvent(canvasRoot)

        await clipboardMock.pasteDone
        await editor.getDispatchFollowUpActionsFinished()

        // open the post-action menu
        const floatingPostActionMenu = editor.renderedDOM.getByTestId(FloatingPostActionMenuTestId)
        await mouseClickAtPoint(floatingPostActionMenu, { x: 2, y: 2 })

        expect(editor.getEditorState().postActionInteractionSession?.activeChoiceId).toEqual(
          PropsReplacedPastePostActionChoiceId,
        )

        await pressKey('2')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().postActionInteractionSession?.activeChoiceId).toEqual(
          PropsPreservedPastePostActionChoiceId,
        )

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const width = 88

const App = () => {
  const width = 44

  return (
    <div data-uid='root'>
      <div data-uid='parent'>
        <div
          style={{
            position: 'absolute',
            width: width,
            height: 33,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          data-uid='child'
        />
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
    <div
      data-uid='roo'
      style={{ top: 420, left: 620, position: 'absolute' }}
    >
      <div data-uid='par'>
        <div
          style={{
            position: 'absolute',
            width: width,
            height: 33,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          data-uid='chi'
        />
      </div>
    </div>
  </Storyboard>
)
`)
      })
    })

    describe('ending the paste session', () => {
      async function setupPasteSession(): Promise<EditorRenderResult> {
        const testCode = `
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='bbb' style={{ position: 'absolute' }}>
              <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
              <div data-uid='ddd' style={{width: 60, height: 60}} />
            </div>
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )

        await selectComponentsForTest(renderResult, [makeTargetPath('aaa/bbb')])
        await pressKey('c', { modifiers: cmdModifier })

        await selectComponentsForTest(renderResult, [makeTargetPath('aaa')])

        const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

        firePasteEvent(canvasRoot)

        await clipboardMock.pasteDone
        await renderResult.getDispatchFollowUpActionsFinished()

        return renderResult
      }

      function expectResultsToBeCommitted(editor: EditorRenderResult) {
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`<div
              data-uid='aaa'
              style={{ contain: 'layout', width: 300, height: 300 }}
            >
              <div data-uid='bbb' style={{ position: 'absolute' }}>
                <div
                  data-uid='ccc'
                  style={{
                    position: 'absolute',
                    left: 20,
                    top: 50,
                    bottom: 150,
                    width: 100,
                  }}
                />
                <div
                  data-uid='ddd'
                  style={{ width: 60, height: 60 }}
                />
              </div>
              <div
                data-uid='aat'
                style={{ position: 'absolute', top: 0, left: 0 }}
              >
                <div
                  data-uid='aai'
                  style={{
                    position: 'absolute',
                    left: 20,
                    top: 50,
                    bottom: 150,
                    width: 100,
                  }}
                />
                <div
                  data-uid='aao'
                  style={{ width: 60, height: 60 }}
                />
              </div>
            </div>
    `),
        )
      }

      it('the paste session ends on non-transient action', async () => {
        const renderResult = await setupPasteSession()
        expect(renderResult.getEditorState().postActionInteractionSession).not.toBeNull()

        keyDown('Backspace')
        await renderResult.getDispatchFollowUpActionsFinished()

        expect(renderResult.getEditorState().postActionInteractionSession).toBeNull()
        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ccc',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ddd',
        ])
      })

      it('the paste session ends on selection change', async () => {
        const renderResult = await setupPasteSession()
        expect(renderResult.getEditorState().postActionInteractionSession).not.toBeNull()

        await selectComponentsForTest(renderResult, [makeTargetPath('aaa/bbb')])
        await renderResult.getDispatchFollowUpActionsFinished()

        expect(renderResult.getEditorState().postActionInteractionSession).toBeNull()
        expectResultsToBeCommitted(renderResult)
        expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
        ])
      })

      it('the paste session ends on keydown', async () => {
        const renderResult = await setupPasteSession()
        expect(renderResult.getEditorState().postActionInteractionSession).not.toBeNull()

        keyDown('Esc')
        await renderResult.getDispatchFollowUpActionsFinished()

        expect(renderResult.getEditorState().postActionInteractionSession).toBeNull()
        expectResultsToBeCommitted(renderResult)
        expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
          'utopia-storyboard-uid/scene-aaa/app-entity',
        ])
      })

      it('the paste session ends on a new paste event', async () => {
        const renderResult = await setupPasteSession()
        clipboardMock.resetDoneSignal()
        const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

        firePasteEvent(canvasRoot)

        await clipboardMock.pasteDone
        await renderResult.getDispatchFollowUpActionsFinished()

        expect(renderResult.getEditorState().postActionInteractionSession).not.toBeNull()
        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ccc',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb/ddd',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/aat', // <- the pasted element
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/aat/aai',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/aat/aao',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/abi',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/abi/aax',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:aaa/abi/abd',
        ])
      })
    })

    xdescribe('Pasting with steganography enabled', () => {
      it('steganography data is cleaned from replaced props', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
        import { Storyboard, Scene } from 'utopia-api'
        
        const MyComponent = ({ title }) => {
          return <div data-uid='root'>heeeello</div>
        }
        
        const hello = 'hello'
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
            data-uid='scene'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 567,
                top: 486,
                width: 288,
                height: 362,
              }}
            >
              <MyComponent data-uid='component' title={hello} />
            </Scene>
          </Storyboard>
        )
        `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString('sb/scene/component')])
        await pressKey('c', { modifiers: cmdModifier })
        await runPaste(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          formatTestProjectCode(`import * as React from 'react'
        import { Storyboard, Scene } from 'utopia-api'

        const MyComponent = ({ title }) => {
          return <div data-uid='root'>heeeello</div>
        }

        const hello = 'hello'

        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              data-uid='scene'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 567,
                top: 486,
                width: 288,
                height: 362,
              }}
            >
              <MyComponent data-uid='component' title={hello} />
              <MyComponent
                data-uid='com'
                title='hello'
                style={{ top: 0, left: 0, position: 'absolute' }}
              />
            </Scene>
          </Storyboard>
        )
`),
        )
      })
    })
  })

  describe('PASTE_HERE', () => {
    const clipboardMock = new MockClipboardHandlers().mock()

    it(`Paste here can place an element on the empty canvas`, async () => {
      const testCode = `
        <div data-uid='aaa' data-testid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='bbb'>
            <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
            <div data-uid='ddd' style={{width: 60, height: 60}} />
          </div>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await selectComponentsForTest(renderResult, [makeTargetPath('aaa/bbb/ccc')])
      await pressKey('c', { modifiers: cmdModifier })

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('aaa')
      const elementBounds = element.getBoundingClientRect()

      await mouseClickAtPoint(canvasControlsLayer, elementBounds)

      const targetPoint = {
        x: elementBounds.x + elementBounds.width + 20,
        y: elementBounds.y,
      } // empty canvas
      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        targetPoint,
        'Paste Here',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Scene, Storyboard, View, Group } from 'utopia-api'
        
          export var App = (props) => {
            return (${testCode})
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
                <div data-uid='aai' style={{position: 'absolute', left: 321, width: 100, height: 100, top: 1}} />
              </Storyboard>
            )
          }
        `),
      )
    })
    it(`Paste here inserts an element to the root div when targeting an element deeper in the hierarchy`, async () => {
      const testCode = (expected: string) => `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='bbb'>
            <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
            <div data-uid='ddd' data-testid='ddd' style={{width: 60, height: 60}} />
          </div>${expected}
        </div>
      `
      const expectedDiv = `
        <div data-uid='aai' style={{position: 'absolute', left: 31, width: 100, height: 100, top: 31 }} />
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode('')),
        'await-first-dom-report',
      )
      await selectComponentsForTest(renderResult, [makeTargetPath('aaa/bbb/ccc')])
      await pressKey('c', { modifiers: cmdModifier })

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const element = renderResult.renderedDOM.getByTestId('ddd')
      const elementCenter = getDomRectCenter(element.getBoundingClientRect())
      await mouseClickAtPoint(canvasControlsLayer, elementCenter)

      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementCenter,
        'Paste Here',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Scene, Storyboard, View, Group } from 'utopia-api'
        
          export var App = (props) => {
            return (${testCode(expectedDiv)})
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
        `),
      )
    })
    it(`Paste here inserts an element to the Scene if there are multiple elements in a Scene`, async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Scene, Storyboard, View, Group } from 'utopia-api'
        
          export var App = (props) => {
            return (
              <div data-uid='root'>
                hello
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
                  <App
                    data-uid='${TestAppUID}'
                    style={{ position: 'absolute', bottom: 0, left: 200, right: 0, top: 200 }}
                  />
                  <div data-uid='aaa' data-testid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
                    <div data-uid='bbb'>
                      <div data-uid='ddd' data-testid='ddd' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
                    </div>
                  </div>
                </Scene>
              </Storyboard>
            )
          }
        `),
        'await-first-dom-report',
      )
      await selectComponentsForTest(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/aaa/bbb/ddd`),
      ])
      await pressKey('c', { modifiers: cmdModifier })

      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
      const element = renderResult.renderedDOM.getByTestId('ddd')
      const elementBounds = element.getBoundingClientRect()

      await mouseClickAtPoint(canvasControlsLayer, elementBounds)

      await openContextMenuAndClickOnItem(
        renderResult,
        canvasControlsLayer,
        elementBounds,
        'Paste Here',
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
          import * as React from 'react'
          import { Scene, Storyboard, View, Group } from 'utopia-api'
        
          export var App = (props) => {
            return (
              <div data-uid='root'>
                hello
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
                  <App
                    data-uid='${TestAppUID}'
                    style={{ position: 'absolute', bottom: 0, left: 200, right: 0, top: 200 }}
                  />
                  <div data-uid='aaa' data-testid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
                    <div data-uid='bbb'>
                      <div data-uid='ddd' data-testid='ddd' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
                    </div>
                  </div>
                  <div data-uid='aaj' data-testid='ddd' style={{position: 'absolute', left: 21, width: 100, height: 100, top: 69}} />
                </Scene>
              </Storyboard>
            )
          }
        `),
      )
    })
    describe('post action menu for paste', () => {
      it('copy and paste an element with code', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          const width = 88
          
          const App = () => {
            const width = 44

            return (
              <div data-uid="root">
                <div data-uid="parent">
                  <div
                    style={{
                      position: 'absolute',
                      width: width,
                      height: 33,
                      top: 100,
                      left: 100,
                      backgroundColor: '#cee5ff',
                    }}
                    data-uid='child'
                  />
                </div>
              </div>
            )
          }
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 200,
                  height: 300,
                  position: 'absolute',
                  left: 212,
                  top: 128,
                }}
                data-label='Playground'
                data-uid='scene'
                data-testid='scene'
              >
                <App data-uid='app' />
              </Scene>
            </Storyboard>
          )
          `,
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

        await expectNoAction(editor, () => pressKey('c', { modifiers: cmdModifier }))

        await selectComponentsForTest(editor, [])

        const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
        const element = editor.renderedDOM.getByTestId('scene')
        const elementBounds = element.getBoundingClientRect()

        await mouseClickAtPoint(canvasControlsLayer, elementBounds)

        const targetPoint = {
          x: elementBounds.x + elementBounds.width + 20,
          y: elementBounds.y,
        } // empty canvas

        await openContextMenuAndClickOnItem(editor, canvasControlsLayer, targetPoint, 'Paste Here')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().postActionInteractionSession?.activeChoiceId).toEqual(
          PropsReplacedPasteHerePostActionChoiceId,
        )

        // open the post-action menu
        const floatingPostActionMenu = editor.renderedDOM.getByTestId(FloatingPostActionMenuTestId)
        await mouseClickAtPoint(floatingPostActionMenu, { x: 2, y: 2 })

        await pressKey('2')
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().postActionInteractionSession?.activeChoiceId).toEqual(
          PropsPreservedPasteHerePostActionChoiceId,
        )

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const width = 88

const App = () => {
  const width = 44

  return (
    <div data-uid='root'>
      <div data-uid='parent'>
        <div
          style={{
            position: 'absolute',
            width: width,
            height: 33,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          data-uid='child'
        />
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
      data-testid='scene'
    >
      <App data-uid='app' />
    </Scene>
    <div
      data-uid='roo'
      style={{ top: 129, left: 433, position: 'absolute' }}
    >
      <div data-uid='par'>
        <div
          style={{
            position: 'absolute',
            width: width,
            height: 33,
            top: 100,
            left: 100,
            backgroundColor: '#cee5ff',
          }}
          data-uid='chi'
        />
      </div>
    </div>
  </Storyboard>
)
`)
      })
    })
  })
  describe('UNWRAP_ELEMENTS', () => {
    it(`Unwraps a fragment-like element`, async () => {
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='bbb'>
          <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
          <div data-uid='ddd' style={{width: 60, height: 60}} />
        </div>
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/bbb')])], true)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
          <div data-uid='ddd' style={{width: 60, height: 60}} />
        </div>`,
        ),
      )
    })
    it(`Unwraps an absolute element and keeps the visual position of its children`, async () => {
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='bbb' style={{position: 'absolute', left: 30, top: 30, width: 150, height: 150}}>
          <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 15, width: 100}} />
        </div>
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/bbb')])], true)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='ccc' style={{position: 'absolute', left: 50, top: 80, bottom: 135, width: 100}} />
        </div>`,
        ),
      )
    })
    it(`Unwraps a flex element`, async () => {
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div
          data-uid='bbb'
          style={{
            position: 'absolute',
            left: 30,
            top: 30,
            width: 150,
            height: 150,
            display: 'flex',
            justifyContent: 'center',
            alignItems: 'center',
          }}>
          <div data-uid='ccc' style={{width: 50, height: 100}} />
        </div>
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/bbb')])], true)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div data-uid='ccc' style={{width: 50, height: 100, position: 'absolute', left: 80, top: 55 }} />
        </div>`,
        ),
      )
    })
    it(`Doesn't unwrap an image, as it cannot have child elements, no changes in the code result`, async () => {
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <img
          src='/editor/utopia-logo-white-fill.png?hash=nocommit'
          alt='Utopia logo'
          data-uid='bbb'
        />
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/bbb')])], true)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <img
            src='/editor/utopia-logo-white-fill.png?hash=nocommit'
            alt='Utopia logo'
            data-uid='bbb'
          />
        </div>`,
        ),
      )
    })
    it(`Unwrap on an element without children deletes the element`, async () => {
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='bbb' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/bbb')])], true)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}} />`,
        ),
      )
    })
    it('can do multiselect unwrap', async () => {
      const testCode = `
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='unwrap-div'>
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 50,
                  height: 50,
                }}
                data-uid='foo'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                }}
                data-uid='bar'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  height: 60,
                }}
                data-uid='baz'
              />
            </div>
            <View data-uid='unwrap-view'>
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                }}
                data-uid='qux'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  height: 60,
                }}
                data-uid='waldo'
              />
            </View>
            <div data-uid='nested'>
                <div data-uid='nested-div' />
                <React.Fragment data-uid='fragment'>
                  <div data-uid='fragment-child1' />
                  <div data-uid='fragment-child2' />
                  <div data-uid='fragment-child3' />
                </React.Fragment>
            </div>
            {
              // @utopia/uid=cond
              true ? <div data-uid='true-branch' /> : <div data-uid='false-branch' />
            }
          </div>
        `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [
          unwrapElements([
            makeTargetPath('aaa/unwrap-div'),
            makeTargetPath('aaa/unwrap-view'),
            makeTargetPath('aaa/nested/fragment'),
            makeTargetPath('aaa/cond'),
          ]),
        ],
        true,
      )
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div
              style={{
                backgroundColor: 'orange',
                width: 50,
                height: 50,
              }}
              data-uid='foo'
            />
            <div
              style={{
                backgroundColor: 'orange',
                width: 30,
                height: 30,
              }}
              data-uid='bar'
            />
            <div
              style={{ backgroundColor: 'orange', height: 60 }}
              data-uid='baz'
            />
            <div
              style={{
                backgroundColor: 'orange',
                width: 30,
                height: 30,
              }}
              data-uid='qux'
            />
            <div
              style={{ backgroundColor: 'orange', height: 60 }}
              data-uid='waldo'
            />
            <div data-uid='nested'>
                <div data-uid='nested-div' />
                <div data-uid='fragment-child1' />
                <div data-uid='fragment-child2' />
                <div data-uid='fragment-child3' />
            </div>
            <div data-uid='true-branch' />
          </div>
        `),
      )
    })
    it('when doing multiselect unwrap for a subtree, predictably limit to the topmost ancestor', async () => {
      const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <Group style={{ position: 'absolute' }} data-uid='group1'>
            <div
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
                backgroundColor: 'blue',
              }}
              data-uid='div'
            >
              <Group
                style={{
                  position: 'absolute',
                  width: 50,
                  height: 50,
                }}
                data-uid='group2'
              >
                <View
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    width: 50,
                    height: 50,
                    left: 0,
                    top: 0,
                  }}
                  data-uid='view'
                />
              </Group>
            </div>
          </Group>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [unwrapElements([makeTargetPath('aaa/group1'), makeTargetPath('aaa/group1/div/group2')])],
        true,
      )
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
              backgroundColor: 'blue',
            }}
            data-uid='div'
            >
            <Group
              style={{
                position: 'absolute',
                width: 50,
                height: 50,
              }}
              data-uid='group2'
            >
              <View
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  width: 50,
                  height: 50,
                  left: 0,
                  top: 0,
                }}
                data-uid='view'
              />
            </Group>
          </div>
        </div>
      `),
      )
    })
    it('when doing multiselect unwrap for a subtree, predictably limit to the topmost ancestor (immediate child)', async () => {
      const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <Group style={{ position: 'absolute' }} data-uid='group1'>
            <div
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
                backgroundColor: 'blue',
              }}
              data-uid='div'
            >
              <Group
                style={{
                  position: 'absolute',
                  width: 50,
                  height: 50,
                }}
                data-uid='group2'
              >
                <View
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    width: 50,
                    height: 50,
                    left: 0,
                    top: 0,
                  }}
                  data-uid='view'
                />
              </Group>
            </div>
          </Group>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [unwrapElements([makeTargetPath('aaa/group1'), makeTargetPath('aaa/group1/div')])],
        true,
      )
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
              backgroundColor: 'blue',
            }}
            data-uid='div'
            >
            <Group
              style={{
                position: 'absolute',
                width: 50,
                height: 50,
              }}
              data-uid='group2'
            >
              <View
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  width: 50,
                  height: 50,
                  left: 0,
                  top: 0,
                }}
                data-uid='view'
              />
            </Group>
          </div>
        </div>
      `),
      )
    })
    it('can do multiselect unwrap with subtrees and isolate elements', async () => {
      const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <Group style={{ position: 'absolute' }} data-uid='group1'>
            <div
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
                backgroundColor: 'blue',
              }}
              data-uid='div'
            >
              <Group
                style={{
                  position: 'absolute',
                  width: 50,
                  height: 50,
                }}
                data-uid='group2'
              >
                <View
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    width: 50,
                    height: 50,
                    left: 0,
                    top: 0,
                  }}
                  data-uid='view'
                />
              </Group>
            </div>
          </Group>
          <div data-uid='another-div'>
            <div data-uid='unwrap-this'>
              <div data-uid='foo' />
              <img data-uid='cat' src='https://placekitten.com/100/100' />
            </div>
          </div>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [
          unwrapElements([
            makeTargetPath('aaa/group1'),
            makeTargetPath('aaa/group1/div'),
            makeTargetPath('aaa/another-div/unwrap-this'),
          ]),
        ],
        true,
      )
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <div
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
              backgroundColor: 'blue',
            }}
            data-uid='div'
          >
            <Group
              style={{
                position: 'absolute',
                width: 50,
                height: 50,
              }}
              data-uid='group2'
            >
              <View
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  width: 50,
                  height: 50,
                  left: 0,
                  top: 0,
                }}
                data-uid='view'
              />
            </Group>
          </div>
          <div data-uid='another-div'>
            <div data-uid='foo' />
            <img data-uid='cat' src='https://placekitten.com/100/100' />
          </div>
        </div>
      `),
      )
    })
    it(`Unwraps a fragment`, async () => {
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <React.Fragment data-uid='fragment'>
          <div data-uid='bbb' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
          <div data-uid='ccc' style={{width: 100, height: 50}} />
        </React.Fragment>
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/fragment')])], true)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='bbb' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
        <div data-uid='ccc' style={{width: 100, height: 50}} />
      </div>
    `),
      )
    })
    describe('conditionals', () => {
      it(`Unwraps a conditional`, async () => {
        const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        {
          // @utopia/uid=conditional
          true ? <div data-uid='bbb'>foo</div> : <div>bar</div>
        }
      </div>
    `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/conditional')])], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='bbb'>foo</div>
          </div>
        `),
        )
      })
      it(`Unwraps a conditional (false)`, async () => {
        const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        {
          // @utopia/uid=conditional
          false ? <div data-uid='bbb'>foo</div> : <div data-uid='ccc'>bar</div>
        }
      </div>
    `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/conditional')])], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='ccc'>bar</div>
          </div>
        `),
        )
      })
      it(`Unwraps a conditional (override)`, async () => {
        const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        {
          // @utopia/uid=conditional
          // @utopia/conditional=false
          true ? <div data-uid='bbb'>foo</div> : <div data-uid='ccc'>bar</div>
        }
      </div>
    `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/conditional')])], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='ccc'>bar</div>
          </div>
        `),
        )
      })
      it(`Unwraps a conditional with inline content`, async () => {
        const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          {
            // @utopia/uid=conditional
            true ? 'hello' : <span>'goodbye'</span>
          }
        </div>
      `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/conditional')])], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            hello
          </div>
        `),
        )
      })
      it(`Unwraps a conditional containing a conditional`, async () => {
        const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          {
            // @utopia/uid=conditional
            true ? true ? <div data-uid='bbb'>foo</div> : <div data-uid='ccc'>bar</div> : <div>baz</div>
          }
        </div>
      `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/conditional')])], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            {
              true ? (
                <div data-uid='bbb'>foo</div>
              ): (
                <div data-uid='ccc'>bar</div>
              )
            }
          </div>
        `),
        )
      })
      it(`Unwraps a conditional inside a conditional`, async () => {
        const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          {
            // @utopia/uid=conditional
            true
            ? true /* @utopia/uid=conditional2 */ ? <div data-uid='bbb'>foo</div> : <div data-uid='ccc'>bar</div>
            : <div data-uid='ddd'>baz</div>
          }
        </div>
      `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch(
          [unwrapElements([makeTargetPath('aaa/conditional/conditional2')])],
          true,
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            {
              // @utopia/uid=conditional
              true ? (
                <div data-uid='bbb'>foo</div>
              ): (
                <div data-uid='ddd'>baz</div>
              )
            }
          </div>
        `),
        )
      })
      it(`Unwraps a conditional inside a conditional with literal content`, async () => {
        const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          {
            // @utopia/uid=conditional
            true
            ? true /* @utopia/uid=conditional2 */ ? 'foo' : <span>'bar'</span>
            : <div data-uid='ddd'>baz</div>
          }
        </div>
      `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch(
          [unwrapElements([makeTargetPath('aaa/conditional/conditional2')])],
          true,
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            {
              // @utopia/uid=conditional
              true ? (
                'foo'
              ): (
                <div data-uid='ddd'>baz</div>
              )
            }
          </div>
        `),
        )
      })
    })
    describe('groups', () => {
      it('makes sure unwrapped children have pins and keep their frame intact', async () => {
        const testCode = `
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <Group data-uid='group'>
              <div
                data-uid='unwrap-me'
                style={{
                  position: 'absolute',
                  left: 20,
                  top: 50,
                  width: 100,
                  display: 'flex',
                  flexDirection: 'column',
                  gap: 2,
                }}>
                  <div
                    style={{
                      backgroundColor: 'orange',
                      width: 50,
                      height: 50,
                    }}
                    data-uid='foo'
                  />
                  <div
                    style={{
                      backgroundColor: 'orange',
                      width: 30,
                      height: 30,
                    }}
                    data-uid='bar'
                  />
                  <div
                    style={{
                      backgroundColor: 'orange',
                      height: 60,
                    }}
                    data-uid='baz'
                  />
              </div>
              <div data-uid='ccc' style={{
                width: 100,
                height: 50,
                left: 200,
                top: 200,
              }} />
            </Group>
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElements([makeTargetPath('aaa/group/unwrap-me')])], true)
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <Group data-uid='group'>
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 20,
                  top: 50,
                }}
                data-uid='foo'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                  position: 'absolute',
                  left: 20,
                  top: 102,
                }}
                data-uid='bar'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  height: 60,
                  width: 100,
                  position: 'absolute',
                  left: 20,
                  top: 134,
                }}
                data-uid='baz'
              />
              <div data-uid='ccc' style={{
                width: 100,
                height: 50,
                left: 200,
                top: 200,
              }} />
            </Group>
          </div>
        `),
        )
      })
      it('selects all unwrapped children on multiselect unwrap', async () => {
        const testCode = `
          <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <Group data-uid='group1'>
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 50,
                  height: 50,
                }}
                data-uid='foo'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                }}
                data-uid='bar'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  height: 60,
                }}
                data-uid='baz'
              />
            </Group>
            <Group data-uid='group2'>
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                }}
                data-uid='qux'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  height: 60,
                }}
                data-uid='waldo'
              />
            </Group>
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch(
          [unwrapElements([makeTargetPath('aaa/group1'), makeTargetPath('aaa/group2')])],
          true,
        )
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 50,
                  height: 50,
                }}
                data-uid='foo'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                }}
                data-uid='bar'
              />
              <div
                style={{ backgroundColor: 'orange', height: 60 }}
                data-uid='baz'
              />
              <div
                style={{
                  backgroundColor: 'orange',
                  width: 30,
                  height: 30,
                }}
                data-uid='qux'
              />
              <div
                style={{ backgroundColor: 'orange', height: 60 }}
                data-uid='waldo'
              />
            </div>
        `),
        )

        const selection = [...renderResult.getEditorState().editor.selectedViews].sort(
          comparePathStrings,
        )
        expect(selection).toEqual(
          [
            makeTargetPath('aaa/foo'),
            makeTargetPath('aaa/bar'),
            makeTargetPath('aaa/baz'),
            makeTargetPath('aaa/qux'),
            makeTargetPath('aaa/waldo'),
          ].sort(comparePathStrings),
        )
      })
    })
  })

  describe('WRAP_IN_ELEMENT', () => {
    it(`Wraps 2 elements`, async () => {
      const testUID = 'bbb'
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
        <div data-uid='ddd' style={{width: 60, height: 60}} />
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )

      await wrapInElement(
        renderResult,
        [makeTargetPath('aaa/ccc'), makeTargetPath('aaa/ddd')],
        testUID,
        'div',
      )

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div
              data-uid='aaa'
              style={{ contain: 'layout', width: 300, height: 300 }}
            >
              <div
                style={{
                  contain: 'layout',
                  width: 120,
                  height: 150,
                  position: 'absolute',
                  top: 0,
                  left: 0,
                }}
              >
                <div
                  data-uid='ccc'
                  style={{
                    position: 'absolute',
                    left: 20,
                    top: 50,
                    width: 100,
                    height: 100,
                  }}
                />
                <div
                  data-uid='ddd'
                  style={{
                    width: 60,
                    height: 60,
                    top: 0,
                    left: 0,
                    position: 'absolute',
                  }}
                />
              </div>
            </div>`,
        ),
      )
      expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        `utopia-storyboard-uid/scene-aaa/app-entity:aaa/${testUID}`,
      ])
    })
    it(`Wraps 2 elements inside a flex layout`, async () => {
      const testUID = 'zzz'
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='bbb' style={{display: 'flex', gap: 10, padding: 10}}>
          <div data-uid='ccc' style={{width: 100, height: 60}} />
          <div data-uid='ddd' style={{flexGrow: 1, height: '100%'}} />
          <div data-uid='eee' style={{width: 100, height: 60}} />
        </div>
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )

      await wrapInElement(
        renderResult,
        [makeTargetPath('aaa/bbb/ddd'), makeTargetPath('aaa/bbb/eee')],
        testUID,
        'div',
      )

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div
            data-uid='aaa'
            style={{ contain: 'layout', width: 300, height: 300 }}
          >
            <div
              data-uid='bbb'
              style={{ display: 'flex', gap: 10, padding: 10 }}
            >
              <div data-uid='ccc' style={{ width: 100, height: 60 }} />
              <div
                style={{
                  contain: 'layout',
                  width: 170,
                  height: 60,
                }}
              >
                <div
                  data-uid='ddd'
                  style={{
                    height: 0,
                    width: 60,
                    top: 0,
                    left: 0,
                    position: 'absolute',
                  }}
                />
                <div
                  data-uid='eee'
                  style={{
                    width: 100,
                    height: 60,
                    top: 0,
                    left: 70,
                    position: 'absolute',
                  }}
                />
              </div>
            </div>
          </div>`,
        ),
      )
    })
    it(`Wraps 2 elements with a fragment`, async () => {
      const testUID = 'zzz'
      const testCode = `
      <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
        <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
        <div data-uid='ddd' style={{width: 60, height: 60}} />
      </div>
    `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )

      await wrapInElement(
        renderResult,
        [makeTargetPath('aaa/ccc'), makeTargetPath('aaa/ddd')],
        testUID,
        'Fragment',
      )

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          ` <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <React.Fragment>
              <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
              <div data-uid='ddd' style={{width: 60, height: 60}} />
            </React.Fragment>
          </div>`,
        ),
      )
    })
    describe('groups', () => {
      it('cannot wrap an empty group', async () => {
        const testCode = `
        <div data-uid='aaa'>
          <Group data-uid='group' />
        </div>
      `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )

        await wrapInElement(renderResult, [makeTargetPath('aaa/group')], 'foo', 'Group')

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
            <Group data-uid='group' />
          </div>
        `),
        )
        expect(renderResult.getEditorState().editor.toasts.length).toEqual(1)
        expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
          'Empty Groups cannot be wrapped',
        )
      })

      it('can group conditionals', async () => {
        const testCode = `
          <div data-uid='aaa'>
            <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
            {
              // @utopia/uid=cond
              true ? <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'blue' }} /> : null
            }
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(makeTestProjectCodeWithSnippet(testCode)),
          'await-first-dom-report',
        )

        await wrapInElement(
          renderResult,
          [makeTargetPath('aaa/foo'), makeTargetPath('aaa/cond')],
          'grp',
          'Group',
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa' style={{ contain: 'layout' }}>
                <Group
                  style={{ position: 'absolute', left: 0, top: 0 }}
                  data-uid='grp'
                >
                  <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
                  {
                    // @utopia/uid=cond
                    true ? <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'blue' }} /> : null
                  }
                </Group>
              </div>
          `),
          ),
        )

        expect(renderResult.getEditorState().editor.toasts).toHaveLength(1)
        const firstToast = safeIndex(renderResult.getEditorState().editor.toasts, 0)
        expect(firstToast?.level).toEqual('INFO')
        expect(firstToast?.message).toEqual(
          "Added `contain: 'layout'` to the parent of the newly added element.",
        )
      })

      it('cannot group empty conditionals', async () => {
        const testCode = `
          <div data-uid='aaa'>
            <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
            {
              // @utopia/uid=cond
              true ? null : null
            }
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(makeTestProjectCodeWithSnippet(testCode)),
          'await-first-dom-report',
        )

        await wrapInElement(
          renderResult,
          [makeTargetPath('aaa/foo'), makeTargetPath('aaa/cond')],
          'grp',
          'Group',
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
                {
                  // @utopia/uid=cond
                  true ? null : null
                }
              </div>
          `),
          ),
        )

        expect(renderResult.getEditorState().editor.toasts.length).toEqual(1)
        expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
          'Not all targets can be wrapped into a Group',
        )
      })

      it('cannot group conditionals with active branch that cannot be a group child', async () => {
        const testCode = `
          <div data-uid='aaa'>
            <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
            {
              // @utopia/uid=cond
              true ? 42 : null
            }
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(makeTestProjectCodeWithSnippet(testCode)),
          'await-first-dom-report',
        )

        await wrapInElement(
          renderResult,
          [makeTargetPath('aaa/foo'), makeTargetPath('aaa/cond')],
          'grp',
          'Group',
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
                {
                  // @utopia/uid=cond
                  true ? 42 : null
                }
              </div>
          `),
          ),
        )

        expect(renderResult.getEditorState().editor.toasts.length).toEqual(1)
        expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
          'Not all targets can be wrapped into a Group',
        )
      })

      it('can wrap nested conditionals', async () => {
        const testCode = `
          <div data-uid='aaa'>
            <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
            {
              // @utopia/uid=cond1
              true ? (
                // @utopia/uid=cond2
                true ? (
                  <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'blue' }} />
                ) : null
              ) : null
            }
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(makeTestProjectCodeWithSnippet(testCode)),
          'await-first-dom-report',
        )

        await wrapInElement(
          renderResult,
          [makeTargetPath('aaa/foo'), makeTargetPath('aaa/cond1')],
          'grp',
          'Group',
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa' style={{ contain: 'layout' }}>
                <Group
                  style={{ position: 'absolute', left: 0, top: 0 }}
                  data-uid='grp'
                >
                  <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
                  {
                    // @utopia/uid=cond1
                    true ? (
                      // @utopia/uid=cond2
                      true ? (
                        <div data-uid='bar' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'blue' }} />
                      ) : null
                    ) : null
                  }
                </Group>
              </div>
          `),
          ),
        )

        expect(renderResult.getEditorState().editor.toasts).toHaveLength(1)
        const firstToast = safeIndex(renderResult.getEditorState().editor.toasts, 0)
        expect(firstToast?.level).toEqual('INFO')
        expect(firstToast?.message).toEqual(
          "Added `contain: 'layout'` to the parent of the newly added element.",
        )
      })

      it(`doesn't wrap nested conditionals with invalid group child in the active branch`, async () => {
        const testCode = `
          <div data-uid='aaa'>
            <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
            {
              // @utopia/uid=cond1
              true ? (
                // @utopia/uid=cond2
                true ? (
                  42
                ) : null
              ) : null
            }
          </div>
        `
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(makeTestProjectCodeWithSnippet(testCode)),
          'await-first-dom-report',
        )

        await wrapInElement(
          renderResult,
          [makeTargetPath('aaa/foo'), makeTargetPath('aaa/cond1')],
          'grp',
          'Group',
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                <div data-uid='foo' style={{ position: 'absolute', width: 50, height: 50, top: 0, left: 0, background: 'red' }} />
                {
                  // @utopia/uid=cond1
                  true ? (
                    // @utopia/uid=cond2
                    true ? (
                      42
                    ) : null
                  ) : null
                }
              </div>
          `),
          ),
        )

        expect(renderResult.getEditorState().editor.toasts.length).toEqual(1)
        expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
          'Not all targets can be wrapped into a Group',
        )
      })
    })
  })
  describe('SELECT_COMPONENTS', () => {
    it('Can not select the same element path twice', async () => {
      const testCode = `<div data-uid='aaa'/>`
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [selectComponents([makeTargetPath('aaa'), makeTargetPath('aaa')], false)],
        true,
      )
      await renderResult.getDispatchFollowUpActionsFinished()
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([makeTargetPath('aaa')])
    })
    describe('setting the left pane when selection changes', () => {
      const deselectActions = (renderResult: EditorRenderResult) => [
        async () => renderResult.dispatch([selectComponents([], false)], true),
        async () => renderResult.dispatch([clearSelection()], true),
        async () =>
          renderResult.dispatch([applyCommandsAction([updateSelectedViews('always', [])])], true),
      ]

      const selectActions = (renderResult: EditorRenderResult) => [
        async (paths: ElementPath[]) =>
          renderResult.dispatch([selectComponents(paths, false)], false),
        async (paths: ElementPath[]) =>
          renderResult.dispatch(
            [applyCommandsAction([updateSelectedViews('always', paths)])],
            true,
          ),
      ]

      it('does not jump when all elements are deselected', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='child1' />
          <div data-uid='child2' />
        </div>`),
          'await-first-dom-report',
        )

        const entryPoints = deselectActions(renderResult)

        const pages = [LeftMenuTab.Github, LeftMenuTab.Pages, LeftMenuTab.Project]

        for await (const page of pages) {
          for await (const deselect of entryPoints) {
            await selectComponentsForTest(renderResult, [
              EP.appendNewElementPath(TestScenePath, ['root', 'child1']),
            ])
            await renderResult.dispatch([setLeftMenuTab(page)], true)
            await deselect()
            expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([])

            // stays on the same page
            expect(renderResult.getEditorState().editor.leftMenu.selectedTab).toEqual(page)
          }
        }
      })

      it('does not jump when an element is selected and the pages tab is selected', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='child1' />
          <div data-uid='child2' />
        </div>`),
          'await-first-dom-report',
        )

        const entryPoints = selectActions(renderResult)

        for await (const select of entryPoints) {
          await selectComponentsForTest(renderResult, [
            EP.appendNewElementPath(TestScenePath, ['root', 'child1']),
          ])
          await renderResult.dispatch([setLeftMenuTab(LeftMenuTab.Pages)], true)
          const pathToSelect = EP.appendNewElementPath(TestScenePath, ['root', 'child2'])
          await select([pathToSelect])
          expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
            EP.toString(pathToSelect),
          ])

          // stays on the pages tab
          expect(renderResult.getEditorState().editor.leftMenu.selectedTab).toEqual(
            LeftMenuTab.Pages,
          )
        }
      })

      it('jumps when an element is selected', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='child1' />
          <div data-uid='child2' />
        </div>`),
          'await-first-dom-report',
        )

        const entryPoints = selectActions(renderResult)

        const pages = [
          { start: LeftMenuTab.Github, end: LeftMenuTab.Navigator },
          { start: LeftMenuTab.Project, end: LeftMenuTab.Navigator },
          { start: LeftMenuTab.Navigator, end: LeftMenuTab.Navigator },
          { start: LeftMenuTab.UIInsert, end: LeftMenuTab.Navigator },

          { start: LeftMenuTab.Pages, end: LeftMenuTab.Pages }, // this doesn't jump, but it's included for completeness
        ]

        for await (const { start, end } of pages) {
          for await (const select of entryPoints) {
            await selectComponentsForTest(renderResult, [
              EP.appendNewElementPath(TestScenePath, ['root', 'child1']),
            ])
            await renderResult.dispatch([setLeftMenuTab(start)], true)
            const pathToSelect = EP.appendNewElementPath(TestScenePath, ['root', 'child2'])
            await select([pathToSelect])
            expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
              EP.toString(pathToSelect),
            ])
            expect(renderResult.getEditorState().editor.leftMenu.selectedTab).toEqual(end)
          }
        }
      })
    })
  })

  describe('TRUNCATE_HISTORY', () => {
    it('truncates history', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div data-uid='foo' style={{ position: 'absolute', top: 10, left: 10, background: 'red', width: 50, height: 50 }} />
            <div data-uid='bar' style={{ position: 'absolute', top: 10, left: 100, background: 'blue', width: 50, height: 50}} />
            <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 50, background: 'green', width: 50, height: 50}} />
          </div>
        `),
        'await-first-dom-report',
      )

      // these go into the undo stack
      await renderResult.dispatch(
        [deleteView(EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:root/bar'))],
        true,
      )
      await renderResult.dispatch(
        [deleteView(EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:root/foo'))],
        true,
      )
      expect(renderResult.getEditorState().history.next.length).toBe(0)
      expect(renderResult.getEditorState().history.previous.length).toBe(2)

      // truncate the stack
      await renderResult.dispatch([truncateHistory()], true)
      expect(renderResult.getEditorState().history.next.length).toBe(0)
      expect(renderResult.getEditorState().history.previous.length).toBe(0)

      // try to undo, nothing happens
      await renderResult.dispatch([undo()], true)
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div data-uid='baz' style={{ position: 'absolute', top: 100, left: 50, background: 'green', width: 50, height: 50}} />
          </div>
        `),
      )
      expect(renderResult.getEditorState().history.next.length).toBe(0)
      expect(renderResult.getEditorState().history.previous.length).toBe(0)
    })
  })
})

function comparePathStrings(a: ElementPath, b: ElementPath): number {
  return EP.toString(a).localeCompare(EP.toString(b))
}

async function wrapInElement(
  renderResult: EditorRenderResult,
  pathsToWrap: ElementPath[],
  uid: string,
  query: string,
) {
  await selectComponentsForTest(renderResult, pathsToWrap)
  await pressKey('w') // open the wrap menu
  FOR_TESTS_setNextGeneratedUid(uid)
  await searchInFloatingMenu(renderResult, query)
}
