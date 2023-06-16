/* eslint-disable jest/expect-expect */
import * as EP from '../../../core/shared/element-path'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../../components/canvas/ui-jsx.test-utils'
import { deleteSelected, selectComponents, unwrapElement, wrapInElement } from './action-creators'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ElementPaste } from '../action-types'
import { act } from '@testing-library/react'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  InsertionPath,
} from '../store/insertion-path'
import { getElementFromRenderResult } from './actions.test-utils'
import {
  JSXConditionalExpression,
  JSXElementChild,
  jsxFragment,
  jsxFragmentWithoutUID,
} from '../../../core/shared/element-template'
import { defaultDivElement } from '../defaults'
import {
  expectNoAction,
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTests,
  wait,
} from '../../../utils/utils.test-utils'
import {
  firePasteEvent,
  keyDown,
  MockClipboardHandlers,
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  mouseMoveToPoint,
  pressKey,
} from '../../canvas/event-helpers.test-utils'
import { cmdModifier, shiftCmdModifier } from '../../../utils/modifiers'
import { FOR_TESTS_setNextGeneratedUids } from '../../../core/model/element-template-utils.test-utils'
import { createTestProjectWithMultipleFiles } from '../../../sample-projects/sample-project-utils.test-utils'
import { navigatorEntryToKey, PlaygroundFilePath, StoryboardFilePath } from '../store/editor-state'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { canvasPoint, windowPoint } from '../../../core/shared/math-utils'
import { assertNever } from '../../../core/shared/utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { maybeConditionalExpression } from '../../../core/model/conditionals'
import { PasteWithPropertiesPreservedStrategyId } from '../../canvas/canvas-strategies/strategies/paste-metastrategy'
import { ControlDelay } from '../../canvas/canvas-strategies/canvas-strategy-types'

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
        wantSelection: [makeTargetPath('aaa/000')],
      },
      {
        name: 'delete empty fragments (multiple targets)',
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
          makeTargetPath('aaa/000/eee'),
          makeTargetPath('aaa/001/fff'),
          makeTargetPath('aaa/001/ggg'),
        ],
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
        wantSelection: [makeTargetPath('aaa/000'), makeTargetPath('aaa')],
      },
    ]
    tests.forEach((tt, idx) => {
      it(`(${idx + 1}) ${tt.name}`, async () => {
        const got = await deleteFromScene(tt.input, tt.targets)
        expect(got.code).toEqual(makeTestProjectCodeWithSnippet(tt.wantCode))
        expect(got.selection).toEqual(tt.wantSelection)
      })
    })
  })
  describe('PASTE_JSX_ELEMENTS', () => {
    const clipboardMock = new MockClipboardHandlers().mock()

    type PasteTest = {
      name: string
      startingCode: string
      elements: (renderResult: EditorRenderResult) => Array<ElementPaste>
      pasteInto: InsertionPath
      want: string
      generatesUndoStep?: boolean
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
            <div data-uid='aad'>foo</div>
        </div>
		`,
      },
      {
        name: 'multiple elements',
        startingCode: `
        <div data-uid='aaa'>
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
        <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            <div data-uid='ddd'>baz</div>
            <div data-uid='aad'>foo</div>
            <div data-uid='aag'>bar</div>
        </div>
		`,
      },
      {
        name: 'a fragment',
        startingCode: `
        <div data-uid='root'>
            <div data-uid='aaa'>
                <div data-uid='bbb'>foo</div>
                <div data-uid='ccc'>bar</div>
            </div>
            <>
                <div data-uid='ddd'>hello</div>
                <div data-uid='eee'>there</div>
            </>
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
                <>
                    <div data-uid='aad'>hello</div>
                    <div data-uid='aah'>there</div>
                </>
            </div>
            <>
                <div data-uid='ddd'>hello</div>
                <div data-uid='eee'>there</div>
            </>
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
            <></>
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
                <></>
            </div>
            <></>
        </div>
		`,
      },
      {
        name: 'a conditional',
        startingCode: `
		<div data-uid='root'>
            <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            </div>
            {
                // @utopia/uid=conditional
                true ? (
                    <div data-uid='ddd'>true</div>
                ): (
                    <div data-uid='eee'>false</div>
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
		<div data-uid='root'>
            <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
                {
                    // @utopia/uid=conditional
                    true ? (
                        <div data-uid='aae'>true</div>
                    ): (
                        <div data-uid='aai'>false</div>
                    )
                }
            </div>
            {
                // @utopia/uid=conditional
                true ? (
                    <div data-uid='ddd'>true</div>
                ): (
                    <div data-uid='eee'>false</div>
                )
            }
        </div>
		`,
      },
      {
        name: 'an element inside a fragment',
        startingCode: `
        <div data-uid='root'>
            <>
                <div data-uid='aaa'>foo</div>
            </>
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
        pasteInto: childInsertionPath(EP.appendNewElementPath(TestScenePath, ['root', 'dbc'])),
        want: `
        <div data-uid='root'>
            <>
                <div data-uid='aaa'>foo</div>
                <div data-uid='aad'>bar</div>
            </>
            <div data-uid='bbb'>bar</div>
        </div>
		`,
      },
      {
        name: 'multiple elements inside a fragment',
        startingCode: `
        <div data-uid='root'>
            <>
                <div data-uid='aaa'>foo</div>
            </>
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
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
        <div data-uid='root'>
            <>
            	<div data-uid='aaa'>foo</div>
                <div data-uid='aad'>bar</div>
                <div data-uid='aag'>baz</div>
            </>
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
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
          'replace',
        ),
        want: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? <div data-uid='aad'>bar</div> : <div data-uid='aaa'>foo</div>
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
                true ? <div data-uid='aaa'>foo</div> : null
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
          'false-case',
          'replace',
        ),
        want: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : <div data-uid='aad'>bar</div>
            }
            <div data-uid='bbb'>bar</div>
    	</div>
		`,
      },
      {
        name: 'multiple elements into an empty conditional branch (true)',
        startingCode: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? null : <div data-uid='aaa'>foo</div>
            }
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
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
          'replace',
        ),
        want: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? (
                    <React.Fragment>
                    	<div data-uid='aad'>bar</div>
                    	<div data-uid='aah'>baz</div>
                    </React.Fragment>
                ) : <div data-uid='aaa'>foo</div>
            }
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
        </div>
		`,
      },
      {
        name: 'multiple elements into an empty conditional branch (false)',
        startingCode: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : null
            }
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
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
          'wrap-with-fragment',
        ),
        want: `
        <div data-uid='root'>
        	{
            	// @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : (
                    <React.Fragment>
                    	<div data-uid='aad'>bar</div>
                    	<div data-uid='aah'>baz</div>
                    </React.Fragment>
                )
            }
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
        </div>
		`,
      },
      {
        name: 'a fragment inside an empty conditional branch',
        startingCode: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? null : <div data-uid='aaa'>foo</div>
            }
            <>
            	<div data-uid='bbb'>bar</div>
                <div data-uid='ccc'>baz</div>
            </>
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
          'replace',
        ),
        want: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? (
                    <>
                    	<div data-uid='aad'>bar</div>
                    	<div data-uid='aah'>baz</div>
                    </>
                ) : <div data-uid='aaa'>foo</div>
            }
            <>
                <div data-uid='bbb'>bar</div>
                <div data-uid='ccc'>baz</div>
            </>
        </div>
		`,
      },
      {
        name: 'multiple fragments inside an empty conditional branch',
        startingCode: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? null : <div data-uid='aaa'>foo</div>
            }
            <>
            	<div data-uid='bbb'>bar</div>
                <div data-uid='ccc'>baz</div>
            </>
            <>
                <div data-uid='ddd'>qux</div>
                <div data-uid='eee'>waldo</div>
            </>
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
          'replace',
        ),
        want: `
      <div data-uid='root'>
      {
        // @utopia/uid=conditional
        true ? (
          <React.Fragment>
            <>
              <div data-uid='aad'>bar</div>
              <div data-uid='aah'>baz</div>
            </>
            <>
              <div data-uid='aam'>qux</div>
              <div data-uid='aaq'>waldo</div>
            </>
          </React.Fragment>
        ) : <div data-uid='aaa'>foo</div>
      }
      <>
        <div data-uid='bbb'>bar</div>
        <div data-uid='ccc'>baz</div>
      </>
      <>
        <div data-uid='ddd'>qux</div>
        <div data-uid='eee'>waldo</div>
      </>
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
        <div data-uid='aad'>foo</div>
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
          data-uid='ffb'
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
          data-uid='ffb'
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
    ]
    tests.forEach((test, i) => {
      it(`${i + 1}/${tests.length} ${test.name}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(test.startingCode),
          'await-first-dom-report',
        )

        const undoCheckerFn =
          test.generatesUndoStep === false ? expectNoAction : expectSingleUndo2Saves

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

        await undoCheckerFn(renderResult, async () => {
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

    describe('end-to-end copy paste', () => {
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
            <div data-uid='aar'>
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
              left: 37.5,
              top: 64,
              width: 204,
              height: 67,
            }}
            data-uid='fc-'
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
      describe('repeated paste', () => {
        async function pasteNTimes(editor: EditorRenderResult, n: number) {
          const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

          for (let counter = 0; counter < n; counter += 1) {
            firePasteEvent(canvasRoot)

            // Wait for the next frame
            await clipboardMock.pasteDone
            await editor.getDispatchFollowUpActionsFinished()

            await pressKey('Esc')
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

          expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
            [
              'regular-utopia-storyboard-uid/scene-aaa',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/div',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aag',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aak',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aam',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/aao',
              'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container/last',
            ],
          )
          expect(getPrintedUiJsCode(editor.getEditorState()))
            .toEqual(`import * as React from 'react'
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
            contain: 'layout',
            width: 100,
            height: 100,
          }}
          data-uid='aao'
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
          left: 470,
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
          left: 724,
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
          left: 978,
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
          left: 1232,
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
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aam',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aao',
            'regular-utopia-storyboard-uid/scene-aaa/app-entity:sb/aaq',
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
            data-uid='aam'
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
            data-uid='aao'
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
            data-uid='aaq'
          />
        </div>`),
          )
        })
      })
      describe('paste into a conditional', () => {
        setFeatureForBrowserTests('Paste wraps into fragment', true)
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
                  <div data-uid='aad'>foo</div>
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
                        <div data-uid='aad'>foo</div>
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
              <div data-uid='root'>
                {
                  // @utopia/uid=conditional
                  true ? <img data-uid='aaa' src='https://placekitten.com/100/100' /> : null
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
                      <React.Fragment>
                        <div data-uid='aad'>foo</div>
                        <img data-uid='aaa' src='https://placekitten.com/100/100' />
                      </React.Fragment>
                    ) : null
                  }
                  <div data-uid='bbb'>foo</div>
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

            await selectComponentsForTest(renderResult, [makeTargetPath('root/conditional/a25')])

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
                    true ? <div data-uid='aaa' /> : <div data-uid='aad'>foo</div>
                  }
                  <div data-uid='bbb'>foo</div>
                </div>
              `),
            )
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
                <div data-uid='aak' style={{ height: 20 }}>
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
              input: `<div data-uid='root'>
                <span data-uid='ccc'>hi</span>
                <div data-uid='bbb' style={{ width: 50, height: 50, contain: 'layout' }} />
              </div>`,
              targets: [makeTargetPath('root/bbb')],
              result: `<div data-uid='root'>
                <span data-uid='ccc'>hi</span>
                <div data-uid='bbb' style={{ width: 50, height: 50, contain: 'layout' }} />
                <div data-uid='aaf' style={{ width: 50, height: 50, contain: 'layout' }} />
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
              result: `<div data-uid='aai' style={{position: 'absolute', width: 50, height: 40, top: 400, left: 567}}>Hello!</div>`,
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
              result: `<div data-uid='aak' style={{ height: 20, top: 410, left: 407, position: 'absolute' }}>
                <div data-uid='aae' style={{ width: 20, height: 20 }}/>
              </div>`,
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
      describe('Paste to Replace', () => {
        const pasteToReplaceTestCases: Array<{
          name: string
          input: string
          copyTargets: Array<ElementPath>
          pasteTargets: Array<ElementPath>
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
            result: `<div data-uid='root'>
              <div data-uid='bbb' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40}}>
                <span data-uid='ccc'>Hello!</span>
              </div>
              <div data-uid='aar' style={{backgroundColor: 'lavender', outline: '1px solid black', width: 40, height: 40, top: 100, left: 100, position: 'absolute' }}>
                <span data-uid='aah'>Hello!</span>
              </div>
              <div data-uid='aan' style={{position: 'absolute', top: 140, left: 140, backgroundColor: 'plum', outline: '1px solid white'}}>
                  <span data-uid='aae' style={{color: 'white'}}>second element</span>
                </div>
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

            await pressKey('Esc')

            // Wait for the next frame
            await renderResult.getDispatchFollowUpActionsFinished()

            expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
              makeTestProjectCodeWithSnippet(tt.result),
            )
          })
        })
      })
      describe('pasting with props replaced', () => {
        setFeatureForBrowserTests('Paste strategies', true)

        async function runPaste(editor: EditorRenderResult) {
          const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          await clipboardMock.pasteDone
          await editor.getDispatchFollowUpActionsFinished()

          await pressKey('Esc')
          await editor.getDispatchFollowUpActionsFinished()
        }

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

          expect(getPrintedUiJsCode(editor.getEditorState()))
            .toEqual(`import * as React from 'react'
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
        top: 403.5,
        left: 570,
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

          expect(getPrintedUiJsCode(editor.getEditorState()))
            .toEqual(`import * as React from 'react'
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
      style={{ top: 420, left: 492, position: 'absolute' }}
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

          expect(getPrintedUiJsCode(editor.getEditorState()))
            .toEqual(`import * as React from 'react'
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
            top: 100,
            left: 100,
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

          expect(getPrintedUiJsCode(editor.getEditorState()))
            .toEqual(`import * as React from 'react'
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
            left: 6,
            top: 6,
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

          expect(getPrintedUiJsCode(editor.getEditorState()))
            .toEqual(`import * as React from 'react'
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
            left: 6,
            top: 6,
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
        setFeatureForBrowserTests('Paste strategies', true)

        it('copy element with code in child and grandchild', async () => {
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

          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          await wait(ControlDelay + 1)

          await pressKey('2')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(
            renderResult.getEditorState().editor.canvas.interactionSession?.userPreferredStrategy,
          ).toEqual(PasteWithPropertiesPreservedStrategyId)

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
              <div data-uid='aar'>
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
      })

      describe('ending the paste session', () => {
        setFeatureForBrowserTests('Paste strategies', true)

        async function setupPasteSession(): Promise<EditorRenderResult> {
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
              <div data-uid='aaf'>
                <div
                  data-uid='aab'
                  style={{
                    position: 'absolute',
                    left: 20,
                    top: 50,
                    bottom: 150,
                    width: 100,
                  }}
                />
                <div
                  data-uid='aad'
                  style={{ width: 60, height: 60 }}
                />
              </div>
            </div>
    `),
          )
        }

        it('the paste session ends on mousedown', async () => {
          const renderResult = await setupPasteSession()
          expect(
            renderResult.getEditorState().editor.canvas.interactionSession?.interactionData.type,
          ).toEqual('DISCRETE_REPARENT')

          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')
          await mouseDownAtPoint(canvasRoot, { x: 42, y: 24 })
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(renderResult.getEditorState().editor.canvas.interactionSession).toBeNull()
          expectResultsToBeCommitted(renderResult)
        })

        it('the paste session ends on keydown', async () => {
          const renderResult = await setupPasteSession()
          expect(
            renderResult.getEditorState().editor.canvas.interactionSession?.interactionData.type,
          ).toEqual('DISCRETE_REPARENT')

          await keyDown('Esc')
          await renderResult.getDispatchFollowUpActionsFinished()

          expect(renderResult.getEditorState().editor.canvas.interactionSession).toBeNull()
          expectResultsToBeCommitted(renderResult)
          expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa/aaf', // this is the element that just got pasted, the selection doesn't jump to the parent
          ])

          await keyDown('Esc')

          expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
            'utopia-storyboard-uid/scene-aaa/app-entity:aaa', // the pasted element's parent is selected, which means the shortcut is not prevented anymore
          ])
        })

        it('the paste session ends on a new paste event', async () => {
          const renderResult = await setupPasteSession()
          clipboardMock.resetDoneSignal()
          const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          await clipboardMock.pasteDone
          await renderResult.getDispatchFollowUpActionsFinished()

          expectResultsToBeCommitted(renderResult)
          expect(
            renderResult.getEditorState().editor.canvas.interactionSession?.interactionData.type,
          ).toEqual('DISCRETE_REPARENT')
        })
      })

      describe('mouse events during paste session', () => {
        setFeatureForBrowserTests('Paste strategies', true)

        it('hover', async () => {
          const editor = await renderTestEditorWithCode(
            `import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          
          const App = () => (
            <div
              style={{
                position: 'relative',
                height: '100%',
                width: '100%',
              }}
              data-uid='root'
            >
              <div
                style={{
                  backgroundColor: '#00FF26',
                  width: 103,
                  height: 90,
                  contain: 'layout',
                  position: 'absolute',
                  left: 26,
                  top: 31,
                }}
                data-testid='element-to-be-copied'
                data-uid='div'
              />
            </div>
          )
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 419,
                  height: 363,
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

          await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root/div')])
          await pressKey('c', { modifiers: cmdModifier })
          await editor.getDispatchFollowUpActionsFinished()

          const canvasRoot = editor.renderedDOM.getByTestId('canvas-root')

          firePasteEvent(canvasRoot)

          await clipboardMock.pasteDone
          await editor.getDispatchFollowUpActionsFinished()

          expect(
            editor.getEditorState().editor.canvas.interactionSession?.interactionData.type,
          ).toEqual('DISCRETE_REPARENT')

          const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
          const originalElementBounds = editor.renderedDOM
            .getAllByTestId('element-to-be-copied')
            .at(0)!
            .getBoundingClientRect()
          await mouseMoveToPoint(canvasControlsLayer, {
            x: originalElementBounds.x + 1,
            y: originalElementBounds.y + 1,
          })

          await editor.getDispatchFollowUpActionsFinished()

          expect(editor.getEditorState().editor.highlightedViews.map(EP.toString)).toEqual([
            'sb/scene/app:root/div',
          ])
        })
      })
    })

    describe('UNWRAP_ELEMENT', () => {
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
        await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/bbb'))], true)

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
        await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/bbb'))], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(
            `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='ccc' style={{position: 'absolute', left: 50, top: 80, bottom: 135, width: 100}} />
          </div>`,
          ),
        )
      })
      it(`Unwraps an flex element`, async () => {
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
        await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/bbb'))], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(
            `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='ccc' style={{width: 50, height: 100, left: 80, top: 55, position: 'absolute'}} />
          </div>`,
          ),
        )
      })
      it(`Doesn't unwrap an image, as it cannot have child elements, no changes in the code result`, async () => {
        const testCode = `
        <div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
          <img
            src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
            alt='Utopia logo'
            data-uid='bbb'
          />
        </div>
      `
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(testCode),
          'await-first-dom-report',
        )
        await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/bbb'))], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(
            `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <img
              src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
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
        await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/bbb'))], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(
            `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}} />`,
          ),
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
        await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/fragment'))], true)

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
          await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/conditional'))], true)

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
          await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/conditional'))], true)

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
          await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/conditional'))], true)

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
          await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/conditional'))], true)

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
          await renderResult.dispatch([unwrapElement(makeTargetPath('aaa/conditional'))], true)

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
            [unwrapElement(makeTargetPath('aaa/conditional/conditional2'))],
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
            [unwrapElement(makeTargetPath('aaa/conditional/conditional2'))],
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
        await renderResult.dispatch(
          [
            wrapInElement([makeTargetPath('aaa/ccc'), makeTargetPath('aaa/ddd')], {
              element: defaultDivElement(testUID),
              importsToAdd: {},
            }),
          ],
          true,
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(
            `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div style={{backgroundColor: '#aaaaaa33', position: 'absolute'}} data-uid='${testUID}'>
              <div data-uid='ccc' style={{position: 'absolute', left: 20, top: 50, bottom: 150, width: 100}} />
              <div data-uid='ddd' style={{width: 60, height: 60}} />
            </div>
          </div>`,
          ),
        )
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
        await renderResult.dispatch(
          [
            wrapInElement([makeTargetPath('aaa/bbb/eee'), makeTargetPath('aaa/bbb/ddd')], {
              element: defaultDivElement(testUID),
              importsToAdd: {},
            }),
          ],
          true,
        )

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(
            `<div data-uid='aaa' style={{contain: 'layout', width: 300, height: 300}}>
            <div data-uid='bbb' style={{display: 'flex', gap: 10, padding: 10}}>
              <div data-uid='ccc' style={{width: 100, height: 60}} />
              <div style={{backgroundColor: '#aaaaaa33', position: 'absolute'}} data-uid='${testUID}'>
                <div data-uid='ddd' style={{flexGrow: 1, height: '100%'}} />
                <div data-uid='eee' style={{width: 100, height: 60}} />
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
        await renderResult.dispatch(
          [
            wrapInElement([makeTargetPath('aaa/ccc'), makeTargetPath('aaa/ddd')], {
              element: jsxFragment(testUID, [], true),
              importsToAdd: {},
            }),
          ],
          true,
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
    })
  })
})
