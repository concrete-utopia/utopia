/* eslint-disable jest/expect-expect */
import * as EP from '../../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../../components/canvas/ui-jsx.test-utils'
import {
  deleteSelected,
  pasteJSXElements,
  selectComponents,
  unwrapElement,
} from './action-creators'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ElementPaste } from '../action-types'
import { act } from '@testing-library/react'
import { ReparentTargetParent } from '../store/reparent-target'
import { getElementFromRenderResult } from './actions.test-utils'
import { JSXConditionalExpression } from '../../../core/shared/element-template'

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
    type PasteTest = {
      name: string
      startingCode: string
      elements: (renderResult: EditorRenderResult) => Array<ElementPaste>
      pasteInto: ReparentTargetParent<ElementPath>
      want: string
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['aaa']),
        want: `
        <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            <div data-uid='aab'>foo</div>
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['aaa']),
        want: `
        <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
            <div data-uid='ddd'>baz</div>
            <div data-uid='aab'>foo</div>
            <div data-uid='aac'>bar</div>
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
          const path = EP.appendNewElementPath(TestScenePath, ['root', '38e'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', 'aaa']),
        want: `
		    <div data-uid='root'>
            <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
                <>
                    <div data-uid='aab'>hello</div>
                    <div data-uid='aac'>there</div>
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
          const path = EP.appendNewElementPath(TestScenePath, ['root', '38e'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', 'aaa']),
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', 'aaa']),
        want: `
		<div data-uid='root'>
            <div data-uid='aaa'>
            <div data-uid='bbb'>foo</div>
            <div data-uid='ccc'>bar</div>
                {
                    // @utopia/uid=conditional
                    true ? (
                        <div data-uid='aab'>true</div>
                    ): (
                        <div data-uid='aac'>false</div>
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', '38e']),
        want: `
        <div data-uid='root'>
            <>
                <div data-uid='aaa'>foo</div>
                <div data-uid='aab'>bar</div>
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', '38e']),
        want: `
        <div data-uid='root'>
            <>
            	<div data-uid='aaa'>foo</div>
                <div data-uid='aab'>bar</div>
                <div data-uid='aac'>baz</div>
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', 'conditional', 'a25']),
        want: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? <div data-uid='aab'>bar</div> : <div data-uid='aaa'>foo</div>
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root', 'conditional', 'a25']),
        want: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : <div data-uid='aab'>bar</div>
            }
            <div data-uid='bbb'>bar</div>
    	</div>
		`,
      },
      {
        name: 'an element inside a non-empty conditional branch (does nothing)',
        startingCode: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : <div data-uid='bbb'>bar</div>
            }
            <div data-uid='ccc'>baz</div>
        </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'ccc'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: {
          clause: 'true-case',
          elementPath: EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
        },
        want: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : <div data-uid='bbb'>bar</div>
            }
            <div data-uid='ccc'>baz</div>
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
              originalElementPath: barPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: {
          clause: 'true-case',
          elementPath: EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
        },
        want: `
        <div data-uid='root'>
            {
            	// @utopia/uid=conditional
                true ? (
                    <React.Fragment>
                    	<div data-uid='aab'>bar</div>
                    	<div data-uid='aac'>baz</div>
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
              originalElementPath: barPath,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: {
          clause: 'false-case',
          elementPath: EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
        },
        want: `
        <div data-uid='root'>
        	{
            	// @utopia/uid=conditional
                true ? <div data-uid='aaa'>foo</div> : (
                    <React.Fragment>
                    	<div data-uid='aab'>bar</div>
                    	<div data-uid='aac'>baz</div>
                    </React.Fragment>
                )
            }
            <div data-uid='bbb'>bar</div>
            <div data-uid='ccc'>baz</div>
        </div>
		`,
      },
      {
        name: 'an fragment inside an empty conditional branch',
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
          const path = EP.appendNewElementPath(TestScenePath, ['root', '38e'])
          return [
            {
              element: getElementFromRenderResult(renderResult, path),
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: {
          clause: 'true-case',
          elementPath: EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
        },
        want: `
        <div data-uid='root'>
            {
                // @utopia/uid=conditional
                true ? (
                    <>
                    	<div data-uid='aab'>bar</div>
                    	<div data-uid='aac'>baz</div>
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
          const firstPath = EP.appendNewElementPath(TestScenePath, ['root', '38e'])
          const secondPath = EP.appendNewElementPath(TestScenePath, ['root', 'c9d'])
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
        pasteInto: {
          clause: 'true-case',
          elementPath: EP.appendNewElementPath(TestScenePath, ['root', 'conditional']),
        },
        want: `
      <div data-uid='root'>
      {
        // @utopia/uid=conditional
        true ? (
          <React.Fragment>
            <>
              <div data-uid='aab'>bar</div>
              <div data-uid='aac'>baz</div>
            </>
            <>
              <div data-uid='aae'>qux</div>
              <div data-uid='aaf'>waldo</div>
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
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root']),
        want: `
      <div data-uid='root'>
        {
          // @utopia/uid=conditional
          true ? <div data-uid='aaa'>foo</div> : null
        }
        <div data-uid='aab'>foo</div>
      </div>
		`,
      },
      {
        name: 'an inactive conditional branch',
        startingCode: `
    <div data-uid='root'>
      {
        // @utopia/uid=conditional
        true ? null : <div data-uid='aaa'>foo</div>
      }
    </div>
		`,
        elements: (renderResult) => {
          const path = EP.appendNewElementPath(TestScenePath, ['root', 'conditional', 'a25'])
          return [
            {
              element: (
                renderResult.getEditorState().editor.jsxMetadata[
                  'utopia-storyboard-uid/scene-aaa/app-entity:root/conditional'
                ].element.value as JSXConditionalExpression
              ).whenFalse,
              originalElementPath: path,
              importsToAdd: {},
            },
          ]
        },
        pasteInto: EP.appendNewElementPath(TestScenePath, ['root']),
        want: `
    <div data-uid='root'>
      {
        // @utopia/uid=conditional
        true ? null : <div data-uid='aaa'>foo</div>
      }
      <div data-uid='aab' style={{ display: 'block' }}>foo</div>
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

        await act(async () => {
          await renderResult.dispatch(
            [
              pasteJSXElements(
                test.pasteInto,
                test.elements(renderResult),
                renderResult.getEditorState().editor.jsxMetadata,
              ),
            ],
            true,
          )
        })
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(test.want),
        )
      })
    })
  })
  describe('UNWRAP_ELEMENT', () => {
    it(`Unwraps a content-affecting element`, async () => {
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
  })
})
