/// <reference types="karma-viewport" />
/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "makeTestCase"] }] */

import * as EP from '../shared/element-path'
import {
  AllFragmentLikeTypes,
  FragmentLikeType,
} from '../../components/canvas/canvas-strategies/strategies/fragment-like-helpers'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  FragmentLikeElementUid,
} from '../../components/canvas/canvas-strategies/strategies/fragment-like-helpers.test-utils'
import {
  formatTestProjectCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../components/canvas/ui-jsx.test-utils'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { cartesianProduct } from '../shared/array-utils'
import type { Either } from '../shared/either'
import { isRight } from '../shared/either'
import { elementPath, fromString, toString } from '../shared/element-path'
import { canvasRectangle, localRectangle, zeroCanvasRect } from '../shared/math-utils'
import {
  textFile,
  type ElementPath,
  textFileContents,
  codeFile,
} from '../shared/project-file-types'
import { MetadataUtils } from './element-metadata-utils'
import { elementOnlyHasTextChildren } from './element-template-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from './scene-utils'
import type { HugPropertyWidthHeight } from '../shared/element-template'
import { contentsToTree } from '../../components/assets'
import type { VariableData } from '../../components/canvas/ui-jsx-canvas'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'

describe('Frame calculation for fragments', () => {
  // Components with root fragments do not appear in the DOM, so the dom walker does not find them, and they
  // do not appear in the dom metadata.
  // To make sure they still have globalFrame and localFrame in the jsxMetadata, we calculate the frames from
  // their descendants which are rendered in the dom (see mergeComponentMetadata function)

  it('Frames of component with root fragment is union of its children', async () => {
    const fragmentComponentPath = 'story/scene/app:frag'

    const renderResult = await renderTestEditorWithCode(
      TestProjectWithFragment,
      'await-first-dom-report',
    )

    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].localFrame,
    ).toEqual(
      localRectangle({
        x: 64,
        y: 114,
        height: 363,
        width: 210,
      }),
    )
    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].globalFrame,
    ).toEqual(
      canvasRectangle({
        x: 101,
        y: 179,
        height: 363,
        width: 210,
      }),
    )
  })
  it('Frames of component with root fragment and map inside is union of its children', async () => {
    const fragmentComponentPath = 'story/scene/app:frag'

    const renderResult = await renderTestEditorWithCode(
      TestProjectWithFragmentAndMap,
      'await-first-dom-report',
    )

    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].localFrame,
    ).toEqual(
      localRectangle({
        x: 64,
        y: 114,
        height: 90,
        width: 10,
      }),
    )
    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].globalFrame,
    ).toEqual(
      canvasRectangle({
        x: 101,
        y: 179,
        height: 90,
        width: 10,
      }),
    )
  })
  it('Conditionals have metadata and their frame is the frame of the active branch', async () => {
    const condComponentPath = 'story/scene/app:root/cond'
    const trueBranchComponentPath = 'story/scene/app:root/cond/truebranch'

    const renderResult = await renderTestEditorWithCode(
      TestProjectWithConditional,
      'await-first-dom-report',
    )

    expect(renderResult.getEditorState().editor.jsxMetadata[condComponentPath].globalFrame).toEqual(
      renderResult.getEditorState().editor.jsxMetadata[trueBranchComponentPath].globalFrame,
    )
    expect(renderResult.getEditorState().editor.jsxMetadata[condComponentPath].localFrame).toEqual(
      renderResult.getEditorState().editor.jsxMetadata[trueBranchComponentPath].localFrame,
    )
  })
})

const elementPathInInnards = (suffix: string): ElementPath =>
  fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:${suffix}`)

describe('globalContentBoxForChildren calculation', () => {
  it('`globalContentBoxForChildren` of an absolute positioned div is the same as its globalFrame', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='container'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 94,
          top: 107,
          width: 205,
          height: 162,
        }}
      >
        <div
          data-uid='child'
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 46,
            top: 38,
            width: 138,
            height: 71,
          }}
        />
      </div>
      `),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [elementPathInInnards('container')])

    const containerInstance = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      elementPathInInnards('container'),
    )
    if (containerInstance == null) {
      throw new Error('containerInstance should not be null')
    }

    const globalFrameOfContainer = MetadataUtils.getFrameInCanvasCoords(
      elementPathInInnards('container'),
      editor.getEditorState().editor.jsxMetadata,
    )

    expect(globalFrameOfContainer).not.toBeNull()
    expect(globalFrameOfContainer).not.toEqual(zeroCanvasRect)

    const globalContentBoxForChildrenOfContainer =
      MetadataUtils.getGlobalContentBoxForChildren(containerInstance)

    expect(globalContentBoxForChildrenOfContainer).not.toBeNull()
    expect(globalContentBoxForChildrenOfContainer).not.toEqual(zeroCanvasRect)

    expect(globalFrameOfContainer).toEqual(globalContentBoxForChildrenOfContainer)

    const globalFrameOfChild = MetadataUtils.getFrameInCanvasCoords(
      elementPathInInnards('container/child'),
      editor.getEditorState().editor.jsxMetadata,
    )

    expect(globalFrameOfChild).not.toBeNull()
    expect(globalFrameOfChild).not.toEqual(zeroCanvasRect)

    const childInstance = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      elementPathInInnards('container/child'),
    )
    if (childInstance == null) {
      throw new Error('childInstance should not be null')
    }

    const globalContentBoxForChildrenOfChild =
      MetadataUtils.getGlobalContentBoxForChildren(childInstance)

    expect(globalContentBoxForChildrenOfChild).not.toBeNull()
    expect(globalContentBoxForChildrenOfChild).not.toEqual(zeroCanvasRect)

    expect(globalFrameOfChild).toEqual(globalContentBoxForChildrenOfChild)
  })

  describe('fragment-like elements', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`globalContentBoxForChildren of a ${type} is the same as the globalContentBoxForChildren of its parent`, async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
          <div
            data-uid='container'
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 94,
              top: 107,
              width: 205,
              height: 162,
            }}
          >
          ${getOpeningFragmentLikeTag(type)}
            <div
              data-uid='child'
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 46,
                top: 38,
                width: 138,
                height: 71,
              }}
            />
            ${getClosingFragmentLikeTag(type)}
          </div>
          `),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [
          elementPathInInnards(`container/${FragmentLikeElementUid}`),
        ])

        const containerInstance = MetadataUtils.findElementByElementPath(
          editor.getEditorState().editor.jsxMetadata,
          elementPathInInnards('container'),
        )
        if (containerInstance == null) {
          throw new Error('containerInstance should not be null')
        }

        const globalContentBoxForChildrenOfContainer =
          MetadataUtils.getGlobalContentBoxForChildren(containerInstance)

        expect(globalContentBoxForChildrenOfContainer).not.toBeNull()
        expect(globalContentBoxForChildrenOfContainer).not.toEqual(zeroCanvasRect)

        const childInstance = MetadataUtils.findElementByElementPath(
          editor.getEditorState().editor.jsxMetadata,
          elementPathInInnards(`container/${FragmentLikeElementUid}`),
        )
        if (childInstance == null) {
          throw new Error('childInstance should not be null')
        }

        const globalContentBoxForChildrenOfFragmentLikeElement =
          MetadataUtils.getGlobalContentBoxForChildren(childInstance)

        expect(globalContentBoxForChildrenOfContainer).not.toBeNull()
        expect(globalContentBoxForChildrenOfContainer).not.toEqual(zeroCanvasRect)

        expect(globalContentBoxForChildrenOfContainer).toEqual(
          globalContentBoxForChildrenOfFragmentLikeElement,
        )
      })
    })
  })

  describe('globalcontentbox for children of sizeless divs', () => {
    it(`globalContentBoxForChildren is correct for a sizeless div with only height`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid='a7b'
        >
          <div
            style={{
              height: 150,
              position: 'absolute',
              left: 140,
              top: 130,
            }}
            data-uid='b15'
          />
        </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [elementPathInInnards('a7b/b15')])

      const containerInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('a7b'),
      )
      if (containerInstance == null) {
        throw new Error('containerInstance should not be null')
      }

      const globalContentBoxForContainer =
        MetadataUtils.getGlobalContentBoxForChildren(containerInstance)

      expect(globalContentBoxForContainer).toEqual({
        x: 0,
        y: 0,
        width: 400,
        height: 400,
      })

      const childInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('a7b/b15'),
      )
      if (childInstance == null) {
        throw new Error('childInstance should not be null')
      }

      const globalContentBoxForChild = MetadataUtils.getGlobalContentBoxForChildren(childInstance)

      expect(globalContentBoxForChild).toEqual({
        x: 140,
        y: 130,
        width: 0,
        height: 150,
      })
    })
  })

  describe('conditional globalframe from ancestors', () => {
    it(`globalFrame and localFrame of conditionals (without siblings and expression in active branch) are coming from the parent of the conditional`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
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
              height: 150,
              width: 150,
              position: 'absolute',
              left: 154,
              top: 134,
            }}
            data-uid='container'
          >
            {
              // @utopia/uid=conditional
              false ? (
                <div />
              ) : 'hello'
            }
          </div>
        </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [elementPathInInnards('root/container')])

      const containerInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container'),
      )
      if (containerInstance == null) {
        throw new Error('containerInstance should not be null')
      }

      expect(containerInstance.globalFrame).toEqual({
        x: 154,
        y: 134,
        width: 150,
        height: 150,
      })

      const conditionalInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container/conditional'),
      )
      if (conditionalInstance == null) {
        throw new Error('conditionalInstance should not be null')
      }

      expect(conditionalInstance.globalFrame).toEqual(containerInstance.globalFrame)
      expect(conditionalInstance.localFrame).toEqual({
        x: 0,
        y: 0,
        width: 150,
        height: 150,
      })
    })
    it(`globalFrame and localFrame of conditionals (without siblings and expression with element inside in the active branch) are NOT coming from the parent of the conditional`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
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
              height: 150,
              width: 150,
              position: 'absolute',
              left: 154,
              top: 134,
            }}
            data-uid='container'
          >
            {
              // @utopia/uid=conditional
              true ? (
                (() => (
                  <div
                    style={{
                      left: 0,
                      top: 5,
                      width: 10,
                      height: 20,
                    }}
                    data-uid='368'
                  />
                ))()
              ) : <div/>
            }
          </div>
        </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [elementPathInInnards('root/container')])

      const containerInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container'),
      )
      if (containerInstance == null) {
        throw new Error('containerInstance should not be null')
      }

      expect(containerInstance.globalFrame).toEqual({
        x: 154,
        y: 134,
        width: 150,
        height: 150,
      })

      const conditionalInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container/conditional'),
      )
      if (conditionalInstance == null) {
        throw new Error('conditionalInstance should not be null')
      }

      // frames are coming from the div returned by the expression in the active branch
      expect(conditionalInstance.globalFrame).toEqual({
        x: 154,
        y: 134,
        width: 10,
        height: 20,
      })
      expect(conditionalInstance.localFrame).toEqual({
        x: 0,
        y: 0,
        width: 10,
        height: 20,
      })
    })
    it(`globalFrame and localFrame of conditionals can be inherited from the parent through muliple conditionals when the final has an active branch with only an expression inside`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
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
              height: 150,
              width: 150,
              position: 'absolute',
              left: 154,
              top: 134,
            }}
            data-uid='container'
          >
          {
            // @utopia/uid=conditional1
            true ? (
                // @utopia/uid=conditional2
                true ? (
                'hello'
              ) : <div />
            ) : (
              <div />
            )
          }
          </div>
        </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [elementPathInInnards('root/container')])

      const containerInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container'),
      )
      if (containerInstance == null) {
        throw new Error('containerInstance should not be null')
      }

      expect(containerInstance.globalFrame).toEqual({
        x: 154,
        y: 134,
        width: 150,
        height: 150,
      })

      const conditional1Instance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container/conditional1'),
      )
      if (conditional1Instance == null) {
        throw new Error('conditional1Instance should not be null')
      }

      expect(conditional1Instance.globalFrame).toEqual(containerInstance.globalFrame)
      expect(conditional1Instance.localFrame).toEqual({
        x: 0,
        y: 0,
        width: 150,
        height: 150,
      })

      const conditional2Instance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container/conditional1/conditional2'),
      )
      if (conditional2Instance == null) {
        throw new Error('conditional2Instance should not be null')
      }

      expect(conditional2Instance.globalFrame).toEqual(containerInstance.globalFrame)
      expect(conditional2Instance.localFrame).toEqual({
        x: 0,
        y: 0,
        width: 150,
        height: 150,
      })
    })

    it(`globalFrame and localFrame of conditionals (with siblings) are null when the active branch is an expression`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
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
              height: 150,
              width: 150,
              position: 'absolute',
              left: 154,
              top: 134,
            }}
            data-uid='container'
          >
            {
              // @utopia/uid=conditional
              false ? (
                <div />
              ) : 'hello'
            }
            <div />
          </div>
        </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [elementPathInInnards('root/container')])

      const conditionalInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container/conditional'),
      )
      if (conditionalInstance == null) {
        throw new Error('nullInstance should not be null')
      }

      expect(conditionalInstance.globalFrame).toBeNull()
      expect(conditionalInstance.localFrame).toBeNull()
    })
  })

  describe('nested fragment-like elements', () => {
    cartesianProduct(AllFragmentLikeTypes, AllFragmentLikeTypes).forEach(
      ([outerType, innerType]) => {
        it(`globalContentBoxForChildren of a ${innerType} wrapped in a ${outerType} is the same as the globalContentBoxForChildren of their closest non-fragment-like parent`, async () => {
          const editor = await renderTestEditorWithCode(
            makeTestProjectCodeWithSnippet(`
          <div
            data-uid='container'
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 94,
              top: 107,
              width: 205,
              height: 162,
            }}
          >
          ${getOpeningFragmentLikeTag(outerType, {
            outerUid: 'outer-caf',
            innerUid: 'inner-caf-fragment',
          })}
          ${getOpeningFragmentLikeTag(innerType)}
            <div
              data-uid='child'
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 46,
                top: 38,
                width: 138,
                height: 71,
              }}
            />
            ${getClosingFragmentLikeTag(innerType)}
            ${getClosingFragmentLikeTag(outerType)}
          </div>
          `),
            'await-first-dom-report',
          )

          await selectComponentsForTest(editor, [
            elementPathInInnards(
              `container/outer-caf/inner-caf-fragment/${FragmentLikeElementUid}`,
            ),
          ])

          const containerInstance = MetadataUtils.findElementByElementPath(
            editor.getEditorState().editor.jsxMetadata,
            elementPathInInnards('container'),
          )
          if (containerInstance == null) {
            throw new Error('containerInstance should not be null')
          }

          const globalContentBoxForChildrenOfContainer =
            MetadataUtils.getGlobalContentBoxForChildren(containerInstance)

          expect(globalContentBoxForChildrenOfContainer).not.toBeNull()
          expect(globalContentBoxForChildrenOfContainer).not.toEqual(zeroCanvasRect)

          const childInstance = MetadataUtils.findElementByElementPath(
            editor.getEditorState().editor.jsxMetadata,
            elementPathInInnards(
              `container/outer-caf/inner-caf-fragment/${FragmentLikeElementUid}`,
            ),
          )
          if (childInstance == null) {
            throw new Error('containerInstance should not be null')
          }

          const globalContentBoxForChildrenOfFragmentLikeElement =
            MetadataUtils.getGlobalContentBoxForChildren(childInstance)

          expect(globalContentBoxForChildrenOfFragmentLikeElement).not.toBeNull()
          expect(globalContentBoxForChildrenOfFragmentLikeElement).not.toEqual(zeroCanvasRect)

          expect(globalContentBoxForChildrenOfContainer).toEqual(
            globalContentBoxForChildrenOfFragmentLikeElement,
          )
        })
      },
    )
  })
})

function asRight<L, R>(e: Either<L, R>): R {
  if (isRight(e)) {
    return e.value
  }
  throw new Error('found left')
}

describe('elementHasTextOnlyChildren', () => {
  it('element containing only text is considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem =
      editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb', 'hello']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(true)
  })
  it('element containing text and <br /> tags is considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem =
      editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb', 'hibr']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(true)
  })
  it('element containing <br /> tags is considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem =
      editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb', 'brbrbr']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(true)
  })
  it('element containing divs is not considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem = editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(false)
  })
})

describe('getSiblingsOrdered', () => {
  it('elephants on the storyboard', async () => {
    const editor = await renderTestEditorWithCode(
      `
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    
    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div data-uid='ddd' />
        <div data-uid='aaa' />
        {
          // @utopia/uid=conditional
          [].length === 0 ? null : null
        }
        <div data-uid='ccc' />
        <div data-uid='bbb' />
      </Storyboard>
    )    
    `,
      'await-first-dom-report',
    )

    const siblingsOrdered = MetadataUtils.getSiblingsOrdered(
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.elementPathTree,
      EP.fromString('sb/aaa'),
    )
    expect(siblingsOrdered.map((i) => EP.toString(i.elementPath))).toEqual([
      'sb/ddd',
      'sb/aaa',
      'sb/conditional',
      'sb/ccc',
      'sb/bbb',
    ])
  })

  it('elephants in a container', async () => {
    const editor = await renderTestEditorWithCode(
      `
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    
    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div data-uid='aaa' />
        <div data-uid='bbb' />
        {
          // @utopia/uid=conditional
          [].length === 0 ? null : null
        }
        <div data-uid='ccc' />
        <div data-uid='ddd' />
        <div data-uid='eee'>
          <div data-uid='xxx' />
          {
            // @utopia/uid=conditional-inside
            [].length === 0 ? null : null
          }
          <div data-uid='ttt' />
        </div>
      </Storyboard>
    )    
    `,
      'await-first-dom-report',
    )

    const siblingsOrdered = MetadataUtils.getSiblingsOrdered(
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.elementPathTree,
      EP.fromString('sb/eee/xxx'),
    )
    expect(siblingsOrdered.map((i) => EP.toString(i.elementPath))).toEqual([
      'sb/eee/xxx',
      'sb/eee/conditional-inside',
      'sb/eee/ttt',
    ])
  })
})

describe('isAutofocusable/isManuallyFocusableComponent', () => {
  it('returns the right result for various elements', async () => {
    const editor = await renderTestEditorWithCode(
      TestProjectWithSeveralComponents,
      'await-first-dom-report',
    )
    const metadata = editor.getEditorState().editor.jsxMetadata
    const pathTrees = editor.getEditorState().editor.elementPathTree
    const autoFocusedPaths = editor.getEditorState().derived.autoFocusedPaths
    const allPaths = MetadataUtils.getAllPaths(metadata, pathTrees)
    const propertyControlsInfo = editor.getEditorState().editor.propertyControlsInfo
    const projectContents = editor.getEditorState().editor.projectContents
    const isAutofocusableResults = allPaths.map((path) => {
      return `${EP.toString(path)}: ${MetadataUtils.isAutofocusable(
        metadata,
        pathTrees,
        path,
        propertyControlsInfo,
        projectContents,
      )}`
    })
    expect(isAutofocusableResults).toEqual([
      'story/scene: false',
      'story/scene/app: true',
      'story/scene/app:app-inner-div: false',
      'story/scene/app:app-inner-div/inner-app-1: false',
      'story/scene/app:app-inner-div/inner-app-2: false',
    ])
    const isFocusableComponentResults = allPaths.map((path) => {
      return `${EP.toString(path)}: ${MetadataUtils.isManuallyFocusableComponent(
        path,
        metadata,
        autoFocusedPaths,
        [],
        propertyControlsInfo,
        projectContents,
      )}`
    })
    expect(isFocusableComponentResults).toEqual([
      'story/scene: false',
      'story/scene/app: false',
      'story/scene/app:app-inner-div: false',
      'story/scene/app:app-inner-div/inner-app-1: true',
      'story/scene/app:app-inner-div/inner-app-2: true',
    ])
  })

  it('uses the focus property in the component descriptor', async () => {
    const editor = await renderTestEditorWithModel(
      TestProjectWithComponentDescriptor,
      'await-first-dom-report',
    )
    const metadata = editor.getEditorState().editor.jsxMetadata
    const pathTrees = editor.getEditorState().editor.elementPathTree
    const autoFocusedPaths = editor.getEditorState().derived.autoFocusedPaths
    const allPaths = MetadataUtils.getAllPaths(metadata, pathTrees)
    const propertyControlsInfo = editor.getEditorState().editor.propertyControlsInfo
    const projectContents = editor.getEditorState().editor.projectContents
    const isAutofocusableResults = allPaths.map((path) => {
      return `${EP.toString(path)}: ${MetadataUtils.isAutofocusable(
        metadata,
        pathTrees,
        path,
        propertyControlsInfo,
        projectContents,
      )}`
    })
    expect(isAutofocusableResults).toEqual([
      'story/scene: false',
      'story/scene/app: true',
      'story/scene/app:app-inner-div: false',
      'story/scene/app:app-inner-div/inner-app-1: true', // focus set to always
      'story/scene/app:app-inner-div/inner-app-1:inner-app-1-div: false',
      'story/scene/app:app-inner-div/inner-app-1:inner-app-1-div/inner-app-1-nested-div: false',
      'story/scene/app:app-inner-div/inner-app-2: false',
    ])
    const isFocusableComponentResults = allPaths.map((path) => {
      return `${EP.toString(path)}: ${MetadataUtils.isManuallyFocusableComponent(
        path,
        metadata,
        autoFocusedPaths,
        [],
        propertyControlsInfo,
        projectContents,
      )}`
    })
    expect(isFocusableComponentResults).toEqual([
      'story/scene: false',
      'story/scene/app: false',
      'story/scene/app:app-inner-div: false',
      'story/scene/app:app-inner-div/inner-app-1: false', // not focusable because autofocused
      'story/scene/app:app-inner-div/inner-app-1:inner-app-1-div: false',
      'story/scene/app:app-inner-div/inner-app-1:inner-app-1-div/inner-app-1-nested-div: false',
      'story/scene/app:app-inner-div/inner-app-2: false', // focus set to never
    ])
  })

  it('supports components imported with a file mapped import statement', async () => {
    const sampleProjectContents = contentsToTree({
      'jsconfig.json': codeFile(
        `
        {
          "compilerOptions": {
            "paths": {
              "~/*": [
                "app/*"
              ]
            }
          }
        }
        `,
        null,
      ),
      '/app/card.js': codeFile(
        `
        import * as React from 'react'

        export const Card = (props) => {
          return (
            <div style={props.style} data-uid='card-root'>
              <div
                style={{
                  position: 'absolute',
                  left: 10,
                  top: 10,
                }}
                data-uid='card-root-child'
              >
                I am a card
              </div>
            </div>
          )
        }
        `,
        null,
      ),
      '/utopia/storyboard.js': codeFile(
        `
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'
        import { Card } from '~/card'

        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Card
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 150,
                height: 150,
              }}
              data-uid='card'
            />
          </Storyboard>
        )
        `,
        null,
      ),
    })
    const renderResult = await renderTestEditorWithProjectContent(
      sampleProjectContents,
      'await-first-dom-report',
    )
    const metadata = renderResult.getEditorState().editor.jsxMetadata
    const pathTrees = renderResult.getEditorState().editor.elementPathTree
    const autoFocusedPaths = renderResult.getEditorState().derived.autoFocusedPaths
    const filePathMappings = renderResult.getEditorState().derived.filePathMappings
    const propertyControlsInfo = renderResult.getEditorState().editor.propertyControlsInfo
    const projectContents = renderResult.getEditorState().editor.projectContents
    const targetPath = EP.fromString('sb/card')
    expect(
      MetadataUtils.isAutofocusable(
        metadata,
        pathTrees,
        targetPath,
        propertyControlsInfo,
        projectContents,
      ),
    ).toBeFalsy()
    expect(
      MetadataUtils.isManuallyFocusableComponent(
        targetPath,
        metadata,
        autoFocusedPaths,
        filePathMappings,
        propertyControlsInfo,
        projectContents,
      ),
    ).toEqual(true)
  })
})

describe('fillSpyOnlyMetadata', () => {
  it('layoutSystemForChildren is filled out correctly for conditionals', async () => {
    const renderResult = await renderTestEditorWithCode(
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
        left: 15,
        top: 18,
        width: 164,
        height: 55,
        display: 'flex',
        flexDirection: 'row',
        gap: 10,
      }}
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 43,
          height: 35,
          contain: 'layout',
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 31,
          height: 29,
          contain: 'layout',
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 33,
          height: 37,
          contain: 'layout',
        }}
      />
      {
        // @utopia/uid=conditional
        true ? null : (
        <div style={{ width: 100, height: 100 }}>
          False branch
        </div>
      )}
    </div>
  </div>`),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['root', 'container', 'conditional'])
    const metadata = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      targetPath,
    )
    expect(metadata).not.toBeNull()

    expect(metadata!.specialSizeMeasurements.layoutSystemForChildren).toEqual('flex')
  })
})

describe('computedHugProperty', () => {
  async function makeTestCase(
    snippet: string,
    target: ElementPath,
    expected: HugPropertyWidthHeight,
  ) {
    const code = `
      import * as React from 'react'
      import { Scene, Storyboard, View, Group } from 'utopia-api'

      const css = \`.minContent {
        width: min-content;
        height: min-content;
      }

      .maxContent {
        width: max-content;
        height: max-content;
      }
      
      .auto {
        width: auto;
        height: auto;
      }\`

      export var App = (props) => (
        <div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid='root'
        >
          ${snippet}
        </div>
      )

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <style>{css}</style>
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

    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(code),
      'await-first-dom-report',
    )

    const metadata = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      target,
    )
    expect(metadata).not.toBeNull()

    expect(metadata!.specialSizeMeasurements.computedHugProperty).toEqual(expected)
  }

  describe('from style props', () => {
    it('computedHugProperty.width/height is hug when width/height is max-content', async () => {
      await makeTestCase(
        `<div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 'max-content',
          height: 'max-content',
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'hug', height: 'hug' },
      )
    })

    it('computedHugProperty.width/height is squeeze when width/height is min-content', async () => {
      await makeTestCase(
        `<div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 'min-content',
          height: 'min-content',
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'squeeze', height: 'squeeze' },
      )
    })

    it('computedHugProperty.width/height is collapsed when it is hugging but children size is zero', async () => {
      await makeTestCase(
        `<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 'max-content',
          height: 'max-content',
        }}
        data-uid='div'
      />`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'collapsed', height: 'collapsed' },
      )
    })

    it('computedHugProperty.width/height is null when TLBR pins are set', async () => {
      await makeTestCase(
        `<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 79,
          top: 357,
          bottom: 457,
          right: 109,
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: null, height: null },
      )
    })

    it('computedHugProperty.width is null and height is hug when width/height is not set and no TLBR set (for display block)', async () => {
      await makeTestCase(
        `<div
        style={{
          backgroundColor: '#aaaaaa33',
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: null, height: 'hug' },
      )
    })

    it('computedHugProperty.width/height is null and height is hug when width/height is not set and no TLBR set (for display inline)', async () => {
      await makeTestCase(
        `<span
        style={{
          backgroundColor: '#aaaaaa33',
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </span>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'hug', height: 'hug' },
      )
    })

    it('computedHugProperty.width is null and height is hug when width/height is auto (for display block)', async () => {
      await makeTestCase(
        `<div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 'auto',
          height: 'auto',
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: null, height: 'hug' },
      )
    })

    it('computedHugProperty.width/height is null and height is hug when width/height is auto (for display inline)', async () => {
      await makeTestCase(
        `<span
        style={{
          backgroundColor: '#aaaaaa33',
          width: 'auto',
          height: 'auto',
        }}
        data-uid='div'
      >
        Hug a Utopia!
      </span>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'hug', height: 'hug' },
      )
    })
  })

  describe('from css', () => {
    it('computedHugProperty.width/height is hug when width/height is max-content', async () => {
      await makeTestCase(
        `<div
          className='maxContent'
          style={{
            backgroundColor: '#aaaaaa33',
          }}
          data-uid='div'
        >
          Hug a Utopia!
        </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'hug', height: 'hug' },
      )
    })

    it('computedHugProperty.width/height is squeeze when width/height is min-content', async () => {
      await makeTestCase(
        `<div
          className='minContent'
          style={{
            backgroundColor: '#aaaaaa33',
          }}
          data-uid='div'
        >
          Hug a Utopia!
        </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'squeeze', height: 'squeeze' },
      )
    })

    it('computedHugProperty.width/height is collapsed when it is hugging but children size is zero', async () => {
      await makeTestCase(
        `<div
          className='maxContent'
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 0,
            top: 0,
          }}
          data-uid='div'
        />`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'collapsed', height: 'collapsed' },
      )
    })

    it('computedHugProperty.width is null and height is hug when width/height is auto (for display block)', async () => {
      await makeTestCase(
        `<div
          className='auto'
          style={{
            backgroundColor: '#aaaaaa33',
          }}
          data-uid='div'
        >
          Hug a Utopia!
        </div>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: null, height: 'hug' },
      )
    })

    it('computedHugProperty.width/height is null and height is hug when width/height is auto (for display inline)', async () => {
      await makeTestCase(
        `<span
          className='auto'
          style={{
            backgroundColor: '#aaaaaa33',
            width: 'auto',
            height: 'auto',
          }}
          data-uid='div'
        >
          Hug a Utopia!
        </span>`,
        EP.appendNewElementPath(TestScenePath, ['root', 'div']),
        { width: 'hug', height: 'hug' },
      )
    })
  })
})

describe('record variable values', () => {
  it('records variables that are defined inside the component', async () => {
    const editor = await renderTestEditorWithCode(ProjectWithVariables, 'await-first-dom-report')
    const { variablesInScope } = editor.getEditorState().editor
    expect(variablesInScope['sb/scene/pg:root']).toEqual<VariableData>({
      add: {
        insertionCeiling: { type: 'file-root' },
        spiedValue: expect.any(Function),
      },
      definedInsideNumber: { spiedValue: 12, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      definedInsideObject: {
        spiedValue: {
          prop: [33],
        },
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      definedInsideString: {
        spiedValue: 'hello',
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      functionResult: { spiedValue: 35, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      style: { spiedValue: {}, insertionCeiling: EP.fromString('sb/scene/pg:root') },
    })
    expect(variablesInScope['sb/scene/pg:root/111']).toEqual<VariableData>({
      add: {
        insertionCeiling: { type: 'file-root' },
        spiedValue: expect.any(Function),
      },
      definedInsideNumber: { spiedValue: 12, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      definedInsideObject: {
        spiedValue: {
          prop: [33],
        },
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      definedInsideString: {
        spiedValue: 'hello',
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      functionResult: { spiedValue: 35, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      style: { spiedValue: {}, insertionCeiling: EP.fromString('sb/scene/pg:root') },
    })
    expect(variablesInScope['sb/scene/pg:root/222']).toEqual<VariableData>({
      add: {
        insertionCeiling: { type: 'file-root' },
        spiedValue: expect.any(Function),
      },
      definedInsideNumber: { spiedValue: 12, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      definedInsideObject: {
        spiedValue: {
          prop: [33],
        },
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      definedInsideString: {
        spiedValue: 'hello',
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      functionResult: { spiedValue: 35, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      style: { spiedValue: {}, insertionCeiling: EP.fromString('sb/scene/pg:root') },
    })
    expect(variablesInScope['sb/scene/pg:root/333']).toEqual<VariableData>({
      add: {
        insertionCeiling: { type: 'file-root' },
        spiedValue: expect.any(Function),
      },
      definedInsideNumber: { spiedValue: 12, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      definedInsideObject: {
        spiedValue: {
          prop: [33],
        },
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      definedInsideString: {
        spiedValue: 'hello',
        insertionCeiling: EP.fromString('sb/scene/pg:root'),
      },
      functionResult: { spiedValue: 35, insertionCeiling: EP.fromString('sb/scene/pg:root') },
      style: { spiedValue: {}, insertionCeiling: EP.fromString('sb/scene/pg:root') },
    })
  })
})

describe('specialSizeMeasurements.globalFrameWithTextContent', () => {
  // Disabled temporarily as we're seeing different results in different places.
  xit('includes the size of a contained text node', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div data-uid='div' style={{position: 'absolute', left: 0, top: 0, width: 50, height: 50}}>
        THIS IS A BIG CHUNK OF TEXT WHICH SHOULD MAKE THE OVERALL SIZE A LITTLE LARGER 
      </div>
    `),
      'await-first-dom-report',
    )
    const divMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        `utopia-storyboard-uid/scene-aaa/app-entity:div`
      ]
    expect(divMetadata.specialSizeMeasurements.globalFrameWithTextContent).toEqual({
      x: 0,
      y: 0,
      height: 251,
      width: 74.5,
    })
  })
  it('does not include the size of a contained div', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div data-uid='div' style={{position: 'absolute', left: 0, top: 0, width: 50, height: 50}}>
        <div style={{position: 'absolute', left: 0, top: 0, width: 30, height: 30}} /> 
      </div>
    `),
      'await-first-dom-report',
    )
    const divMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        `utopia-storyboard-uid/scene-aaa/app-entity:div`
      ]
    expect(divMetadata.specialSizeMeasurements.globalFrameWithTextContent).toEqual({
      x: 0,
      y: 0,
      width: 50,
      height: 50,
    })
  })
})

const TestProjectWithSeveralComponents = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var InnerApp1 = (props) => {
  return (
    <div data-uid='inner-app-1-div'>
      <div
        data-uid='inner-app-1-nested-div'
        style={{
          left: 0,
          top: 0,
          width: 50,
          height: 50,
          position: 'absolute',
          backgroundColor: 'red',
        }}
      />
    </div>
  )
}

export const InnerApp2 = (props) => {
  return (
    <div data-uid='inner-app-2-div'>
      <div
        data-uid='inner-app-2-nested-div'
        style={{
          left: 0,
          top: 60,
          width: 50,
          height: 50,
          position: 'absolute',
          backgroundColor: 'blue',
        }}
      />
    </div>
  )
}

export var App = (props) => {
  return (
    <div
      data-uid='app-inner-div'
      style={{
        left: 0,
        top: 0,
        width: 400,
        height: 400,
        position: 'absolute',
        backgroundColor: 'white',
      }}
    >
      <InnerApp1 data-uid='inner-app-1' />
      <InnerApp2 data-uid='inner-app-2' />
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
`

const TestProjectWithComponentDescriptor = createModifiedProject({
  [StoryboardFilePath]: TestProjectWithSeveralComponents,
  ['/utopia/components.utopia.js']: `import { InnerApp1, InnerApp2 } from './storyboard'
  
  const Components = {
    '/utopia/storyboard': {
      InnerApp1: {
        component: InnerApp1,
        focus: 'always',
        properties: {},
        variants: [],
      },
      InnerApp2: {
        component: InnerApp2,
        focus: 'never',
        properties: {},
        variants: [],
      },
    },
  }

  export default Components
  `,
})

const TestProjectWithFragment = `
import * as React from 'react'
import { Scene, Storyboard } from "utopia-api";

export var App = (props) => {
  return (
    <Frag data-uid='frag'>
      <div
        style={{
          left: 64,
          top: 114,
          width: 210,
          height: 199,
          position: 'absolute',
          backgroundColor: '#d3d3d3',
        }}
      >
        <div
          style={{
            left: 33,
            top: 91,
            width: 97,
            height: 94,
            position: 'absolute',
            backgroundColor: '#FFFFFF',
            border: '0px solid rgb(0, 0, 0, 1)',
          }}
        />
      </div>
      <div
        style={{
          left: 95,
          top: 356,
          width: 148,
          height: 121,
          position: 'absolute',
          backgroundColor: '#F76565',
        }}
      >
        <div
          style={{
            left: 35,
            top: 29,
            width: 48,
            height: 48,
          }}
          data-uid='3bb'
        />
      </div>
    </Frag>
  )
}

const Frag = (props) => {
  return <>{props.children}</>
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
);
`

const TestProjectWithFragmentAndMap = `
import * as React from 'react'
import { Scene, Storyboard } from "utopia-api";

export var App = (props) => {
  const arr = [0, 1, 2, 3, 4]
  return (
    <Frag data-uid='frag'>
      {arr.map((idx) => {
        return (
          <div
            style={{
              left: 64,
              top: 114 + idx * 20,
              width: 10,
              height: 10,
              position: 'absolute',
              backgroundColor: '#d3d3d3',
            }}
          />
        )
      })}
    </Frag>
  )
}

const Frag = (props) => {
  return <>{props.children}</>
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
);
`

const TestProjectWithConditional = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        left: 64,
        top: 114,
        width: 210,
        height: 199,
        position: 'absolute',
        backgroundColor: '#d3d3d3',
      }}
      data-uid='root'
    >
      {
        // @utopia/uid=cond
        [].length === 0 ? (
        <div
          style={{
            left: 33,
            top: 91,
            width: 97,
            height: 94,
            position: 'absolute',
            backgroundColor: '#FFFFFF',
            border: '0px solid rgb(0, 0, 0, 1)',
          }}
          data-uid='truebranch'
        />
      ) : null}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
`

const projectWithText = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='stb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 108,
        top: 133,
        width: 154,
        height: 96,
      }}
      data-uid='hello'
    >
      Hello
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 412,
        top: 139,
        width: 198,
        height: 99,
      }}
      data-uid='hibr'
    >
      hi here
      <br />
      <br />
      <br />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 213,
        top: 303,
        width: 270,
        height: 128,
      }}
      data-uid='brbrbr'
    >
      <br />
      <br />
      <br />
      <br />
    </div>
  </Storyboard>
)
`

const ProjectWithVariables = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

function add(a, b) {
  return a + b
}

var Playground = ({ style }) => {
  const definedInsideString = 'hello'
  const definedInsideNumber = 12
  const definedInsideObject = { prop: [33] }
  const functionResult = add(12, 23)

  return (
    <div
      data-uid='root'
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
        ...style,
      }}
    >
      <div
        data-uid='111'
        style={{
          backgroundColor: '#0074ff',
          position: 'absolute',
          left: 123,
          top: 220,
          width: 51,
          height: 48,
        }}
      >
        {definedInsideString}
      </div>
      <div
      data-uid='222'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 184,
          top: 220,
          width: 51,
          height: 48,
        }}
      >
        {definedInsideNumber}
      </div>
      <div
      data-uid='333'
        style={{
          backgroundColor: '#f000ae',
          position: 'absolute',
          left: 245,
          top: 220,
          width: 51,
          height: 48,
        }}
      >
        {definedInsideObject.hello}
      </div>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
    >
      <Playground style={{}} data-uid='pg' />
    </Scene>
  </Storyboard>
)
`
