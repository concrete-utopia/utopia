/// <reference types="karma-viewport" />

import {
  AllContentAffectingTypes,
  ContentAffectingType,
} from '../../components/canvas/canvas-strategies/strategies/group-like-helpers'
import {
  getClosingGroupLikeTag,
  getOpeningGroupLikeTag,
  GroupLikeElementUid,
} from '../../components/canvas/canvas-strategies/strategies/group-like-helpers.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../components/canvas/ui-jsx.test-utils'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { cartesianProduct } from '../shared/array-utils'
import { Either, isRight } from '../shared/either'
import { elementPath, fromString, toString } from '../shared/element-path'
import { canvasRectangle, localRectangle, zeroCanvasRect } from '../shared/math-utils'
import { ElementPath } from '../shared/project-file-types'
import { MetadataUtils } from './element-metadata-utils'
import { elementOnlyHasTextChildren } from './element-template-utils'
import { wait } from './performance-scripts'
import { BakedInStoryboardUID } from './scene-utils'

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

  describe('content-affecting elements', () => {
    AllContentAffectingTypes.forEach((type) => {
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
          ${getOpeningGroupLikeTag(type)}
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
            ${getClosingGroupLikeTag(type)}
          </div>
          `),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [
          elementPathInInnards(`container/${GroupLikeElementUid}`),
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
          elementPathInInnards(`container/${GroupLikeElementUid}`),
        )
        if (childInstance == null) {
          throw new Error('childInstance should not be null')
        }

        const globalContentBoxForChildrenOfChildrenAffectingElement =
          MetadataUtils.getGlobalContentBoxForChildren(childInstance)

        expect(globalContentBoxForChildrenOfContainer).not.toBeNull()
        expect(globalContentBoxForChildrenOfContainer).not.toEqual(zeroCanvasRect)

        expect(globalContentBoxForChildrenOfContainer).toEqual(
          globalContentBoxForChildrenOfChildrenAffectingElement,
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

      await selectComponentsForTest(editor, [elementPathInInnards('a7b/b15/b0e')])

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

  describe('metadata from ancestors', () => {
    it(`globalFrame and localFrame for null in conditionals is coming from the parent of the conditional`, async () => {
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
                <img
                  src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
                  alt='Utopia logo'
                  style={{ height: '100%' }}
                  data-uid='b0e'
                />
              ) : null
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

      const nullInstance = MetadataUtils.findElementByElementPath(
        editor.getEditorState().editor.jsxMetadata,
        elementPathInInnards('root/container/conditional/a25'),
      )
      if (nullInstance == null) {
        throw new Error('nullInstance should not be null')
      }

      // the null metadata should have the same global frame as the parent of the conditional
      expect(nullInstance.globalFrame).toEqual(containerInstance.globalFrame)
      expect(nullInstance.localFrame).toEqual({
        x: 0,
        y: 0,
        width: 150,
        height: 150,
      })
    })
  })

  it(`specialSizeMeasurements.display is coming from the parent of the conditional`, async () => {
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
          data-uid='parent'
        >
          {
            // @utopia/uid=conditional
            false ? (
              <div data-uid='child'/>
            ) : null
          }
        </div>
      </div>
      `),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [elementPathInInnards('root/container')])

    const parentInstance = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      elementPathInInnards('root/parent'),
    )
    if (parentInstance == null) {
      throw new Error('parentInstance should not be null')
    }

    const parentSpecialSizeMeasurements = parentInstance.specialSizeMeasurements

    const conditionalInstance = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      elementPathInInnards('root/parent/conditional'),
    )
    if (conditionalInstance == null) {
      throw new Error('conditionalInstance should not be null')
    }

    expect(conditionalInstance.specialSizeMeasurements.display).toEqual(
      parentSpecialSizeMeasurements.display,
    )
  })

  describe('nested content-affecting elements', () => {
    cartesianProduct(AllContentAffectingTypes, AllContentAffectingTypes).forEach(
      ([outerType, innerType]) => {
        it(`globalContentBoxForChildren of a ${innerType} wrapped in a ${outerType} is the same as the globalContentBoxForChildren of their closest non-content affecting parent`, async () => {
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
          ${getOpeningGroupLikeTag(outerType, {
            outerUid: 'outer-caf',
            innerUid: 'inner-caf-fragment',
          })}
          ${getOpeningGroupLikeTag(innerType)}
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
            ${getClosingGroupLikeTag(innerType)}
            ${getClosingGroupLikeTag(outerType)}
          </div>
          `),
            'await-first-dom-report',
          )

          await selectComponentsForTest(editor, [
            elementPathInInnards(`container/outer-caf/inner-caf-fragment/${GroupLikeElementUid}`),
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
            elementPathInInnards(`container/outer-caf/inner-caf-fragment/${GroupLikeElementUid}`),
          )
          if (childInstance == null) {
            throw new Error('containerInstance should not be null')
          }

          const globalContentBoxForChildrenOfChildrenAffectingElement =
            MetadataUtils.getGlobalContentBoxForChildren(childInstance)

          expect(globalContentBoxForChildrenOfChildrenAffectingElement).not.toBeNull()
          expect(globalContentBoxForChildrenOfChildrenAffectingElement).not.toEqual(zeroCanvasRect)

          expect(globalContentBoxForChildrenOfContainer).toEqual(
            globalContentBoxForChildrenOfChildrenAffectingElement,
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
