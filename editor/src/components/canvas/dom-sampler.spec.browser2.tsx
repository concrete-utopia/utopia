/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert", "matchInlineSnapshotBrowser"] }] */
import { matchInlineSnapshotBrowser } from '../../../test/karma-snapshots'
import { wait } from '../../core/model/performance-scripts'
import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { objectMap } from '../../core/shared/object-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { runDOMWalker, setFocusedElement } from '../editor/actions/action-creators'
import { navigatorEntryToKey } from '../editor/store/editor-state'
import { getNavigatorTargetsFromEditorState } from '../navigator/navigator-utils'
import { NavigatorTestProjectWithSyntheticElements } from '../navigator/navigator.test-utils'
import {
  formatTestProjectCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from './ui-jsx.test-utils'

describe('Basic Dom Sampler tests', () => {
  it('a with mapped content', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
export var Playground = ({ style }) => {
  return (
    <div
      data-uid='aaa'
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
        ...style,
      }}
    >
      <div
        data-uid='bbb'
        style={{
          height: 'max-content',
          position: 'absolute',
          left: 163,
          top: 305,
          display: 'flex',
          flexDirection: 'row',
          width: 'max-content',
          gap: 10,
        }}
      >
        {[1, 2, 3].map(() => {
          return (
            <div data-uid='ccc'>
              <img
                data-uid='ddd'
                src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'
                alt='Utopia logo'
                style={{ width: 118, height: 150 }}
              />
            </div>
          )
        })}
        {[1, 2, 3].map(() => {
          return (
            <React.Fragment>
              <img
                data-uid='yyy'
                src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'
                alt='Utopia logo'
                style={{ width: 118, height: 150 }}
              />
              <img
                data-uid='zzz'
                src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'
                alt='Utopia logo'
                style={{ width: 118, height: 150 }}
              />
            </React.Fragment>
          )
        })}
      </div>
    </div>
  )
}

    `),
      'await-first-dom-report',
    )

    await editor.dispatch([runDOMWalker(null)], true)

    matchInlineSnapshotBrowser(
      Object.keys(editor.getEditorState().editor.jsxMetadata),
      `
      Array [
        "sb",
        "sb/pg-sc",
        "sb/pg-sc/pg",
        "sb/pg-sc/pg:aaa",
        "sb/pg-sc/pg:aaa/bbb",
        "sb/pg-sc/pg:aaa/bbb/266",
        "sb/pg-sc/pg:aaa/bbb/266/ccc~~~1",
        "sb/pg-sc/pg:aaa/bbb/266/ccc~~~2",
        "sb/pg-sc/pg:aaa/bbb/266/ccc~~~3",
        "sb/pg-sc/pg:aaa/bbb/266/ccc~~~1/ddd",
        "sb/pg-sc/pg:aaa/bbb/266/ccc~~~2/ddd",
        "sb/pg-sc/pg:aaa/bbb/266/ccc~~~3/ddd",
        "sb/pg-sc/pg:aaa/bbb/7f0",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~1",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~2",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~3",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~1/yyy",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~2/yyy",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~3/yyy",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~1/zzz",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~2/zzz",
        "sb/pg-sc/pg:aaa/bbb/7f0/486~~~3/zzz",
      ]
    `,
    )
  })

  it('nested Fragments', async () => {
    const editor = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

const MyComponent = () => {
  return (
    <React.Fragment>
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 400,
            top: 125,
            width: 150,
            height: 159,
          }}
          data-uid='a316f87ba6496ae558ad185cc2d97e48'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 220,
            top: 300,
            width: 200,
            height: 200,
          }}
          data-uid='4caed5fc09f8dd5e71825e38080c86be'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 128,
              top: 146,
              width: 130,
              height: 90,
            }}
            data-uid='e0fefd5b8d935b5e595cec81414a5003'
          />
        </div>
      </React.Fragment>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 50,
          width: 100,
          height: 150,
        }}
        data-uid='d7664a7591ab5ac8808f2ca9b0bb0596'
      />
    </React.Fragment>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{ width: 600, height: 560 }}
    >
      <MyComponent data-uid='comp' />
    </Scene>
  </Storyboard>
)
    `,
      'await-first-dom-report',
    )

    const metadata = editor.getEditorState().editor.jsxMetadata

    expect(metadata['sb/scene/comp'].globalFrame).toEqual({
      x: 50,
      y: 50,
      width: 500,
      height: 450,
    })
  })

  it('with a dynamic non-dom element', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
    import { Storyboard } from 'utopia-api'

    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div data-uid='div'>
          {
            // @utopia/uid=map1
            [0, 1, 2].map((i) => (
              <div data-uid='el'>first {i}</div>
            ))
          }
        </div>
        </Storyboard>
    )
    `,
      'await-first-dom-report',
    )

    matchInlineSnapshotBrowser(
      Object.keys(editor.getEditorState().editor.jsxMetadata),
      `
      Array [
        "sb",
        "sb/div",
        "sb/div/map1",
        "sb/div/map1/el~~~1",
        "sb/div/map1/el~~~2",
        "sb/div/map1/el~~~3",
        "sb/div/map1/el~~~1/b1f",
        "sb/div/map1/el~~~2/b1f",
        "sb/div/map1/el~~~3/b1f",
      ]
    `,
    )
  })

  it('conditional expressions', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
    import { Storyboard } from 'utopia-api'

    export var storyboard = (
      <Storyboard data-uid='sb'>
        {
          // @utopia/uid=cond1
          true ? null : <>null</>
        }
        {
          // @utopia/uid=cond2
          true ? <span>hello</span> : null
        }
        {
          // @utopia/uid=cond3
          true ? null : <span>world</span>
        }
        {
          // @utopia/uid=cond4
          true ? 'hello' : <span>world</span>
        }
        </Storyboard>
    )
    `,
      'await-first-dom-report',
    )

    matchInlineSnapshotBrowser(
      Object.keys(editor.getEditorState().editor.jsxMetadata),
      `
      Array [
        "sb",
        "sb/cond1",
        "sb/cond2",
        "sb/cond2/d5e",
        "sb/cond3",
        "sb/cond4",
      ]
    `,
    )
  })

  it('The ElementPathTree and thus the navigator tree are correct for a test project with synthetic elements', async () => {
    const editor = await renderTestEditorWithCode(
      NavigatorTestProjectWithSyntheticElements,
      'await-first-dom-report',
    )

    await editor.dispatch([setFocusedElement(EP.fromString('sb/sc/app:app-root/card'))], true)
    await editor.getDispatchFollowUpActionsFinished()

    expect(
      getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual([
      'regular-sb/sc',
      'regular-sb/sc/app',
      'regular-sb/sc/app:app-root',
      'regular-sb/sc/app:app-root/card',
      'regular-sb/sc/app:app-root/card:card-root',
      'regular-sb/sc/app:app-root/card:card-root/card-span',
      'regular-sb/sc/app:app-root/card:card-root/30d',
      'regular-sb/sc/app:app-root/card/card-child',
      'regular-sb/sc/app:app-root/frag',
      'regular-sb/sc/app:app-root/frag/frag-child',
      'regular-sb/sc/app:app-root/frag/cond-1',
      'conditional-clause-sb/sc/app:app-root/frag/cond-1-true-case',
      'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true',
      'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-1-true-child',
      'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2',
      'conditional-clause-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2-true-case',
      'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2/cond-2-child',
      'conditional-clause-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2-false-case',
      'synthetic-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2/d84-attribute',
      'conditional-clause-sb/sc/app:app-root/frag/cond-1-false-case',
      'synthetic-sb/sc/app:app-root/frag/cond-1/019-attribute',
      'regular-sb/sc/app:app-root/children-code-block',
      'regular-sb/sc/app/app-child',
      'regular-sb/1e7',
    ])
  })

  it('a conditional', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div data-uid='root'>
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
          </div>`),
      'await-first-dom-report',
    )

    matchInlineSnapshotBrowser(
      objectMap(
        (m, path) =>
          optionalMap(EP.humanReadableDebugPath, m.specialSizeMeasurements.closestOffsetParentPath),
        editor.getEditorState().editor.jsxMetadata,
      ),
      `
      Object {
        "utopia-storyboard-uid": "",
        "utopia-storyboard-uid/scene-aaa": null,
        "utopia-storyboard-uid/scene-aaa/app-entity": "utopia-storyboard-uid/scene-aaa",
        "utopia-storyboard-uid/scene-aaa/app-entity:root": "utopia-storyboard-uid/scene-aaa",
        "utopia-storyboard-uid/scene-aaa/app-entity:root/bbb": "utopia-storyboard-uid/scene-aaa",
        "utopia-storyboard-uid/scene-aaa/app-entity:root/bbb/ccc": "utopia-storyboard-uid/scene-aaa",
        "utopia-storyboard-uid/scene-aaa/app-entity:root/cond": "utopia-storyboard-uid/scene-aaa",
        "utopia-storyboard-uid/scene-aaa/app-entity:root/cond/ddd": "utopia-storyboard-uid/scene-aaa",
        "utopia-storyboard-uid/scene-aaa/app-entity:root/cond/ddd/eee": "utopia-storyboard-uid/scene-aaa/app-entity:root/cond/ddd",
      }
    `,
    )

    matchInlineSnapshotBrowser(
      objectMap(
        (m, path) => m.specialSizeMeasurements.globalContentBoxForChildren,
        editor.getEditorState().editor.jsxMetadata,
      ),
      `Object {
  \"utopia-storyboard-uid\": Object {
    \"type\": \"INFINITY_RECTANGLE\",
  },
  \"utopia-storyboard-uid/scene-aaa\": Object {
    \"height\": 400,
    \"width\": 400,
    \"x\": 0,
    \"y\": 0,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity\": Object {
    \"height\": 400,
    \"width\": 400,
    \"x\": 0,
    \"y\": 0,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity:root\": Object {
    \"height\": 400,
    \"width\": 400,
    \"x\": 0,
    \"y\": 0,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity:root/bbb\": Object {
    \"height\": 400,
    \"width\": 400,
    \"x\": 0,
    \"y\": 0,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity:root/bbb/ccc\": Object {
    \"height\": 400,
    \"width\": 400,
    \"x\": 0,
    \"y\": 0,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity:root/cond\": Object {
    \"height\": 400,
    \"width\": 400,
    \"x\": 0,
    \"y\": 0,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity:root/cond/ddd\": Object {
    \"height\": 40,
    \"width\": 50,
    \"x\": 100,
    \"y\": 100,
  },
  \"utopia-storyboard-uid/scene-aaa/app-entity:root/cond/ddd/eee\": Object {
    \"height\": 40,
    \"width\": 50,
    \"x\": 100,
    \"y\": 100,
  },
}`,
    )
  })
})

function makeTestProjectCodeWithStoryboard(codeForComponents: string): string {
  const code = `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'

    ${codeForComponents}

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='pg-sc'
            commentId='playground-scene'
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 0,
              top: 0,
            }}
            data-label='Playground'
          >
            <Playground data-uid='pg' style={{}} />
          </Scene>
        </Storyboard>
      )
    }
  `

  return formatTestProjectCode(code)
}

describe('Grid dom sampler tests', () => {
  it('gridCellGlobalFrames calculated correctly', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid={'storyboard'}>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid={'scene'}
    >
      <div
        style={{
          backgroundColor: '#fff',
          position: 'absolute',
          left: 46,
          top: 36,
          width: 553,
          height: 499,
          display: 'grid',
          gridTemplateColumns: '150px min-content 1fr 1fr',
          gridTemplateRows: 'min-content 1fr 1fr 1fr 1fr',
          gridGap: 20,
          padding: 10,
          justifyContent: 'space-between',
          justifyItems: 'space-evenly',
        }}
        data-uid='grid'
      >
        <div
          style={{
            backgroundColor: '#09f',
            width: '100%',
            height: '100%',
            gridColumn: 1,
            gridRow: 1,
            borderRadius: 10,
          }}
          data-uid='child'
        />
        <div
          style={{
            backgroundColor: '#9f0',
            width: 80,
            height: 50,
            gridColumn: 2,
            gridRow: 1,
            borderTopLeftRadius: 10,
            borderTopRightRadius: 10,
            borderBottomRightRadius: 10,
            borderBottomLeftRadius: 10,
          }}
        />
        <div
          style={{
            backgroundColor: '#f09',
            width: '100%',
            height: '100%',
            gridColumn: 1,
            gridRowStart: 2,
            gridRowEnd: 6,
            borderTopLeftRadius: 10,
            borderTopRightRadius: 10,
            borderBottomRightRadius: 10,
            borderBottomLeftRadius: 10,
          }}
        />
        <div
          style={{
            backgroundColor: '#f90',
            width: '100%',
            height: '100%',
            gridColumn: 2,
            gridRow: 3,
            borderRadius: 10,
          }}
        />
        <div
          style={{
            backgroundColor: '#f90',
            width: '100%',
            height: '100%',
            gridColumn: 2,
            gridRow: 3,
            borderRadius: 10,
          }}
        />
        <div
          style={{
            backgroundColor: '#90f',
            width: '100%',
            height: '100%',
            gridColumn: 2,
            gridRow: 4,
            borderRadius: 10,
          }}
        />
        <div
          style={{
            backgroundColor: '#0f9',
            width: 39,
            height: 39,
            alignSelf: 'center',
            justifySelf: 'center',
            gridColumn: 2,
            gridRow: 5,
            borderRadius: 10,
          }}
        />
      </div>
    </Scene>
  </Storyboard>
)
`,
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('storyboard/scene/grid')])
    await editor.dispatch([runDOMWalker(null)], true)

    // non-grids don't have cell measurements:
    expect(
      editor.getEditorState().editor.jsxMetadata['storyboard/scene/grid/child']
        .specialSizeMeasurements.gridCellGlobalFrames,
    ).toBeNull()

    // grids have cell measurements:
    matchInlineSnapshotBrowser(
      editor.getEditorState().editor.jsxMetadata['storyboard/scene/grid'].specialSizeMeasurements
        .gridCellGlobalFrames,
      `Array [
  Array [
    Object {
      \"height\": 50,
      \"width\": 150,
      \"x\": 268,
      \"y\": 174,
    },
    Object {
      \"height\": 50,
      \"width\": 80,
      \"x\": 438,
      \"y\": 174,
    },
    Object {
      \"height\": 50,
      \"width\": 121.5,
      \"x\": 538,
      \"y\": 174,
    },
    Object {
      \"height\": 50,
      \"width\": 121.5,
      \"x\": 679.5,
      \"y\": 174,
    },
  ],
  Array [
    Object {
      \"height\": 87.5,
      \"width\": 150,
      \"x\": 268,
      \"y\": 244,
    },
    Object {
      \"height\": 87.5,
      \"width\": 80,
      \"x\": 438,
      \"y\": 244,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 538,
      \"y\": 244,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 679.5,
      \"y\": 244,
    },
  ],
  Array [
    Object {
      \"height\": 87.5,
      \"width\": 150,
      \"x\": 268,
      \"y\": 351.5,
    },
    Object {
      \"height\": 87.5,
      \"width\": 80,
      \"x\": 438,
      \"y\": 351.5,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 538,
      \"y\": 351.5,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 679.5,
      \"y\": 351.5,
    },
  ],
  Array [
    Object {
      \"height\": 87.5,
      \"width\": 150,
      \"x\": 268,
      \"y\": 458.5,
    },
    Object {
      \"height\": 87.5,
      \"width\": 80,
      \"x\": 438,
      \"y\": 458.5,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 538,
      \"y\": 458.5,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 679.5,
      \"y\": 458.5,
    },
  ],
  Array [
    Object {
      \"height\": 87.5,
      \"width\": 150,
      \"x\": 268,
      \"y\": 566,
    },
    Object {
      \"height\": 87.5,
      \"width\": 80,
      \"x\": 438,
      \"y\": 566,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 538,
      \"y\": 566,
    },
    Object {
      \"height\": 87.5,
      \"width\": 121.5,
      \"x\": 679.5,
      \"y\": 566,
    },
  ],
]`,
    )
  })
})
