import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { runDOMWalker, setFocusedElement } from '../editor/actions/action-creators'
import { navigatorEntryToKey } from '../editor/store/editor-state'
import { getNavigatorTargetsFromEditorState } from '../navigator/navigator-utils'
import { NavigatorTestProjectWithSyntheticElements } from '../navigator/navigator.test-utils'
import { formatTestProjectCode, renderTestEditorWithCode } from './ui-jsx.test-utils'

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

    await editor.dispatch([runDOMWalker()], true)

    expect(Object.keys(editor.getEditorState().editor.jsxMetadata)).toMatchInlineSnapshot(`
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
        "sb/pg-sc/pg:aaa/bbb/6cc",
      ]
    `)
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

    expect(Object.keys(editor.getEditorState().editor.jsxMetadata)).toMatchInlineSnapshot(`
      Array [
        "sb",
        "sb/cond1",
        "sb/cond2",
        "sb/cond2/d5e",
        "sb/cond3",
        "sb/cond4",
      ]
    `)
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
      'regular-sb/1e7',
      'regular-sb/sc',
      'regular-sb/sc/app',
      'regular-sb/sc/app:app-root',
      'regular-sb/sc/app:app-root/card',
      'regular-sb/sc/app:app-root/card:card-root',
      'regular-sb/sc/app:app-root/card:card-root/30d',
      'regular-sb/sc/app:app-root/card:card-root/card-span',
      'regular-sb/sc/app:app-root/card/card-child',
      'regular-sb/sc/app:app-root/children-code-block',
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
      'regular-sb/sc/app/app-child',
    ])
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
