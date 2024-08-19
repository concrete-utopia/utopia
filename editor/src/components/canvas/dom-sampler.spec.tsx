import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { runDOMWalker } from '../editor/actions/action-creators'
import { formatTestProjectCode, renderTestEditorWithCode } from './ui-jsx.test-utils'

describe('Basic Dom Sampler tests', () => {
  it('a with mapped content', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStoryboard(`
export var Playground = ({ style }) => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
        ...style,
      }}
    >
      <div
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
            <div>
              <img
                src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'
                alt='Utopia logo'
                style={{ width: 118, height: 150 }}
              />
            </div>
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
        "sb/pg-sc/pg:571/1c8/c7d",
        "sb/pg-sc/pg:571",
        "sb/pg-sc/pg:571/1c8",
        "sb/pg-sc/pg:571/1c8/c7d/401~~~1",
        "sb/pg-sc/pg:571/1c8/c7d/401~~~1/bec",
        "sb/pg-sc/pg:571/1c8/c7d/401~~~2",
        "sb/pg-sc/pg:571/1c8/c7d/401~~~2/bec",
        "sb/pg-sc/pg:571/1c8/c7d/401~~~3",
        "sb/pg-sc/pg:571/1c8/c7d/401~~~3/bec",
      ]
    `)
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
