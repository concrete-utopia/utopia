import { testCanvasRender, testCanvasRenderInline } from './ui-jsx-canvas.test-utils'

describe('UiJsxCanvas', () => {
  it('#747 - DOM object constructor cannot be called as a function', () => {
    testCanvasRender(
      null,
      `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const DefaultComments = [
  {
    userName: 'forbidden_one',
    contents: 'Integer eu imperdiet enim. Aenean vitae sem et ex feugiat accumsan et a mi.',
  },
]

const Comment = () => <div data-uid='comment-root'>hat</div>

export var App = () =>
  true ? DefaultComments.map((comment) => <Comment comment={comment} />) : null

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
    `,
    )
  })
  it('Supports in-scope variables with the same names as intrinsic components', () => {
    const result = testCanvasRenderInline(
      null,
      `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const App = () => {
  const div = React.useRef()
  return <div data-uid='app-root' ref={div} />
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
    `,
    )

    expect(result).toMatchInlineSnapshot(`
      "<div style=\\"all: initial;\\">
        <div
          id=\\"canvas-container\\"
          style=\\"position: absolute;\\"
          data-utopia-valid-paths=\\"sb sb/scene sb/scene/app sb/scene/app:app-root\\"
          data-utopia-root-element-path=\\"sb\\"
        >
          <div
            data-utopia-scene-id=\\"sb/scene\\"
            data-paths=\\"sb/scene sb\\"
            style=\\"
              position: absolute;
              background-color: rgba(255, 255, 255, 1);
              box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
              left: 0;
              top: 0;
              width: 375px;
              height: 812px;
            \\"
            data-uid=\\"scene sb\\"
          >
            <div
              data-uid=\\"app-root app\\"
              data-paths=\\"sb/scene/app:app-root sb/scene/app\\"
            ></div>
          </div>
        </div>
      </div>
      "
    `)
  })
})
