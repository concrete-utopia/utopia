import {
  testCanvasRender,
  testCanvasRenderInline,
  testCanvasRenderInlineMultifile,
} from './ui-jsx-canvas.test-utils'

describe('UiJsxCanvas', () => {
  it('#747 - DOM object constructor cannot be called as a function', () => {
    testCanvasRender(
      null,
      `
import React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'

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
import React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'

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
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"sb sb/scene sb/scene/app sb/scene/app:app-root\\"
          data-utopia-root-element-path=\\"sb\\"
        >
          <div
            data-utopia-scene-id=\\"sb/scene\\"
            data-path=\\"sb/scene\\"
            style=\\"
              overflow: hidden;
              position: absolute;
              background-color: rgba(255, 255, 255, 1);
              box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
              background-image: linear-gradient(
                  to bottom left,
                  #e7e7e7 25%,
                  transparent 25%
                ),
                linear-gradient(to bottom left, transparent 75%, #e7e7e7 75%),
                linear-gradient(to bottom right, #e7e7e7 25%, transparent 25%),
                linear-gradient(to bottom right, transparent 75%, #e7e7e7 75%);
              background-size: 12px 12px, 12px 12px, 12px 12px, 12px 12px;
              background-position: -9px 0px, -3px -6px, 3px 6px, -3px 0;
              left: 0;
              top: 0;
              width: 375px;
              height: 812px;
            \\"
            data-uid=\\"scene\\"
          >
            <div data-uid=\\"app-root\\" data-path=\\"sb/scene/app:app-root\\"></div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('Handles importing default exports', () => {
    const result = testCanvasRenderInlineMultifile(
      null,
      `
import React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'
import Appy from './app'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <Appy data-uid='app' />
    </Scene>
  </Storyboard>
)
`,
      {
        'app.js': `
import React from 'react'
export default function App(props) {
  return <div data-uid='app-outer-div'>
    <div data-uid='inner-div'>hello</div>
  </div>
}`,
      },
    )

    expect(result).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"sb sb/scene sb/scene/app\\"
          data-utopia-root-element-path=\\"sb\\"
        >
          <div
            data-utopia-scene-id=\\"sb/scene\\"
            data-path=\\"sb/scene\\"
            style=\\"
              overflow: hidden;
              position: absolute;
              background-color: rgba(255, 255, 255, 1);
              box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
              background-image: linear-gradient(
                  to bottom left,
                  #e7e7e7 25%,
                  transparent 25%
                ),
                linear-gradient(to bottom left, transparent 75%, #e7e7e7 75%),
                linear-gradient(to bottom right, #e7e7e7 25%, transparent 25%),
                linear-gradient(to bottom right, transparent 75%, #e7e7e7 75%);
              background-size: 12px 12px, 12px 12px, 12px 12px, 12px 12px;
              background-position: -9px 0px, -3px -6px, 3px 6px, -3px 0;
              left: 0;
              top: 0;
              width: 375px;
              height: 812px;
            \\"
            data-uid=\\"scene\\"
          >
            <div data-uid=\\"app-outer-div\\" data-path=\\"sb/scene/app\\">
              <div data-uid=\\"inner-div\\">hello</div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('#1717 - Works with user components called Scene', () => {
    const result = testCanvasRenderInlineMultifile(
      null,
      `
import React from 'react'
import { Scene as SC, Storyboard } from 'utopia-api'
import App from './app'

export var Scene = (props) => {
  return (
    <div
      data-uid='same-file-app-div'
      data-label='Scene Thing'
      style={{
        position: 'relative',
        width: '100%',
        height: '100%',
        backgroundColor: 'blue',
      }}
    />
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <SC
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: 375,
        height: 812,
      }}
    >
      <App data-uid='app-entity' />
    </SC>
    <SC
      data-label='Same File App'
      data-uid='scene-2-entity'
      style={{
        position: 'absolute',
        left: 400,
        top: 0,
        width: 375,
        height: 812,
      }}
    >
      <Scene data-uid='same-file-app-entity' />
    </SC>
  </Storyboard>
)
`,
      {
        'app.js': `
import React from 'react'
export default function App(props) {
  return <div data-uid='app-outer-div'>
    <div data-uid='inner-div'>hello</div>
  </div>
}`,
      },
    )

    expect(result).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"storyboard-entity storyboard-entity/scene-1-entity storyboard-entity/scene-1-entity/app-entity storyboard-entity/scene-2-entity storyboard-entity/scene-2-entity/same-file-app-entity storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div\\"
          data-utopia-root-element-path=\\"storyboard-entity\\"
        >
          <div
            data-utopia-scene-id=\\"storyboard-entity/scene-1-entity\\"
            data-path=\\"storyboard-entity/scene-1-entity\\"
            style=\\"
              overflow: hidden;
              position: absolute;
              background-color: rgba(255, 255, 255, 1);
              box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
              background-image: linear-gradient(
                  to bottom left,
                  #e7e7e7 25%,
                  transparent 25%
                ),
                linear-gradient(to bottom left, transparent 75%, #e7e7e7 75%),
                linear-gradient(to bottom right, #e7e7e7 25%, transparent 25%),
                linear-gradient(to bottom right, transparent 75%, #e7e7e7 75%);
              background-size: 12px 12px, 12px 12px, 12px 12px, 12px 12px;
              background-position: -9px 0px, -3px -6px, 3px 6px, -3px 0;
              left: 0;
              top: 0;
              width: 375px;
              height: 812px;
            \\"
            data-uid=\\"scene-1-entity\\"
            data-label=\\"Imported App\\"
          >
            <div
              data-uid=\\"app-outer-div\\"
              data-path=\\"storyboard-entity/scene-1-entity/app-entity\\"
            >
              <div data-uid=\\"inner-div\\">hello</div>
            </div>
          </div>
          <div
            data-utopia-scene-id=\\"storyboard-entity/scene-2-entity\\"
            data-path=\\"storyboard-entity/scene-2-entity\\"
            style=\\"
              overflow: hidden;
              position: absolute;
              background-color: rgba(255, 255, 255, 1);
              box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
              background-image: linear-gradient(
                  to bottom left,
                  #e7e7e7 25%,
                  transparent 25%
                ),
                linear-gradient(to bottom left, transparent 75%, #e7e7e7 75%),
                linear-gradient(to bottom right, #e7e7e7 25%, transparent 25%),
                linear-gradient(to bottom right, transparent 75%, #e7e7e7 75%);
              background-size: 12px 12px, 12px 12px, 12px 12px, 12px 12px;
              background-position: -9px 0px, -3px -6px, 3px 6px, -3px 0;
              left: 400px;
              top: 0;
              width: 375px;
              height: 812px;
            \\"
            data-uid=\\"scene-2-entity\\"
            data-label=\\"Same File App\\"
          >
            <div
              data-uid=\\"same-file-app-div\\"
              data-label=\\"Scene Thing\\"
              style=\\"
                position: relative;
                width: 100%;
                height: 100%;
                background-color: blue;
              \\"
              data-path=\\"storyboard-entity/scene-2-entity/same-file-app-entity:same-file-app-div\\"
            ></div>
          </div>
        </div>
      </div>
      "
    `)
  })
  it(`#1737 - Parser is broken for 'export const thing = "hello"'`, () => {
    const result = testCanvasRenderInlineMultifile(
      null,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'
import { App } from '/src/app'

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app-entity' />
    </Scene>
  </Storyboard>
)`,
      {
        '/src/app.js': `import * as React from 'react'
import DefaultFunction, { Card, thing } from '/src/card.js'
export var App = (props) => {
  return (
    <div
      data-uid='app-outer-div'
      style={{
        position: 'relative',
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
      }}
    >
      <Card
        data-uid='card-instance'
        style={{
          position: 'absolute',
          left: 67,
          top: 0,
          width: 133,
          height: 300,
        }}
      />
      {thing}
      <DefaultFunction />
    </div>
  )
}`,
        '/src/card.js': `import * as React from 'react'
import { Rectangle } from 'utopia-api'
import { Spring } from 'non-existant-dummy-library'
export var Card = (props) => {
  return (
    <div
      data-uid='card-outer-div'
      style={{ ...props.style }}
    >
      <div
        data-uid='card-inner-div'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 50,
          height: 50,
          backgroundColor: 'red',
        }}
      />
      <Spring
        data-uid='card-inner-spring'
        style={{
          position: 'absolute',
          left: 100,
          top: 200,
          width: 50,
          height: 50,
          backgroundColor: 'blue',
        }}
      />
    </div>
  )
}

export const thing = 'hello'

export default function () {
  return <div>Default Function Time</div>
}`,
      },
    )
    expect(result).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"storyboard-entity storyboard-entity/scene-1-entity storyboard-entity/scene-1-entity/app-entity storyboard-entity/scene-1-entity/app-entity:app-outer-div storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance storyboard-entity/scene-1-entity/app-entity:app-outer-div/d7f\\"
          data-utopia-root-element-path=\\"storyboard-entity\\"
        >
          <div
            data-utopia-scene-id=\\"storyboard-entity/scene-1-entity\\"
            data-path=\\"storyboard-entity/scene-1-entity\\"
            style=\\"
              overflow: hidden;
              position: absolute;
              background-color: rgba(255, 255, 255, 1);
              box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
              background-image: linear-gradient(
                  to bottom left,
                  #e7e7e7 25%,
                  transparent 25%
                ),
                linear-gradient(to bottom left, transparent 75%, #e7e7e7 75%),
                linear-gradient(to bottom right, #e7e7e7 25%, transparent 25%),
                linear-gradient(to bottom right, transparent 75%, #e7e7e7 75%);
              background-size: 12px 12px, 12px 12px, 12px 12px, 12px 12px;
              background-position: -9px 0px, -3px -6px, 3px 6px, -3px 0;
              left: 0;
              top: 0;
              width: 375px;
              height: 812px;
            \\"
            data-uid=\\"scene-1-entity\\"
            data-label=\\"Imported App\\"
          >
            <div
              data-uid=\\"app-outer-div\\"
              style=\\"
                position: relative;
                width: 100%;
                height: 100%;
                background-color: #ffffff;
              \\"
              data-path=\\"storyboard-entity/scene-1-entity/app-entity:app-outer-div\\"
            >
              <div
                data-uid=\\"card-outer-div\\"
                style=\\"
                  position: absolute;
                  left: 67px;
                  top: 0;
                  width: 133px;
                  height: 300px;
                \\"
                data-path=\\"storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div\\"
              >
                <div
                  data-uid=\\"card-inner-div\\"
                  style=\\"
                    position: absolute;
                    left: 0;
                    top: 0;
                    width: 50px;
                    height: 50px;
                    background-color: red;
                  \\"
                  data-path=\\"storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-div\\"
                ></div>
                <div
                  data-uid=\\"card-inner-spring\\"
                  style=\\"
                    position: absolute;
                    left: 100px;
                    top: 200px;
                    width: 50px;
                    height: 50px;
                    background-color: blue;
                  \\"
                  data-path=\\"storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance:card-outer-div/card-inner-spring\\"
                ></div>
              </div>
              hello
              <div
                data-uid=\\"4cf\\"
                data-path=\\"storyboard-entity/scene-1-entity/app-entity:app-outer-div/d7f:4cf\\"
              >
                Default Function Time
              </div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })
})
