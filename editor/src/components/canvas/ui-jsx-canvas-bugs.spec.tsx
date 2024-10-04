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
} from 'utopia-api'

const DefaultComments = [
  {
    userName: 'forbidden_one',
    contents: 'Integer eu imperdiet enim. Aenean vitae sem et ex feugiat accumsan et a mi.',
  },
]

const Comment = () => <div data-uid='comment-root'>hat</div>

export var App = () => DefaultComments.map((comment) => <Comment comment={comment} />)

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
          data-testid=\\"canvas-container\\"
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
              background-color: var(--utopitheme-emphasizedBackground);
              box-shadow: 0px 1px 2px 0px var(--utopitheme-shadow90),
                0px 2px 4px -1px var(--utopitheme-shadow50);
              background-image: conic-gradient(
                var(--utopitheme-checkerboardLight) 0.25turn,
                var(--utopitheme-checkerboardDark) 0.25turn 0.5turn,
                var(--utopitheme-checkerboardLight) 0.5turn 0.75turn,
                var(--utopitheme-checkerboardDark) 0.75turn
              );
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
          data-testid=\\"canvas-container\\"
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
              background-color: var(--utopitheme-emphasizedBackground);
              box-shadow: 0px 1px 2px 0px var(--utopitheme-shadow90),
                0px 2px 4px -1px var(--utopitheme-shadow50);
              background-image: conic-gradient(
                var(--utopitheme-checkerboardLight) 0.25turn,
                var(--utopitheme-checkerboardDark) 0.25turn 0.5turn,
                var(--utopitheme-checkerboardLight) 0.5turn 0.75turn,
                var(--utopitheme-checkerboardDark) 0.75turn
              );
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

  it('Handles importing default exports declared separately', () => {
    const result = testCanvasRenderInlineMultifile(
      null,
      `
import React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
import {default as Appy} from './app'

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
function App(props) {
  return <div data-uid='app-outer-div'>
    <div data-uid='inner-div'>hello</div>
  </div>
}
export default App`,
      },
    )

    expect(result).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
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
              background-color: var(--utopitheme-emphasizedBackground);
              box-shadow: 0px 1px 2px 0px var(--utopitheme-shadow90),
                0px 2px 4px -1px var(--utopitheme-shadow50);
              background-image: conic-gradient(
                var(--utopitheme-checkerboardLight) 0.25turn,
                var(--utopitheme-checkerboardDark) 0.25turn 0.5turn,
                var(--utopitheme-checkerboardLight) 0.5turn 0.75turn,
                var(--utopitheme-checkerboardDark) 0.75turn
              );
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
          data-testid=\\"canvas-container\\"
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
              background-color: var(--utopitheme-emphasizedBackground);
              box-shadow: 0px 1px 2px 0px var(--utopitheme-shadow90),
                0px 2px 4px -1px var(--utopitheme-shadow50);
              background-image: conic-gradient(
                var(--utopitheme-checkerboardLight) 0.25turn,
                var(--utopitheme-checkerboardDark) 0.25turn 0.5turn,
                var(--utopitheme-checkerboardLight) 0.5turn 0.75turn,
                var(--utopitheme-checkerboardDark) 0.75turn
              );
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
              background-color: var(--utopitheme-emphasizedBackground);
              box-shadow: 0px 1px 2px 0px var(--utopitheme-shadow90),
                0px 2px 4px -1px var(--utopitheme-shadow50);
              background-image: conic-gradient(
                var(--utopitheme-checkerboardLight) 0.25turn,
                var(--utopitheme-checkerboardDark) 0.25turn 0.5turn,
                var(--utopitheme-checkerboardLight) 0.5turn 0.75turn,
                var(--utopitheme-checkerboardDark) 0.75turn
              );
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
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"storyboard-entity storyboard-entity/scene-1-entity storyboard-entity/scene-1-entity/app-entity storyboard-entity/scene-1-entity/app-entity:app-outer-div storyboard-entity/scene-1-entity/app-entity:app-outer-div/card-instance storyboard-entity/scene-1-entity/app-entity:app-outer-div/ff8 storyboard-entity/scene-1-entity/app-entity:app-outer-div/070\\"
          data-utopia-root-element-path=\\"storyboard-entity\\"
        >
          <div
            data-utopia-scene-id=\\"storyboard-entity/scene-1-entity\\"
            data-path=\\"storyboard-entity/scene-1-entity\\"
            style=\\"
              overflow: hidden;
              position: absolute;
              background-color: var(--utopitheme-emphasizedBackground);
              box-shadow: 0px 1px 2px 0px var(--utopitheme-shadow90),
                0px 2px 4px -1px var(--utopitheme-shadow50);
              background-image: conic-gradient(
                var(--utopitheme-checkerboardLight) 0.25turn,
                var(--utopitheme-checkerboardDark) 0.25turn 0.5turn,
                var(--utopitheme-checkerboardLight) 0.5turn 0.75turn,
                var(--utopitheme-checkerboardDark) 0.75turn
              );
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
                data-uid=\\"01b\\"
                data-path=\\"storyboard-entity/scene-1-entity/app-entity:app-outer-div/070:01b\\"
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
