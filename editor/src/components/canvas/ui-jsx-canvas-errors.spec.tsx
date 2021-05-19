import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { testCanvasErrorInline, testCanvasRenderInline } from './ui-jsx-canvas.test-utils'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'

describe('UiJsxCanvas errors', () => {
  it('handles a component that is not imported by throwing a ReferenceError', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
    import * as React from "react"
    import { View, Storyboard, Scene } from 'utopia-api'

    export var App = props => <MyCard data-uid={'bbb'} />
    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid={'${BakedInStoryboardUID}'}>
          <Scene
            style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
            data-uid={'${TestSceneUID}'}
          >
            <App
              data-uid='${TestAppUID}'
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            />
          </Scene>
        </Storyboard>
      )
    }
    `,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })
})

describe('UiJsxCanvas runtime errors', () => {
  it('throws an error!', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>
a.a // 16,1 this shall throw an error!
export var App = (props) => {
  return (<MyComp data-uid={'aaa'}/>)
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })
  it('an arbitrary jsx child has correct source map', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>
export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'}>
      {'hello' + a.a /* 20,18 */}
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('an arbitrary jsx child has correct source map even if the entire expression is broken', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'}>
      {a.a /* 20,8 */}
    </MyComp>
  )
}
  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid={'${BakedInStoryboardUID}'}>
        <Scene
          style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
          data-uid={'${TestSceneUID}'}
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('an arbitrary jsx attribute has correct source map', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'} someAttribute={'hello' + a.a /* 19,55 */}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('an arbitrary jsx attribute has correct source map even if the entire expression is broken', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'} someAttribute={a.a /* 19,45 */}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('arbitrary at the top of a component has correct source map', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  '5' + a.a // 18,9
  return (
    <MyComp data-uid={'aaa'}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('arbitrary at the top of a component has correct source map even if the entire expression is broken', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  a.a // 18,3
  return (
    <MyComp data-uid={'aaa'}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('React.useEffect at the root fails usefully', () => {
    const result = testCanvasRenderInline(
      null,
      `import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
React.useEffect()
export var App = (props) => {
  return "hello!"
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}'
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
    )
    expect(result).toMatchInlineSnapshot()
  })

  it('handles an undefined component gracefully', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

      const MyCard = undefined
      export var App = props => <MyCard data-uid={'bbb'} />
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })

  it('handles an non-existent component gracefully', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
      import { View, Storyboard, Scene, MyCard } from 'utopia-api'

      export var App = props => <MyCard data-uid={'bbb'} />
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {},
    )
    expect(canvasErrors).toMatchInlineSnapshot()
  })
})
