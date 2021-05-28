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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": undefined,
          "lineNumber": undefined,
          "message": "MyCard is not defined",
          "name": "ReferenceError",
          "originalCode": undefined,
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 2,
          "lineNumber": 4,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "import * as React from \\"react\\"",
              "highlight": false,
              "lineNumber": 1,
            },
            ScriptLine {
              "content": "import { View, Storyboard, Scene } from 'utopia-api'",
              "highlight": false,
              "lineNumber": 2,
            },
            ScriptLine {
              "content": "const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>",
              "highlight": false,
              "lineNumber": 3,
            },
            ScriptLine {
              "content": "a.a // 16,1 this shall throw an error!",
              "highlight": true,
              "lineNumber": 4,
            },
            ScriptLine {
              "content": "export var App = (props) => {",
              "highlight": false,
              "lineNumber": 5,
            },
            ScriptLine {
              "content": "  return (<MyComp data-uid={'aaa'}/>)",
              "highlight": false,
              "lineNumber": 6,
            },
            ScriptLine {
              "content": "}",
              "highlight": false,
              "lineNumber": 7,
            },
          ],
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 13,
          "lineNumber": 1,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "",
              "highlight": true,
              "lineNumber": 1,
            },
            ScriptLine {
              "content": "import * as React from \\"react\\"",
              "highlight": false,
              "lineNumber": 2,
            },
            ScriptLine {
              "content": "import { View, Storyboard, Scene } from 'utopia-api'",
              "highlight": false,
              "lineNumber": 3,
            },
            ScriptLine {
              "content": "const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>",
              "highlight": false,
              "lineNumber": 4,
            },
          ],
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 3,
          "lineNumber": 1,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "",
              "highlight": true,
              "lineNumber": 1,
            },
            ScriptLine {
              "content": "import * as React from \\"react\\"",
              "highlight": false,
              "lineNumber": 2,
            },
            ScriptLine {
              "content": "import { View, Storyboard, Scene } from 'utopia-api'",
              "highlight": false,
              "lineNumber": 3,
            },
            ScriptLine {
              "content": "const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>",
              "highlight": false,
              "lineNumber": 4,
            },
          ],
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 57,
          "lineNumber": 8,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "",
              "highlight": false,
              "lineNumber": 5,
            },
            ScriptLine {
              "content": "export var App = (props) => {",
              "highlight": false,
              "lineNumber": 6,
            },
            ScriptLine {
              "content": "  return (",
              "highlight": false,
              "lineNumber": 7,
            },
            ScriptLine {
              "content": "    <MyComp data-uid={'aaa'} someAttribute={'hello' + a.a /* 19,55 */}>",
              "highlight": true,
              "lineNumber": 8,
            },
            ScriptLine {
              "content": "      hello!",
              "highlight": false,
              "lineNumber": 9,
            },
            ScriptLine {
              "content": "    </MyComp>",
              "highlight": false,
              "lineNumber": 10,
            },
            ScriptLine {
              "content": "  )",
              "highlight": false,
              "lineNumber": 11,
            },
          ],
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 47,
          "lineNumber": 8,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "",
              "highlight": false,
              "lineNumber": 5,
            },
            ScriptLine {
              "content": "export var App = (props) => {",
              "highlight": false,
              "lineNumber": 6,
            },
            ScriptLine {
              "content": "  return (",
              "highlight": false,
              "lineNumber": 7,
            },
            ScriptLine {
              "content": "    <MyComp data-uid={'aaa'} someAttribute={a.a /* 19,45 */}>",
              "highlight": true,
              "lineNumber": 8,
            },
            ScriptLine {
              "content": "      hello!",
              "highlight": false,
              "lineNumber": 9,
            },
            ScriptLine {
              "content": "    </MyComp>",
              "highlight": false,
              "lineNumber": 10,
            },
            ScriptLine {
              "content": "  )",
              "highlight": false,
              "lineNumber": 11,
            },
          ],
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 10,
          "lineNumber": 7,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>",
              "highlight": false,
              "lineNumber": 4,
            },
            ScriptLine {
              "content": "",
              "highlight": false,
              "lineNumber": 5,
            },
            ScriptLine {
              "content": "export var App = (props) => {",
              "highlight": false,
              "lineNumber": 6,
            },
            ScriptLine {
              "content": "  '5' + a.a // 18,9",
              "highlight": true,
              "lineNumber": 7,
            },
            ScriptLine {
              "content": "  return (",
              "highlight": false,
              "lineNumber": 8,
            },
            ScriptLine {
              "content": "    <MyComp data-uid={'aaa'}>",
              "highlight": false,
              "lineNumber": 9,
            },
            ScriptLine {
              "content": "      hello!",
              "highlight": false,
              "lineNumber": 10,
            },
          ],
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": 4,
          "lineNumber": 7,
          "message": "a is not defined",
          "name": "ReferenceError",
          "originalCode": Array [
            ScriptLine {
              "content": "const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>",
              "highlight": false,
              "lineNumber": 4,
            },
            ScriptLine {
              "content": "",
              "highlight": false,
              "lineNumber": 5,
            },
            ScriptLine {
              "content": "export var App = (props) => {",
              "highlight": false,
              "lineNumber": 6,
            },
            ScriptLine {
              "content": "  a.a // 18,3",
              "highlight": true,
              "lineNumber": 7,
            },
            ScriptLine {
              "content": "  return (",
              "highlight": false,
              "lineNumber": 8,
            },
            ScriptLine {
              "content": "    <MyComp data-uid={'aaa'}>",
              "highlight": false,
              "lineNumber": 9,
            },
            ScriptLine {
              "content": "      hello!",
              "highlight": false,
              "lineNumber": 10,
            },
          ],
        },
      ]
    `)
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
    expect(result).toMatchInlineSnapshot(`
      "<div
        id=\\"canvas-container\\"
        style=\\"all: initial; position: absolute;\\"
        data-utopia-valid-paths=\\"\\"
      ></div>
      "
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": undefined,
          "lineNumber": undefined,
          "message": "Element type is invalid: expected a string (for built-in components) or a class/function (for composite components) but got: undefined. You likely forgot to export your component from the file it's defined in, or you might have mixed up default and named imports.",
          "name": "Error",
          "originalCode": undefined,
        },
      ]
    `)
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
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "columnNumber": undefined,
          "lineNumber": undefined,
          "message": "Element type is invalid: expected a string (for built-in components) or a class/function (for composite components) but got: undefined. You likely forgot to export your component from the file it's defined in, or you might have mixed up default and named imports.",
          "name": "Error",
          "originalCode": undefined,
        },
      ]
    `)
  })
})
