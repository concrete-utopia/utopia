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
          "message": "MyCard is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 32,
              "fileName": "code.tsx",
              "lineNumber": 5,
              "originalCode": Array [
                ScriptLine {
                  "content": "    import * as React from \\"react\\"",
                  "highlight": false,
                  "lineNumber": 2,
                },
                ScriptLine {
                  "content": "    import { View, Storyboard, Scene } from 'utopia-api'",
                  "highlight": false,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "    export var App = props => <MyCard data-uid={'bbb'} />",
                  "highlight": true,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "    export var storyboard = (props) => {",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "      return (",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "        <Storyboard data-uid={'utopia-storyboard-uid'}>",
                  "highlight": false,
                  "lineNumber": 8,
                },
              ],
            },
          ],
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 2,
              "fileName": "code.tsx",
              "lineNumber": 4,
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
            Object {
              "columnNumber": 61,
              "fileName": "code.tsx",
              "lineNumber": 3,
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
                  "highlight": true,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "a.a // 16,1 this shall throw an error!",
                  "highlight": false,
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
              ],
            },
          ],
        },
      ]
    `)
  })
  it('throws an error for a property access where the onValue resolves to undefined', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
function something() {
  return undefined
}
export var App = (props) => {
  return <div data-uid={'aaa'} style={{left: something().value}}/>
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
          "message": "Cannot read properties of undefined (reading 'value')",
          "name": "TypeError",
          "stackFrames": Array [
            Object {
              "columnNumber": 46,
              "fileName": "code.tsx",
              "lineNumber": 7,
              "originalCode": Array [
                ScriptLine {
                  "content": "  return undefined",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "  return <div data-uid={'aaa'} style={{left: something().value}}/>",
                  "highlight": true,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "export var storyboard = (props) => {",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "  return (",
                  "highlight": false,
                  "lineNumber": 10,
                },
              ],
            },
          ],
        },
      ]
    `)
  })
  it('throws an error for an element access where the onValue resolves to undefined', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
function something() {
  return undefined
}
export var App = (props) => {
  return <div data-uid={'aaa'} style={{left: something()[0]}}/>
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
          "message": "Cannot read properties of undefined (reading '0')",
          "name": "TypeError",
          "stackFrames": Array [
            Object {
              "columnNumber": 46,
              "fileName": "code.tsx",
              "lineNumber": 7,
              "originalCode": Array [
                ScriptLine {
                  "content": "  return undefined",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "  return <div data-uid={'aaa'} style={{left: something()[0]}}/>",
                  "highlight": true,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "export var storyboard = (props) => {",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "  return (",
                  "highlight": false,
                  "lineNumber": 10,
                },
              ],
            },
          ],
        },
      ]
    `)
  })
  it('throws an error for an element access where the element expression throws an error', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
import { View, Storyboard, Scene } from 'utopia-api'
function something() {
  return [1]
}
function throwError() {
  throw new Error('This is an error.')
}
export var App = (props) => {
  return <div data-uid={'aaa'} style={{left: something()[throwError()]}}/>
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
          "message": "This is an error.",
          "name": "Error",
          "stackFrames": Array [
            Object {
              "columnNumber": 9,
              "fileName": "code.tsx",
              "lineNumber": 7,
              "originalCode": Array [
                ScriptLine {
                  "content": "  return [1]",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "function throwError() {",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "  throw new Error('This is an error.')",
                  "highlight": true,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "  return <div data-uid={'aaa'} style={{left: something()[throwError()]}}/>",
                  "highlight": false,
                  "lineNumber": 10,
                },
              ],
            },
            Object {
              "columnNumber": 46,
              "fileName": "code.tsx",
              "lineNumber": 10,
              "originalCode": Array [
                ScriptLine {
                  "content": "  throw new Error('This is an error.')",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "  return <div data-uid={'aaa'} style={{left: something()[throwError()]}}/>",
                  "highlight": true,
                  "lineNumber": 10,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 11,
                },
                ScriptLine {
                  "content": "export var storyboard = (props) => {",
                  "highlight": false,
                  "lineNumber": 12,
                },
                ScriptLine {
                  "content": "  return (",
                  "highlight": false,
                  "lineNumber": 13,
                },
              ],
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 20,
              "fileName": "code.tsx",
              "lineNumber": 8,
              "originalCode": Array [
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "  return (",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "    <MyComp data-uid={'aaa'}>",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "      {'hello' + a.a /* 20,18 */}",
                  "highlight": true,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "    </MyComp>",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "  )",
                  "highlight": false,
                  "lineNumber": 10,
                },
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 11,
                },
              ],
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 8,
              "fileName": "code.tsx",
              "lineNumber": 9,
              "originalCode": Array [
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
                  "content": "    <MyComp data-uid={'aaa'}>",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "      {a.a /* 20,8 */}",
                  "highlight": true,
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
                ScriptLine {
                  "content": "}",
                  "highlight": false,
                  "lineNumber": 12,
                },
              ],
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 57,
              "fileName": "code.tsx",
              "lineNumber": 8,
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 45,
              "fileName": "code.tsx",
              "lineNumber": 8,
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 10,
              "fileName": "code.tsx",
              "lineNumber": 7,
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
            Object {
              "columnNumber": 30,
              "fileName": "code.tsx",
              "lineNumber": 6,
              "originalCode": Array [
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
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": true,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "  '5' + a.a // 18,9",
                  "highlight": false,
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
              ],
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
          "message": "a is not defined",
          "name": "ReferenceError",
          "stackFrames": Array [
            Object {
              "columnNumber": 4,
              "fileName": "code.tsx",
              "lineNumber": 7,
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
            Object {
              "columnNumber": 30,
              "fileName": "code.tsx",
              "lineNumber": 6,
              "originalCode": Array [
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
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "export var App = (props) => {",
                  "highlight": true,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "  a.a // 18,3",
                  "highlight": false,
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
              ],
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
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"\\"
          data-utopia-root-element-path=\\"\\"
        ></div>
      </div>
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
          "message": "(MyCard) Element type is invalid: expected a string (for built-in components) or a class/function (for composite components) but got: undefined.",
          "name": "Error",
          "stackFrames": Array [
            Object {
              "columnNumber": 32,
              "fileName": "test.js",
              "lineNumber": 5,
              "originalCode": Array [
                ScriptLine {
                  "content": "import * as React from \\"react\\"",
                  "highlight": false,
                  "lineNumber": 1,
                },
                ScriptLine {
                  "content": "      import { View, Storyboard, Scene } from 'utopia-api'",
                  "highlight": false,
                  "lineNumber": 2,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "      const MyCard = undefined",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "      export var App = props => <MyCard data-uid={'bbb'} />",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "      export var storyboard = (props) => {",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "        return (",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "          <Storyboard data-uid={'utopia-storyboard-uid'}>",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "            <Scene",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "              style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}",
                  "highlight": false,
                  "lineNumber": 10,
                },
                ScriptLine {
                  "content": "              data-uid={'scene-aaa'}",
                  "highlight": false,
                  "lineNumber": 11,
                },
                ScriptLine {
                  "content": "            >",
                  "highlight": false,
                  "lineNumber": 12,
                },
                ScriptLine {
                  "content": "              <App",
                  "highlight": false,
                  "lineNumber": 13,
                },
                ScriptLine {
                  "content": "                data-uid='app-entity'",
                  "highlight": false,
                  "lineNumber": 14,
                },
                ScriptLine {
                  "content": "                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}",
                  "highlight": false,
                  "lineNumber": 15,
                },
                ScriptLine {
                  "content": "              />",
                  "highlight": false,
                  "lineNumber": 16,
                },
                ScriptLine {
                  "content": "            </Scene>",
                  "highlight": false,
                  "lineNumber": 17,
                },
                ScriptLine {
                  "content": "          </Storyboard>",
                  "highlight": false,
                  "lineNumber": 18,
                },
                ScriptLine {
                  "content": "        )",
                  "highlight": false,
                  "lineNumber": 19,
                },
                ScriptLine {
                  "content": "      }",
                  "highlight": false,
                  "lineNumber": 20,
                },
                ScriptLine {
                  "content": "      ",
                  "highlight": false,
                  "lineNumber": 21,
                },
              ],
            },
          ],
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
          "message": "(MyCard) Element type is invalid: expected a string (for built-in components) or a class/function (for composite components) but got: undefined.",
          "name": "Error",
          "stackFrames": Array [
            Object {
              "columnNumber": 32,
              "fileName": "test.js",
              "lineNumber": 4,
              "originalCode": Array [
                ScriptLine {
                  "content": "import * as React from \\"react\\"",
                  "highlight": false,
                  "lineNumber": 1,
                },
                ScriptLine {
                  "content": "      import { View, Storyboard, Scene, MyCard } from 'utopia-api'",
                  "highlight": false,
                  "lineNumber": 2,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "      export var App = props => <MyCard data-uid={'bbb'} />",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "      export var storyboard = (props) => {",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "        return (",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "          <Storyboard data-uid={'utopia-storyboard-uid'}>",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "            <Scene",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "              style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}",
                  "highlight": false,
                  "lineNumber": 9,
                },
                ScriptLine {
                  "content": "              data-uid={'scene-aaa'}",
                  "highlight": false,
                  "lineNumber": 10,
                },
                ScriptLine {
                  "content": "            >",
                  "highlight": false,
                  "lineNumber": 11,
                },
                ScriptLine {
                  "content": "              <App",
                  "highlight": false,
                  "lineNumber": 12,
                },
                ScriptLine {
                  "content": "                data-uid='app-entity'",
                  "highlight": false,
                  "lineNumber": 13,
                },
                ScriptLine {
                  "content": "                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}",
                  "highlight": false,
                  "lineNumber": 14,
                },
                ScriptLine {
                  "content": "              />",
                  "highlight": false,
                  "lineNumber": 15,
                },
                ScriptLine {
                  "content": "            </Scene>",
                  "highlight": false,
                  "lineNumber": 16,
                },
                ScriptLine {
                  "content": "          </Storyboard>",
                  "highlight": false,
                  "lineNumber": 17,
                },
                ScriptLine {
                  "content": "        )",
                  "highlight": false,
                  "lineNumber": 18,
                },
                ScriptLine {
                  "content": "      }",
                  "highlight": false,
                  "lineNumber": 19,
                },
                ScriptLine {
                  "content": "      ",
                  "highlight": false,
                  "lineNumber": 20,
                },
              ],
            },
          ],
        },
      ]
    `)
  })

  it('Handles errors with multifile stack traces', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `import * as React from "react"
      import { Storyboard, Scene } from 'utopia-api'
      import { throwErrorFromCard } from './app'

      export var App = (props) => {
        throwErrorFromCard()

        return <div />
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
      {
        '/app.js': `
    import { throwError } from './card'

    export function throwErrorFromCard() {
      throwError()
    }`,
        'card.js': `
    export function throwError() {
      throw new Error('I am an error!')
    }
    `,
      },
    )
    expect(canvasErrors).toMatchInlineSnapshot(`
      Array [
        Object {
          "message": "I am an error!",
          "name": "Error",
          "stackFrames": Array [
            Object {
              "columnNumber": 12,
              "fileName": "/card.js",
              "lineNumber": 3,
              "originalCode": Array [
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 1,
                },
                ScriptLine {
                  "content": "    export function throwError() {",
                  "highlight": false,
                  "lineNumber": 2,
                },
                ScriptLine {
                  "content": "      throw new Error('I am an error!')",
                  "highlight": true,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "    }",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "    ",
                  "highlight": false,
                  "lineNumber": 5,
                },
              ],
            },
            Object {
              "columnNumber": 6,
              "fileName": "/app.js",
              "lineNumber": 5,
              "originalCode": Array [
                ScriptLine {
                  "content": "    import { throwError } from './card'",
                  "highlight": false,
                  "lineNumber": 2,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "    export function throwErrorFromCard() {",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "      throwError()",
                  "highlight": true,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "    }",
                  "highlight": false,
                  "lineNumber": 6,
                },
              ],
            },
            Object {
              "columnNumber": 9,
              "fileName": "code.tsx",
              "lineNumber": 6,
              "originalCode": Array [
                ScriptLine {
                  "content": "      import { throwErrorFromCard } from './app'",
                  "highlight": false,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "      export var App = (props) => {",
                  "highlight": false,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "        throwErrorFromCard()",
                  "highlight": true,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "        return <div />",
                  "highlight": false,
                  "lineNumber": 8,
                },
                ScriptLine {
                  "content": "      }",
                  "highlight": false,
                  "lineNumber": 9,
                },
              ],
            },
            Object {
              "columnNumber": 36,
              "fileName": "code.tsx",
              "lineNumber": 5,
              "originalCode": Array [
                ScriptLine {
                  "content": "      import { Storyboard, Scene } from 'utopia-api'",
                  "highlight": false,
                  "lineNumber": 2,
                },
                ScriptLine {
                  "content": "      import { throwErrorFromCard } from './app'",
                  "highlight": false,
                  "lineNumber": 3,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 4,
                },
                ScriptLine {
                  "content": "      export var App = (props) => {",
                  "highlight": true,
                  "lineNumber": 5,
                },
                ScriptLine {
                  "content": "        throwErrorFromCard()",
                  "highlight": false,
                  "lineNumber": 6,
                },
                ScriptLine {
                  "content": "",
                  "highlight": false,
                  "lineNumber": 7,
                },
                ScriptLine {
                  "content": "        return <div />",
                  "highlight": false,
                  "lineNumber": 8,
                },
              ],
            },
          ],
        },
      ]
    `)
  })
})
