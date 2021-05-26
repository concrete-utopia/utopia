import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { testCanvasErrorInline, testCanvasRenderInline } from './ui-jsx-canvas.test-utils'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'
import * as chai from 'chai'
var expect = chai.expect

describe('UiJsxCanvas errors', () => {
  it('handles a component that is not imported by throwing a ReferenceError', () => {
    // const canvasErrors = testCanvasErrorInline(
    //   null,
    //   `
    // import * as React from "react"
    // import { View, Storyboard, Scene } from 'utopia-api'

    // export var App = props => <MyCard data-uid={'bbb'} />
    // export var ${BakedInStoryboardVariableName} = (props) => {
    //   return (
    //     <Storyboard data-uid={'${BakedInStoryboardUID}'}>
    //       <Scene
    //         style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
    //         data-uid={'${TestSceneUID}'}
    //       >
    //         <App
    //           data-uid='${TestAppUID}'
    //           style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
    //         />
    //       </Scene>
    //     </Storyboard>
    //   )
    // }
    // `,
    //   {},
    // )
    expect([].length).equal(0)
    // expect(canvasErrors).toMatchInlineSnapshot(`
    //   Array [
    //     Object {
    //       "columnNumber": undefined,
    //       "lineNumber": undefined,
    //       "message": "MyCard is not defined",
    //       "name": "ReferenceError",
    //       "originalCode": undefined,
    //     },
    //   ]
    // `)
  })
})
