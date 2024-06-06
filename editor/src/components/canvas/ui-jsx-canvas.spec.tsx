/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "testCanvasRender", "testCanvasRenderMultifile", "testCanvasErrorMultifile"] }] */
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { AwkwardFragmentsCode } from '../../core/workers/parser-printer/parser-printer-fragments.test-utils'
import {
  testCanvasRender,
  testCanvasRenderInline,
  testCanvasRenderMultifile,
  testCanvasRenderInlineMultifile,
  testCanvasErrorMultifile,
} from './ui-jsx-canvas.test-utils'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'

describe('UiJsxCanvas render', () => {
  it('renders a canvas testing a multitude of export styles', () => {
    testCanvasRenderMultifile(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import { App } from '/app'
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 600, left: 0, width: 600, top: 0 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {
        '/app.js': `import * as React from 'react'
import { OriginallyUnassigned1 } from '/originallyunassigned'
import { OriginallyAssigned1 } from '/originallyassigned'
import { NamedFunction } from '/namedfunction'
import { NamedClass } from '/namedclass'
import { NamedExport, RenamedExport } from '/namedexport'
import { FirstInStructure, SecondInStructure } from '/destructuredassignment'
import DefaultExpression from '/defaultexpression'
import DefaultFunction from '/defaultfunction'
import DefaultClass from '/defaultclass'
import DefaultNamedFunction from '/defaultnamedfunction'
import DefinedThenDefaultExported from '/definedthendefaultexported'
import NamedAsDefault from '/namedasdefault'
import * as ReexportWildcard from '/reexportwildcard'
import { assigned } from '/reexportmoduleintonamedexport'
import { OriginallyAssigned1 as OriginallyAssigned2 } from '/reexportspecificnamed'
import { NewlyAssigned } from '/reexportspecificrenamed'
import DefaultFunction2 from '/reexportdefault'

export var App = (props) => {
  return (
    <div>
      <OriginallyUnassigned1 />
      <OriginallyAssigned1 />
      <NamedFunction />
      <NamedClass />
      <NamedExport />
      <RenamedExport />
      <FirstInStructure />
      <SecondInStructure />
      <div>The Number Is {DefaultExpression}</div>
      <DefaultFunction />
      <DefaultClass />
      <DefaultNamedFunction />
      <DefinedThenDefaultExported />
      <NamedAsDefault />
      <ReexportWildcard.OriginallyAssigned1 />
      <assigned.OriginallyAssigned1 />
      <OriginallyAssigned2 />
      <NewlyAssigned />
      <DefaultFunction2 />
    </div>
  )
}`,
        '/originallyunassigned.js': `import * as React from 'react'
export let OriginallyUnassigned1
OriginallyUnassigned1 = () => <div>Originally Unassigned</div>`,
        '/originallyassigned.js': `import * as React from 'react'
export let OriginallyAssigned1 = () => <div>Originally Assigned</div>`,
        '/namedfunction.js': `import * as React from 'react'
export function NamedFunction() { return <div>Named Function</div> }`,
        '/namedclass.js': `import * as React from 'react'
export class NamedClass extends React.Component { render() { return <div>Named Class</div> } }`,
        '/namedexport.js': `import * as React from 'react'
const NamedExport = () => <div>Named Export</div>
const ToBeRenamedExport = () => <div>Renamed Export</div>
export { NamedExport, ToBeRenamedExport as RenamedExport }`,
        '/destructuredassignment.js': `import * as React from 'react'
const toDestructure = { FirstInStructure: () => <div>First In Structure</div>, SceodnInStructure: () => <div>Second In Structure</div> }
        export const { FirstInStructure, SceodnInStructure: SecondInStructure } = toDestructure`,
        '/defaultexpression.js': `import * as React from 'react'
export default 2 + 2`,
        '/defaultfunction.js': `import * as React from 'react'
export default function() { return <div>Default Function</div> }`,
        '/defaultclass.js': `import * as React from 'react'
export default class DefaultClass extends React.Component { render() { return <div>Export Default Class</div> } }`,
        '/defaultnamedfunction.js': `import * as React from 'react'
export default function DefaultNamedFunction() { return <div>Default Named Function</div> }`,
        '/definedthendefaultexported.js': `import * as React from 'react'
        const DefinedThenDefaultExported = () => <div>Defined Then Default Exported</div>
        export default DefinedThenDefaultExported`,
        '/namedasdefault.js': `import * as React from 'react'
const ToBeDefaultExported = () => <div>Named As Default</div>
export { ToBeDefaultExported as default }`,
        '/reexportwildcard.js': `export * from '/originallyassigned'`,
        '/reexportmoduleintonamedexport.js': `export * as assigned from '/originallyassigned'`,
        '/reexportspecificnamed.js': `export { OriginallyAssigned1 } from '/originallyassigned'`,
        '/reexportspecificrenamed.js': `export { OriginallyAssigned1 as NewlyAssigned } from '/originallyassigned'`,
        '/reexportdefault.js': `export { default } from '/defaultfunction'`,
      },
    )
  })

  it('renders a canvas reliant on another file that uses module.exports', () => {
    testCanvasRenderMultifile(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      import Test from '/testspecialvalue'
      
      export var App = (props) => {
        return (
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <View style={{position: 'absolute'}} data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: Test.specialValue * 2, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      { '/testspecialvalue.js': 'module.exports = { specialValue: 100 }' },
    )
  })

  it('renders a canvas with a component wrapped in React.memo', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      
      export var App = React.memo((props) => {
        return (
          <div
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <span style={{position: 'absolute'}} data-uid={'bbb'}>hi</span>
          </div>
        )
      })
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('renders a canvas with a component wrapped in a function that wraps the component with more content', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'

      function wrapComponent(Component) {
        return () => {
          return <div>
            <div>
              <span>wrapComponent</span>
            </div>
            <div><Component /></div>
          </div>
        }
      }
      
      export var App = wrapComponent(() => {
        return (
          <div
            style={{ backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <span style={{position: 'absolute'}} data-uid={'bbb'}>hi</span>
          </div>
        )
      })
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('renders a canvas with a component wrapped in a function that has more that one parameter', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'

      function twoParameterWrapper(componentFunction) {
        return (props) => {
          return componentFunction(props, 'Some Text')
        }
      }
      
      export var App = twoParameterWrapper((props, otherValue) => {
        return (
          <div
            style={{ backgroundColor: props.backgroundColor }}
            data-uid={'aaa'}
          >
            <span style={{position: 'absolute'}} data-uid={'bbb'}>{otherValue}</span>
          </div>
        )
      })

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                backgroundColor='green'
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('renders a canvas with a component wrapped in forwardRef', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'

      export var ComponentThatForwards = React.forwardRef((props, ref) => {
        return (
          <div
            style={{ backgroundColor: props.backgroundColor }}
            data-uid={'aaa'}
            ref={ref}
          >
            <span style={{position: 'absolute'}} data-uid={'bbb'}>Something</span>
          </div>
        )
      })

      export var App = () => {
        const ref = React.useRef(null)
        return (
          <ComponentThatForwards ref={ref} />
        )
      }

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                backgroundColor='green'
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('renders a canvas defined by a utopia storyboard component', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <View style={{position: 'absolute'}} data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component that renames its props object', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      const Card = (cardProps) => <div data-uid={'xxx'}>{cardProps.title}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{ position: 'absolute', backgroundColor: '#000000'}} title={props.title} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component that destructures its props object', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      
      const Card = ({style, title}) => <div style={style} data-uid={'xxx'}>{title}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{position: 'absolute', backgroundColor: '#000000'}} title={props.title} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component with a props object written by someone that wants to watch the world burn', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      const Card = ({style, titles: [title1, , {title: title2}]}) => <div style={style} data-uid={'xxx'}>{title1} - {title2}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{ position: 'absolute', backgroundColor: '#000000' }} titles={[props.title, 'ignored', {title: 'and hello!'}]} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component with a props object written by someone that wants to watch the world burn and also loves defaults', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      const Card = ({style, titles: [title1, , {title: title2}, title3 = 'Now begone!']}) => <div style={style} data-uid={'xxx'}>{title1} - {title2} - {title3}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{ position: 'absolute', backgroundColor: '#000000' }} titles={[props.title, 'ignored', {title: 'and hello!'}]} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('renders a component used in an arbitrary block correctly', () => {
    testCanvasRender(
      null,
      `
      import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(n => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
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
  })
  it('renders a component used in an arbitrary block correctly, with an HTML element name as a parameter name', () => {
    testCanvasRender(
      null,
      `
      import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(div => (
             <MyCard data-uid={'bbb'} title={'n' + div} />
           ))}
         </View>
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
  })
  it('supports passing down the scope to children of components', () => {
    testCanvasRender(
      null,
      `
      import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

       export var App = (props) => {
         return <View data-uid={'aaa'}>
           { [1, 2, 3].map(n => {
             return <div data-uid={'bbb'}>
               <div data-uid={'ccc'}>{n}</div>
             </div>
           })}
         </View>
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
  })
  it('renders a component used in an arbitrary block with eye-stabbingly awful nested destructuring correctly', () => {
    testCanvasRender(
      null,
      `
      import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         const nestedThings = [ {a: { b: { c: [ 1 ] } } }, {a: { b: { c: [ 2 ] } } }, {a: { b: { c: [ 3 ] } } } ]
         return <View data-uid={'aaa'}>
           {nestedThings.map(({ a: { b: { c: [ n ] } } }) => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
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
      }`,
    )
  })
  xit('renders a component used in an arbitrary block with even more eye-stabbingly awful nested destructuring correctly', () => {
    // FIXME Nested array destructuring doesn't work
    testCanvasRender(
      null,
      `
      import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         const nestedThings = [ {a: { b: { c: [ [ { d: [ 1 ] } ] ] } } }, {a: { b: { c: [ [ { d: [ 2 ] } ] ] } } }, {a: { b: { c: [ [ { d: [ 3 ] } ] ] } } } ]
         return <View data-uid={'aaa'}>
           {nestedThings.map(({ a: { b: { c: [ [ { d: [ n ] } ] ] } } }) => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
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
  })
  it('renders a 1st party component with uids correctly, using the passed uid instead inside App', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      export var Inner = (props) => {
        return (
          <View style={{ ...props.style, backgroundColor: '#DDDDDD' }} data-uid={'6be'}>
            <View
              style={{ backgroundColor: '#000000', left: 10, top: 10, right: 10, height: 30 }}
              data-uid={'d03'}
            />
            {props.children}
            <View
              style={{ backgroundColor: '#000000', left: 10, top: 100, right: 10, height: 30 }}
              data-uid={'41e'}
            />
          </View>
        )
      }
      export var App = (props) => {
        return (
          <View
            style={{ position: 'absolute', backgroundColor: 'lightgrey' }}
            data-uid={'aaa'}
          >
            <Inner data-uid={'d59'} style={{ left: 28, top: 27, width: 221, height: 348 }}>
              <View
                style={{ backgroundColor: '#fff', left: 14, top: 21, width: 193, height: 244 }}
                data-uid={'dd5'}
              />
            </Inner>
          </View>
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
    )
  })

  it('class component is available from arbitrary block in JSX element', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
      class Thing extends React.Component {
        render() {
          return <div data-uid="ccc-unparsed-no-template-path">Thing</div>
        }
      }
      export var App = (props) => {
        return <div data-uid="zzz">
          <Thing data-uid="aaa" />
          <Thing data-uid="bbb" />
        </div>
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
      }`,
    )
  })
  it('function component is available from arbitrary block in JSX element', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
      const Thing = (props) => <div data-uid="ccc">Thing</div>
      export var App = (props) => {
        return <div data-uid="zzz">
          <Thing data-uid="aaa" />
          <Thing data-uid="bbb" />
        </div>
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
      }`,
    )
  })
  it('function component works inside a map', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export const Thing = (props) => <div data-uid="ccc" >Thing</div>
       export var App = (props) => {
         return <div data-uid="zzz">
            {[1, 2].map((data) => {
              return (<Thing data-uid="aaa" />)
            })}
         </div>
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
  })
  it('props can be accessed inside the arbitrary js block inside a text range', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export const Thing = (props) => <div data-uid="ccc">Hello {props.name}!</div>
       const Thang = (props) => <div data-uid="ddd">Hello {props.name}!</div>

       export var App = (props) => {
         return <div data-uid="zzz">
           <Thing data-uid="aaa" name='World!'/>
           <Thang data-uid="bbb" name='Dolly!'/>
         </div>
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
  })
  it('arbitrary jsx block inside an element inside an arbitrary jsx block', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export var App = (props) => {
         return <div data-uid="zzz">
           {[1, 2, 3].map((n) => {
             return <div data-uid="aaa">{n}</div>
           })}
         </div>
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
  })
  it('arbitrary jsx block inside an element inside an arbitrary jsx block inside an element inside an arbitrary jsx block', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"

       export var App = (props) => {
         return <div data-uid="zzz">
           {[1, 2, 3].map((n) => {
             return <div data-uid="aaa">
               {[4, 5, 6].map((m) => {
                 return <div data-uid="bbb">{n * m}</div>
               })}
             </div>
           })}
         </div>
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
                style={{ position: 'absolute', position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }
       `,
    )
  })
  it('renders img tag', () => {
    testCanvasRender(
      null,
      `
      import { View, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"

      export var App = (props) => {
        return <View data-uid={'aaa'}>
        <img data-uid={'bbb'} src="data:image/gif;base64,R0lGODlhAQABAIAAAAUEBAAAACwAAAAAAQABAAACAkQBADs=" />
        </View>
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
  })
  it('the spy wrapper is compatible with React.cloneElement', () => {
    testCanvasRender(
      null,
      `
      import { Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export const ClonerComponent = (props) => {
        return (
          <div data-uid='cloner-root'>
            {React.Children.map(props.children, (child, index) => {
              if (React.isValidElement(child)) {
                return React.cloneElement(child, {
                  style: { color: 'red', fontWeight: 800 },
                })
              } else {
                return child
              }
            })}
          </div>
        )
      }
      
      export var App = (props) => {
        return (
          <div data-uid='zzz'>
            <ClonerComponent data-uid='cloner'>
              <div data-uid='cloned'>ha</div>
            </ClonerComponent>
          </div>
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
    )
  })

  it('handles chaining dependencies into the appropriate order', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'
export var App = (props) => {
  return <Widget data-uid={'bbb'} />
}
export var Widget = (props) => {
  return <View data-uid={'aaa'} />
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
  })

  it('respects a jsx pragma', () => {
    testCanvasRender(
      null,
      `/** @jsx myFactoryFunction */
      import * as React from "react"
      import { Storyboard, Scene } from 'utopia-api'
      const MyComp = (props) => <div>Utopia</div>
      export var App = (props) => {
        return (<MyComp data-uid={'aaa'}/>)
      }

      function myFactoryFunction(type, props, children) {
        const modifiedProps = {...props, 'data-factory-function-works': "true"}
        if (children == null) {
          return React.createElement(type, modifiedProps)
        } else {
          return React.createElement(type, modifiedProps, children)
        }
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
  })

  it('the canvas supports emotion CSS prop', () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import { css, jsx } from '@emotion/react'
      import Utopia, {
        Scene,
        Storyboard,
      } from 'utopia-api'
      const MyComp = (props) => <div css={{ backgroundColor: 'blue', position: 'absolute', left: 15, top: 15, width: 50, height: 50, flex: 15 }}>Utopia</div>
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
    )
  })

  xit('mutated variable refers to code component', () => {
    testCanvasRender(
      null,
      `
      import Utopia, {
        Scene,
        Storyboard,
      } from 'utopia-api'
      import * as React from "react"
      let MyComp
      MyComp = (props) => <div>Utopia</div>
      export var App = (props) => {
        return (<MyComp />)
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
  })
  it('does not crash if the metadata scenes are not the appropriate value', () => {
    testCanvasRender(
      null,
      `
      import * as React from "react"
      import { View, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(n => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
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
  })
  it('does not crash if the metadata scenes are undefined', () => {
    testCanvasRender(
      null,
      `
      import { View, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(n => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
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
  })

  it('Renders input tag without errors', async () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      export var InputElement = (props) => <input data-uid={props['data-uid']} style={{top: 10}} />
      export var App = (props) => {
        return <InputElement data-uid={'567'} />
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
  })

  it('Label carried through for normal elements', async () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...props.style}} data-uid={'aaa'} data-label={'Hat'} />
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
  })

  it('Label carried through for generated elements', async () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...props.style}} data-uid={'aaa'}>
          {[1, 2, 3].map(n => {
            return <div data-uid={'bbb'} data-label={'Plane'} />
          })}
        </div>
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
  })

  it('console logging does not do anything bizarre', async () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      console.log('root log')
      export var App = (props) => {
        console.log('inside component log')
        return <div style={{ ...props.style}} data-uid={'aaa'} />
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
  })

  it('refs are handled and triggered correctly in a functional component', async () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      export var App = (props) => {
        return <div ref={() => console.log('functional component')} style={{ ...props.style}} data-uid={'aaa'} />
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
  })

  it('refs are handled and triggered correctly in a class component', async () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      class Thing extends React.Component {
        render() {
          return <div data-uid="ccc-unparsed-no-template-path">Thing</div>
        }
      }
      export var App = (props) => {
        return <Thing ref={() => console.log('class component')} data-uid={'aaa'} />
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
  })

  it('class components have working contexts', async () => {
    const printedDom = testCanvasRenderInline(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      const MyContext = React.createContext({ textToShow: 'hello' });

      class Thing extends React.Component {
        render() {
          const { textToShow } = this.context;
          return <div data-uid="ccc-unparsed-no-template-path">{textToShow}</div>
        }
      }
      Thing.contextType = MyContext;
      export var App = (props) => {
        return <Thing ref={() => console.log('class component')} data-uid={'aaa'} />
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
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:aaa\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              width: 400px;
              height: 400px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              data-uid=\\"ccc-unparsed-no-template-path\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:aaa\\"
            >
              hello
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('weird antd-like class components have working uid', async () => {
    const printedDom = testCanvasRenderInline(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      export class RenderPropsFunctionChild extends React.Component {
        render() {
          return this.props.children('huha')
        }
      }

      export const ParsedComponentToBreakUpArbitraryBlocks = (props) => {
        return <div />
      }

      export function getPicker() {
        class Picker extends React.Component {
          renderPicker(locale) {
            return (
              <RenderPropsFunctionChild>
                {(size) => {
                  return (
                    <div id='nasty-div'>
                      {locale} {size}
                    </div>
                  )
                }}
              </RenderPropsFunctionChild>
            )
          }

          render() {
            return <RenderPropsFunctionChild>{this.renderPicker}</RenderPropsFunctionChild>
          }
        }

        return Picker
      }

      const Thing = getPicker()

      export var App = (props) => {
        return <Thing data-uid={'aaa'} />
      }
      export var ${BakedInStoryboardVariableName}  = (props) => {
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
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:aaa\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              width: 400px;
              height: 400px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              id=\\"nasty-div\\"
              data-uid=\\"aaa\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:aaa\\"
            >
              huha huha
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('handles fragments in an arbitrary block', () => {
    testCanvasRender(
      null,
      `
import * as React from 'react'
import { View, Scene, Storyboard } from 'utopia-api'
const keyboardShortcuts = [
  {
    label: 'Copy',
    macKeys: ['⌘', '⎇', 'C'],
  },
  {
    label: 'Paste',
    macKeys: '⌘⎇V',
  },
  {
    label: 'Cut',
    macKeys: '⌘⎇C',
  },
]
export var KeyboardShortcut = (props) => {
  return (
    <span data-uid={'6a8'}>
      {[...props.shortcut].map((keyb) => (
        <span
          style={{
            padding: 6,
          }}
          data-uid={'726'}
        >
          {keyb}
        </span>
      ))}
    </span>
  )
}
export var Grid = (props) => {
  return (
    <div
      style={{
        display: 'grid',
        gridTemplateColumns: '2fr 1fr',
        gridAutoRows: '31px',
      }}
      data-uid={'eb1'}
    >
      {props.children}
    </div>
  )
}
export var App = (props) => {
  return (
    <View
      style={{
        ...props.style,
        fontSize: '12px',
        fontFamily: 'Inter',
        color: '#237AFF',
        left: 0,
        top: 0,
        width: 376,
        height: 812,
        backgroundColor: '#171111',
      }}
      
      data-uid={'aaa'}
    >
      <Grid data-uid={'03a'}>
        {keyboardShortcuts.map((pair) => (
          <>
            <div data-uid={'834'} data-label={pair.label}>
              {pair.label}
            </div>
            <div data-uid={'999'} data-label={pair.macKeys}>
              <KeyboardShortcut data-uid={'000'} shortcut={pair.macKeys}></KeyboardShortcut>
            </div>
          </>
        ))}
      </Grid>
    </View>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
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
  })
  it('renders fragments correctly', () => {
    testCanvasRender(null, AwkwardFragmentsCode)
  })

  it('renders a component with a fragment at the root', () => {
    testCanvasRender(
      null,
      `import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

export var Cat = (props) => {
  return (
    <React.Fragment data-uid={'fff'}>
      <div data-uid={'aaa'}>hello</div>
      <div data-uid={'bbb'}>bello</div>
    </React.Fragment>
  )
}

export var storyboard = (
  <Storyboard data-uid={'${BakedInStoryboardUID}'} >
    <Scene
      data-label={'Scene 2'}
      style={{ position: 'absolute', left: 406, top: 62, width: 212, height: 188 }}
      data-uid={'${TestSceneUID}'}
    >
      <Cat data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
    )
  })

  it('renders fine with two components that reference each other', () => {
    testCanvasRender(
      null,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'

export var A = (props) => {
  // @utopia/uid=aaa-root
  if (props.x === 0) {
    return <div data-uid='aaa-root-true'>great</div>
  } else {
    return <B data-uid='aaa-root-false' x={props.x - 1} />
  }
}

export var B = (props) => {
  // @utopia/uid=bbb-root
  if (props.x === 0) {
    return <div data-uid='bbb-root-true'>great</div>
  } else {
    return <A data-uid='bbb-root-false' x={props.x - 1} />
  }
}

export var App = (props) => {
  return (
    <B data-uid={'BBB'} x={5} />
  )
}
export var storyboard = (
  <Storyboard data-uid={'${BakedInStoryboardUID}'} >
    <Scene
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      data-uid={'scene'}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
    )
  })

  it('renders fine with two circularly referencing arbitrary blocks', () => {
    testCanvasRender(
      null,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'

function a(n) {
  if (n <= 0) {
    return 0
  } else {
    return b(n - 1)
  }
}

export var App = (props) => {
  return (
    <div
      data-uid={'aaa'}
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      
    >{b(5)} - {a(5)}</div>
  )
}

function b(n) {
  if (n <= 0) {
    return 0
  } else {
    return a(n - 1)
  }
}

export var storyboard = (
  <Storyboard data-uid={'${BakedInStoryboardUID}'} >
    <Scene
      data-uid={'scene'}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
    )
  })
  it('renders correctly with a context', () => {
    testCanvasRender(
      null,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'

const AppContext = React.createContext({})
const useStoreRef = () => useContext(AppContext)

export var App = (props) => {
  const storeRef = React.useRef({})
  return (
    <AppContext.Provider value={storeRef} data-uid={'aaa'}>
      <div
        data-uid={'bbb'}
        style={{ width: '100%', height: '100%', backgroundColor: '#EB1010' }}
        
      />
    </AppContext.Provider>
  )
}

export var storyboard = (
  <Storyboard  data-uid={'ccc'}>
    <Scene
      data-uid={'ddd'}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
    )
  })

  it('renders correctly with a nested context in another component', () => {
    testCanvasRender(
      null,
      `
import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'

const AppContext = React.createContext({})
const InnerContext = React.createContext({})

export var Card = (props) => {
  return (
    <AppContext.Provider value={{color: 'blue'}} data-uid={'aaa'}>
      <InnerContext.Provider value={{otherColor: 'red'}} data-uid={'inner'}>
        <div
          data-uid={'bbb'}
          style={{ width: '100%', height: '100%', backgroundColor: '#EB1010' }}
        />
      </InnerContext.Provider>
    </AppContext.Provider>
  )
}

export var App = (props) => {
  return (
    <Card data-uid='card' />
  )
}

export var storyboard = (
  <Storyboard  data-uid={'ccc'}>
    <Scene
      data-uid={'ddd'}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
    )
  })

  it('renders correctly when a component is passed in via a prop', () => {
    testCanvasRender(
      null,
      `
import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
export var App = (props) => {
  return (
    <div
      data-uid={'aaa'}
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      
    >
      Test
      <Thing data-uid={'bbb'} thing={<div data-uid={'ccc'}>test</div>} />
    </div>
  )
}

export var Thing = (props) => {
  return <div data-uid={'ddd'} style={{backgroundColor: 'pink', width: '100%', height: 20}}>{props.thing}</div>
}

export var storyboard = (
  <Storyboard data-uid={'eee'} >
    <Scene
      data-uid={'fff'}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
    )
  })

  it('renders a component using the spread operator', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <View
            {...props}
            data-uid={'aaa'}
          >
            <View style={{position: 'absolute'}} data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '99.9', width: '77.7' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })

  it('renders fine with a valid registerModule call', () => {
    testCanvasRender(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <View
            style={{ position: 'absolute', height: '99.9', width: '77.7' }}
            data-uid={'aaa'}
          >
            <View style={{position: 'absolute'}} data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
    )
  })
})

describe('UiJsxCanvas render multifile projects', () => {
  it('renders a canvas with App imported from a file', () => {
    const printedDom = testCanvasRenderInlineMultifile(
      null,
      `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      import { App } from '/app'

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {
        '/app.js': `
      import * as React from 'react'
      export var App = (props) => {
        return <div data-uid='app-outer-div'>
          <div data-uid='inner-div'>hello</div>
        </div>
      }`,
      },
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/inner-div\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              height: 200px;
              left: 59px;
              width: 200px;
              top: 79px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              data-uid=\\"app-outer-div\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div\\"
            >
              <div
                data-uid=\\"inner-div\\"
                data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/inner-div\\"
              >
                hello
              </div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })
  it('renders a canvas with App and Card imported from files', () => {
    const printedDom = testCanvasRenderInlineMultifile(
      null,
      `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      import { App } from '/app'

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {
        '/app.js': `
      import * as React from 'react'
      import { Card } from '/card'
      export var App = (props) => {
        return <div data-uid='app-outer-div'>
          <Card data-uid='card-instance'>
            <span data-uid='card-content'>hello</span>
          </Card>
        </div>
      }`,
        '/card.js': `
        import * as React from 'react'
        export var Card = (props) => {
          return <div data-uid='card-outer-div'>
            <div data-uid='card-header'>Card</div>
            {props.children}
          </div>
        }`,
      },
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance/card-content\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              height: 200px;
              left: 59px;
              width: 200px;
              top: 79px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              data-uid=\\"app-outer-div\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div\\"
            >
              <div
                data-uid=\\"card-outer-div\\"
                data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance:card-outer-div\\"
              >
                <div
                  data-uid=\\"card-header\\"
                  data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance:card-outer-div/card-header\\"
                >
                  Card
                </div>
                <span
                  data-uid=\\"card-content\\"
                  data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance/card-content\\"
                  >hello
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('renders a canvas with App a class imported as the default import', () => {
    const printedDom = testCanvasRenderInlineMultifile(
      null,
      `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      import App from '/app'

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {
        '/app.js': `
      import * as React from 'react'
      export default class App extends React.Component {
        render() {
          return <div data-uid='app-outer-div'>
            <div data-uid='inner-div'>{this.props.title}</div>
          </div>
        }
      }`,
      },
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              height: 200px;
              left: 59px;
              width: 200px;
              top: 79px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              data-uid=\\"app-outer-div\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity\\"
            >
              <div data-uid=\\"inner-div\\">Hi there!</div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('renders a canvas with App a class imported by name', () => {
    const printedDom = testCanvasRenderInlineMultifile(
      null,
      `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      import { App } from '/app'

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {
        '/app.js': `
      import * as React from 'react'
      export class App extends React.Component {
        render() {
          return <div data-uid='app-outer-div'>
            <div data-uid='inner-div'>{this.props.title}</div>
          </div>
        }
      }`,
      },
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              height: 200px;
              left: 59px;
              width: 200px;
              top: 79px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              data-uid=\\"app-outer-div\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity\\"
            >
              <div data-uid=\\"inner-div\\">Hi there!</div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('Properly renders an element that uses props.children', () => {
    const printedDom = testCanvasRenderInline(
      null,
      `
      import * as React from 'react'
      import { View, Storyboard, Scene } from 'utopia-api'

      export var App = (props) => {
        return (
          <div
            data-uid='outer-div'
          >
            <div
              data-uid='aaa'
              children={<div
                data-uid='bbb'
              />}
            />
            <div
              data-uid='ccc'
              children={[
                <div
                  data-uid='ddd'
                />,
                <div
                  data-uid='eee'
                />
              ]}
            />
          </div>
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
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div style=\\"all: initial\\">
        <div
          id=\\"canvas-container\\"
          data-testid=\\"canvas-container\\"
          style=\\"position: absolute\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:outer-div utopia-storyboard-uid/scene-aaa/app-entity:outer-div/aaa utopia-storyboard-uid/scene-aaa/app-entity:outer-div/aaa/bbb utopia-storyboard-uid/scene-aaa/app-entity:outer-div/ccc\\"
          data-utopia-root-element-path=\\"utopia-storyboard-uid\\"
        >
          <div
            data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa\\"
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
              width: 400px;
              height: 400px;
            \\"
            data-uid=\\"scene-aaa\\"
          >
            <div
              data-uid=\\"outer-div\\"
              data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:outer-div\\"
            >
              <div
                data-uid=\\"aaa\\"
                data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:outer-div/aaa\\"
              >
                <div
                  data-uid=\\"bbb\\"
                  data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:outer-div/aaa/bbb\\"
                ></div>
              </div>
              <div
                data-uid=\\"ccc\\"
                data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:outer-div/ccc\\"
              >
                <div
                  data-uid=\\"ddd~~~1\\"
                  data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:outer-div/ccc/ddd~~~1\\"
                ></div>
                <div
                  data-uid=\\"eee~~~2\\"
                  data-path=\\"utopia-storyboard-uid/scene-aaa/app-entity:outer-div/ccc/eee~~~2\\"
                ></div>
              </div>
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('renders a canvas with App imported from a file 2', () => {
    testCanvasErrorMultifile(
      null,
      `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      import { App } from '/app'

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', height: '100%', width: '100%' }}
                title={'Hi there!'}
              />
            </Scene>
          </Storyboard>
        )
      }
      `,
      {
        '/app.js': `
        import * as React from 'react'
        import { App2 } from '/app2'
        export var App = (props) => {
          return <div data-uid='app-outer-div'>
            <App2 />
          </div>
        }`,
        '/app2.js': `
        import * as React from 'react'
        import { App } from '/app'
        export var App2 = (props) => {
          return <div data-uid='app-outer-div'>
            <App />
          </div>
        }`,
      },
    )
  })
})
