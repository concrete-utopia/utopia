import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { AwkwardFragmentsCode } from '../../core/workers/parser-printer/parser-printer-fragments.test-utils'
import {
  testCanvasRender,
  testCanvasError,
  testCanvasRenderInline,
  testCanvasRenderMultifile,
  testCanvasRenderInlineMultifile,
} from './ui-jsx-canvas.test-utils'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'

describe('UiJsxCanvas render', () => {
  it('renders a canvas defined by a utopia storyboard component', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      
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
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      
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
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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

  it('handles a component that is not imported by throwing a ReferenceError', () => {
    testCanvasError(
      null,
      `/** @jsx jsx */
    import * as React from "react"
    import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
    )
  })
  it('renders a component used in an arbitrary block correctly', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      }
       `,
    )
  })
  it('function component is available from arbitrary block in JSX element', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      }
       `,
    )
  })
  it('function component works inside a map', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      `/** @jsx jsx */
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
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
      `/** @jsx jsx */
      import { jsx } from 'utopia-api'
      import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'
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
      import { jsx, Storyboard, Scene } from 'utopia-api'
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

  it('the utopia jsx pragma (and layout prop) works well', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { Scene, Storyboard, jsx } from 'utopia-api'
      const MyComp = (props) => <div layout={{ left: 15, top: 15, width: 50, height: 50, flex: 15 }}>Utopia</div>
      export var App = (props) => {
        return (<MyComp data-uid={'aaa'}/>)
      }

      function myFactoryFunction(type, props, children) {
        const modifiedProps = {...props, 'data-factory-function-works': "true"}
        return React.createElement(type, modifiedProps, children)
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

  it('the utopia jsx pragma supports emotion CSS prop', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { Scene, Storyboard, jsx } from 'utopia-api'
      const MyComp = (props) => <div layout={{ left: 15, top: 15, width: 50, height: 50, flex: 15 }} css={{ backgroundColor: 'blue' }}>Utopia</div>
      export var App = (props) => {
        return (<MyComp data-uid={'aaa'}/>)
      }

      function myFactoryFunction(type, props, children) {
        const modifiedProps = {...props, 'data-factory-function-works': "true"}
        return React.createElement(type, modifiedProps, children)
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
      `/** @jsx jsx */
      import { Scene, Storyboard, jsx } from 'utopia-api'
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
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      `/** @jsx jsx */
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      "<div
        id=\\"canvas-container\\"
        style=\\"all: initial; position: absolute;\\"
        data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:aaa\\"
      >
        <div
          data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
          data-paths=\\"utopia-storyboard-uid/scene-aaa utopia-storyboard-uid\\"
          style=\\"
            position: absolute;
            background-color: rgba(255, 255, 255, 1);
            box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
            left: 0;
            top: 0;
            width: 400px;
            height: 400px;
          \\"
          data-uid=\\"scene-aaa utopia-storyboard-uid\\"
        >
          <div
            data-uid=\\"ccc-unparsed-no-template-path aaa app-entity\\"
            data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:aaa utopia-storyboard-uid/scene-aaa/app-entity\\"
          >
            hello
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
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
      "<div
        id=\\"canvas-container\\"
        style=\\"all: initial; position: absolute;\\"
        data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:aaa\\"
      >
        <div
          data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
          data-paths=\\"utopia-storyboard-uid/scene-aaa utopia-storyboard-uid\\"
          style=\\"
            position: absolute;
            background-color: rgba(255, 255, 255, 1);
            box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
            left: 0;
            top: 0;
            width: 400px;
            height: 400px;
          \\"
          data-uid=\\"scene-aaa utopia-storyboard-uid\\"
        >
          <div
            id=\\"nasty-div\\"
            data-uid=\\"8cd~~~1 833~~~2 65e~~~1 aaa app-entity\\"
            data-paths=\\"833~~~2/8cd~~~1 833~~~2 65e~~~1 utopia-storyboard-uid/scene-aaa/app-entity:aaa utopia-storyboard-uid/scene-aaa/app-entity\\"
          >
            huhahuha
          </div>
        </div>
      </div>
      "
    `)
  })

  it('antd DatePicker works', () => {
    const printedDom = testCanvasRenderInline(
      null,
      `
    /** @jsx jsx */
import { DatePicker } from 'antd'
import * as React from 'react'
import { Ellipse, Scene, Storyboard, View, jsx } from 'utopia-api'
export var App = (props) => {
  return (
    <View
      data-uid="aaa"
      style={{ ...props.style, backgroundColor: '#FFFFFF' }}
      
    >
      <DatePicker data-uid="antd-date-picker" style={{ width: 123, height: 51, left: 113, top: 395 }} />
    </View>
  )
}
export var storyboard = (
  <Storyboard  data-uid="storyboard">
    <Scene
      data-uid="scene"
      style={{ position: 'absolute', height: 812, left: 0, width: 375, top: 0 }}
    >
      <App
        data-uid='${TestAppUID}'
        style={{ bottom: 0, left: 0, right: 0, top: 0 }}
      />
    </Scene>
  </Storyboard>
)
`,
    )

    expect(printedDom).toMatchInlineSnapshot(`
      "<div
        id=\\"canvas-container\\"
        style=\\"all: initial; position: absolute;\\"
        data-utopia-valid-paths=\\"storyboard storyboard/scene storyboard/scene/app-entity storyboard/scene/app-entity:aaa storyboard/scene/app-entity:aaa/antd-date-picker\\"
      >
        <div
          data-utopia-scene-id=\\"storyboard/scene\\"
          data-paths=\\"storyboard/scene storyboard\\"
          style=\\"
            position: absolute;
            background-color: rgba(255, 255, 255, 1);
            box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
            height: 812px;
            left: 0;
            width: 375px;
            top: 0;
          \\"
          data-uid=\\"scene storyboard\\"
        >
          <div
            style=\\"left: 0; top: 0; right: 0; bottom: 0; background-color: #ffffff;\\"
            data-paths=\\"storyboard/scene/app-entity:aaa storyboard/scene/app-entity\\"
            data-uid=\\"aaa app-entity\\"
          >
            <div
              class=\\"ant-picker\\"
              style=\\"width: 123px; height: 51px; left: 113px; top: 395px;\\"
              data-uid=\\"antd-date-picker\\"
              data-paths=\\"storyboard/scene/app-entity:aaa/antd-date-picker\\"
            >
              <div class=\\"ant-picker-input\\">
                <input
                  readonly=\\"\\"
                  value=\\"\\"
                  placeholder=\\"Select date\\"
                  title=\\"\\"
                  size=\\"12\\"
                  data-uid=\\"antd-date-picker\\"
                  data-paths=\\"storyboard/scene/app-entity:aaa/antd-date-picker\\"
                  autocomplete=\\"off\\"
                /><span class=\\"ant-picker-suffix\\"
                  ><span
                    role=\\"img\\"
                    aria-label=\\"calendar\\"
                    class=\\"anticon anticon-calendar\\"
                    ><svg
                      viewBox=\\"64 64 896 896\\"
                      focusable=\\"false\\"
                      class=\\"\\"
                      data-icon=\\"calendar\\"
                      width=\\"1em\\"
                      height=\\"1em\\"
                      fill=\\"currentColor\\"
                      aria-hidden=\\"true\\"
                    >
                      <path
                        d=\\"M880 184H712v-64c0-4.4-3.6-8-8-8h-56c-4.4 0-8 3.6-8 8v64H384v-64c0-4.4-3.6-8-8-8h-56c-4.4 0-8 3.6-8 8v64H144c-17.7 0-32 14.3-32 32v664c0 17.7 14.3 32 32 32h736c17.7 0 32-14.3 32-32V216c0-17.7-14.3-32-32-32zm-40 656H184V460h656v380zM184 392V256h128v48c0 4.4 3.6 8 8 8h56c4.4 0 8-3.6 8-8v-48h256v48c0 4.4 3.6 8 8 8h56c4.4 0 8-3.6 8-8v-48h128v136H184z\\"
                      ></path></svg></span
                ></span>
              </div>
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
      `/** @jsx jsx */
import * as React from 'react'
import { View, jsx, Scene, Storyboard } from 'utopia-api'
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
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, View, jsx } from 'utopia-api'

export var Cat = (props) => {
  return (
    <React.Fragment>
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
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'

export var A = (props) => {
  if (props.x === 0) {
    return <div>great</div>
  } else {
    return <B data-uid={'bbb-unparsed-no-template-path'} x={props.x - 1} />
  }
}

export var B = (props) => {
  if (props.x === 0) {
    return <div>great</div>
  } else {
    return <A data-uid={'aaa-unparsed-no-template-path'} x={props.x - 1} />
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
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'

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
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'

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
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'

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

  it('renderrs correctly when a component is passed in via a prop', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
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
})

describe('UiJsxCanvas runtime errors', () => {
  it('throws an error!', () => {
    testCanvasError(
      null,
      `import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })
  it('an arbitrary jsx child has correct source map', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })

  it('an arbitrary jsx child has correct source map even if the entire expression is broken', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })

  it('an arbitrary jsx attribute has correct source map', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })

  it('an arbitrary jsx attribute has correct source map even if the entire expression is broken', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })

  it('arbitrary at the top of a component has correct source map', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })

  it('arbitrary at the top of a component has correct source map even if the entire expression is broken', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
    )
  })

  it('React.useEffect at the root fails usefully', () => {
    testCanvasRender(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
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
  })

  it('handles an undefined component gracefully', () => {
    testCanvasError(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

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
    )
  })

  it('handles an non-existent component gracefully', () => {
    testCanvasError(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene, MyCard } from 'utopia-api'

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
    )
  })
})

describe('UiJsxCanvas render multifile projects', () => {
  it('renders a canvas with App imported from a file', () => {
    const printedDom = testCanvasRenderInlineMultifile(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import { App } from 'app.js'

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
        'app.js': `/** @jsx jsx */
      import * as React from 'react'
      import { jsx } from 'utopia-api'
      export var App = (props) => {
        return <div data-uid='app-outer-div'>
          <div data-uid='inner-div'>hello</div>
        </div>
      }`,
      },
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div
        id=\\"canvas-container\\"
        style=\\"all: initial; position: absolute;\\"
        data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/inner-div\\"
      >
        <div
          data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
          data-paths=\\"utopia-storyboard-uid/scene-aaa utopia-storyboard-uid\\"
          style=\\"
            position: absolute;
            background-color: rgba(255, 255, 255, 1);
            box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
            height: 200px;
            left: 59px;
            width: 200px;
            top: 79px;
          \\"
          data-uid=\\"scene-aaa utopia-storyboard-uid\\"
        >
          <div
            data-uid=\\"app-outer-div app-entity\\"
            data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div utopia-storyboard-uid/scene-aaa/app-entity\\"
          >
            <div
              data-uid=\\"inner-div\\"
              data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/inner-div\\"
            >
              hello
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
      `/** @jsx jsx */
      import * as React from 'react'
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import { App } from 'app.js'

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
        'app.js': `/** @jsx jsx */
      import * as React from 'react'
      import { jsx } from 'utopia-api'
      import { Card } from 'card.js'
      export var App = (props) => {
        return <div data-uid='app-outer-div'>
          <Card data-uid='card-instance'>
            <span data-uid='card-content'>hello</span>
          </Card>
        </div>
      }`,
        'card.js': `/** @jsx jsx */
        import * as React from 'react'
        import { jsx } from 'utopia-api'
        export var Card = (props) => {
          return <div data-uid='card-outer-div'>
            <div data-uid='card-header'>Card</div>
            {props.children}
          </div>
        }`,
      },
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div
        id=\\"canvas-container\\"
        style=\\"all: initial; position: absolute;\\"
        data-utopia-valid-paths=\\"utopia-storyboard-uid utopia-storyboard-uid/scene-aaa utopia-storyboard-uid/scene-aaa/app-entity utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance/card-content\\"
      >
        <div
          data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
          data-paths=\\"utopia-storyboard-uid/scene-aaa utopia-storyboard-uid\\"
          style=\\"
            position: absolute;
            background-color: rgba(255, 255, 255, 1);
            box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
            height: 200px;
            left: 59px;
            width: 200px;
            top: 79px;
          \\"
          data-uid=\\"scene-aaa utopia-storyboard-uid\\"
        >
          <div
            data-uid=\\"app-outer-div app-entity\\"
            data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div utopia-storyboard-uid/scene-aaa/app-entity\\"
          >
            <div
              data-uid=\\"card-outer-div card-instance\\"
              data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance:card-outer-div utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance\\"
            >
              <div
                data-uid=\\"card-header\\"
                data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance:card-outer-div/card-header\\"
              >
                Card
              </div>
              <span
                data-uid=\\"card-content\\"
                data-paths=\\"utopia-storyboard-uid/scene-aaa/app-entity:app-outer-div/card-instance/card-content\\"
                >hello</span
              >
            </div>
          </div>
        </div>
      </div>
      "
    `)
  })
})
