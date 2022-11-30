import React from 'react'
import { applyUIDMonkeyPatch } from './canvas-react-utils'
applyUIDMonkeyPatch()
import * as PropTypes from 'prop-types'
import * as ReactDOMServer from 'react-dom/server'
import * as Prettier from 'prettier'
import { DatePicker } from 'antd'
import { BakedInStoryboardUID } from '../core/model/scene-utils'
import {
  View as UtopiaView,
  Scene as UtopiaScene,
  Storyboard as UtopiaStoryboard,
} from 'utopia-api'
const { useRef } = React

function renderToFormattedString(element: React.ReactElement) {
  const renderResult = ReactDOMServer.renderToStaticMarkup(element)
  const formattedResult = Prettier.format(renderResult, { parser: 'html' })
  return formattedResult
}

const Storyboard = (props: React.PropsWithChildren<any>) => {
  return <React.Fragment key='monkey-oh-monkey-please-leave-me-be'>{props.children}</React.Fragment>
}

const Scene = (props: { 'data-uid': string; style?: any; children?: any }) => {
  const { 'data-uid': uid, style, children } = props
  return (
    <div data-uid={uid} style={style}>
      {children}
    </div>
  )
}

const SceneComponent = () => {
  return <div data-uid='cica'>Hello!</div>
}

describe('Monkey Function', () => {
  it('works for simple class components', () => {
    class TestClass extends React.Component {
      render() {
        return (
          <div data-uid='cica'>
            <div data-uid='kutya'>Hello!</div>
            <div data-uid='majom'>Hello!</div>
          </div>
        )
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\" data-path=\\"test1:cica\\">
        <div data-uid=\\"kutya\\" data-path=\\"test1:cica/kutya\\">Hello!</div>
        <div data-uid=\\"majom\\" data-path=\\"test1:cica/majom\\">Hello!</div>
      </div>
      "
    `)
  })

  it('class components have a working context', () => {
    const MyContext = React.createContext({ value: 'wrong!' })
    class TestClass extends React.Component {
      context: any
      render() {
        return <div data-uid='inner-div'>{this.context.value}</div>
      }
    }
    TestClass.contextType = MyContext

    expect(
      renderToFormattedString(
        <MyContext.Provider value={{ value: 'hello!' }} data-uid='provider'>
          <TestClass data-uid={'test-class'} />
        </MyContext.Provider>,
      ),
    ).toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-div\\" data-path=\\"provider/test-class:inner-div\\">hello!</div>
      "
    `)
  })

  it('class components have a working context, second variant', () => {
    const MyContext = React.createContext({ value: 'wrong!' })
    class TestClass extends React.Component {
      context: any
      render() {
        return <div data-uid='inner-div'>{this.context.value}</div>
      }
    }
    TestClass.contextType = MyContext

    const Renderer = () => {
      return (
        <MyContext.Provider value={{ value: 'hello!' }} data-uid='provider'>
          <TestClass data-uid='test-class' />
        </MyContext.Provider>
      )
    }

    expect(renderToFormattedString(<Renderer data-uid={'renderer'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-div\\" data-path=\\"renderer:provider/test-class:inner-div\\">
        hello!
      </div>
      "
    `)
  })

  it('works for elements imported from utopia-api', () => {
    const TestComponent = () => {
      return (
        <UtopiaView data-uid='component-root'>
          <UtopiaView data-uid='kutya'>Hello!</UtopiaView>
          <UtopiaView data-uid='majom'>Hello!</UtopiaView>
        </UtopiaView>
      )
    }

    const TestStoryboard: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return (
        <UtopiaStoryboard data-uid='sb'>
          <UtopiaScene data-uid='scene'>
            <TestComponent data-uid='component-instance' />
          </UtopiaScene>
        </UtopiaStoryboard>
      )
    }

    expect(renderToFormattedString(<TestStoryboard data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"scene\\" data-path=\\"test1:sb/scene\\">
        <div
          data-path=\\"test1:sb/scene/component-instance:component-root\\"
          data-uid=\\"component-root\\"
        >
          <div
            data-path=\\"test1:sb/scene/component-instance:component-root/kutya\\"
            data-uid=\\"kutya\\"
          >
            Hello!
          </div>
          <div
            data-path=\\"test1:sb/scene/component-instance:component-root/majom\\"
            data-uid=\\"majom\\"
          >
            Hello!
          </div>
        </div>
      </div>
      "
    `)
  })

  it('works for simple function components', () => {
    const TestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return (
        <div data-uid='test1'>
          <div data-uid='kutya'>Hello!</div>
          <div data-uid='majom'>Hello!</div>
        </div>
      )
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\" data-path=\\"test1\\">
        <div data-uid=\\"kutya\\" data-path=\\"test1/kutya\\">Hello!</div>
        <div data-uid=\\"majom\\" data-path=\\"test1/majom\\">Hello!</div>
      </div>
      "
    `)
  })

  it('function components have working hooks', () => {
    const TestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      const [value, setValue] = React.useState('Hello!')
      return (
        <div>
          <div>{value}</div>
        </div>
      )
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\" data-path=\\"test1\\"><div>Hello!</div></div>
      "
    `)
  })

  it('works for function components', () => {
    const OtherTestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (
      props,
    ) => {
      return <div data-uid='root-div'>Hello!</div>
    }

    const TestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return <OtherTestComponent data-uid={'cica'} />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"root-div\\" data-path=\\"test1:cica:root-div\\">Hello!</div>
      "
    `)
  })

  it('works for function components that have uid returning function components', () => {
    const MyComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return <div>Hello!</div>
    }
    const OtherTestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (
      props,
    ) => {
      return <MyComponent />
    }

    const TestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return <OtherTestComponent data-uid={'cica'} />
    }

    expect(renderToFormattedString(<TestComponent />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\" data-path=\\"cica\\">Hello!</div>
      "
    `)
  })

  it('works for function components that have uid returning function components wrapped in divs', () => {
    const MyComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return <div>Hello!</div>
    }
    const OtherTestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (
      props,
    ) => {
      return (
        <div>
          <MyComponent />
        </div>
      )
    }

    const TestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return <OtherTestComponent data-uid={'cica'} />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\" data-path=\\"test1:cica\\"><div>Hello!</div></div>
      "
    `)
  })

  it('works for function components that have no uid returning function components', () => {
    const OtherTestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (
      props,
    ) => {
      return <div>Hello!</div>
    }

    const TestComponent: React.FunctionComponent<React.PropsWithChildren<unknown>> = (props) => {
      return <OtherTestComponent />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\" data-path=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('works for class components returning class components', () => {
    class OtherTestClass extends React.Component {
      render() {
        return <div data-uid='root-div'>Hello!</div>
      }
    }

    class TestClass extends React.Component {
      render() {
        return <OtherTestClass data-uid='test-class' />
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"root-div\\" data-path=\\"test1:test-class:root-div\\">Hello!</div>
      "
    `)
  })

  it('works with a silly render prop', () => {
    const CallRenderPropChild: React.FunctionComponent<any> = (props) => {
      return props.children('Hello!')
    }

    const Cica = (props: any) => {
      return (
        <CallRenderPropChild data-uid='wrapper-component'>
          {(data: string) => {
            return <div data-uid='root-div'>{data}</div>
          }}
        </CallRenderPropChild>
      )
    }

    expect(renderToFormattedString(<Cica data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"root-div\\" data-path=\\"test1:wrapper-component:root-div\\">
        Hello!
      </div>
      "
    `)
  })

  it('works with a render prop with a class component if there is a uid', () => {
    const CallRenderPropChild: React.FunctionComponent<any> = (props) => {
      return props.children('Hello!')
    }

    class TestClass extends React.Component {
      renderComponent(data: string) {
        return <div data-uid='root-div'>{data}</div>
      }

      render() {
        return <CallRenderPropChild data-uid='cica'>{this.renderComponent}</CallRenderPropChild>
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"root-div\\" data-path=\\"test1:cica:root-div\\">Hello!</div>
      "
    `)
  })

  it('works with a render prop with a class component if there is NO uid', () => {
    const CallRenderPropChild: React.FunctionComponent<any> = (props) => {
      return props.children('Hello!')
    }

    class TestClass extends React.Component {
      renderComponent(data: string) {
        return <div>{data}</div>
      }

      render() {
        return <CallRenderPropChild>{this.renderComponent}</CallRenderPropChild>
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-path=\\"test1\\" data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('Fragments work if theres a uid', () => {
    const Component = () => {
      return (
        <>
          <div>Hello!</div>
        </>
      )
    }

    expect(renderToFormattedString(<Component data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-path=\\"test1\\" data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('Fragments work if theres a uid 2', () => {
    const Component = () => {
      return (
        <React.Fragment data-uid='fragment'>
          <div data-uid='root-div'>Hello!</div>
        </React.Fragment>
      )
    }

    expect(renderToFormattedString(<Component data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"root-div\\" data-path=\\"test1:fragment/root-div\\">Hello!</div>
      "
    `)
  })

  it('Fragments work if in absence of uid too', () => {
    const Component = () => {
      return (
        <>
          <div>Hello!</div>
        </>
      )
    }

    expect(renderToFormattedString(<Component />)).toMatchInlineSnapshot(`
      "<div>Hello!</div>
      "
    `)
  })

  it('We discard the uid of a fragment if appropriate', () => {
    const Component = () => {
      return (
        <div data-uid='cica'>
          <React.Fragment data-uid='kutya'>Hello!</React.Fragment>
        </div>
      )
    }

    expect(renderToFormattedString(<Component />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\">Hello!</div>
      "
    `)
  })

  it('If a fragment has an array of children, we put the uid on all of the children, for lack of a better idea', () => {
    const Component = () => {
      return (
        <div data-uid='cica'>
          <React.Fragment data-uid='kutya'>
            <div data-uid='hello-div'>Hello</div>
            <div data-uid='world-div'>world!</div>
          </React.Fragment>
        </div>
      )
    }

    expect(renderToFormattedString(<Component data-uid='component' />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\" data-path=\\"component:cica\\">
        <div data-uid=\\"hello-div\\" data-path=\\"component:cica/kutya/hello-div\\">
          Hello
        </div>
        <div data-uid=\\"world-div\\" data-path=\\"component:cica/kutya/world-div\\">
          world!
        </div>
      </div>
      "
    `)
  })

  it('The Storyboard is a special fragment that we leave alone', () => {
    expect(
      renderToFormattedString(
        <Storyboard data-uid='ignore'>
          <Scene data-uid='scene'>
            <SceneComponent data-uid='scene-component' />
          </Scene>
        </Storyboard>,
      ),
    ).toMatchInlineSnapshot(`
      "<div data-uid=\\"scene\\" data-path=\\"ignore/scene\\">
        <div data-uid=\\"cica\\" data-path=\\"ignore/scene/scene-component:cica\\">
          Hello!
        </div>
      </div>
      "
    `)
  })

  it('Context providers work', () => {
    const MyContext = React.createContext('wrong!')
    const Component = () => {
      const context = React.useContext(MyContext)
      return <div>{context}</div>
    }

    const WrappedComponent = () => {
      return (
        <MyContext.Provider value='Hello!'>
          <Component />
        </MyContext.Provider>
      )
    }

    expect(renderToFormattedString(<WrappedComponent data-uid='cica' />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\" data-path=\\"cica\\">Hello!</div>
      "
    `)
  })

  it('antd-like weird component renders', () => {
    const MyContext = React.createContext('wrong!')
    class NotAnotherClass extends React.Component<{ locale: string; size: string }> {
      render() {
        return (
          <div>
            <div>{this.props.locale}</div> {this.props.size}
          </div>
        )
      }
    }

    class RenderPropsFunctionChild extends React.Component<any> {
      render() {
        return (this.props.children as any)('huha')
      }
    }

    function getPicker() {
      class Picker extends React.Component {
        pickerRef = React.createRef()

        renderPicker = (locale: any) => {
          return (
            <MyContext.Consumer>
              {(size: any) => {
                return <NotAnotherClass locale={locale} size={size} />
              }}
            </MyContext.Consumer>
          )
        }

        render() {
          return <RenderPropsFunctionChild>{this.renderPicker}</RenderPropsFunctionChild>
        }
      }

      return Picker
    }

    const Thing = getPicker()

    var App = (props: any) => {
      return (
        <MyContext.Provider value='huha!'>
          <Thing data-uid={'aaa'} />
        </MyContext.Provider>
      )
    }

    expect(renderToFormattedString(<App />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"aaa\\" data-path=\\"aaa\\">
        <div>huha</div>
        huha!
      </div>
      "
    `)
  })

  it('mapped children works', () => {
    const App = () => {
      return (
        <div data-uid='zzz'>
          {[1, 2, 3].map((n) => {
            return (
              <div data-uid='aaa' key={n}>
                {[4, 5, 6].map((m) => {
                  return (
                    <div key={m} data-uid='bbb'>
                      {n * m}
                    </div>
                  )
                })}
              </div>
            )
          })}
        </div>
      )
    }
    expect(renderToFormattedString(<App data-uid='cica' />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"zzz\\" data-path=\\"cica:zzz\\">
        <div data-uid=\\"aaa\\" data-path=\\"cica:zzz/aaa\\">
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">4</div>
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">5</div>
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">6</div>
        </div>
        <div data-uid=\\"aaa\\" data-path=\\"cica:zzz/aaa\\">
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">8</div>
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">10</div>
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">12</div>
        </div>
        <div data-uid=\\"aaa\\" data-path=\\"cica:zzz/aaa\\">
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">12</div>
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">15</div>
          <div data-uid=\\"bbb\\" data-path=\\"cica:zzz/aaa/bbb\\">18</div>
        </div>
      </div>
      "
    `)
  })

  it('this weird guy works', () => {
    const App = () => {
      return <div data-uid='zzz'>{[' ']}hello!</div>
    }
    expect(renderToFormattedString(<App data-uid='cica' />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"zzz\\" data-path=\\"cica:zzz\\">hello!</div>
      "
    `)
  })

  it('this doesnt throw', () => {
    const Thing = (props: any) => <div data-uid='ccc'>Hello {props.name}!</div>
    const Thang = (props: any) => <div data-uid='ddd'>Hello {props.name}!</div>

    var App = (props: any) => {
      return (
        <div data-uid='zzz'>
          <Thing data-uid='aaa' name='World!' />
          <Thang data-uid='bbb' name='Dolly!' />
        </div>
      )
    }
    var storyboard = (
      <Storyboard data-uid={BakedInStoryboardUID}>
        <Scene style={{ left: 0, top: 0, width: 400, height: 400 }} data-uid={'scene-aaa'}>
          <App data-uid='app' style={{ bottom: 0, left: 0, right: 0, top: 0 }} />
        </Scene>
      </Storyboard>
    )
    expect(renderToFormattedString(storyboard)).toMatchInlineSnapshot(`
      "<div
        data-uid=\\"scene-aaa\\"
        style=\\"left: 0; top: 0; width: 400px; height: 400px\\"
        data-path=\\"utopia-storyboard-uid/scene-aaa\\"
      >
        <div data-uid=\\"zzz\\" data-path=\\"utopia-storyboard-uid/scene-aaa/app:zzz\\">
          <div
            data-uid=\\"ccc\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa/app:zzz/aaa:ccc\\"
          >
            Hello World!!
          </div>
          <div
            data-uid=\\"ddd\\"
            data-path=\\"utopia-storyboard-uid/scene-aaa/app:zzz/bbb:ddd\\"
          >
            Hello Dolly!!
          </div>
        </div>
      </div>
      "
    `)
  })

  it('function components have a working context', () => {
    const TestFunction = (props: any, context: any) => {
      return <div data-uid='bbb'>{context.value}</div>
    }
    TestFunction.contextTypes = {
      value: PropTypes.string,
    }
    TestFunction.childContextTypes = {
      value: PropTypes.string,
    }

    class App extends React.Component {
      getChildContext() {
        return { value: 'hello!' }
      }
      render() {
        return (
          <div data-uid='aaa'>
            <TestFunction />
          </div>
        )
      }
    }
    ;(App as any).childContextTypes = {
      value: PropTypes.string,
    }

    expect(renderToFormattedString(<App />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"aaa\\"><div data-uid=\\"bbb\\">hello!</div></div>
      "
    `)
  })

  it('Does not add a uid to a non-html intrinsic element', () => {
    const Component = () => {
      return <mesh />
    }

    expect(renderToFormattedString(<Component data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<mesh></mesh>
      "
    `)
  })

  it('Does not add a uid to a non-html intrinsic element wrapped in a fragment', () => {
    const Component = () => {
      return (
        <>
          <mesh />
        </>
      )
    }

    expect(renderToFormattedString(<Component data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<mesh></mesh>
      "
    `)
  })

  it('builds the correct paths for function components', () => {
    const Red = () => {
      return <div data-uid='red-root' />
    }

    const Blue = (props: any) => {
      return <div data-uid='blue-root'>{props.children}</div>
    }

    var InnerComponent = () => {
      const red = <Red data-uid='red' />

      return (
        <Blue data-uid='inner-parent'>
          <Blue data-uid='inner-child'>
            <Blue data-uid='blue' />
            {red}
          </Blue>
        </Blue>
      )
    }

    var OuterComponent = () => {
      return <InnerComponent data-uid='outer' />
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"blue-root\\" data-path=\\"component:outer:inner-parent:blue-root\\">
        <div
          data-uid=\\"blue-root\\"
          data-path=\\"component:outer:inner-parent/inner-child:blue-root\\"
        >
          <div
            data-uid=\\"blue-root\\"
            data-path=\\"component:outer:inner-parent/inner-child/blue:blue-root\\"
          ></div>
          <div
            data-uid=\\"red-root\\"
            data-path=\\"component:outer:inner-parent/inner-child/red:red-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for function components with children', () => {
    const Red = () => {
      return <div data-uid='red-root' />
    }

    const Blue = (props: any) => {
      return (
        <div data-uid='blue-root'>
          <div data-uid='blue-child' />
          {props.children}
        </div>
      )
    }

    var Component = () => {
      return (
        <Blue data-uid='inner-parent'>
          <Red data-uid='red' />
        </Blue>
      )
    }

    expect(renderToFormattedString(<Component data-uid={'component'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"blue-root\\" data-path=\\"component:inner-parent:blue-root\\">
        <div
          data-uid=\\"blue-child\\"
          data-path=\\"component:inner-parent:blue-root/blue-child\\"
        ></div>
        <div
          data-uid=\\"red-root\\"
          data-path=\\"component:inner-parent/red:red-root\\"
        ></div>
      </div>
      "
    `)
  })

  it('builds the correct paths for class components', () => {
    class Red extends React.Component {
      render() {
        return <div data-uid='red-root' />
      }
    }

    class Blue extends React.Component<React.PropsWithChildren<{ 'data-uid': string }>> {
      render() {
        return <div data-uid='blue-root'>{this.props.children}</div>
      }
    }

    class InnerComponent extends React.Component {
      render() {
        const red = <Red data-uid='red' />

        return (
          <Blue data-uid='inner-parent'>
            <Blue data-uid='inner-child'>
              <Blue data-uid='blue' />
              {red}
            </Blue>
          </Blue>
        )
      }
    }

    class OuterComponent extends React.Component {
      render() {
        return <InnerComponent data-uid='outer' />
      }
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"blue-root\\" data-path=\\"component:outer:inner-parent:blue-root\\">
        <div
          data-uid=\\"blue-root\\"
          data-path=\\"component:outer:inner-parent/inner-child:blue-root\\"
        >
          <div
            data-uid=\\"blue-root\\"
            data-path=\\"component:outer:inner-parent/inner-child/blue:blue-root\\"
          ></div>
          <div
            data-uid=\\"red-root\\"
            data-path=\\"component:outer:inner-parent/inner-child/red:red-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for intrinsic components', () => {
    const Red = () => {
      return <div data-uid='red-root' />
    }

    const OuterComponent = () => {
      const red = <Red data-uid='red' />

      return (
        <div data-uid='outer'>
          <div data-uid='inner-parent'>
            <div data-uid='inner-child'>
              <div data-uid='blue' />
              {red}
            </div>
          </div>
        </div>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"outer\\" data-path=\\"component:outer\\">
        <div data-uid=\\"inner-parent\\" data-path=\\"component:outer/inner-parent\\">
          <div
            data-uid=\\"inner-child\\"
            data-path=\\"component:outer/inner-parent/inner-child\\"
          >
            <div
              data-uid=\\"blue\\"
              data-path=\\"component:outer/inner-parent/inner-child/blue\\"
            ></div>
            <div
              data-uid=\\"red-root\\"
              data-path=\\"component:outer/inner-parent/inner-child/red:red-root\\"
            ></div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for fragments at the root', () => {
    const Red = () => {
      return <div data-uid='red-root' />
    }

    const OuterComponent = () => {
      const red = <Red data-uid='red' />

      return (
        <>
          <div data-uid='inner-parent'>
            <>
              <div data-uid='inner-child'>
                <div data-uid='blue' />
                {red}
              </div>
            </>
          </div>
        </>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div data-uid=\\"inner-child\\" data-path=\\"component:inner-parent/inner-child\\">
          <div
            data-uid=\\"blue\\"
            data-path=\\"component:inner-parent/inner-child/blue\\"
          ></div>
          <div
            data-uid=\\"red-root\\"
            data-path=\\"component:inner-parent/inner-child/red:red-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for fragments below the root', () => {
    const Red = () => {
      return <div data-uid='red-root' />
    }

    const OuterComponent = () => {
      const red = <Red data-uid='red' />

      return (
        <div data-uid='root'>
          <>
            <div data-uid='inner-parent'>
              <>
                <div data-uid='inner-child'>
                  <div data-uid='blue' />
                  {red}
                </div>
              </>
            </div>
          </>
        </div>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"root\\" data-path=\\"component:root\\">
        <div data-uid=\\"inner-parent\\" data-path=\\"component:root/inner-parent\\">
          <div
            data-uid=\\"inner-child\\"
            data-path=\\"component:root/inner-parent/inner-child\\"
          >
            <div
              data-uid=\\"blue\\"
              data-path=\\"component:root/inner-parent/inner-child/blue\\"
            ></div>
            <div
              data-uid=\\"red-root\\"
              data-path=\\"component:root/inner-parent/inner-child/red:red-root\\"
            ></div>
          </div>
        </div>
      </div>
      "
    `)
  })

  it('fragment ordering should not affect the printed paths', () => {
    const FragmentAtRoot = () => {
      return (
        <>
          <div data-uid='inner-parent'>
            <div data-uid='inner-child' />
          </div>
        </>
      )
    }

    const FragmentBelowRoot = () => {
      return (
        <div data-uid='inner-parent'>
          <>
            <div data-uid='inner-child' />
          </>
        </div>
      )
    }

    const renderedFragmentAtRoot = renderToFormattedString(
      <FragmentAtRoot data-uid={'component'} />,
    )
    const renderedFragmentBelowRoot = renderToFormattedString(
      <FragmentBelowRoot data-uid={'component'} />,
    )

    expect(renderedFragmentAtRoot).toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div
          data-uid=\\"inner-child\\"
          data-path=\\"component:inner-parent/inner-child\\"
        ></div>
      </div>
      "
    `)

    expect(renderedFragmentAtRoot).toEqual(renderedFragmentBelowRoot)
  })

  it('nested fragments work', () => {
    const FragmentAtRoot = () => {
      return (
        <>
          <>
            <div data-uid='inner-parent'>
              <div data-uid='inner-child' />
            </div>
          </>
        </>
      )
    }

    const FragmentBelowRoot = () => {
      return (
        <div data-uid='inner-parent'>
          <>
            <>
              <div data-uid='inner-child' />
            </>
          </>
        </div>
      )
    }

    const renderedFragmentAtRoot = renderToFormattedString(
      <FragmentAtRoot data-uid={'component'} />,
    )
    const renderedFragmentBelowRoot = renderToFormattedString(
      <FragmentBelowRoot data-uid={'component'} />,
    )

    expect(renderedFragmentAtRoot).toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div
          data-uid=\\"inner-child\\"
          data-path=\\"component:inner-parent/inner-child\\"
        ></div>
      </div>
      "
    `)

    expect(renderedFragmentAtRoot).toEqual(renderedFragmentBelowRoot)
  })

  it('fragment correctly handles path on child with no uid', () => {
    const FragmentAtRoot = () => {
      return (
        <>
          <div />
        </>
      )
    }

    const FragmentBelowRoot = () => {
      return (
        <div data-uid='inner-parent'>
          <>
            <div />
          </>
        </div>
      )
    }

    const renderedFragmentAtRoot = renderToFormattedString(
      <FragmentAtRoot data-uid={'component'} />,
    )

    expect(renderedFragmentAtRoot).toMatchInlineSnapshot(`
      "<div data-path=\\"component\\" data-uid=\\"component\\"></div>
      "
    `)

    const renderedFragmentBelowRoot = renderToFormattedString(
      <FragmentBelowRoot data-uid={'component'} />,
    )

    expect(renderedFragmentBelowRoot).toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div></div>
      </div>
      "
    `)
  })

  it('builds the correct paths for Exotic-type with-memo components', () => {
    const Red = React.memo(() => <div data-uid='red-root' />)

    const OuterComponent = () => {
      const red = <Red data-uid='red' />

      return (
        <div data-uid='inner-parent'>
          <div data-uid='inner-child'>
            <div data-uid='blue' />
            {red}
          </div>
        </div>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div data-uid=\\"inner-child\\" data-path=\\"component:inner-parent/inner-child\\">
          <div
            data-uid=\\"blue\\"
            data-path=\\"component:inner-parent/inner-child/blue\\"
          ></div>
          <div
            data-uid=\\"red-root\\"
            data-path=\\"component:inner-parent/inner-child/red:red-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for Exotic-type with-memo components 2', () => {
    const Red = React.memo(() => <div data-uid='red-root' />)

    const OuterComponent = () => {
      return (
        <div data-uid='inner-parent'>
          <div data-uid='inner-child'>
            <div data-uid='blue' />
            <Red data-uid='red' />
          </div>
        </div>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div data-uid=\\"inner-child\\" data-path=\\"component:inner-parent/inner-child\\">
          <div
            data-uid=\\"blue\\"
            data-path=\\"component:inner-parent/inner-child/blue\\"
          ></div>
          <div
            data-uid=\\"red-root\\"
            data-path=\\"component:inner-parent/inner-child/red:red-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for Exotic-type with-forwardRef components', () => {
    const Red = React.forwardRef((props, test) => {
      return <div data-uid='red-root' ref={test as any} />
    })

    const OuterComponent = () => {
      const test = useRef(null)
      const red = <Red data-uid='red' ref={test} />

      return (
        <div data-uid='inner-parent'>
          <div data-uid='inner-child'>
            <div data-uid='blue' />
            {red}
          </div>
        </div>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"inner-parent\\" data-path=\\"component:inner-parent\\">
        <div data-uid=\\"inner-child\\" data-path=\\"component:inner-parent/inner-child\\">
          <div
            data-uid=\\"blue\\"
            data-path=\\"component:inner-parent/inner-child/blue\\"
          ></div>
          <div
            data-uid=\\"red-root\\"
            data-path=\\"component:inner-parent/inner-child/red:red-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('builds the correct paths for forwardRef components', () => {
    const Blue = React.forwardRef((props: any, test) => {
      return (
        <div data-uid='blue-root' ref={test as any}>
          {props.children}
        </div>
      )
    })

    const OuterComponent = () => {
      const parentRef = useRef(null)
      const childRef = useRef(null)
      const blueRef = useRef(null)

      return (
        <Blue data-uid='inner-parent' ref={parentRef}>
          <Blue data-uid='inner-child' ref={childRef}>
            <Blue data-uid='blue' ref={blueRef} />
          </Blue>
        </Blue>
      )
    }

    expect(renderToFormattedString(<OuterComponent data-uid={'component'} />))
      .toMatchInlineSnapshot(`
      "<div data-uid=\\"blue-root\\" data-path=\\"component:inner-parent:blue-root\\">
        <div
          data-uid=\\"blue-root\\"
          data-path=\\"component:inner-parent/inner-child:blue-root\\"
        >
          <div
            data-uid=\\"blue-root\\"
            data-path=\\"component:inner-parent/inner-child/blue:blue-root\\"
          ></div>
        </div>
      </div>
      "
    `)
  })
})
