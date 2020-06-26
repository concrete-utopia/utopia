import * as React from 'react'
import { applyUIDMonkeyPatch } from './canvas-react-utils'
applyUIDMonkeyPatch()
import * as ReactDOMServer from 'react-dom/server'
import * as Prettier from 'prettier'
import { DatePicker } from 'antd'

function renderToFormattedString(element: React.ReactElement) {
  const renderResult = ReactDOMServer.renderToStaticMarkup(element)
  const formattedResult = Prettier.format(renderResult, { parser: 'html' })
  return formattedResult
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
      "<div data-uid=\\"test1\\">
        <div data-uid=\\"kutya\\">Hello!</div>
        <div data-uid=\\"majom\\">Hello!</div>
      </div>
      "
    `)
  })

  it('class components have a working context', () => {
    const MyContext = React.createContext({ value: 'wrong!' })
    class TestClass extends React.Component {
      render() {
        return <div>{this.context.value}</div>
      }
    }
    TestClass.contextType = MyContext

    expect(
      renderToFormattedString(
        <MyContext.Provider value={{ value: 'hello!' }}>
          <TestClass data-uid={'test1'} />
        </MyContext.Provider>,
      ),
    ).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">hello!</div>
      "
    `)
  })

  it('class components have a working context, second variant', () => {
    const MyContext = React.createContext({ value: 'wrong!' })
    class TestClass extends React.Component {
      render() {
        return <div>{this.context.value}</div>
      }
    }
    TestClass.contextType = MyContext

    expect(
      renderToFormattedString(
        <MyContext.Provider value={{ value: 'hello!' }} data-uid={'test1'}>
          <TestClass />
        </MyContext.Provider>,
      ),
    ).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">hello!</div>
      "
    `)
  })

  it('class components have a working context, third variant', () => {
    const MyContext = React.createContext({ value: 'wrong!' })
    class TestClass extends React.Component {
      render() {
        return <div>{this.context.value}</div>
      }
    }
    TestClass.contextType = MyContext

    const Renderer = () => {
      return (
        <MyContext.Provider value={{ value: 'hello!' }}>
          <TestClass />
        </MyContext.Provider>
      )
    }

    expect(renderToFormattedString(<Renderer data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">hello!</div>
      "
    `)
  })

  it('works for simple function components', () => {
    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return (
        <div data-uid='test1'>
          <div data-uid='kutya'>Hello!</div>
          <div data-uid='majom'>Hello!</div>
        </div>
      )
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">
        <div data-uid=\\"kutya\\">Hello!</div>
        <div data-uid=\\"majom\\">Hello!</div>
      </div>
      "
    `)
  })

  it('function components have working hooks', () => {
    const TestComponent: React.FunctionComponent<{}> = (props) => {
      const [value, setValue] = React.useState('Hello!')
      return (
        <div>
          <div>{value}</div>
        </div>
      )
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\"><div>Hello!</div></div>
      "
    `)
  })

  it('works for function components', () => {
    const OtherTestComponent: React.FunctionComponent<{}> = (props) => {
      return <div>Hello!</div>
    }

    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return <OtherTestComponent data-uid={'cica'} />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('works for function components that have uid returning function components', () => {
    const MyComponent: React.FunctionComponent<{}> = (props) => {
      return <div>Hello!</div>
    }
    const OtherTestComponent: React.FunctionComponent<{}> = (props) => {
      return <MyComponent />
    }

    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return <OtherTestComponent data-uid={'cica'} />
    }

    expect(renderToFormattedString(<TestComponent />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\">Hello!</div>
      "
    `)
  })

  it('works for function components that have uid returning function components wrapped in divs', () => {
    const MyComponent: React.FunctionComponent<{}> = (props) => {
      return <div>Hello!</div>
    }
    const OtherTestComponent: React.FunctionComponent<{}> = (props) => {
      return (
        <div>
          <MyComponent />
        </div>
      )
    }

    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return <OtherTestComponent data-uid={'cica'} />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\"><div>Hello!</div></div>
      "
    `)
  })

  it('works for function components that have no uid returning function components', () => {
    const OtherTestComponent: React.FunctionComponent<{}> = (props) => {
      return <div>Hello!</div>
    }

    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return <OtherTestComponent />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('works for class components returning class components', () => {
    class OtherTestClass extends React.Component {
      render() {
        return <div>Hello!</div>
      }
    }

    class TestClass extends React.Component {
      render() {
        return <OtherTestClass />
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('works with a silly render prop', () => {
    const CallRenderPropChild: React.FunctionComponent<{}> = (props) => {
      return (props.children as any)('Hello!')
    }

    const Cica = (props: any) => {
      return (
        <CallRenderPropChild>
          {(data: string) => {
            return <div>{data}</div>
          }}
        </CallRenderPropChild>
      )
    }

    expect(renderToFormattedString(<Cica data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('works with a render prop with a class component if there is a uid', () => {
    const CallRenderPropChild: React.FunctionComponent<{}> = (props) => {
      return (props.children as any)('Hello!')
    }

    class TestClass extends React.Component {
      renderComponent(data: string) {
        return <div>{data}</div>
      }

      render() {
        return <CallRenderPropChild data-uid='cica'>{this.renderComponent}</CallRenderPropChild>
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('works with a render prop with a class component if there is NO uid', () => {
    const CallRenderPropChild: React.FunctionComponent<{}> = (props) => {
      return (props.children as any)('Hello!')
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
      "<div data-uid=\\"test1\\">Hello!</div>
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
      "<div data-uid=\\"test1\\">Hello!</div>
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

  it('The Storyboard is a special fragment that we leave alone', () => {
    const Component = () => {
      return <div data-uid='cica'>Hello!</div>
    }

    const SceneRoot = (props: { toRender: any; 'data-uid': string }) => {
      const { toRender, 'data-uid': uid, ...restProps } = props
      const result = React.createElement(toRender, restProps)
      return { ...result, monkeyEscapeHatch: true }
    }

    const Storyboard = (props: React.PropsWithChildren<any>) => {
      return (
        <React.Fragment key='monkey-oh-monkey-please-leave-me-be'>{props.children}</React.Fragment>
      )
    }

    expect(
      renderToFormattedString(
        <Storyboard data-uid='ignore'>
          <SceneRoot data-uid='scene' toRender={Component} />
        </Storyboard>,
      ),
    ).toMatchInlineSnapshot(`
      "<div data-uid=\\"cica\\">Hello!</div>
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
      "<div data-uid=\\"cica\\">Hello!</div>
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

    class RenderPropsFunctionChild extends React.Component {
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
      "<div data-uid=\\"aaa\\">
        <div>huha</div>
        huha!
      </div>
      "
    `)
  })

  it('the real antd datepicker works!', () => {
    var App = (props: any) => {
      return <div>{<DatePicker data-uid={'aaa'} />}</div>
    }

    expect(renderToFormattedString(<App />)).toMatchInlineSnapshot(`
      "<div>
        <div class=\\"ant-picker\\" data-uid=\\"aaa\\">
          <div class=\\"ant-picker-input\\">
            <input
              readonly=\\"\\"
              value=\\"\\"
              placeholder=\\"Select date\\"
              title=\\"\\"
              size=\\"12\\"
              data-uid=\\"aaa\\"
              autocomplete=\\"off\\"
            /><span class=\\"ant-picker-suffix\\"
              ><span role=\\"img\\" aria-label=\\"calendar\\" class=\\"anticon anticon-calendar\\"
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
      "<div data-uid=\\"cica\\">
        <div data-uid=\\"aaa\\">
          <div data-uid=\\"bbb\\">4</div>
          <div data-uid=\\"bbb\\">5</div>
          <div data-uid=\\"bbb\\">6</div>
        </div>
        <div data-uid=\\"aaa\\">
          <div data-uid=\\"bbb\\">8</div>
          <div data-uid=\\"bbb\\">10</div>
          <div data-uid=\\"bbb\\">12</div>
        </div>
        <div data-uid=\\"aaa\\">
          <div data-uid=\\"bbb\\">12</div>
          <div data-uid=\\"bbb\\">15</div>
          <div data-uid=\\"bbb\\">18</div>
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
      "<div data-uid=\\"cica\\">hello!</div>
      "
    `)
  })
})
