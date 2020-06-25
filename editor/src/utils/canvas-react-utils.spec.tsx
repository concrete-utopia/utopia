import * as React from 'react'
import * as ReactDOMServer from 'react-dom/server'
import * as Prettier from 'prettier'
import { applyUIDMonkeyPatch } from './canvas-react-utils'

applyUIDMonkeyPatch()

function renderToFormattedString(element: React.ReactElement) {
  const renderResult = ReactDOMServer.renderToStaticMarkup(element)
  const formattedResult = Prettier.format(renderResult, { parser: 'html' })
  return formattedResult
}

describe('Monkey Function', () => {
  it('works for simple class components', () => {
    class TestClass extends React.Component {
      render() {
        return <div>Hello!</div>
      }
    }

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  it('class components have a working context', () => {
    const MyContext = React.createContext({ value: 'hello!' })
    class TestClass extends React.Component {
      render() {
        return <div>{this.context.value}</div>
      }
    }
    TestClass.contextType = MyContext

    expect(renderToFormattedString(<TestClass data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">hello!</div>
      "
    `)
  })

  it('works for simple function components', () => {
    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return (
        <div>
          <div>hi!</div>
        </div>
      )
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\"><div>hi!</div></div>
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
})
