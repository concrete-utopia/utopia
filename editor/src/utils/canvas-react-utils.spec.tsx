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

  it('works for function components returning function components', () => {
    const OtherTestComponent: React.FunctionComponent<{}> = (props) => {
      return <div>Hello!</div>
    }

    const TestComponent: React.FunctionComponent<{}> = (props) => {
      return <OtherTestComponent data-uid={'test2'} />
    }

    expect(renderToFormattedString(<TestComponent data-uid={'test1'} />)).toMatchInlineSnapshot(`
      "<div data-uid=\\"test1\\">Hello!</div>
      "
    `)
  })

  xit('works for function components that have no uid returning function components', () => {
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

  xit('works for class components returning class components', () => {
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

  xit('works with a silly render prop', () => {
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

  xit('works with a render prop with a class component', () => {
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
