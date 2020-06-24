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

  it('works with a silly render prop', () => {
    const CallRenderPropChild: React.FunctionComponent<{}> = (props) => {
      return (props.children as any)('hello')
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
      "<div data-uid=\\"test1\\">hello</div>
      "
    `)
  })

  it('works with a silly render prop with a class component', () => {
    const CallRenderPropChild: React.FunctionComponent<{}> = (props) => {
      return (props.children as any)('hello')
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
      "<div data-uid=\\"test1\\">hello</div>
      "
    `)
  })
})
