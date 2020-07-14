/** @jsx jsx */
import { jsx } from '../pragma/pragma'
import * as React from 'react'
import * as ReactDOMServer from 'react-dom/server'
import * as Prettier from 'prettier'

import { Rectangle } from '../primitives/rectangle'
import { Ellipse } from '../primitives/ellipse'
import { FlexRow, FlexColumn } from './test-utils'

function testRenderScene(TestScene: React.ComponentType<any>): string {
  const flatFormat = ReactDOMServer.renderToStaticMarkup(<TestScene />)
  const formatted = Prettier.format(flatFormat, { parser: 'html' })
  return formatted
}

describe('Self TLWH', () => {
  it('a single <img /> positions itself', () => {
    const TestScene = () => (
      <img
        src='test-url.jpg'
        style={{
          width: 123,
          height: 15,
          top: 10,
          left: 5,
          objectFit: 'cover',
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                  "<img
                    src=\\"test-url.jpg\\"
                    style=\\"width:123px;height:15px;top:10px;left:5px;object-fit:cover\\"
                  />
                  "
            `)
  })

  it('a single Rectangle positions itself', () => {
    const TestScene = () => <Rectangle style={{ width: 123, height: 15, top: 10, left: 5 }} />
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                        "<div
                          style=\\"width:123px;height:15px;top:10px;left:5px\\"
                          data-utopia-do-not-traverse=\\"true\\"
                        ></div>
                        "
                `)
  })

  it('a single Ellipse positions itself', () => {
    const TestScene = () => <Ellipse style={{ width: 123, height: 15, top: 10, left: 5 }} />
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                        "<div
                          style=\\"border-radius:50%;width:123px;height:15px;top:10px;left:5px\\"
                          data-utopia-do-not-traverse=\\"true\\"
                        ></div>
                        "
                `)
  })
})

describe('divs work', () => {
  it('divs without layout do not get position: absolutes on them', () => {
    const TestScene = () => (
      <div id='flex-row'>
        <div id='child' />
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                        "<div id=\\"flex-row\\"><div id=\\"child\\"></div></div>
                        "
                `)
  })
})

describe('FlexRow works', () => {
  it('div grandparent, FlexRow parent, div children with no layout prop', () => {
    const TestScene = () => (
      <div>
        <FlexRow id='flex-row'>
          <div id='child' style={{ width: 100, height: 100 }} />
        </FlexRow>
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                        "<div>
                          <div id=\\"flex-row\\" class=\\"css-1pyjm1n\\">
                            <div id=\\"child\\" style=\\"width:100px;height:100px\\"></div>
                          </div>
                        </div>
                        "
                `)
  })

  it('FlexColumn grandparent, FlexRow parent, div children with no layout prop', () => {
    const TestScene = () => (
      <FlexColumn id='flex-col'>
        <FlexRow id='flex-row-1'>
          <div id='div-1' style={{ width: 100, height: 100 }} />
        </FlexRow>
        <FlexRow id='flex-row-2'>
          <div id='div-1' style={{ width: 100, height: 100 }} />
        </FlexRow>
      </FlexColumn>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                        "<div id=\\"flex-col\\" class=\\"css-11bzz9m\\">
                          <div id=\\"flex-row-1\\" class=\\"css-1pyjm1n\\">
                            <div id=\\"div-1\\" style=\\"width:100px;height:100px\\"></div>
                          </div>
                          <div id=\\"flex-row-2\\" class=\\"css-1pyjm1n\\">
                            <div id=\\"div-1\\" style=\\"width:100px;height:100px\\"></div>
                          </div>
                        </div>
                        "
                `)
  })
})
