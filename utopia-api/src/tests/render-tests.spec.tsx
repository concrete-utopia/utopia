/** @jsx jsx */
import { jsx } from '../pragma/pragma'
import * as React from 'react'
import * as ReactDOMServer from 'react-dom/server'
import * as Prettier from 'prettier'

import { View } from '../primitives/view'
import { LayoutSystem } from '../layout/layout'
import { Rectangle } from '../primitives/rectangle'
import { Ellipse } from '../primitives/ellipse'
import { FlexAlignment, FlexDirection, FlexJustifyContent, FlexWrap } from '../layout/flex'
import { FlexRow, FlexColumn } from './test-utils'

function testRenderScene(TestScene: React.ComponentType<any>): string {
  const flatFormat = ReactDOMServer.renderToStaticMarkup(<TestScene />)
  const formatted = Prettier.format(flatFormat, { parser: 'html' })
  return formatted
}

describe('Self TLWH', () => {
  it('a single View with no layout will print no layout', () => {
    const TestScene = () => <View />
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                              "<div></div>
                              "
                    `)
  })

  // these will probably need to change as we start testing Views passed into other react components
  it('a single View positions itself', () => {
    const TestScene = () => <View style={{ width: 123, height: 15, top: 10, left: 5 }} />
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 123px; height: 15px; top: 10px; left: 5px;\\"></div>
      "
    `)
  })

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
        style=\\"width: 123px; height: 15px; top: 10px; left: 5px; object-fit: cover;\\"
      />
      "
    `)
  })

  it('a single Rectangle positions itself', () => {
    const TestScene = () => <Rectangle style={{ width: 123, height: 15, top: 10, left: 5 }} />
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"width: 123px; height: 15px; top: 10px; left: 5px;\\"
        data-utopia-do-not-traverse=\\"true\\"
      ></div>
      "
    `)
  })

  it('a single Ellipse positions itself', () => {
    const TestScene = () => <Ellipse style={{ width: 123, height: 15, top: 10, left: 5 }} />
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"border-radius: 50%; width: 123px; height: 15px; top: 10px; left: 5px;\\"
        data-utopia-do-not-traverse=\\"true\\"
      ></div>
      "
    `)
  })
})

describe('Default Parent', () => {
  it('Default Child works', () => {
    const TestScene = () => (
      <div layout={{}}>
        <div layout={{}}>Neither divs should have position: absolute</div>
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div layout=\\"[object Object]\\">
        <div layout=\\"[object Object]\\">
          Neither divs should have position: absolute
        </div>
      </div>
      "
    `)
  })

  it('Default View Child works', () => {
    const TestScene = () => (
      <div layout={{}}>
        <View layout={{}}>Neither divs should have position: absolute</View>
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div layout=\\"[object Object]\\">
        <div>Neither divs should have position: absolute</div>
      </div>
      "
    `)
  })

  it('View Parent View Child works', () => {
    const TestScene = () => (
      <View layout={{}}>
        <View layout={{}}>Neither divs should have position: absolute</View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                              "<div><div>Neither divs should have position: absolute</div></div>
                              "
                    `)
  })

  it('View Parent without layout prop View Child works', () => {
    const TestScene = () => (
      <View>
        <View layout={{}}>Neither divs should have position: absolute</View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                              "<div><div>Neither divs should have position: absolute</div></div>
                              "
                    `)
  })

  it('View Parent without layout prop View Child without layout prop works', () => {
    const TestScene = () => (
      <View>
        <View>Neither divs should have position: absolute</View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                              "<div><div>Neither divs should have position: absolute</div></div>
                              "
                    `)
  })

  it('View Parent View Child without layout prop works', () => {
    const TestScene = () => (
      <View layout={{}}>
        <View>Neither divs should have position: absolute</View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                              "<div><div>Neither divs should have position: absolute</div></div>
                              "
                    `)
  })

  it('Child with size-only layout props work', () => {
    const TestScene = () => (
      <View>
        <View style={{ width: 50, height: '40%' }}>
          The child div shouls have width and height set in style
        </View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div>
        <div style=\\"width: 50px; height: 40%;\\">
          The child div shouls have width and height set in style
        </div>
      </div>
      "
    `)
  })
})

describe('Pin Parent', () => {
  it('TLWH pins work', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        <Rectangle
          style={{
            top: 9,
            left: 13,
            width: 99,
            height: 101,
          }}
        />
        <Ellipse
          style={{
            top: 9,
            left: 13,
            width: 99,
            height: 101,
          }}
        />
        <img
          src='test-url.jpg'
          style={{
            top: 9,
            left: 13,
            width: 99,
            height: 101,
            objectFit: 'cover',
          }}
        />
        <div
          style={{
            top: 9,
            left: 13,
            width: 99,
            height: 101,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px;\\">
        <div
          style=\\"width: 99px; height: 101px; left: 13px; top: 9px;\\"
          data-utopia-do-not-traverse=\\"true\\"
        ></div>
        <div
          style=\\"
            border-radius: 50%;
            width: 99px;
            height: 101px;
            left: 13px;
            top: 9px;
          \\"
          data-utopia-do-not-traverse=\\"true\\"
        ></div>
        <img
          src=\\"test-url.jpg\\"
          style=\\"width: 99px; height: 101px; left: 13px; top: 9px; object-fit: cover;\\"
        />
        <div style=\\"width: 99px; height: 101px; left: 13px; top: 9px;\\"></div>
      </div>
      "
    `)
  })

  it("respects the user's style prop", () => {
    const TestScene = () => (
      <View
        style={{
          position: 'relative',
        }}
        layout={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        <View
          layout={{
            position: 'absolute',
            top: 9,
            left: 13,
            width: 99,
            height: 101,
          }}
          style={{
            left: 25,
          }}
        />
        <div
          // @ts-ignore :(
          layout={{
            position: 'absolute',
            top: 9,
            left: 13,
            width: 99,
            height: 101,
          }}
          style={{
            left: 25,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          position: relative;
          width: 100px;
          height: 100px;
          left: 10px;
          top: 10px;
        \\"
      >
        <div
          style=\\"
            position: absolute;
            width: 99px;
            height: 101px;
            left: 25px;
            top: 9px;
          \\"
        ></div>
        <div
          layout=\\"[object Object]\\"
          style=\\"
            position: absolute;
            width: 99px;
            height: 101px;
            left: 25px;
            top: 9px;
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('TLWH pins work with generated children', () => {
    const TestScene = () => (
      <View
        style={{
          position: 'relative',
        }}
        layout={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        {[1, 2, 3].map((i) => (
          <div
            key={i}
            style={{
              position: 'absolute',
              top: i * 9,
              left: i * 13,
              width: 99,
              height: 101,
            }}
          />
        ))}
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          position: relative;
          width: 100px;
          height: 100px;
          left: 10px;
          top: 10px;
        \\"
      >
        <div
          style=\\"
            position: absolute;
            width: 99px;
            height: 101px;
            left: 13px;
            top: 9px;
          \\"
        ></div>
        <div
          style=\\"
            position: absolute;
            width: 99px;
            height: 101px;
            left: 26px;
            top: 18px;
          \\"
        ></div>
        <div
          style=\\"
            position: absolute;
            width: 99px;
            height: 101px;
            left: 39px;
            top: 27px;
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('TLRB pins work', () => {
    const TestScene = () => (
      <View
        style={{
          position: 'relative',
        }}
        layout={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        <View
          style={{
            position: 'absolute',
            top: 9,
            left: 13,
            right: 15,
            bottom: 13,
          }}
        />
        <Rectangle
          style={{
            position: 'absolute',
            top: 9,
            left: 13,
            right: 99,
            height: 101,
          }}
        />
        <Ellipse
          style={{
            position: 'absolute',
            top: 9,
            left: 13,
            right: 99,
            bottom: 101,
          }}
        />
        <img
          src='test-url.jpg'
          style={{
            position: 'absolute',
            right: 9,
            bottom: 13,
            width: 99,
            height: 101,
            objectFit: 'cover',
          }}
        />
        <div
          style={{
            position: 'absolute',
            top: 9,
            left: 13,
            right: 15,
            bottom: 13,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          position: relative;
          width: 100px;
          height: 100px;
          left: 10px;
          top: 10px;
        \\"
      >
        <div
          style=\\"position: absolute; left: 13px; top: 9px; right: 15px; bottom: 13px;\\"
        ></div>
        <div
          style=\\"
            position: absolute;
            height: 101px;
            left: 13px;
            top: 9px;
            right: 99px;
          \\"
          data-utopia-do-not-traverse=\\"true\\"
        ></div>
        <div
          style=\\"
            border-radius: 50%;
            position: absolute;
            left: 13px;
            top: 9px;
            right: 99px;
            bottom: 101px;
          \\"
          data-utopia-do-not-traverse=\\"true\\"
        ></div>
        <img
          src=\\"test-url.jpg\\"
          style=\\"
            position: absolute;
            width: 99px;
            height: 101px;
            right: 9px;
            bottom: 13px;
            object-fit: cover;
          \\"
        />
        <div
          style=\\"position: absolute; left: 13px; top: 9px; right: 15px; bottom: 13px;\\"
        ></div>
      </div>
      "
    `)
  })

  it('centerX, centerY, width, height pins work', () => {
    const TestScene = () => (
      <View
        style={{
          position: 'relative',
        }}
        layout={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        <View
          layout={{
            centerX: 35,
            centerY: 33,
          }}
          style={{
            position: 'absolute',
            width: 50,
            height: 50,
          }}
        />
        <div
          layout={{
            centerX: 35,
            centerY: 33,
          }}
          style={{
            position: 'absolute',
            width: 50,
            height: 50,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          position: relative;
          width: 100px;
          height: 100px;
          left: 10px;
          top: 10px;
        \\"
      >
        <div
          style=\\"
            position: absolute;
            width: 50px;
            height: 50px;
            left: calc((50% + 35px) - (50px / 2));
            top: calc((50% + 33px) - (50px / 2));
          \\"
        ></div>
        <div
          layout=\\"[object Object]\\"
          style=\\"
            position: absolute;
            width: 50px;
            height: 50px;
            left: calc((50% + 35px) - (50px / 2));
            top: calc((50% + 33px) - (50px / 2));
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('Flex Child works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        <View
          style={{
            width: 100,
            height: 100,
            top: 5,
            left: 13,
            display: 'flex',
          }}
        >
          <View
            layout={{
              width: 10,
              height: 10,
            }}
          />
          <View layout={{}} />
        </View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px;\\">
        <div
          style=\\"width: 100px; height: 100px; left: 13px; top: 5px; display: flex;\\"
        >
          <div style=\\"width: 10px; height: 10px;\\"></div>
          <div></div>
        </div>
      </div>
      "
    `)
  })

  it('Group Child works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
        }}
      >
        <View
          layout={{
            layoutSystem: LayoutSystem.Group,
          }}
          style={{
            width: 100,
            height: 100,
            top: 5,
            left: 13,
          }}
        >
          <View
            style={{
              width: 10,
              height: 10,
              top: 20,
              left: 20,
            }}
          />
          <View
            layout={{
              width: 10,
              height: 10,
              top: 70,
              left: 50,
            }}
          />
        </View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px;\\">
        <div style=\\"position: absolute; left: 0; top: 0; width: 40px; height: 60px;\\">
          <div
            style=\\"width: 10px; height: 10px; top: 0; left: 0; position: absolute;\\"
          ></div>
          <div
            style=\\"
              position: absolute;
              width: 10px;
              height: 10px;
              left: 30px;
              top: 50px;
            \\"
          ></div>
        </div>
      </div>
      "
    `)
  })
})

describe('Flex Parent', () => {
  it('No Child works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      ></View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\"
      ></div>
      "
    `)
  })

  it('Pinned Child works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            width: 100,
            height: 100,
          }}
        >
          <View
            style={{
              top: 15,
              left: 15,
              width: 10,
              height: 10,
            }}
          />
          <View
            style={{
              right: 15,
              bottom: 15,
              width: 10,
              height: 10,
            }}
          />
        </View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div style=\\"width: 100px; height: 100px;\\">
          <div style=\\"width: 10px; height: 10px; left: 15px; top: 15px;\\"></div>
          <div style=\\"width: 10px; height: 10px; right: 15px; bottom: 15px;\\"></div>
        </div>
      </div>
      "
    `)
  })

  it('Children of various types but no props.layout.position will have no offset', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            width: 100,
            height: 100,
          }}
        />
        <div
          layout={{
            width: 10,
            height: 10,
          }}
        />
        <View
          layout={{
            width: 10,
            height: 10,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div style=\\"width: 100px; height: 100px;\\"></div>
        <div layout=\\"[object Object]\\" style=\\"width: 10px; height: 10px;\\"></div>
        <div style=\\"width: 10px; height: 10px;\\"></div>
      </div>
      "
    `)
  })

  it('respects props.style of the children', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            width: 100,
            height: 100,
            top: 5, // we expect this to be ignored
            left: 13, // we expect this to be ignored
          }}
          style={{
            left: 15,
            top: 15,
          }}
        />
        <div
          layout={{
            top: 15, // we expect this to be ignored
            left: 15, // we expect this to be ignored
            width: 10,
            height: 10,
          }}
          style={{
            left: 15,
            top: 15,
          }}
        />
        <View
          layout={{
            right: 15, // we expect this to be ignored
            bottom: 15, // we expect this to be ignored
            width: 10,
            height: 10,
          }}
          style={{
            bottom: 15,
            right: 15,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div style=\\"width: 100px; height: 100px; left: 15px; top: 15px;\\"></div>
        <div
          layout=\\"[object Object]\\"
          style=\\"width: 10px; height: 10px; left: 15px; top: 15px;\\"
        ></div>
        <div style=\\"width: 10px; height: 10px; right: 15px; bottom: 15px;\\"></div>
      </div>
      "
    `)
  })

  it('Pinned Child with position: relative works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          style={{
            position: 'relative',
            top: 5,
            left: 13,
            width: 100,
            height: 100,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div
          style=\\"
            position: relative;
            width: 100px;
            height: 100px;
            left: 13px;
            top: 5px;
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('Group Child works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            layoutSystem: LayoutSystem.Group,
          }}
          style={{
            width: 100,
            height: 100,
            top: 5,
            left: 13,
          }}
        >
          <View
            style={{
              top: 15,
              left: 15,
              width: 10,
              height: 10,
            }}
          />
          <View
            style={{
              left: 50,
              top: 50,
              width: 10,
              height: 10,
            }}
          />
        </View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div style=\\"position: absolute; left: 0; top: 0; width: 45px; height: 45px;\\">
          <div
            style=\\"top: 0; left: 0; width: 10px; height: 10px; position: absolute;\\"
          ></div>
          <div
            style=\\"
              left: 35px;
              top: 35px;
              width: 10px;
              height: 10px;
              position: absolute;
            \\"
          ></div>
        </div>
      </div>
      "
    `)
  })

  it('Various primitives work', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            layoutSystem: LayoutSystem.Group,
          }}
          style={{
            width: 100,
            height: 100,
          }}
        />
        <img
          src='test-url.jpg'
          style={{
            width: 10,
            height: 10,
            objectFit: 'cover',
          }}
        />
        <Rectangle
          style={{
            width: 10,
            height: 10,
          }}
        />
        <Ellipse
          style={{
            width: 10,
            height: 10,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div style=\\"position: absolute; left: 0; top: 0; width: 0; height: 0;\\"></div>
        <img
          src=\\"test-url.jpg\\"
          style=\\"width: 10px; height: 10px; object-fit: cover;\\"
        />
        <div
          style=\\"width: 10px; height: 10px;\\"
          data-utopia-do-not-traverse=\\"true\\"
        ></div>
        <div
          style=\\"border-radius: 50%; width: 10px; height: 10px;\\"
          data-utopia-do-not-traverse=\\"true\\"
        ></div>
      </div>
      "
    `)
  })

  it('Flex Child works', () => {
    const TestScene = () => (
      <View
        style={{
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          style={{
            width: 100,
            height: 100,
            display: 'flex',
          }}
        >
          <View
            style={{
              width: 10,
              height: 10,
            }}
          />
          <View
            style={{
              width: 10,
              height: 10,
            }}
          />
        </View>
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"width: 100px; height: 100px; top: 10px; left: 10px; display: flex;\\">
        <div style=\\"width: 100px; height: 100px; display: flex;\\">
          <div style=\\"width: 10px; height: 10px;\\"></div>
          <div style=\\"width: 10px; height: 10px;\\"></div>
        </div>
      </div>
      "
    `)
  })

  it('Pinned child respects horizontal stretching', () => {
    const TestScene = () => (
      <View
        style={{
          alignItems: FlexAlignment.Stretch,
          flexDirection: FlexDirection.Column,
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          style={{ position: 'relative', top: 5, left: 13 }}
          layout={{
            crossBasis: 100, // We expect this to be ignored
            flexBasis: 100,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          align-items: stretch;
          flex-direction: column;
          width: 100px;
          height: 100px;
          top: 10px;
          left: 10px;
          display: flex;
        \\"
      >
        <div
          style=\\"position: relative; flex-basis: 100px; left: 13px; top: 5px;\\"
        ></div>
      </div>
      "
    `)
  })

  it('Pinned child respects vertical stretching', () => {
    const TestScene = () => (
      <View
        style={{
          alignItems: FlexAlignment.Stretch,
          flexDirection: FlexDirection.Row,
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          style={{
            position: 'absolute',
            top: 5,
            left: 13,
          }}
          layout={{
            crossBasis: 100, // We expect this to be ignored
            flexBasis: 100,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          align-items: stretch;
          flex-direction: row;
          width: 100px;
          height: 100px;
          top: 10px;
          left: 10px;
          display: flex;
        \\"
      >
        <div
          style=\\"position: absolute; flex-basis: 100px; left: 13px; top: 5px;\\"
        ></div>
      </div>
      "
    `)
  })

  it('Flex child respects horizontal stretching', () => {
    const TestScene = () => (
      <View
        style={{
          flexDirection: FlexDirection.Column,
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            crossBasis: 100, // We expect this to be ignored
            flexBasis: 100,
          }}
          style={{
            alignSelf: FlexAlignment.Stretch,
            display: 'flex',
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          flex-direction: column;
          width: 100px;
          height: 100px;
          top: 10px;
          left: 10px;
          display: flex;
        \\"
      >
        <div style=\\"align-self: stretch; flex-basis: 100px; display: flex;\\"></div>
      </div>
      "
    `)
  })

  it('Flex child respects vertical stretching', () => {
    const TestScene = () => (
      <View
        style={{
          flexDirection: FlexDirection.Row,
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            crossBasis: 100, // We expect this to be ignored
            flexBasis: 100,
          }}
          style={{
            alignSelf: FlexAlignment.Stretch,
            display: 'flex',
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          flex-direction: row;
          width: 100px;
          height: 100px;
          top: 10px;
          left: 10px;
          display: flex;
        \\"
      >
        <div style=\\"align-self: stretch; flex-basis: 100px; display: flex;\\"></div>
      </div>
      "
    `)
  })

  it('Flex child uses crossBasis for width if main axis is vertical', () => {
    const TestScene = () => (
      <View
        style={{
          flexDirection: FlexDirection.Column,
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            crossBasis: 100, // we expect this to become width
            flexBasis: 100,
          }}
          style={{
            display: 'flex',
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          flex-direction: column;
          width: 100px;
          height: 100px;
          top: 10px;
          left: 10px;
          display: flex;
        \\"
      >
        <div style=\\"flex-basis: 100px; width: 100px; display: flex;\\"></div>
      </div>
      "
    `)
  })

  it('Flex child uses crossBasis for height if main axis is horizontal', () => {
    const TestScene = () => (
      <View
        style={{
          flexDirection: FlexDirection.Row,
          width: 100,
          height: 100,
          top: 10,
          left: 10,
          display: 'flex',
        }}
      >
        <View
          layout={{
            crossBasis: 100, // we expect this to become height
            flexBasis: 100,
          }}
          style={{
            display: 'flex',
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          flex-direction: row;
          width: 100px;
          height: 100px;
          top: 10px;
          left: 10px;
          display: flex;
        \\"
      >
        <div style=\\"flex-basis: 100px; height: 100px; display: flex;\\"></div>
      </div>
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

  it('divs with empty layout do not get position: absolutes on them', () => {
    const TestScene = () => (
      <div id='flex-row' layout={{}}>
        <div id='child' layout={{}} />
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"flex-row\\" layout=\\"[object Object]\\">
        <div id=\\"child\\" layout=\\"[object Object]\\"></div>
      </div>
      "
    `)
  })
})

describe('Pragma works', () => {
  it('Style from layout added as emotion css prop adds css-class to the div', () => {
    const TestScene = () => (
      <View
        layout={{ width: 123, height: 15, top: 10, left: 5 }}
        css={{
          border: '1px solid hotpink',
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        class=\\"css-x47szw\\"
        style=\\"width: 123px; height: 15px; left: 5px; top: 10px;\\"
      ></div>
      "
    `)
  })
  it('Style style added as emotion css prop adds css-class to the div', () => {
    const TestScene = () => (
      <View
        style={{ width: 123, height: 15, top: 10, left: 5 }}
        css={{
          border: '1px solid hotpink',
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"width: 123px; height: 15px; top: 10px; left: 5px;\\"
        class=\\"css-x47szw\\"
      ></div>
      "
    `)
  })
})

describe('FlexRow works', () => {
  it('FlexRow parent, View children work', () => {
    const TestScene = () => (
      <FlexRow id='flex-row'>
        <View
          id='child'
          layout={{
            crossBasis: 100, // we expect this to become height
            flexBasis: 100,
          }}
        />
      </FlexRow>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"flex-row\\" class=\\"css-1pyjm1n\\">
        <div id=\\"child\\" style=\\"flex-basis: 100px; height: 100px;\\"></div>
      </div>
      "
    `)
  })

  it('div grandparent, FlexRow parent, View child', () => {
    const TestScene = () => (
      <div>
        <FlexRow id='flex-row'>
          <View
            id='child'
            layout={{
              crossBasis: 100, // we expect this to become height
              flexBasis: 100,
            }}
          />
        </FlexRow>
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div>
        <div id=\\"flex-row\\" class=\\"css-1pyjm1n\\">
          <div id=\\"child\\" style=\\"flex-basis: 100px; height: 100px;\\"></div>
        </div>
      </div>
      "
    `)
  })

  it('div grandparent, FlexRow parent, div child', () => {
    const TestScene = () => (
      <div>
        <FlexRow id='flex-row'>
          <div
            id='child'
            layout={{
              crossBasis: 100, // we expect this to become height
              flexBasis: 100,
            }}
          />
        </FlexRow>
      </div>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div>
        <div id=\\"flex-row\\" class=\\"css-1pyjm1n\\">
          <div
            id=\\"child\\"
            layout=\\"[object Object]\\"
            style=\\"flex-basis: 100px; height: 100px;\\"
          ></div>
        </div>
      </div>
      "
    `)
  })

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
          <div id=\\"child\\" style=\\"width: 100px; height: 100px;\\"></div>
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
          <div id=\\"div-1\\" style=\\"width: 100px; height: 100px;\\"></div>
        </div>
        <div id=\\"flex-row-2\\" class=\\"css-1pyjm1n\\">
          <div id=\\"div-1\\" style=\\"width: 100px; height: 100px;\\"></div>
        </div>
      </div>
      "
    `)
  })
})

describe('Layout props are copied to style', () => {
  it('View with layout prop without layoutsystem', () => {
    const TestScene = () => (
      <View
        layout={{
          top: 50,
          bottom: 40,
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"top: 50px; bottom: 40px;\\"></div>
      "
    `)
  })
  it('View with style pop without layoutsystem', () => {
    const TestScene = () => (
      <View
        style={{
          top: 50,
          bottom: 40,
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"top: 50px; bottom: 40px;\\"></div>
      "
    `)
  })
  it('View with flex layoutsystem', () => {
    const TestScene = () => (
      <View
        layout={{
          gapMain: 20,
        }}
        style={{
          paddingTop: 1,
          paddingRight: 1,
          paddingLeft: 1,
          left: 116,
          top: 375,
          width: 155,
          height: 133,
          flexDirection: FlexDirection.Column,
          justifyContent: FlexJustifyContent.Center,
          alignItems: FlexAlignment.Center,
          display: 'flex',
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          width: 155px;
          height: 133px;
          left: 116px;
          top: 375px;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          padding-top: 1px;
          padding-right: 1px;
          padding-left: 1px;
          display: flex;
        \\"
      ></div>
      "
    `)
  })
  it('View without layoutsystem, but with flex children props', () => {
    const TestScene = () => (
      <View
        layout={{
          crossBasis: 55, // TODO this becomes height by default, but if the parent is column it should be width
        }}
        style={{
          flexBasis: 48,
          alignSelf: FlexAlignment.FlexEnd,
          marginRight: 20,
          marginBottom: 20,
          maxHeight: 100,
          minWidth: 10,
          maxWidth: 100,
          minHeight: 5,
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          min-width: 10px;
          max-width: 100px;
          min-height: 5px;
          max-height: 100px;
          align-self: flex-end;
          flex-basis: 48px;
          height: 55px;
          margin-right: 20px;
          margin-bottom: 20px;
        \\"
      ></div>
      "
    `)
  })

  it('Every non-magic prop works even if there is no layoutSystem', () => {
    const TestScene = () => (
      <View
        style={{
          position: 'relative',
          alignSelf: FlexAlignment.FlexEnd,
          marginLeft: 9,
          marginTop: 3,
          marginRight: 20,
          marginBottom: 20,
          maxHeight: 100,
          minWidth: 10,
          maxWidth: 100,
          minHeight: 5,
          left: 116,
          top: 375,
          width: 155,
          height: 133,
          right: 19,
          bottom: 29,
          flexDirection: FlexDirection.Column,
          justifyContent: FlexJustifyContent.Center,
          alignItems: FlexAlignment.Center,
          paddingLeft: 1,
          paddingTop: 1,
          paddingRight: 1,
          paddingBottom: 1,
        }}
        layout={{
          flexBasis: 48,
        }}
      />
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        style=\\"
          position: relative;
          min-width: 10px;
          max-width: 100px;
          min-height: 5px;
          max-height: 100px;
          align-self: flex-end;
          flex-basis: 48px;
          width: 155px;
          height: 133px;
          left: 116px;
          top: 375px;
          right: 19px;
          bottom: 29px;
          margin-left: 9px;
          margin-top: 3px;
          margin-right: 20px;
          margin-bottom: 20px;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          padding-left: 1px;
          padding-top: 1px;
          padding-right: 1px;
          padding-bottom: 1px;
        \\"
      ></div>
      "
    `)
  })

  it('Flex parent: Every non-magic prop works even if there is no layoutSystem', () => {
    const TestScene = () => (
      <View style={{ display: 'flex' }}>
        <View
          style={{
            position: 'relative',
            flex: 10,
            alignSelf: FlexAlignment.FlexEnd,
            marginLeft: 9,
            marginTop: 3,
            marginRight: 20,
            marginBottom: 20,
            maxHeight: 100,
            minWidth: 10,
            maxWidth: 100,
            minHeight: 5,
            left: 116,
            top: 375,
            width: 155,
            height: 133,
            right: 19,
            bottom: 29,
            flexDirection: FlexDirection.Column,
            justifyContent: FlexJustifyContent.Center,
            alignItems: FlexAlignment.Center,
            paddingLeft: 1,
            paddingTop: 1,
            paddingRight: 1,
            paddingBottom: 1,
          }}
          layout={{
            flexBasis: 48,
            gapMain: 20,
          }}
        />
      </View>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div style=\\"display: flex;\\">
        <div
          style=\\"
            position: relative;
            min-width: 10px;
            max-width: 100px;
            min-height: 5px;
            max-height: 100px;
            align-self: flex-end;
            flex-basis: 48px;
            width: 155px;
            height: 133px;
            left: 116px;
            top: 375px;
            right: 19px;
            bottom: 29px;
            margin-left: 9px;
            margin-top: 3px;
            margin-right: 20px;
            margin-bottom: 20px;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            flex: 10;
            padding-left: 1px;
            padding-top: 1px;
            padding-right: 1px;
            padding-bottom: 1px;
          \\"
        ></div>
      </div>
      "
    `)
  })
})

function wrapInTestParents(element: React.ReactElement) {
  return () => (
    <React.Fragment>
      <div id='parent-nolayout' layout={{}}>
        {element}
      </div>
      <div id='parent-flex' style={{ display: 'flex' }}>
        {element}
      </div>
    </React.Fragment>
  )
}

describe('Non-magic props are blindly copied over to style, regardless of the parent layoutSystem:', () => {
  it('position', () => {
    const TestScene = wrapInTestParents(<View id='child' style={{ position: 'relative' }} />)
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div id=\\"child\\" style=\\"position: relative;\\"></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div id=\\"child\\" style=\\"position: relative;\\"></div>
      </div>
      "
    `)
  })

  it('all nonmagic frame props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        layout={{
          left: 116,
          top: 375,
          right: 19,
          bottom: 29,
          width: 155,
          height: 133,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            width: 155px;
            height: 133px;
            left: 116px;
            top: 375px;
            right: 19px;
            bottom: 29px;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            width: 155px;
            height: 133px;
            left: 116px;
            top: 375px;
            right: 19px;
            bottom: 29px;
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('all nonmagic frame props coming from style', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          left: 116,
          top: 375,
          right: 19,
          bottom: 29,
          width: 155,
          height: 133,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            width: 155px;
            height: 133px;
            left: 116px;
            top: 375px;
            right: 19px;
            bottom: 29px;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            left: 116px;
            top: 375px;
            right: 19px;
            bottom: 29px;
            width: 155px;
            height: 133px;
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('Position nonmagic flex element props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{ position: 'relative' }}
        layout={{
          left: 12,
          top: 13,
          right: 25,
          bottom: 43,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            position: relative;
            left: 12px;
            top: 13px;
            right: 25px;
            bottom: 43px;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            position: relative;
            left: 12px;
            top: 13px;
            right: 25px;
            bottom: 43px;
          \\"
        ></div>
      </div>
      "
    `)
  })
  it('Min/max width/height nonmagic flex element props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          minWidth: 16,
          maxWidth: 99,
          minHeight: 17,
          maxHeight: 98,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            min-width: 16px;
            max-width: 99px;
            min-height: 17px;
            max-height: 98px;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            min-width: 16px;
            max-width: 99px;
            min-height: 17px;
            max-height: 98px;
          \\"
        ></div>
      </div>
      "
    `)
  })
  it('Margin nonmagic flex element props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          marginLeft: 9,
          marginTop: 3,
          marginRight: 20,
          marginBottom: 20,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            margin-left: 9px;
            margin-top: 3px;
            margin-right: 20px;
            margin-bottom: 20px;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            margin-left: 9px;
            margin-top: 3px;
            margin-right: 20px;
            margin-bottom: 20px;
          \\"
        ></div>
      </div>
      "
    `)
  })
  it('AlignSelf, Flex Grow/Shrink and Width/Height nonmagic flex element props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          alignSelf: FlexAlignment.FlexEnd,
          flex: 10,
          flexGrow: 1,
          flexShrink: 2,
        }}
        layout={{ width: 50, height: 50 }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            flex-grow: 1;
            flex-shrink: 2;
            align-self: flex-end;
            width: 50px;
            height: 50px;
            flex: 10;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            flex-grow: 1;
            flex-shrink: 2;
            align-self: flex-end;
            width: 50px;
            height: 50px;
            flex: 10;
          \\"
        ></div>
      </div>
      "
    `)
  })

  it('Flex Direction and Align Content nonmagic flex parent props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          flexDirection: FlexDirection.ColumnReverse,
          alignContent: FlexAlignment.FlexEnd,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"flex-direction: column-reverse; align-content: flex-end;\\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"flex-direction: column-reverse; align-content: flex-end;\\"
        ></div>
      </div>
      "
    `)
  })
  it('Align Items and Justify Content nonmagic flex parent props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          alignItems: FlexAlignment.FlexStart,
          justifyContent: FlexJustifyContent.SpaceBetween,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"align-items: flex-start; justify-content: space-between;\\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"align-items: flex-start; justify-content: space-between;\\"
        ></div>
      </div>
      "
    `)
  })
  it('Wrap and Padding nonmagic flex parent props', () => {
    const TestScene = wrapInTestParents(
      <View
        id='child'
        style={{
          flexWrap: FlexWrap.Wrap,
          paddingLeft: 1,
          paddingTop: 1,
          paddingRight: 1,
          paddingBottom: 1,
        }}
      />,
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"parent-nolayout\\" layout=\\"[object Object]\\">
        <div
          id=\\"child\\"
          style=\\"
            flex-wrap: wrap;
            padding-left: 1px;
            padding-top: 1px;
            padding-right: 1px;
            padding-bottom: 1px;
          \\"
        ></div>
      </div>
      <div id=\\"parent-flex\\" style=\\"display: flex;\\">
        <div
          id=\\"child\\"
          style=\\"
            flex-wrap: wrap;
            padding-left: 1px;
            padding-top: 1px;
            padding-right: 1px;
            padding-bottom: 1px;
          \\"
        ></div>
      </div>
      "
    `)
  })
})

describe('Magic props work in the right context', () => {
  it('flexBasis, crossBasis works given it knows parent flex direction', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          style={{
            display: 'flex',
            flexDirection: FlexDirection.Row,
          }}
        >
          <View
            id='child'
            layout={{
              flexBasis: 50,
              crossBasis: 45,
            }}
          />
        </View>
        <View
          id='flex-column'
          style={{
            display: 'flex',
            flexDirection: FlexDirection.Column,
          }}
        >
          <View
            id='child'
            layout={{
              flexBasis: 50,
              crossBasis: 45,
            }}
          />
        </View>
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div id=\\"flex-row\\" style=\\"display: flex; flex-direction: row;\\">
        <div id=\\"child\\" style=\\"flex-basis: 50px; height: 45px;\\"></div>
      </div>
      <div id=\\"flex-column\\" style=\\"display: flex; flex-direction: column;\\">
        <div id=\\"child\\" style=\\"flex-basis: 50px; width: 45px;\\"></div>
      </div>
      "
    `)
  })

  it('centerX and centerY works if there is width and height', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          style={{
            width: 120,
            height: 130,
          }}
          layout={{
            centerX: 40,
            centerY: 20,
          }}
        />
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        id=\\"flex-row\\"
        style=\\"
          width: 120px;
          height: 130px;
          left: calc((50% + 40px) - (120px / 2));
          top: calc((50% + 20px) - (130px / 2));
        \\"
      ></div>
      "
    `)
  })

  it('centerX % and centerY % works if there is width and height', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          style={{
            width: 50,
            height: 40,
          }}
          layout={{
            centerX: '40%',
            centerY: '20%',
          }}
        />
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        id=\\"flex-row\\"
        style=\\"
          width: 50px;
          height: 40px;
          left: calc((50% + 40%) - (50px / 2));
          top: calc((50% + 20%) - (40px / 2));
        \\"
      ></div>
      "
    `)
  })

  it('centerX and centerY works if there is percentage width and percentage height', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          style={{
            width: '50%',
            height: '40%',
          }}
          layout={{
            centerX: 40,
            centerY: 20,
          }}
        />
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        id=\\"flex-row\\"
        style=\\"
          width: 50%;
          height: 40%;
          left: calc((50% + 40px) - (50% / 2));
          top: calc((50% + 20px) - (40% / 2));
        \\"
      ></div>
      "
    `)
  })

  it('centerX and centerY works if there is left and bottom', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          style={{
            left: 15,
            bottom: 23,
          }}
          layout={{
            centerX: 40,
            centerY: 20,
          }}
        />
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        id=\\"flex-row\\"
        style=\\"
          width: calc(calc(calc(50% + 40px) - 15px) * 2);
          left: 15px;
          top: calc(23px - calc(50% - 20px - 23px) * 2);
          bottom: 23px;
        \\"
      ></div>
      "
    `)
  })

  it('centerX and centerY works if there is bottom and right', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          style={{
            bottom: 23,
            right: 50,
          }}
          layout={{
            centerX: 40,
            centerY: 20,
          }}
        />
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
      "<div
        id=\\"flex-row\\"
        style=\\"
          left: calc(50px - calc(50% - 40px - 50px) * 2);
          top: calc(23px - calc(50% - 20px - 23px) * 2);
          right: 50px;
          bottom: 23px;
        \\"
      ></div>
      "
    `)
  })

  it('centerX and centerY fails if there is no width and height', () => {
    const TestScene = () => (
      <React.Fragment>
        <View
          id='flex-row'
          layout={{
            centerX: 40,
            centerY: 20,
          }}
        />
      </React.Fragment>
    )
    expect(testRenderScene(TestScene)).toMatchInlineSnapshot(`
                              "<div id=\\"flex-row\\"></div>
                              "
                    `)
  })
})
