import type { JSXAttributes, JSExpressionOtherJavaScript } from '../../shared/element-template'
import {
  emptyComments,
  isJSXElement,
  isUtopiaJSXComponent,
  jsxAttributesFromMap,
  jsExpressionValue,
  UtopiaJSXComponent,
  clearAttributesUniqueIDs,
  simplifyAttributesIfPossible,
  getDefinedElsewhereFromElementChild,
} from '../../shared/element-template'
import { forEachLeft, isRight } from '../../shared/either'
import {
  clearParseResultUniqueIDsAndEmptyBlocks,
  elementsStructure,
  testParseCode,
  testParseThenPrint,
  testParseThenPrintWithoutUids,
} from './parser-printer.test-utils'
import { objectMap, omit } from '../../shared/object-utils'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../model/scene-utils'
import type { ParseSuccess } from '../../shared/project-file-types'
import { isParseSuccess } from '../../shared/project-file-types'
import { findJSXElementAtStaticPath } from '../../model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../..//model/project-file-utils'
import * as EP from '../../shared/element-path'

describe('JSX parser', () => {
  it('definedElsewhere values in a block including those from elementsWithin', () => {
    const code = `import * as React from "react";

const cake = 'chocolate'

export var App = props => {
  return <div style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="aaa">
    {[1, 2, 3].map(n => {
      return <div data-uid="bbb">{n} - {cake}</div>
    })}
  </div>
};`
    const parsedCode = testParseCode(code)
    if (isParseSuccess(parsedCode)) {
      expect(elementsStructure(parsedCode.topLevelElements)).toMatchInlineSnapshot(`
        "IMPORT_STATEMENT
        UNPARSED_CODE
        ARBITRARY_JS_BLOCK
        UNPARSED_CODE
        UTOPIA_JSX_COMPONENT - App
          JSX_ELEMENT - div - aaa
            JSX_MAP_EXPRESSION - 1cc9873e534a27825c940da0a7cfc3ab
                ATTRIBUTE_VALUE - 8f773de0675f8e356c6200f23e9acdeb
                ATTRIBUTE_VALUE - 5cd3401c35a8ee4bae2e2dfb4474d7c3
                ATTRIBUTE_VALUE - 7a4e3051f3fb0ea3e8102cc3682ba028
              ATTRIBUTE_OTHER_JAVASCRIPT - 83160e700c2c1248251c587751cd5bbe
                JSX_ELEMENT - div - bbb
                  JS_IDENTIFIER - aaef9cd128e93c1e3f7f37582c38024a
                  JSX_TEXT_BLOCK - 3bdb453dde9b4f69e73d62c9425f1452
                  JS_IDENTIFIER - 8041faf6fb3bb182954ddc56eeb901a9
              ATTRIBUTE_VALUE - 9e407a73a86f3a36ffd603dc1e621527
              ATTRIBUTE_VALUE - 7537c2399208a65294ab51886a38a78b"
      `)

      const aaaElement = findJSXElementAtStaticPath(
        getUtopiaJSXComponentsFromSuccess(parsedCode),
        EP.dynamicPathToStaticPath(EP.elementPath([['App'], ['aaa']])),
      )
      const aaaJSXMapExpression = aaaElement?.children[0]
      if (aaaJSXMapExpression != null) {
        const definedElsewhere = getDefinedElsewhereFromElementChild([], aaaJSXMapExpression)
        expect(definedElsewhere).toMatchInlineSnapshot(`
          Array [
            "React",
            "cake",
            "utopiaCanvasJSXLookup",
          ]
        `)
      } else {
        throw new Error('Was not a JSX_MAP_EXPRESSION as expected.')
      }

      const bbbElement = findJSXElementAtStaticPath(
        getUtopiaJSXComponentsFromSuccess(parsedCode),
        EP.dynamicPathToStaticPath(EP.elementPath([['App'], ['aaa', 'bbb']])),
      )
      const bbbJSXSecondIdentifier = bbbElement?.children[2]
      if (bbbJSXSecondIdentifier?.type === 'JS_IDENTIFIER') {
        expect(bbbJSXSecondIdentifier.name).toEqual('cake')
      } else {
        throw new Error('Was not a JS_IDENTIFIER as expected.')
      }
    } else {
      throw new Error(JSON.stringify(parsedCode))
    }
  })
  it('adds in data-uid attributes for elements in arbitrary code', () => {
    const code = `import * as React from "react";
import { Ellipse, UtopiaUtils, Image, Rectangle, Text, View } from "utopia-api";

export var App = props => {
  return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx">
    {[1, 2, 3].map(n => {
      return <div>{n}</div>
    })}
  </View>
};`
    const parsedCode = testParseCode(code)
    if (!isParseSuccess(parsedCode)) {
      throw new Error(JSON.stringify(parsedCode))
    }
  })
  it('fails on unprettied code', () => {
    const code = `import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, UtopiaUtils, Image, Rectangle, Text, View } from "utopia-api";

    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx" />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isParseSuccess(parsedPlainCode)) {
      const topComponent = parsedPlainCode.topLevelElements.find(isUtopiaJSXComponent)
      if (topComponent != null) {
        if (isJSXElement(topComponent.rootElement)) {
          const expectedProps: JSXAttributes = clearAttributesUniqueIDs(
            jsxAttributesFromMap({
              style: jsExpressionValue(
                {
                  backgroundColor: 'green',
                  position: 'absolute',
                },
                emptyComments,
              ),
              'data-uid': jsExpressionValue('xxx', emptyComments),
            }),
          )
          expect(
            simplifyAttributesIfPossible(clearAttributesUniqueIDs(topComponent.rootElement.props)),
          ).toEqual(expectedProps)
        } else {
          throw new Error('Root element not a JSX element.')
        }
      } else {
        throw new Error('Not a component.')
      }
    } else {
      throw new Error(JSON.stringify(parsedPlainCode))
    }
  })
  it('parses elements that use props spreading - #1365', () => {
    const nonSpreadCode = `
    import * as React from "react"
    import { Card } from './card'
    const Test = (props) => {
      return <div data-uid='mapper-parent'>
        {props.data.map((entry) => (
          <Card
            data-uid='card'
            style={{ flexBasis: 50 }}
            name={entry.name}
            country={entry.country}
            image={entry.image}
          />
        ))}
      </div>
    }`
    const spreadCode = `
    import * as React from "react"
    import { Card } from './card'
    const Test = (props) => {
      return <div data-uid='mapper-parent'>
        {props.data.map((entry) => (
          <Card
            {...entry}
            data-uid='card'
            style={{ flexBasis: 50 }}
          />
        ))}
      </div>
    }`

    expect(elementsStructure((testParseCode(nonSpreadCode) as any).topLevelElements))
      .toMatchInlineSnapshot(`
      "UNPARSED_CODE
      IMPORT_STATEMENT
      UNPARSED_CODE
      IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - Test
        JSX_ELEMENT - div - mapper-parent
          JSX_MAP_EXPRESSION - 3d508bafaddfd321260d1c4fa12859a2
            JS_PROPERTY_ACCESS - 741c3a9ad91f91180dfe033abc626758
              JS_IDENTIFIER - f384033d077c42938485a789a4ca9e36
            ATTRIBUTE_OTHER_JAVASCRIPT - 2d73a750d1471dc4189d8a04bafb7c88
              JSX_ELEMENT - Card - card
                  ATTRIBUTE_VALUE - d53927ce38ba55f25e983cc74737ece5
                JS_PROPERTY_ACCESS - 160ff9b21e7cb5c2ce92005de66c6914
                  JS_IDENTIFIER - ec0f0f8059c2125fbe1daf48227c5365
                JS_PROPERTY_ACCESS - 10981486a4ec307429b16fdddadf87a3
                  JS_IDENTIFIER - c9e81315b1e546fbafc3acd479fa3486
                JS_PROPERTY_ACCESS - 032a6aa9dbffc09a3c82738ef908c2d0
                  JS_IDENTIFIER - a8dd5c20bc634c99b39f3b9fe98f0a0c"
    `)
    expect(elementsStructure((testParseCode(spreadCode) as any).topLevelElements))
      .toMatchInlineSnapshot(`
      "UNPARSED_CODE
      IMPORT_STATEMENT
      UNPARSED_CODE
      IMPORT_STATEMENT
      UNPARSED_CODE
      UTOPIA_JSX_COMPONENT - Test
        JSX_ELEMENT - div - mapper-parent
          JSX_MAP_EXPRESSION - 84ef4f39affaba4af867fa85a20518c5
            JS_PROPERTY_ACCESS - 741c3a9ad91f91180dfe033abc626758
              JS_IDENTIFIER - f384033d077c42938485a789a4ca9e36
            ATTRIBUTE_OTHER_JAVASCRIPT - f544b1d8a7dfe7f2a7995229f88db586
              JSX_ELEMENT - Card - card
                  ATTRIBUTE_VALUE - d53927ce38ba55f25e983cc74737ece5"
    `)
  })
})

describe('JSX printer', () => {
  it('reprints spread object values correctly', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

const myCoolTheme = {}

export var App = (props) => {
  return (
    <View
      data-uid='xxx'
      style={{ ...myCoolTheme, backgroundColor: 'purple' }}
    />
  )
}

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}' />
)
`
    testParseThenPrint('/index.js', code, code)
  })
  it('components are not reordered when printing', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return <Widget data-uid='bbb' />
}

export var Widget = (props) => {
  return <View data-uid='aaa' />
}

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}' />
)
`
    testParseThenPrint('/index.js', code, code)
  })

  it('object property names with special characters should be printed in string quotes', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

const color = 'white'

export var App = (props) => {
  return (
    <View
      data-uid='aaa'
      css={{
        backgroundColor: 'regular propety, without quotes',
        '& :hover': { color: color },
      }}
    />
  )
}

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}' />
)
`
    testParseThenPrint('/index.js', code, code)
  })

  it('definedWithin and definedElsewhere are mutually exclusive properties', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (
    <View data-uid='aaa' css={{ backgroundColor: 'red' }}>
      <View data-uid='bbb'>
        {elements.map((e) => null)}
      </View>
    </View>
  )
}

const elements = []
export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}' />
)
`
    testParseThenPrint('/index.js', code, code)
  })
  it('parses elements that use props spreading - #1365', () => {
    const spreadCode = `import * as React from 'react'
const Test = (props) => {
  return (
    <div data-uid='mapper-parent'>
      {props.data.map((entry) => (
        <div
          {...entry}
          data-uid='card'
          style={{ flexBasis: 50 }}
        />
      ))}
    </div>
  )
}

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}' />
)
`

    testParseThenPrint('/index.js', spreadCode, spreadCode)
  })
  it('parses elements that use props spreading without an explicit uid - #1515', () => {
    const spreadCode = `import * as React from 'react'
const Test = (props) => {
  return (
    <div>
      {props.data.map((entry) => (
        <div {...entry} style={{ flexBasis: 50 }} />
      ))}
    </div>
  )
}

export var ${BakedInStoryboardVariableName} = <Storyboard />
`

    testParseThenPrintWithoutUids('/index.js', spreadCode, spreadCode)
  })

  it('#1737 - Produces the same value for an exported default function', () => {
    const spreadCode = `import * as React from 'react'
export default function () {
  return (
    <div>
      <div>Default Function Time</div>
    </div>
  )
}
`

    testParseThenPrintWithoutUids('/index.js', spreadCode, spreadCode)
  })

  it('#1773 - Handles imports which relate to the same file but have differing paths.', () => {
    const spreadCode = `import * as React from 'react'
import { FirstComponent } from './components.js'
import { SecondComponent } from '/components.js'
export default function () {
  return (
    <div>
      <div>Default Function Time</div>
    </div>
  )
}
`

    testParseThenPrintWithoutUids('/index.js', spreadCode, spreadCode)
  })
})

describe('Imports', () => {
  it('side-effects-only imports work', () => {
    const code = `import './style.css'
import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (
    <View data-uid='aaa' css={{ backgroundColor: 'red' }}>
      <View data-uid='bbb'>
        {elements.map((e) => null)}
      </View>
    </View>
  )
}

const elements = []
export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}' />
)
`
    testParseThenPrint('/index.js', code, code)
  })
})

describe('Identifiers as return statements', () => {
  it('Arrow functions which return an identifier will correctly add it to defined elsewhere', () => {
    const code = `import './style.css'
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'

    const hello = 'hi'

    export var ${BakedInStoryboardVariableName} = (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        {(() => hello)()}
      </Storyboard>
    )
    `

    const parsedCode = testParseCode(code)
    expect(isParseSuccess(parsedCode)).toBeTruthy()
    const parseSuccess = parsedCode as ParseSuccess

    const storyboardElement = findJSXElementAtStaticPath(
      getUtopiaJSXComponentsFromSuccess(parseSuccess),
      EP.dynamicPathToStaticPath(EP.elementPath([[BakedInStoryboardUID]])),
    )
    expect(storyboardElement).not.toBeNull()

    const jsxBlock = storyboardElement!.children[0]

    expect(jsxBlock.type).toEqual('ATTRIBUTE_OTHER_JAVASCRIPT')
    expect((jsxBlock as JSExpressionOtherJavaScript).definedElsewhere).toMatchInlineSnapshot(`
      Array [
        "hello",
      ]
    `)
  })

  it('Regular functions which return an identifier will correctly add it to defined elsewhere', () => {
    const code = `import './style.css'
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'

    const hello = 'hi'

    export var App = props => {
      function sayHi() { return hello }
      return <div>{sayHi()}</div>
    }

    export var ${BakedInStoryboardVariableName} = (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <App />
      </Storyboard>
    )
    `

    const parsedCode = testParseCode(code)
    expect(isParseSuccess(parsedCode)).toBeTruthy()
    const parseSuccess = parsedCode as ParseSuccess

    const appComponent = getUtopiaJSXComponentsFromSuccess(parseSuccess)[0]
    expect(appComponent).toBeDefined()

    const jsBlock = appComponent.arbitraryJSBlock
    expect(jsBlock).not.toBeNull()

    expect(jsBlock!.definedElsewhere).toMatchInlineSnapshot(`
      Array [
        "hello",
        "utopiaCanvasJSXLookup",
        "utopiaCanvasBlockRanToEnd",
        "utopiaCanvasEarlyReturnResult",
        "utopiaCanvasEarlyReturnVoid",
      ]
    `)
  })
})
