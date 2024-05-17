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
            JSX_MAP_EXPRESSION - 16c3d4759604aa746568f67367262605
                ATTRIBUTE_VALUE - e0d3bb6fb9a1d4f5a56d76f72112daa5
                ATTRIBUTE_VALUE - dd00bf551c48aa6b325c309b6c5a2430
                ATTRIBUTE_VALUE - eb4a9dcca51da14e190a1c149d2e0b82
              ATTRIBUTE_OTHER_JAVASCRIPT - 83160e700c2c1248251c587751cd5bbe
                JSX_ELEMENT - div - bbb
                  JS_IDENTIFIER - f2f219fe2a421a49ea5ad67fb087f175
                  JSX_TEXT_BLOCK - b6f647609b7e3c9b68f55a18ca1b3dca
                  JS_IDENTIFIER - f56b89a2e7dd50854343c420319659a2
              ATTRIBUTE_VALUE - 0c5bb3f0faaefe6375f1ec8f6fe66984
              ATTRIBUTE_VALUE - 474a7c77fe1bb091ea931baefdb6c85d"
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
          JSX_MAP_EXPRESSION - 10f8481b24d8415bcef90504209f7888
            JS_PROPERTY_ACCESS - 2f0a2b029743d9f9f62384c498d2d5d5
              JS_IDENTIFIER - e1bc922c4e4015ff66443180841a788c
            ATTRIBUTE_OTHER_JAVASCRIPT - 2d73a750d1471dc4189d8a04bafb7c88
              JSX_ELEMENT - Card - card
                  ATTRIBUTE_VALUE - 8971078115e762d1ef6c2b6965b61b25
                JS_PROPERTY_ACCESS - c2c903ebfbf2085d15f39ab05a59b49b
                  JS_IDENTIFIER - d8e303aa43a859d01aa6f5c0c272bbc9
                JS_PROPERTY_ACCESS - 30d8fcf4dcd58d96283e415b6a8946d2
                  JS_IDENTIFIER - 846d1a646703c704e0437f24ed7b52eb
                JS_PROPERTY_ACCESS - 26e6b1dc52f06d84e9ad8f0eee421bf9
                  JS_IDENTIFIER - 944bcba7f90cf73635ead84eff167884"
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
          JSX_MAP_EXPRESSION - 7fb8daf000c351dd789d9f55a3395d5c
            JS_PROPERTY_ACCESS - 2f0a2b029743d9f9f62384c498d2d5d5
              JS_IDENTIFIER - e1bc922c4e4015ff66443180841a788c
            ATTRIBUTE_OTHER_JAVASCRIPT - f544b1d8a7dfe7f2a7995229f88db586
              JSX_ELEMENT - Card - card
                  ATTRIBUTE_VALUE - a9e817a9b2d30f5e6c88376d6ce9a54e"
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
