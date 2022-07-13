import {
  emptyComments,
  isJSXElement,
  isUtopiaJSXComponent,
  JSXAttributes,
  jsxAttributesFromMap,
  jsxAttributeValue,
  UtopiaJSXComponent,
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
            JSX_ARBITRARY_BLOCK
              JSX_ELEMENT - div - bbb
                JSX_ARBITRARY_BLOCK
                JSX_TEXT_BLOCK
                JSX_ARBITRARY_BLOCK"
      `)

      const aaaElement = findJSXElementAtStaticPath(
        getUtopiaJSXComponentsFromSuccess(parsedCode),
        EP.dynamicPathToStaticPath(EP.elementPath([['App'], ['aaa']])),
      )
      const aaaJSXArbBlock = aaaElement?.children[0]
      if (aaaJSXArbBlock?.type === 'JSX_ARBITRARY_BLOCK') {
        expect(aaaJSXArbBlock.definedElsewhere).toMatchInlineSnapshot(`
          Array [
            "cake",
            "React",
            "utopiaCanvasJSXLookup",
          ]
        `)
      } else {
        throw new Error('Was not a JSX_ARBITRARY_BLOCK as expected.')
      }

      const bbbElement = findJSXElementAtStaticPath(
        getUtopiaJSXComponentsFromSuccess(parsedCode),
        EP.dynamicPathToStaticPath(EP.elementPath([['App'], ['aaa', 'bbb']])),
      )
      const bbbJSXArbBlock = bbbElement?.children[2]
      if (bbbJSXArbBlock?.type === 'JSX_ARBITRARY_BLOCK') {
        expect(bbbJSXArbBlock.definedElsewhere).toMatchInlineSnapshot(`
          Array [
            "cake",
          ]
        `)
      } else {
        throw new Error('Was not a JSX_ARBITRARY_BLOCK as expected.')
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
          const expectedProps: JSXAttributes = jsxAttributesFromMap({
            style: jsxAttributeValue(
              {
                backgroundColor: 'green',
                position: 'absolute',
              },
              emptyComments,
            ),
            'data-uid': jsxAttributeValue('xxx', emptyComments),
          })
          expect(topComponent.rootElement.props).toEqual(expectedProps)
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
          JSX_ARBITRARY_BLOCK
            JSX_ELEMENT - Card - card"
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
          JSX_ARBITRARY_BLOCK
            JSX_ELEMENT - Card - card"
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
