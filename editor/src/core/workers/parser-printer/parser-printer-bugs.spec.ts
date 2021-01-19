import {
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
  testParseCode,
  testParseThenPrint,
} from './parser-printer.test-utils'
import { objectMap, omit } from '../../shared/object-utils'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../model/scene-utils'
import { isParseSuccess } from '../../shared/project-file-types'
import { emptyComments } from './parser-printer-comments'

export function stripUnhelpfulFields(value: any): any {
  switch (typeof value) {
    case 'object': {
      if (Array.isArray(value)) {
        return value.map(stripUnhelpfulFields)
      } else {
        return objectMap(
          stripUnhelpfulFields,
          omit(['sourceMap', 'javascript', 'uniqueID', 'code'], value),
        )
      }
    }
    default:
      return value
  }
}

describe('JSX parser', () => {
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
      fail(parsedCode)
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
          fail('Root element not a JSX element.')
        }
      } else {
        fail('Not a component.')
      }
    } else {
      fail(parsedPlainCode)
    }
  })
})

describe('JSX printer', () => {
  it('reprints spread object values correctly', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

const myCoolTheme = {}

export var App = (props) => {
  return <View data-uid='xxx' style={{ ...myCoolTheme, backgroundColor: 'purple' }} />
}

export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`
    testParseThenPrint(code, code)
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

export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`
    testParseThenPrint(code, code)
  })

  it('object property names with special characters should be printed in string quotes', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

const color = 'white'

export var App = (props) => {
  return (
    <View
      data-uid='aaa'
      css={{ backgroundColor: 'regular propety, without quotes', '& :hover': { color: color } }}
    />
  )
}

export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`
    testParseThenPrint(code, code)
  })

  it('definedWithin and definedElsewhere are mutually exclusive properties', () => {
    const code = `import * as React from 'react'
import { Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (
    <View data-uid='aaa' css={{ backgroundColor: 'red' }}>
      <View data-uid='bbb'>{elements.map((e) => null)}</View>
    </View>
  )
}

const elements = []
export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`
    testParseThenPrint(code, code)
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
      <View data-uid='bbb'>{elements.map((e) => null)}</View>
    </View>
  )
}

const elements = []
export var ${BakedInStoryboardVariableName} = <Storyboard data-uid='${BakedInStoryboardUID}' />
`
    testParseThenPrint(code, code)
  })
})
