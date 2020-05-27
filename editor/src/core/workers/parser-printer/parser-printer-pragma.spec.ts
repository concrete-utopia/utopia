import { isRight } from '../../shared/either'
import { applyPrettier } from './prettier-utils'
import { testParseCode, testParseThenPrint } from './parser-printer-test-utils'
import { BakedInStoryboardUID } from '../../model/scene-utils'

describe('Parsing JSX Pragma:', () => {
  it('no pragma, no problem', () => {
    const code = `
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid={"xxx"} />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isRight(parsedPlainCode)) {
      expect(parsedPlainCode.value.jsxFactoryFunction).toEqual(null)
    } else {
      fail(parsedPlainCode.value)
    }
  })

  it('parses a simple pragma', () => {
    const code = `/** @jsx jsx */
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View, jsx } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid={"xxx"} />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isRight(parsedPlainCode)) {
      expect(parsedPlainCode.value.jsxFactoryFunction).toEqual('jsx')
    } else {
      fail(parsedPlainCode.value)
    }
  })

  it('parses and prints back the same code', () => {
    const code = applyPrettier(
      `/** @jsx jsx */
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Scene, Storyboard, Text, UtopiaUtils, View, jsx } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid={"xxx"} />
    };
    export var storyboard = (props) => {
      return (
        <Storyboard data-uid={'${BakedInStoryboardUID}'}>
          <Scene
            style={{ left: 0, top: 0, width: 400, height: 400 }}
            component={App}
            layout={{ layoutSystem: 'pinSystem' }}
            props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
            data-uid={'scene-aaa'}
          />
        </Storyboard>
      )
    }`,
      false,
    ).formatted
    testParseThenPrint(code, code)
  })

  it('parses a pragma with dot notation', () => {
    const code = `/** @jsx preact.h */
    import preact from "preact";
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid={"xxx"} />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isRight(parsedPlainCode)) {
      expect(parsedPlainCode.value.jsxFactoryFunction).toEqual('preact.h')
    } else {
      fail(parsedPlainCode.value)
    }
  })

  it('in case of multiple pragma declarations, the first one wins', () => {
    const code = `/** @jsx preact.h */
    /** @jsx jsx */
    import preact from "preact";
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid={"xxx"} />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isRight(parsedPlainCode)) {
      expect(parsedPlainCode.value.jsxFactoryFunction).toEqual('preact.h')
    } else {
      fail(parsedPlainCode.value)
    }
  })
})
