/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "testParseThenPrint"] }] */
import { applyPrettier } from 'utopia-vscode-common'
import { testParseCode, testParseThenPrint } from './parser-printer.test-utils'
import { BakedInStoryboardUID } from '../../model/scene-utils'
import { isParseSuccess } from '../../shared/project-file-types'

describe('Parsing JSX Pragma:', () => {
  it('no pragma, no problem', () => {
    const code = `
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx" />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isParseSuccess(parsedPlainCode)) {
      expect(parsedPlainCode.jsxFactoryFunction).toEqual(null)
    } else {
      throw new Error(JSON.stringify(parsedPlainCode))
    }
  })

  it('parses a simple pragma', () => {
    const code = `
    /** @jsx jsx */
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx" />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isParseSuccess(parsedPlainCode)) {
      expect(parsedPlainCode.jsxFactoryFunction).toEqual('jsx')
    } else {
      throw new Error(JSON.stringify(parsedPlainCode))
    }
  })

  it('parses and prints back the same code', () => {
    const code = applyPrettier(
      `
    /** @jsx jsx */
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Scene, Storyboard, Text, UtopiaUtils, View } from "utopia-api";

    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx" />
    };
    
    export var storyboard = (props) => {
      return (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene style={{ left: 0, top: 0, width: 400, height: 400 }} data-uid='scene-aaa'>
            <App style={{ bottom: 0, left: 0, right: 0, top: 0 }} data-uid='app' />
          </Scene>
        </Storyboard>
      )
    }`,
      false,
    ).formatted
    testParseThenPrint('/index.js', code, code)
  })

  it('parses a pragma with dot notation', () => {
    const code = `/** @jsx preact.h */
    import preact from "preact";
    import Button, { LABEL } from "./src/components";
    import * as React from "react";
    import { Ellipse, Image, Rectangle, Text, UtopiaUtils, View } from "utopia-api";
    export var App = props => {
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx" />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isParseSuccess(parsedPlainCode)) {
      expect(parsedPlainCode.jsxFactoryFunction).toEqual('preact.h')
    } else {
      throw new Error(JSON.stringify(parsedPlainCode))
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
        return <View style={{ "backgroundColor": "green", "position": "absolute" }} data-uid="xxx" />
    };`
    const parsedPlainCode = testParseCode(code)
    if (isParseSuccess(parsedPlainCode)) {
      expect(parsedPlainCode.jsxFactoryFunction).toEqual('preact.h')
    } else {
      throw new Error(JSON.stringify(parsedPlainCode))
    }
  })
})
