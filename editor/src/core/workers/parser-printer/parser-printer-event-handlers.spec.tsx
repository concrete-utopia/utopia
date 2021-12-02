import { printCode, printCodeOptions } from './parser-printer'
import { applyPrettier } from 'utopia-vscode-common'
import { testParseCode } from './parser-printer.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'
import { isParseSuccess } from '../../shared/project-file-types'
import { TestAppUID, TestSceneUID } from '../../../components/canvas/ui-jsx.test-utils'

describe('JSX parser', () => {
  it('should preserve arrow functions in printed code', () => {
    const code = applyPrettier(
      `
import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Scene,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";

export var App = props => {
  return (
    <View data-uid='aaa' onClick={() => {console.log('hat')}}/>
  )
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App data-uid='${TestAppUID}' style={{ bottom: 0, left: 0, right: 0, top: 0 }} />
      </Scene>
    </Storyboard>
  )
}`,
      false,
    ).formatted
    const parseResult = testParseCode(code)
    if (isParseSuccess(parseResult)) {
      const printedCode = printCode(
        '/index.js',
        printCodeOptions(false, true, true),
        parseResult.imports,
        parseResult.topLevelElements,
        parseResult.jsxFactoryFunction,
        parseResult.exportsDetail,
      )

      const expectedCode = applyPrettier(
        `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Scene,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";

export var App = props => {
  return (
    <View
      data-uid='aaa'
      onClick={() => {
        console.log("hat");
      }}
    />
  );
};

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App data-uid='${TestAppUID}' style={{ bottom: 0, left: 0, right: 0, top: 0 }} />
      </Scene>
    </Storyboard>
  )
}
`,
        false,
      ).formatted
      expect(printedCode).toEqual(expectedCode)
    } else {
      throw new Error(JSON.stringify(parseResult))
    }
  })
})
