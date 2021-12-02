import { applyPrettier } from 'utopia-vscode-common'
import {
  clearParseResultUniqueIDsAndEmptyBlocks,
  testParseCode,
  testParseThenPrint,
} from './parser-printer.test-utils'
import { BakedInStoryboardUID } from '../../model/scene-utils'
import { foldParsedTextFile } from '../../shared/project-file-types'
import { TestAppUID, TestSceneUID } from '../../../components/canvas/ui-jsx.test-utils'

describe('JSX parser', () => {
  it('should parse out dot-notation element names', () => {
    const code = applyPrettier(
      `
import * as React from "react";
import * as Utopia from "utopia-api";

export var App = props => {
  return (
    <Utopia.View data-uid="aaa">
      {<div data-uid="bbb" />}
    </Utopia.View>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`,
      false,
    ).formatted
    const parseResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    foldParsedTextFile(
      (failure) => {
        throw new Error(JSON.stringify(failure))
      },
      (success) => {
        expect(success.imports).toMatchSnapshot()
        expect(success.topLevelElements).toMatchSnapshot()
      },
      (unparsed) => {
        throw new Error(JSON.stringify(unparsed))
      },
      parseResult,
    )
  })

  // eslint-disable-next-line jest/expect-expect
  it('dot-notated elements cycle fully back and forth', () => {
    const originalCode = `
import * as React from "react";
import * as Utopia from "utopia-api";
import { Scene, Storyboard, View } from "utopia-api";

export var App = props => {
  return (
    <Utopia.View data-uid="aaa">
      <View data-uid='bbb' />
    </Utopia.View>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene style={{ left: 0, top: 0, width: 400, height: 400 }} data-uid='${TestSceneUID}'>
        <App data-uid='${TestAppUID}' style={{ bottom: 0, left: 0, right: 0, top: 0 }} />
      </Scene>
    </Storyboard>
  )
}`
    const printedCode = `import * as React from 'react'
import * as Utopia from 'utopia-api'
import { Scene, Storyboard, View } from 'utopia-api'

export var App = (props) => {
  return (
    <Utopia.View data-uid='aaa'>
      <View data-uid='bbb' />
    </Utopia.View>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`
    testParseThenPrint('/index.js', originalCode, printedCode)
  })
})
