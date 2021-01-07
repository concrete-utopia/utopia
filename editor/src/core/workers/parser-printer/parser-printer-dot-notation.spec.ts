import { applyPrettier } from './prettier-utils'
import {
  clearParseResultUniqueIDs,
  testParseCode,
  testParseThenPrint,
} from './parser-printer.test-utils'
import { BakedInStoryboardUID } from '../../model/scene-utils'
import { foldParsedTextFile } from '../../shared/project-file-types'

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
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid='scene-aaa'
      />
    </Storyboard>
  )
}
`,
      false,
    ).formatted
    const parseResult = clearParseResultUniqueIDs(testParseCode(code))
    foldParsedTextFile(
      (failure) => {
        fail(failure)
      },
      (success) => {
        expect(success.imports).toMatchSnapshot()
        expect(success.topLevelElements).toMatchSnapshot()
      },
      (unparsed) => {
        fail(unparsed)
      },
      parseResult,
    )
  })
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
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid='scene-aaa'
      />
    </Storyboard>
  )
}`
    const printedCode = `import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'
import * as Utopia from 'utopia-api'
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
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid='scene-aaa'
      />
    </Storyboard>
  )
}
`
    testParseThenPrint(originalCode, printedCode)
  })
})
