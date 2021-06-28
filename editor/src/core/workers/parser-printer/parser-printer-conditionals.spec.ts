import { applyPrettier } from 'utopia-vscode-common'
import { BakedInStoryboardUID } from '../../model/scene-utils'
import { foldParsedTextFile } from '../../shared/project-file-types'
import { clearParseResultUniqueIDsAndEmptyBlocks, testParseCode } from './parser-printer.test-utils'

describe('JSX Parser', () => {
  it('ternaries with basic values', () => {
    const code = applyPrettier(
      `
/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
export var App = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      layout={{ layoutSystem: 'pinSystem' }}
    >{false ? 'cake' : 'cookie'}
    </div>
  )
}
export var storyboard = (
  <Storyboard data-uid={'${BakedInStoryboardUID}'} layout={{ layoutSystem: 'pinSystem' }}>
    <Scene
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
    />
  </Storyboard>
)
`,
      false,
    ).formatted
    const parseResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
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
  it('ternaries with elements', () => {
    const code = applyPrettier(
      `
/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
export var App = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      layout={{ layoutSystem: 'pinSystem' }}
    >{false ? <div /> : <span />}
    </div>
  )
}
export var storyboard = (
  <Storyboard data-uid={'${BakedInStoryboardUID}'} layout={{ layoutSystem: 'pinSystem' }}>
    <Scene
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
    />
  </Storyboard>
)
`,
      false,
    ).formatted
    const parseResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
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
})
