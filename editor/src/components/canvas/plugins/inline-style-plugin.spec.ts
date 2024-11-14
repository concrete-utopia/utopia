import * as EP from '../../../core/shared/element-path'
import { cssNumber } from '../../inspector/common/css-utils'
import {
  cssStyleProperty,
  cssStylePropertyNotFound,
  cssStylePropertyNotParsable,
} from '../canvas-types'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { InlineStylePlugin } from './inline-style-plugin'

describe('inline style plugin', () => {
  it('can parse style info from element', async () => {
    const editor = await renderTestEditorWithCode(
      `
import React from 'react'
import { Scene, Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='scene'
      commentId='scene'
      data-uid='scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
    >
      <div
        data-uid='div'
        style={{ display: 'flex', flexDirection: 'column', gap: '2rem'}}
      />
    </Scene>
  </Storyboard>
)

`,
      'await-first-dom-report',
    )

    const styleInfo = getStyleInfoFromInlineStyle(editor)

    expect(styleInfo).not.toBeNull()
    const { flexDirection, gap } = styleInfo!
    expect(flexDirection).toEqual(cssStyleProperty('column'))
    expect(gap).toEqual(cssStyleProperty(cssNumber(2, 'rem')))
  })

  it('can parse style info with missing/unparsable props', async () => {
    const editor = await renderTestEditorWithCode(
      `
  import React from 'react'
  import { Scene, Storyboard } from 'utopia-api'

  const gap = { small: '1rem' }

  export var storyboard = (
    <Storyboard data-uid='sb'>
      <Scene
        id='scene'
        commentId='scene'
        data-uid='scene'
        style={{
          width: 700,
          height: 759,
          position: 'absolute',
          left: 212,
          top: 128,
        }}
      >
        <div
          data-uid='div'
          style={{ display: 'flex', gap: gap.small }}
        />
      </Scene>
    </Storyboard>
  )
  
  `,
      'await-first-dom-report',
    )

    const styleInfo = getStyleInfoFromInlineStyle(editor)

    expect(styleInfo).not.toBeNull()
    const { flexDirection, gap } = styleInfo!
    expect(flexDirection).toEqual(cssStylePropertyNotFound())
    expect(gap).toEqual(cssStylePropertyNotParsable())
  })
})

function getStyleInfoFromInlineStyle(editor: EditorRenderResult) {
  const { jsxMetadata, projectContents, elementPathTree } = editor.getEditorState().editor

  const styleInfoReader = InlineStylePlugin.styleInfoFactory({
    projectContents: projectContents,
  })
  const styleInfo = styleInfoReader(EP.fromString('sb/scene/div'))
  return styleInfo
}
