import type { JSXAttributes } from 'utopia-shared/src/types'
import * as EP from '../../../core/shared/element-path'
import {
  getJSXElementFromProjectContents,
  StoryboardFilePath,
} from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { TailwindPlugin } from './tailwind-style-plugin'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { TailwindConfigPath } from '../../../core/tailwind/tailwind-config'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'

const Project = (style: Record<string, unknown>) =>
  createModifiedProject({
    [StoryboardFilePath]: `
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
        style={${JSON.stringify(style)}}
      />
    </Scene>
  </Storyboard>
)

`,
    [TailwindConfigPath]: `
    const TailwindConfig = {
        content: [],
        theme: { extend: { gap: { enormous: '222px' } } }
    }
    export default TailwindConfig
`,
  })

describe('tailwind style plugin', () => {
  it('can normalize inline style', async () => {
    const editor = await renderTestEditorWithModel(
      Project({
        top: 2,
        left: 2,
        width: 100,
        height: 100,
        backgroundColor: 'blue',
        display: 'flex',
        flexDirection: 'row',
        gap: '12px',
      }),
      'await-first-dom-report',
    )
    const target = EP.fromString('sb/scene/div')
    const normalizedEditor = TailwindPlugin(null).normalizeFromInlineStyle(
      editor.getEditorState().editor,
      [target],
    )

    const normalizedElement = getJSXElementFromProjectContents(
      target,
      normalizedEditor.projectContents,
    )!

    expect(formatJSXAttributes(normalizedElement.props)).toEqual({
      className: 'flex-row gap-[12px]',
      'data-uid': 'div',
      style: { backgroundColor: 'blue', display: 'flex', height: 100, left: 2, top: 2, width: 100 },
    })
  })
  it('can normalize inline style with custom Tailwind config', async () => {
    const editor = await renderTestEditorWithModel(
      Project({
        top: 2,
        left: 2,
        width: 100,
        height: 100,
        backgroundColor: 'blue',
        display: 'flex',
        flexDirection: 'row',
        gap: '222px',
      }),
      'await-first-dom-report',
    )
    const target = EP.fromString('sb/scene/div')
    const normalizedEditor = TailwindPlugin(
      getTailwindConfigCached(editor.getEditorState().editor),
    ).normalizeFromInlineStyle(editor.getEditorState().editor, [target])

    const normalizedElement = getJSXElementFromProjectContents(
      target,
      normalizedEditor.projectContents,
    )!

    expect(formatJSXAttributes(normalizedElement.props)).toEqual({
      className: 'flex-row gap-enormous',
      'data-uid': 'div',
      style: { backgroundColor: 'blue', display: 'flex', height: 100, left: 2, top: 2, width: 100 },
    })
  })
})

function formatJSXAttributes(attributes: JSXAttributes) {
  return attributes.reduce((acc: Record<string, unknown>, attribute) => {
    if (attribute.type === 'JSX_ATTRIBUTES_SPREAD' || attribute.value.type !== 'ATTRIBUTE_VALUE') {
      return acc
    }
    return { ...acc, [attribute.key]: attribute.value.value }
  }, {})
}
